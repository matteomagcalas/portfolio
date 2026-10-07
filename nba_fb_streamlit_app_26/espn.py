"""ESPN data pulls. Finished seasons are cached forever; live data (projections,
injuries, rosters, schedules) is re-pulled when older than MAX_AGE_HOURS."""
import re
import time
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd
import requests

SEASON = 2027  # ESPN labels a season by its end year: 2027 = 2026-27
FIRST_SEASON = 2022
MAX_AGE_HOURS = 12
DATA = Path(__file__).parent / "data"

FANTASY = "https://lm-api-reads.fantasy.espn.com/apis/v3/games/fba/seasons/{}/segments/0/leaguedefaults/3?view=kona_player_info"
SITE = "https://site.api.espn.com/apis/site/v2/sports/basketball/nba"
FILTER = '{"players":{"limit":2000,"sortPercOwned":{"sortPriority":1,"sortAsc":false}}}'
STAT = {"0": "PTS", "1": "BLK", "2": "STL", "3": "AST", "6": "REB", "11": "TO", "13": "FGM", "14": "FGA",
        "15": "FTM", "16": "FTA", "17": "3PM", "18": "3PA", "40": "MPG", "41": "GS", "42": "GP"}
SLOT = {0: "PG", 1: "SG", 2: "SF", 3: "PF", 4: "C"}
SEASON_OUT = re.compile(rf"(miss|out for) the (remainder of the |rest of the |entire )?({SEASON - 1}-{SEASON % 100} )?season", re.I)


def _get(url, **kw):
    for attempt in range(3):
        try:
            r = requests.get(url, timeout=60, **kw)
            r.raise_for_status()
            return r.json()
        except requests.RequestException:
            if attempt == 2:
                raise
            time.sleep(2)


def _stat_rows(players, season):
    """Full-season per-game lines: source 'actual' or 'espn' (ESPN preseason projection).
    A season's player list carries each player's team at the end of that season."""
    rows = []
    for e in players:
        p = e["player"]
        for s in p.get("stats", []):
            if s.get("statSplitTypeId") == 0 and s.get("statSourceId") in (0, 1) and s.get("averageStats"):
                a = s["averageStats"]
                rows.append({"id": p["id"], "season": s["seasonId"], "source": "actual" if s["statSourceId"] == 0 else "espn",
                             "team_id": p.get("proTeamId") if s["seasonId"] == season and s["statSourceId"] == 0 else None,
                             **{name: a.get(k, 0.0) for k, name in STAT.items()}})
    return rows


def _players(season):
    return _get(FANTASY.format(season), headers={"X-Fantasy-Filter": FILTER})["players"]


def refresh(force=False):
    DATA.mkdir(exist_ok=True)
    stamp = DATA / "updated.txt"
    if not force and stamp.exists() and time.time() - stamp.stat().st_mtime < MAX_AGE_HOURS * 3600:
        return

    history = DATA / "history.csv"  # finished seasons never change, pull once
    if not history.exists() or "GS" not in pd.read_csv(history, nrows=0).columns:
        rows = [r for s in range(FIRST_SEASON, SEASON) for r in _stat_rows(_players(s), s)]
        pd.DataFrame(rows).drop_duplicates(["id", "season", "source"]).to_csv(history, index=False)

    current = _players(SEASON)
    pd.DataFrame(_stat_rows(current, SEASON)).to_csv(DATA / "current.csv", index=False)  # last season + this season's ESPN proj

    teams = {int(t["team"]["id"]): t["team"]["abbreviation"]
             for t in _get(f"{SITE}/teams")["sports"][0]["leagues"][0]["teams"]}

    ages, games, depth = {}, [], []
    for tid in teams:
        # depth chart: order at each position; slot 0 = projected starter
        for pos, v in _get(f"{SITE}/teams/{tid}/depthcharts")["depthchart"][0]["positions"].items():
            depth += [{"id": int(a["id"]), "team_id": tid, "pos": pos, "slot": i} for i, a in enumerate(v["athletes"])]
        for a in _get(f"{SITE}/teams/{tid}/roster")["athletes"]:
            ages[int(a["id"])] = a.get("age")
        for g in _get(f"{SITE}/teams/{tid}/schedule", params={"season": SEASON, "seasontype": 2}).get("events", []):
            games.append({"team_id": tid, "date": g["date"][:10]})
    pd.DataFrame(games).to_csv(DATA / "schedule.csv", index=False)
    pd.DataFrame(depth, columns=["id", "team_id", "pos", "slot"]).to_csv(DATA / "depth.csv", index=False)

    pd.DataFrame([{
        "id": e["player"]["id"],
        "name": e["player"]["fullName"],
        "team": teams.get(e["player"].get("proTeamId"), "FA"),
        "team_id": e["player"].get("proTeamId", 0),
        "pos": "/".join(v for k, v in SLOT.items() if k in e["player"].get("eligibleSlots", [])),
        "primary": SLOT.get(e["player"].get("defaultPositionId", 0) - 1),  # ESPN: 1=PG ... 5=C
        "age": ages.get(e["player"]["id"]),
        "adp": (e["player"].get("ownership") or {}).get("averageDraftPosition"),
    } for e in current]).to_csv(DATA / "players.csv", index=False)

    inj = []
    for t in _get(f"{SITE}/injuries")["injuries"]:
        for i in t["injuries"]:
            note = i.get("shortComment", "")
            inj.append({
                "id": int(re.search(r"/id/(\d+)", i["athlete"]["links"][0]["href"]).group(1)),
                "status": i["status"],
                "return_date": i.get("details", {}).get("returnDate"),
                "season_out": bool(SEASON_OUT.search(note + " " + i.get("longComment", ""))),
                "note": note,
            })
    pd.DataFrame(inj, columns=["id", "status", "return_date", "season_out", "note"]).to_csv(DATA / "injuries.csv", index=False)

    stamp.write_text(datetime.now(timezone.utc).isoformat())


def load():
    stats = pd.concat([pd.read_csv(DATA / "history.csv"), pd.read_csv(DATA / "current.csv")]).drop_duplicates(["id", "season", "source"])
    return {
        "stats": stats,
        "players": pd.read_csv(DATA / "players.csv"),
        "injuries": pd.read_csv(DATA / "injuries.csv"),
        "schedule": pd.read_csv(DATA / "schedule.csv"),
        "depth": pd.read_csv(DATA / "depth.csv"),
        "updated": (DATA / "updated.txt").read_text(),
    }


if __name__ == "__main__":
    refresh(force=True)
    d = load()
    print({k: (len(v) if hasattr(v, "__len__") and not isinstance(v, str) else v) for k, v in d.items()})
    print(d["stats"].groupby(["season", "source"]).size())
