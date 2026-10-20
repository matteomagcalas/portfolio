"""ESPN data pulls. Finished seasons are cached forever; live data (projections,
injuries, rosters, schedules) is re-pulled when older than MAX_AGE_HOURS."""
import re
import time
import unicodedata
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd
import requests

SEASON = 2027  # ESPN labels a season by its end year: 2027 = 2026-27
FIRST_SEASON = 2022
MAX_AGE_HOURS = 6  # in season: box scores and injury reports change daily
DATA = Path(__file__).parent / "data"

FANTASY = "https://lm-api-reads.fantasy.espn.com/apis/v3/games/fba/seasons/{}/segments/0/leaguedefaults/3?view=kona_player_info"
SITE = "https://site.api.espn.com/apis/site/v2/sports/basketball/nba"
FILTER = '{"players":{"limit":2000,"sortPercOwned":{"sortPriority":1,"sortAsc":false}}}'
STAT = {"0": "PTS", "1": "BLK", "2": "STL", "3": "AST", "6": "REB", "11": "TO", "13": "FGM", "14": "FGA",
        "15": "FTM", "16": "FTA", "17": "3PM", "18": "3PA", "40": "MPG", "41": "GS", "42": "GP"}
SLOT = {0: "PG", 1: "SG", 2: "SF", 3: "PF", 4: "C"}
SEASON_OUT = re.compile(rf"season-ending|(miss|out for) the (remainder of the |rest of the |entire )?({SEASON - 1}-{SEASON % 100} )?season", re.I)


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


def _game_rows(players, season):
    """Box score per player per game played: day = ESPN scoring period (1 = opening night), team = team that night."""
    rows = []
    for e in players:
        p = e["player"]
        for s in p.get("stats", []):
            g = s.get("stats") or {}
            if s.get("statSplitTypeId") == 5 and s.get("seasonId") == season and s.get("statSourceId") == 0 and g.get("42"):
                rows.append({"id": p["id"], "day": s["scoringPeriodId"], "team_id": s.get("proTeamId"),
                             **{name: g.get(k, 0.0) for k, name in STAT.items() if name != "GP"}})
    return pd.DataFrame(rows, columns=["id", "day", "team_id", *[n for n in STAT.values() if n != "GP"]])


def games(season):
    """Game logs for a season: finished seasons cached forever, the current one written by refresh()."""
    f = DATA / f"games_{season}.csv"
    if not f.exists():
        _game_rows(_players(season), season).to_csv(f, index=False)
    # explicit dtypes: before opening night the file is just a header, which pandas would read as text
    return pd.read_csv(f, dtype={"id": "int64", "day": "int64", "team_id": "int64",
                                 **{n: "float64" for n in STAT.values() if n != "GP"}})


YAHOO = ("https://pub-api-rw.fantasysports.yahoo.com/fantasy/v2/game/nba/players;sort=DA_AP;start={};count=50;"
         "out=draft_analysis?format=json")


def _norm(name):
    """'Nikola Jokić Jr.' -> 'nikola jokic': matches player names across ESPN and Yahoo."""
    name = unicodedata.normalize("NFKD", name).encode("ascii", "ignore").decode().lower()
    name = re.sub(r"[^a-z ]", "", name.replace("-", " "))
    return " ".join(w for w in name.split() if w not in {"jr", "sr", "ii", "iii", "iv"})


def _yahoo_adp(max_players=400):
    """Yahoo average draft position (public draft-analysis feed), keyed by normalized name."""
    rows = []
    for start in range(0, max_players, 50):
        ps = _get(YAHOO.format(start), headers={"User-Agent": "Mozilla/5.0"})["fantasy_content"]["game"][1]["players"]
        if not isinstance(ps, dict) or not ps.get("count"):
            break
        for k in range(ps["count"]):
            info, da = ps[str(k)]["player"]
            info = {key: v for d in info if isinstance(d, dict) for key, v in d.items()}
            pick = {key: v for d in da["draft_analysis"] for key, v in d.items()}["average_pick"]
            if pick not in ("-", None, ""):
                rows.append({"key": _norm(info["name"]["full"]), "yahoo_adp": float(pick)})
    return pd.DataFrame(rows).drop_duplicates("key")


def _flat(parts):
    """Yahoo splits one record over a list of one-key dicts: merge them."""
    return {k: v for p in parts if isinstance(p, dict) for k, v in p.items()}


def yahoo_league(league_id):
    """Live rosters and recent moves of a public Yahoo league (no login needed).
    Returns (rosters: team_id, team, key, name, slot (today's Yahoo slot: PG..Util, BN, IL); moves: time, type, name, from, to); key = normalized name."""
    url = f"https://pub-api-rw.fantasysports.yahoo.com/fantasy/v2/league/nba.l.{league_id}/{{}}?format=json"
    h = {"User-Agent": "Mozilla/5.0"}
    teams = _get(url.format("teams/roster"), headers=h)["fantasy_content"]["league"][1]["teams"]
    rosters = []
    for k in range(teams["count"]):
        t = teams[str(k)]["team"]
        info, ps = _flat(t[0]), t[1]["roster"]["0"]["players"]
        for j in range(ps.get("count", 0) if isinstance(ps, dict) else 0):
            p = ps[str(j)]["player"]
            name = _flat(p[0])["name"]["full"]
            slot = _flat(p[1]["selected_position"]).get("position", "") if len(p) > 1 else ""
            rosters.append({"team_id": int(info["team_id"]), "team": info["name"], "key": _norm(name), "name": name,
                            "slot": slot})
    tx = _get(url.format("transactions"), headers=h)["fantasy_content"]["league"][1]["transactions"]
    moves = []
    for k in range(tx.get("count", 0) if isinstance(tx, dict) else 0):
        t = tx[str(k)]["transaction"]
        ps = t[1].get("players", {}) if len(t) > 1 else {}
        for j in range(ps.get("count", 0)):
            p = ps[str(j)]["player"]
            move = p[1]["transaction_data"]
            move = move[0] if isinstance(move, list) else move
            moves.append({"time": pd.Timestamp(int(t[0]["timestamp"]), unit="s", tz="UTC").tz_convert("America/New_York"),
                          "type": move["type"], "name": _flat(p[0])["name"]["full"],
                          "from": move.get("source_team_name", move.get("source_type", "")),
                          "to": move.get("destination_team_name", move.get("destination_type", ""))})
    return pd.DataFrame(rosters), pd.DataFrame(moves, columns=["time", "type", "name", "from", "to"])


def _players(season):
    return _get(FANTASY.format(season), headers={"X-Fantasy-Filter": FILTER})["players"]


def refresh(force=False):
    DATA.mkdir(exist_ok=True)
    stamp = DATA / "updated.txt"
    if not force and stamp.exists() and (DATA / f"games_{SEASON}.csv").exists() and time.time() - stamp.stat().st_mtime < MAX_AGE_HOURS * 3600:
        return

    history = DATA / "history.csv"  # finished seasons never change, pull once
    if not history.exists() or "GS" not in pd.read_csv(history, nrows=0).columns:
        rows = [r for s in range(FIRST_SEASON, SEASON) for r in _stat_rows(_players(s), s)]
        pd.DataFrame(rows).drop_duplicates(["id", "season", "source"]).to_csv(history, index=False)

    current = _players(SEASON)
    pd.DataFrame(_stat_rows(current, SEASON)).to_csv(DATA / "current.csv", index=False)  # last season + this season's ESPN proj
    _game_rows(current, SEASON).to_csv(DATA / f"games_{SEASON}.csv", index=False)  # this season's box scores so far

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
            # ESPN gives UTC; a 10 pm ET tip is the next day in UTC. Fantasy days are US Eastern.
            et = pd.Timestamp(g["date"]).tz_convert("America/New_York")
            games.append({"team_id": tid, "date": f"{et:%Y-%m-%d}"})
    pd.DataFrame(games).to_csv(DATA / "schedule.csv", index=False)
    pd.DataFrame(depth, columns=["id", "team_id", "pos", "slot"]).to_csv(DATA / "depth.csv", index=False)

    players = pd.DataFrame([{
        "id": e["player"]["id"],
        "name": e["player"]["fullName"],
        "team": teams.get(e["player"].get("proTeamId"), "FA"),
        "team_id": e["player"].get("proTeamId", 0),
        "pos": "/".join(v for k, v in SLOT.items() if k in e["player"].get("eligibleSlots", [])),
        "primary": SLOT.get(e["player"].get("defaultPositionId", 0) - 1),  # ESPN: 1=PG ... 5=C
        "age": ages.get(e["player"]["id"]),
        "adp": (e["player"].get("ownership") or {}).get("averageDraftPosition"),
    } for e in current])
    try:  # draft on Yahoo, so use Yahoo's ADP; ESPN's stays as the fallback if Yahoo is unreachable
        y = _yahoo_adp()
        players["adp"] = players.name.map(_norm).map(y.set_index("key").yahoo_adp)
    except Exception:
        pass
    players.to_csv(DATA / "players.csv", index=False)

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
        "games": games(SEASON),
        "preseason": pd.read_csv(DATA / "preseason.csv"),  # frozen before opening night: the in-season prior
        "preseason_depth": pd.read_csv(DATA / "preseason_depth.csv"),
        "updated": (DATA / "updated.txt").read_text(),
    }


if __name__ == "__main__":
    refresh(force=True)
    d = load()
    print({k: (len(v) if hasattr(v, "__len__") and not isinstance(v, str) else v) for k, v in d.items()})
    print(d["stats"].groupby(["season", "source"]).size())
