"""NBA Fantasy Hub: draft board, team view and waiver helper. Run: streamlit run app.py"""
import hashlib
import importlib
from pathlib import Path

import numpy as np
import pandas as pd
import streamlit as st

import espn
import model

# A deploy can update app.py while the server keeps the old espn/model modules loaded (AttributeError on a new
# function). Reloading them each run costs milliseconds and guarantees the code on disk is the code that runs.
espn, model = importlib.reload(espn), importlib.reload(model)

st.set_page_config(page_title="NBA Fantasy Hub", page_icon="🏀", layout="wide")
# Streamlit's default ~6rem top padding wastes board space; keep just enough to clear the toolbar
st.markdown("""<style>
[data-testid="stMainBlockContainer"], .block-container { padding-top: 1.5rem; }
[data-testid="stSidebarUserContent"] { padding-top: 1rem; }
</style>""", unsafe_allow_html=True)


CODE = hashlib.md5(Path(model.__file__).read_bytes() + Path(espn.__file__).read_bytes()).hexdigest()
ESPN_IMG = "https://a.espncdn.com/combiner/i?img="  # resizes ESPN headshots/logos server side


@st.cache_data(ttl=900, show_spinner="Pulling latest ESPN data (about a minute the first time)...")
def data(code):  # code: espn.py/model.py version, so a push never serves data loaded by older code
    espn.refresh()
    return espn.load()


def this_week():
    """Fantasy week Monday-Sunday, US Eastern."""
    today = pd.Timestamp.now(tz="America/New_York").tz_localize(None).normalize()
    start = today - pd.Timedelta(days=today.weekday())
    return start, start + pd.Timedelta(days=6)


@st.cache_data(show_spinner=False)
def week_board(cats, pool_size, updated, code):
    """This fantasy week's category TOTALS per player: box scores so far + expected remaining games x per-game
    projection (with the starting-lineup minutes boost), valued like the season board but on the totals."""
    d = data(code)
    v = board(cats, pool_size, updated, code).reset_index()
    boost = ((v.MPG + v.opp_min) / v.MPG).where(v.MPG > 0, 1)
    per = v.assign(**{c: v[c] * boost for c in model.COUNT}, MPG=v.MPG + v.opp_min)
    start, end = this_week()
    wk = model.week(per, d["games"], d["schedule"], d["injuries"], d["players"], start, end,
                    pd.to_datetime(d["schedule"].date).min())
    # value on totals: GP 82 = no availability scaling (it's already in the totals); pool picked by total minutes
    tot = wk.assign(GP=82.0, MPG=wk.MPG.fillna(0) * wk.GP).reset_index(names="id")
    val = model.value(tot, list(cats), pool_size).set_index("id")
    out = wk.join(val[[f"z_{c}" for c in cats] + ["value"]])
    out["Rank"] = out.value.rank(ascending=False, method="first").astype(int)
    return out


def season_day(d):
    """ESPN scoring period for today: 1 = opening night (the schedule's first date)."""
    opener = pd.to_datetime(d["schedule"].date).min()
    return (pd.Timestamp.now(tz="America/New_York").tz_localize(None).normalize() - opener).days + 1


@st.cache_data(show_spinner=False)
def board(cats, pool_size, updated, code):  # code: model/espn version, so a push recomputes the board
    d = data(CODE)
    # Rest of season: the preseason projection (frozen the night before tip-off) updated with every game
    # played so far, as much as the backtest says games should count (model.in_season). Before opening
    # night there are no games and this is just the preseason projection.
    pre, games = d["preseason"], d["games"]
    today = season_day(d)
    proj = model.in_season(pre.assign(health=pre.health / model.AVAIL_SCALE), games, today, **model.IN_SEASON)
    proj["GP"] = proj.health * model._games_left(proj.id, d["injuries"], d["schedule"], d["players"], None).values
    # Who's on the board: ESPN projects him, Yahoo leagues draft him, or he's on a roster with a real NBA sample
    # last season (30+ games, 500+ minutes). Keeps out 10-game G League call-ups, whose stints at 30 MPG
    # would otherwise project as full-season starters.
    s, pl = d["stats"], d["players"]
    last = s[(s.source == "actual") & (s.season == espn.SEASON - 1)]
    sample = last.id[(last.GP >= 30) & (last.GP * last.MPG >= 500)]
    rostered = pl.id[pl.team_id > 0]
    keep = (set(s.id[(s.source == "espn") & (s.season == espn.SEASON) & (s.MPG > 0)]) | set(pl.id[pl.adp.notna()])
            | (set(sample) & set(rostered)) | set(games.id))  # plus anyone who has played this season
    proj = proj[proj.id.isin(keep)]
    pre_rank = model.value(pre[pre.id.isin(keep)], list(cats), pool_size).id.reset_index(drop=True)
    pre_rank = pd.Series(pre_rank.index + 1, index=pre_rank.values)
    # Opportunity: bench player now in the starting lineup (or a starter now benched). Starter = first healthy name at
    # each spot on ESPN's depth chart, so the next man up counts as soon as a starter is ruled out.
    out_now = set(d["injuries"].id[d["injuries"].status == "Out"])
    dc = d["depth"][~d["depth"].id.isin(out_now)].sort_values("slot")
    starter_now = pd.Series(1.0, index=dc.groupby(["team_id", "pos"]).id.first().unique())
    pre_start = pd.Series(1.0, index=d["preseason_depth"].query("slot == 0").id.unique())
    pre_start = pre_start.reindex(pre.id).fillna(0)  # preseason role: starter or bench, worth 4 games
    # an injured starter keeps his spot here: he returns to it (past seasons: 'starting now' = his first game back)
    held = d["depth"][d["depth"].id.isin(out_now) & (d["depth"].slot == 0)].id
    promo = model.promotion(games, today, pd.concat([starter_now, pd.Series(1.0, index=held.unique())]),
                            model.IN_SEASON["tau_m"], pre_start)
    # rest of season: role kept or lost (ROLE_ROS); next 2 weeks: the fresh-change extra on top (OPP_MIN_PER_START)
    proj = model.with_role(proj.set_index("id"), promo, model.ROLE_ROS).reset_index()
    proj["opp_min"] = (proj.id.map(promo).fillna(0) * model.OPP_MIN_PER_START).where(~proj.id.isin(out_now), 0)
    v = model.value(proj, list(cats), pool_size)
    inj = d["injuries"].drop_duplicates("id").set_index("id")
    v = v.merge(d["players"], on="id", how="left")
    status = inj.status.map({"Out": "O", "Day-To-Day": "DTD"}).fillna(inj.status)  # anything new stays as-is
    back = inj.return_date.map(lambda r: f"{pd.Timestamp(r):%m/%d}" if isinstance(r, str) else "")
    v["Injury"] = v.id.map((status + " " + back.where(~inj.season_out.astype(bool), "season")).str.strip()).fillna("")
    v["note"] = v.id.map(inj.note).fillna("")
    out = v.GP == 0  # out for the season: no stats to project
    v.loc[out, ["MPG", "PTS", "REB", "AST", "STL", "BLK", "3PM", "FG%", "FT%", "TO", "FGM", "FGA", "FTM", "FTA",
               *[f"z_{c}" for c in cats]]] = None
    v["Rank"] = range(1, len(v) + 1)
    v["ADPval"] = v.adp - v.Rank  # + = we rank them ahead of the market (a steal)
    v["PreRank"] = v.id.map(pre_rank)
    v["RankChg"] = v.PreRank - v.Rank  # + = climbed since preseason
    primary = v.primary.fillna(v.pos.str.split("/").str[0]).map({"PG": "G", "SG": "G", "SF": "F", "PF": "F", "C": "C"}).fillna("")
    v["PosRank"] = primary + (v.groupby(primary).cumcount() + 1).astype(str)
    v["Photo"] = v.id.map(lambda i: f"{ESPN_IMG}/i/headshots/nba/players/full/{i}.png&w=96&h=70")
    v["Logo"] = v.team.map(lambda t: f"{ESPN_IMG}/i/teamlogos/nba/500/{t.lower()}.png&w=40&h=40"
                           if isinstance(t, str) and t != "FA" else None)
    return v.set_index("id")


@st.cache_data(ttl=600, show_spinner="Syncing your Yahoo league...")
def league(league_id, code):  # code: espn.py version, so a push never serves rosters cached by older code
    return espn.yahoo_league(league_id)


POSITIONS = ["PG", "SG", "G", "SF", "PF", "F", "C"]


def pos_match(df, picked):
    """G = any guard (PG/SG), F = any forward (SF/PF)."""
    expand = {"G": ["PG", "SG"], "F": ["SF", "PF"]}
    wanted = {p for x in picked for p in expand.get(x, [x])}
    return df.pos.fillna("").str.split("/").map(lambda ps: bool(wanted & set(ps)))


def shade(x):
    """x in [-1, 1] -> green (good) / red (bad) background, stronger further from 0."""
    if pd.isna(x) or x == 0:
        return ""
    return f"background-color: rgba({'34,160,80' if x > 0 else '220,60,60'},{min(abs(x), 1) * 0.75:.2f})"


def board_style(df, z, cats):
    """Rank: green when we rank a player ahead of ADP (a steal), red when behind.
    Category cells: the player's z-score vs the draftable pool, the same number that drives Value."""
    styles = pd.DataFrame("", index=df.index, columns=df.columns)
    if "ADPval" in df:
        styles["Rank"] = styles["ADPval"] = (df.ADPval / 30).map(shade)  # full color at 30+ spots
    if "RankChg" in df:
        styles["Rank"] = styles["RankChg"] = (df.RankChg / 30).map(shade)
    for c in cats:
        if c in df:
            styles[c] = (z.loc[df.index, f"z_{c}"] / 2.5).map(shade)  # full color at +/-2.5 sd
    if "Injury" in df:
        styles["Injury"] = df.Injury.map(injury_badge)
    return styles


WHOLE = {"Rank", "PreRank", "adp", "age", "GP", "gp_so_far", "g_played", "g_left"}


def styled(df, z, cats):
    """Board coloring plus number formats. A Styler ignores column_config formats (pandas would show 6 decimals),
    so formats live here: whole numbers for ranks/games, 3 decimals for FG%/FT%, signed changes, 2 decimals otherwise."""
    fmt = {}
    for c in df.select_dtypes("number").columns:
        fmt[c] = ("{:.0f}" if c in WHOLE else "{:.3f}" if c in ("FG%", "FT%") else
                  "{:+.0f}" if c in ("RankChg", "ADPval") else "{:.2f}")
    return df.style.apply(board_style, z=z, cats=cats, axis=None).format(fmt, na_rep="")


def injury_badge(s):
    if not s:
        return ""
    bad = s.startswith("O")
    return (f"background-color: {'rgba(220,60,60,0.35)' if bad else 'rgba(245,166,35,0.30)'}; "
            f"color: {'#FF8A8A' if bad else '#FFC266'}; font-weight: 600")


SLOTS = ["PG", "SG", "G", "SF", "PF", "F", "C", "Util", "Util", "Util"]  # this league: 1 C, 3 Util
SLOT_OK = {"G": {"PG", "SG"}, "F": {"SF", "PF"}, "Util": {"PG", "SG", "SF", "PF", "C"}}


def fits(pos, slot):
    return bool(set(str(pos).split("/")) & SLOT_OK.get(slot, {slot}))


def daily_starts(roster_ids, gl, pgv, pos):
    """Daily lineups: each remaining day, fill the active slots from the roster players with a game that day, best
    per-game value first, each into the narrowest open slot he qualifies for. Returns (id, date, slot) per start.
    ponytail: greedy per day, can misplace a dual-eligible player; an assignment solver if it matters."""
    started = []
    narrow = sorted(SLOTS, key=lambda sl: len(SLOT_OK.get(sl, {sl})))
    by_day = gl if isinstance(gl, dict) else by_date(gl)  # {date: ids with a game}; prebuilt when called in a loop
    for date, ids in sorted(by_day.items()):
        ids = [i for i in ids if i in roster_ids]
        open_slots = list(narrow)
        for i in sorted(ids, key=lambda i: -pgv.get(i, 0)):
            slot = next((sl for sl in open_slots if fits(pos.get(i), sl)), None)
            if slot is not None:
                open_slots.remove(slot)
                started.append((i, date, slot))
    return started


def by_date(gl):
    """(id, date) game rows -> {date: [ids playing that day]}."""
    return {dt: list(g.id) for dt, g in gl.groupby("date")}


def started_value(roster_ids, gl, pgv, pos):
    """Expected value of the games a roster would actually start the rest of the week."""
    return sum(pgv.get(i, 0) for i, _, _ in daily_starts(roster_ids, gl, pgv, pos)) * model.P_PLAY


@st.cache_data(show_spinner=False)
def week_plan_inputs(cats, pool_size, updated, code):
    """Per-game projection with this week's role boost, its per-game value over replacement (never negative), and
    every player's remaining healthy games this week by date."""
    d, v = data(code), board(cats, pool_size, updated, code)
    per = v.reset_index()
    boost = ((per.MPG + per.opp_min) / per.MPG).where(per.MPG > 0, 1)
    per = per.assign(**{c: per[c] * boost for c in model.COUNT}, MPG=per.MPG + per.opp_min, GP=82.0)
    pgv = model.value(per, list(cats), pool_size).set_index("id").value.reindex(v.index).fillna(0)
    ws, we = this_week()
    gl = model.week_games(v.index, d["games"], d["schedule"], d["injuries"], d["players"], ws, we,
                          pd.to_datetime(d["schedule"].date).min())
    return per.set_index("id"), pgv, gl


def adds_used(moves, my_name, since):
    """Adds your team has made since `since` (Yahoo transactions)."""
    if moves is None or not my_name or not len(moves):
        return 0
    return int(((moves.type == "add") & (moves.to == my_name) & (moves.time.dt.tz_localize(None) >= since)).sum())


@st.cache_data(show_spinner="Working out your matchup...")
def matchup_plan(my_ids, opp_ids, fa_ids, banked, banked_opp, cats, today, updated, code, pool_size, give, season_value):
    """Win chance now, and how each start/bench switch and each add/drop would change it. Rosters as tuples of ids.
    Remaining games = the ones each side's daily lineup would start (best players with a game, by position)."""
    per, pgv, gl = week_plan_inputs(cats, pool_size, updated, code)
    v = board(cats, pool_size, updated, code)
    pgv, pos = pgv.to_dict(), v.pos.to_dict()
    vec = {i: r for i, r in zip(per.index, per[model.COUNT].fillna(0).to_numpy())}
    zero = np.zeros(len(model.COUNT))
    gl = gl[gl.date >= today]
    days = by_date(gl)
    banked, banked_opp = dict(banked), dict(banked_opp)

    def odds(starts, opp_starts):
        return model.week_odds(banked, [vec.get(i, zero) for i, _, _ in starts],
                               banked_opp, [vec.get(i, zero) for i, _, _ in opp_starts], list(cats))

    opp_starts = daily_starts(set(opp_ids), days, pgv, pos)
    starts = daily_starts(set(my_ids), days, pgv, pos)
    base, p0 = odds(starts, opp_starts)
    moves = []
    # start/bench: put a benched player with a game into a starter's spot that day
    playing = gl[gl.id.isin(my_ids)]
    started = {(i, dt) for i, dt, _ in starts}
    for i, dt, slot in starts:
        for j in playing.id[playing.date == dt]:
            if (j, dt) not in started and fits(pos.get(j), slot):
                alt = [(j, dt, slot) if (a, b) == (i, dt) else (a, b, c) for a, b, c in starts]
                o, p = odds(alt, opp_starts)
                moves.append({"kind": "start", "date": dt, "in": j, "out": i, "p": p, "cats": o.p_win - base.p_win})
    # add/drop: free agents with games left, swapped for anyone on your active roster
    left = gl.groupby("id").size()
    fas = [f for f in fa_ids if f in left.index]
    fas = sorted(fas, key=lambda f: -pgv.get(f, 0) * left[f])[:80]
    for dp in my_ids:
        for fa in fas:
            if season_value[fa] - season_value[dp] < -give:
                continue
            roster = (set(my_ids) - {dp}) | {fa}
            o, p = odds(daily_starts(roster, days, pgv, pos), opp_starts)
            moves.append({"kind": "add", "date": None, "in": fa, "out": dp, "p": p, "cats": o.p_win - base.p_win})
    return base, p0, pd.DataFrame(moves), starts, opp_starts


def stream_planner(v, team, il, cats, teams, roster, max_adds, moves, my_name):
    """Swap expendable players for free agents whose games you'd actually START. Lineups are set daily, so an added
    game only counts on a day you have an open slot he fits; a game on a night your 10 spots are full adds nothing.
    Week gain = started value of the roster after the swap minus before (each game x P_PLAY). Games your dropped
    player already played stay yours. Season change = rest-of-season value of the add minus the drop."""
    st.subheader("Streaming: turn adds into started games")
    ws, we = this_week()
    used = adds_used(moves, my_name, ws)
    c1, c2 = st.columns(2)
    cap = c1.number_input("Adds per week (league setting)", 0, 50, max_adds if max_adds is not None else 4,
                          help="Read from your Yahoo league when synced")
    give = c2.number_input("Most season value you'll give up per swap", 0.0, 5.0, 0.5, 0.1,
                           help="Rest-of-season value lost by a swap (dropped player's value minus the add's). "
                                "0 = only swaps that don't cost you anything long term")
    left_adds = max(cap - used, 0)
    _, pgv, gl = week_plan_inputs(tuple(cats), int(teams * roster), d["updated"], CODE)
    pgv, pos = pgv.to_dict(), v.pos.to_dict()
    games_left = gl.groupby("id").size()
    roster_ids = set(team.index) - il
    gl = by_date(gl)  # built once for the loop below
    fas = v[~v.Mine & ~v.Taken].index.intersection(games_left.index)
    fas = (pd.Series(pgv).reindex(fas) * games_left.reindex(fas)).nlargest(80).index  # the best weeks left
    options, plan, cur = pd.DataFrame(), [], set(roster_ids)
    for step in range(max(left_adds, 1)):  # greedy: best swap, apply it, re-plan the rest of the week
        base = started_value(cur, gl, pgv, pos)
        rows = [(dp, fa, started_value((cur - {dp}) | {fa}, gl, pgv, pos) - base, v.value[fa] - v.value[dp])
                for dp in cur if dp in roster_ids for fa in fas if fa not in cur]
        rows = pd.DataFrame(rows, columns=["drop", "add", "gain", "season"])
        rows = rows[(rows.gain > 0.005) & (rows.season >= -give)].sort_values("gain", ascending=False)
        if step == 0:
            options = rows.head(30)
        if rows.empty or step >= left_adds:
            break
        best = rows.iloc[0]
        plan.append(best)
        cur = (cur - {best["drop"]}) | {best["add"]}
    st.caption(f"{used} of {cap} adds used this week ({ws:%b %d} - {we:%b %d}), {left_adds} left. Gains count only "
               f"games the player would start in your daily lineup, each as {model.P_PLAY:.0%} of a game. "
               "Re-check daily: after a streamer plays, he can be swapped again.")

    def show(rows):
        return pd.DataFrame([{"Drop": v.name[r["drop"]], "His games left": int(games_left.get(r["drop"], 0)),
                              "Add": v.name[r["add"]], "Team": v.team[r["add"]], "Pos": v.pos[r["add"]],
                              "Their games left": int(games_left.get(r["add"], 0)), "Week gain": r["gain"],
                              "Season value change": r["season"]} for r in rows])
    fmt = {"Week gain": st.column_config.NumberColumn(format="%+.2f",
                                                      help="Value of the games you'd start this week after the swap minus before"),
           "Season value change": st.column_config.NumberColumn(format="%+.2f", help="Rest-of-season value: add minus drop")}
    if not plan:
        st.info("No swap adds started games this week within your adds and season-value limit." if left_adds else
                "No adds left this week.")
    else:
        st.markdown("**Suggested swaps** (in order; each one assumes the ones above it)")
        st.dataframe(show(plan), hide_index=True, column_config=fmt)
    with st.expander("More swap options"):
        st.dataframe(show([r for _, r in options.iterrows()]), hide_index=True, column_config=fmt)


# ---- sidebar: league setup ----
with st.sidebar:
    st.header("League")
    # Yahoo league id and your team live in the URL: bookmark it and a reload or lost tab picks up where you were.
    league_id = st.text_input("Yahoo league ID", st.query_params.get("league", ""),
                              help="The number in your league URL: basketball.fantasysports.yahoo.com/nba/<ID>. "
                                   "The league must be public. Leave blank to tick Mine/Taken by hand.").strip()
    rosters = moves = max_adds = scoreboard = None
    if league_id != st.query_params.get("league", ""):
        st.query_params["league"] = league_id
    if league_id:
        try:
            rosters, moves, max_adds, scoreboard = league(league_id, CODE)
        except Exception as e:
            st.error(f"Couldn't read Yahoo league {league_id} (is it public?): {e}")
    if rosters is not None and len(rosters):
        names = rosters.drop_duplicates("team_id").set_index("team_id").team
        my_team = st.selectbox("Your team", names.index, format_func=names.get,
                               index=list(names.index).index(int(st.query_params["team"]))
                               if st.query_params.get("team", "").isdigit() and int(st.query_params["team"]) in names.index else 0)
        st.query_params["team"] = str(my_team)
        if st.button("Sync now", help="Pull rosters from Yahoo right away instead of waiting for the 10-minute refresh"):
            league.clear()
            st.rerun()
    fmt = st.selectbox("Format", list(model.CATS))
    teams = st.number_input("Teams", 4, 30, rosters.team_id.nunique() if rosters is not None and len(rosters) else 10)
    roster = st.number_input("Roster spots per team", 5, 20, 13)
    cats = st.multiselect("Categories (remove one to punt it)", model.CATS[fmt], default=model.CATS[fmt])
    st.divider()
    d = data(CODE)
    st.caption(f"ESPN data updated {pd.Timestamp(d['updated']).tz_convert('America/New_York'):%b %d, %I:%M %p} ET. "
               "Refreshes itself about every 6 hours.")
    if st.button("Refresh now"):
        espn.refresh(force=True)
        st.cache_data.clear()
        st.rerun()

if not cats:
    st.warning("Pick at least one category.")
    st.stop()

v = board(tuple(cats), int(teams * roster), d["updated"], CODE)

# Draft state lives in the URL, so a page refresh or bookmark keeps your picks.
qp = st.query_params
mine = {int(x) for x in qp.get("mine", "").split(",") if x}
taken = {int(x) for x in qp.get("taken", "").split(",") if x}
il = set()  # my players in Yahoo IL slots: off the lineup, shown as their own IL rows
synced = rosters is not None and len(rosters) > 0
if synced:  # Yahoo is the source of truth: rosters already include every waiver and trade
    ids = d["players"].assign(key=d["players"].name.map(espn._norm)).drop_duplicates("key").set_index("key").id
    rosters["id"] = rosters.key.map(ids)
    rostered = rosters[rosters.id.isin(v.index)]
    mine = set(rostered.id[rostered.team_id == my_team].astype(int))
    taken = set(rostered.id[rostered.team_id != my_team].astype(int))
    il = set(rostered.id[(rostered.team_id == my_team) & rostered.slot.str.startswith("IL")].astype(int))
    unmatched = rosters.name[rosters.id.isna()]
    if len(unmatched):
        st.sidebar.caption("Not on the board (can't project): " + ", ".join(unmatched))
starred = {int(x) for x in qp.get("star", "").split(",") if x}
v["Mine"], v["Taken"] = v.index.isin(mine), v.index.isin(taken)
v["Star"] = v.index.map(lambda i: "⭐" if i in starred else "☆")
in_season = len(d["games"]) > 0  # opening night has happened


def toggle_star(key):
    """Button-column callback: flip the star on the clicked row (ids saved when that table was drawn)."""
    pid = st.session_state[f"{key}_ids"][st.session_state[key]["row"]]
    stars = {int(x) for x in st.query_params.get("star", "").split(",") if x} ^ {pid}
    st.query_params["star"] = ",".join(map(str, sorted(stars)))


def star_col(key):
    return st.column_config.ButtonColumn("", width=40, type="tertiary", on_click=toggle_star, args=(key,), key=key,
                                         help="Star a player to bookmark them in the Starred tab")
NBA_TEAMS = sorted(v.team.dropna().unique())

STAT_COLS = ["GP", "MPG", "PTS", "REB", "AST", "STL", "BLK", "3PM", "FG%", "FT%", "TO"]
FMT = {c: st.column_config.NumberColumn(format="%.1f") for c in STAT_COLS}
FMT.update({"FG%": st.column_config.NumberColumn(format="%.3f"), "FT%": st.column_config.NumberColumn(format="%.3f"),
            "GP": st.column_config.NumberColumn(format="%.0f", help="Projected games played: games left after known "
                                                "injuries x expected availability from injury history"),
            "adp": st.column_config.NumberColumn("ADP", format="%.1f", help="Yahoo average draft position. "
                                                 "Blank = not being drafted in Yahoo leagues"),
            "age": st.column_config.NumberColumn("Age", format="%.0f"),
            "value": st.column_config.NumberColumn("Value", format="%.2f",
                                                   help="Rest-of-season value: z-scores x projected games left. Never negative: about 0.7 = "
                                                        "replacement level, near 0 = won't play"),
            "Injury": st.column_config.TextColumn(width="small", help="O = out, DTD = day-to-day; date = expected return"),
            "Rank": st.column_config.NumberColumn(help="Green = we rank them ahead of ADP (a steal). Red = behind ADP."),
            "ADPval": st.column_config.NumberColumn("ADP Val", format="%+.1f",
                                                    help="ADP minus our rank. Green/+ = we rank them ahead of ADP "
                                                         "(a steal). Red/- = behind ADP."),
            "RankChg": st.column_config.NumberColumn("Rk Chg", format="%+.0f",
                                                     help="Preseason rank minus rank now. Green/+ = climbed since opening night"),
            "PreRank": st.column_config.NumberColumn("Pre Rk", help="Rank in the preseason projection, frozen before opening night"),
            "opp_min": st.column_config.NumberColumn("+Min (role)", format="%+.1f",
                                                     help="Extra minutes per game for the next 2 weeks from a fresh move into (out of) "
                                                          "the starting lineup, on top of the rest-of-season role change already in his projection"),
            "gp_so_far": st.column_config.NumberColumn("GP so far", format="%.0f"),
            "g_played": st.column_config.NumberColumn("Played", format="%.0f", help="Games played this week"),
            "g_left": st.column_config.NumberColumn("Left", format="%.0f",
                                                    help="Games left this week he's healthy for (injury report). Totals count "
                                                         "each as 0.89 of a game, the share rotation players actually play"),
            "PosRank": st.column_config.TextColumn("Pos Rk", help="Rank among players at their primary position group: G, F or C"),
            "name": "Player", "team": "Team", "pos": "Pos",
            "Photo": st.column_config.ImageColumn("", width=48),
            "Logo": st.column_config.ImageColumn("", width=28)})

st.title("🏀 NBA Fantasy Hub 2026-27")
ago = (pd.Timestamp.now(tz="UTC") - pd.Timestamp(d["updated"])).total_seconds() / 3600
avail = v[~v.Mine & ~v.Taken]
steals = avail[avail.ADPval.notna()]
m1, m2, m3, m4 = st.columns(4)
m1.metric("Best available", avail.iloc[0]["name"] if len(avail) else "-",
          f"{avail.iloc[0].value:.2f} value" if len(avail) else None, delta_color="off", border=True)
risers = avail[avail.Rank <= teams * roster * 1.5]  # rosterable range: a jump from 400th to 250th means little
if in_season and len(risers):
    s = risers.loc[risers.RankChg.idxmax()]
    m2.metric("Biggest riser available", s["name"], f"up {s.RankChg:.0f} spots since preseason", border=True)
elif len(steals):
    s = steals.loc[steals.ADPval.idxmax()]
    m2.metric("Biggest steal vs ADP", s["name"], f"ranked {s.ADPval:.0f} spots ahead of ADP", border=True)
else:
    m2.metric("Biggest steal vs ADP", "-", border=True)
m3.metric("Your picks", len(mine), f"{len(taken)} taken by others", delta_color="off", border=True)
m4.metric("ESPN data", f"{ago:.0f}h ago" if ago >= 1 else "just now", "refreshes about every 6h",
          delta_color="off", border=True)
# Season = rest-of-season per-game projection. This week = category totals for Mon-Sun: what's been played plus
# what's projected for the games left (injury report, role changes), which is what wins 7 of 9 H2H categories.
weekly = st.segmented_control(
    "Stats shown", ["Season (per game)", "This week (totals)"], default="Season (per game)", key="stat_mode",
    help="This week = box scores so far this week + projected remaining games. Updates as games are played, "
         "injuries are reported and players move into or out of the starting lineup.") == "This week (totals)"
vv = v
if weekly:
    wk = week_board(tuple(cats), int(teams * roster), d["updated"], CODE)
    vv = v.copy()
    wcols = [*model.COUNT, "FG%", "FT%", "MPG", "GP", "g_played", "g_left", *[f"z_{c}" for c in cats], "value", "Rank"]
    vv[wcols] = wk[wcols].reindex(vv.index)
    ws, we = this_week()
    st.caption(f"Week of {ws:%b %d} - {we:%b %d}: totals = played so far + expected remaining games "
               f"(each healthy game counts {model.P_PLAY:.0%}, the share rotation players actually play). Weeks run Monday-Sunday.")

tab_draft, tab_team, tab_match, tab_star, tab_waiver = st.tabs([":material/sports_basketball: Available Players",
                                                     ":material/groups: My team", ":material/scoreboard: Matchup",
                                                     ":material/star: Starred",
                                           ":material/swap_horiz: Waivers / pickups"])

with tab_draft:
    c1, c2, c3, c4 = st.columns([2, 2, 2, 1])
    search = c1.text_input("Search player")
    positions = c2.multiselect("Position", POSITIONS)
    nba_teams = c3.multiselect("Team", NBA_TEAMS)
    hide = c4.toggle("Hide drafted", value=True)
    with c4.popover("Reset draft", disabled=synced or not (mine or taken)):
        st.write(f"Clear all {len(mine)} Mine and {len(taken)} Taken picks?")
        if st.button("Yes, reset", type="primary"):
            for k in ("mine", "taken"):  # picks live only in the URL; stars stay
                if k in qp:
                    del qp[k]
            st.session_state.resets = st.session_state.get("resets", 0) + 1  # drop the editor's remembered ticks
            st.rerun()
    view = vv
    if hide:
        view = view[~view.Mine & ~view.Taken]
    if search:
        view = view[view.name.str.contains(search, case=False, na=False)]
    if positions:
        view = view[pos_match(view, positions)]
    if nba_teams:
        view = view[view.team.isin(nba_teams)]
    # preseason: market order; in season: our rest-of-season rank. Click a header to re-sort.
    view = view.sort_values("Rank") if in_season else view.sort_values(["adp", "Rank"], na_position="last")
    if weekly:
        view = view[view.GP > 0]  # no games this week = nothing to add
    st.caption(("Mine/Taken come from your Yahoo league (re-synced every 10 minutes). " if synced else
                "Tick **Mine** for your picks and **Taken** for everyone else's. ") +
               f"{len(mine)} mine, {len(taken)} taken by others. Category colors: green = helps you vs. the "
               f"draftable pool, red = hurts (FG%/FT% weighted by attempts, high TO is red).")
    cols = (["Star", "Mine", "Taken", "Rank", "name", "team", "pos", "value", "g_played", "g_left", "MPG",
             *[c for c in STAT_COLS if c not in ("GP", "MPG")], "Injury"] if weekly else
            ["Star", "Mine", "Taken", "Rank", "RankChg", "PreRank", "PosRank", "name", "team", "pos", "age", "value",
             "gp_so_far", *STAT_COLS, "Injury"] if in_season else
            ["Star", "Mine", "Taken", "adp", "Rank", "ADPval", "PosRank", "name", "team", "pos", "age", "value",
             *STAT_COLS, "Injury"])
    # The key follows the rows shown (not the picks), so a tick keeps the same editor and your scroll spot,
    # and edits never replay onto different rows after a filter change.
    st.session_state.star_draft_ids = list(view.index[:400])
    edited = st.data_editor(styled(view[cols].head(400), vv, cats), hide_index=True,
                            height=650, column_config={**FMT, "Star": star_col("star_draft")}, disabled=[c for c in cols if c not in (("Star",) if synced else ("Star", "Mine", "Taken"))],
                            key=f"ed-{hash((tuple(view.index[:400]), st.session_state.get('resets', 0)))}")
    if nba_teams and len(view):
        rows = []
        for t in nba_teams:  # one totals row per selected team, in the order picked
            g = view[view.team == t]
            if len(g):
                tot = {c: g[c].sum() for c in ["value", "PTS", "REB", "AST", "STL", "BLK", "3PM", "TO"]}
                tot["FG%"], tot["FT%"] = g.FGM.sum() / g.FGA.sum(), g.FTM.sum() / g.FTA.sum()  # attempt-weighted
                rows.append({"name": f"{t} totals ({len(g)} players)", **tot})
        st.dataframe(pd.DataFrame(rows)[["name", "value", *[c for c in STAT_COLS if c not in ("GP", "MPG")]]], hide_index=True,
                     column_config=FMT)
        st.caption("Sum of per-game projections. Players miss games, so these run above what the team scores on a "
                   "given night.")
    shown = set(view.index[:400])
    new_mine = (mine - shown) | set(view.index[:400][edited.Mine.values])
    new_taken = (taken - shown) | set(view.index[:400][edited.Taken.values])
    if not synced and (new_mine != mine or new_taken != taken):
        qp["mine"] = ",".join(map(str, sorted(new_mine)))
        qp["taken"] = ",".join(map(str, sorted(new_taken)))
        st.rerun()

with tab_star:
    fav = vv[vv.Star == "⭐"]
    if fav.empty:
        st.info("Click the ☆ next to a player on the Available Players tab to star them.")
    else:
        with st.popover("Clear starred"):
            st.write(f"Unstar all {len(fav)} players?")
            if st.button("Yes, clear", type="primary"):
                del st.query_params["star"]
                st.rerun()
        st.session_state.star_tab_ids = list(fav.index)
        st.dataframe(styled(fav[[c for c in cols if c not in ("Mine", "Taken")]], vv, cats),
                     hide_index=True, column_config={**FMT, "Star": star_col("star_tab")})

with tab_team:
    team = vv[vv.Mine]
    if team.empty:
        st.info("Tick **Mine** on the Available Players tab to build your team.")
    else:
        st.subheader("Category strength" + (" this week" if weekly else ""))
        st.caption("Sum of your players' z-scores per category" + (" on this week's totals" if weekly else "") +
                   ". Positive = above an average team's pace.")
        avg_team = vv.sort_values("Rank").head(int(teams * roster))[[f"z_{c}" for c in cats]].sum() / teams
        strength = team[[f"z_{c}" for c in cats]].sum() - avg_team * len(team) / roster
        for col, c in zip(st.columns(len(cats)), cats):
            col.metric(c, f"{strength[f'z_{c}']:+.1f}")
        tcols = (["Rank", "Photo", "name", "Logo", "team", "pos", "value", "g_played", "g_left", "MPG",
                  *[c for c in STAT_COLS if c not in ("GP", "MPG")], "Injury"] if weekly else
                 ["Rank", "PosRank", "Photo", "name", "Logo", "team", "pos", "value", *STAT_COLS, "Injury"])
        st.dataframe(styled(team.sort_values("Rank")[tcols], vv, cats),
                     hide_index=True, column_config=FMT)
        if weekly:
            # box scores so far this week + the remaining games your daily lineups would start (IL slots don't score)
            per, pgv, gl = week_plan_inputs(tuple(cats), int(teams * roster), d["updated"], CODE)
            active = set(team.index) - il
            starts = pd.DataFrame(daily_starts(active, gl, pgv.to_dict(), v.pos.to_dict()), columns=["id", "date", "slot"])
            g = d["games"]
            week_day1 = (this_week()[0] - pd.to_datetime(d["schedule"].date).min()).days + 1  # Monday's season day
            played = g[g.id.isin(active) & (g.day >= week_day1)]
            tot = played[model.COUNT].sum() + per.loc[starts.id, model.COUNT].sum() * model.P_PLAY
            row = {c: tot[c] for c in ["PTS", "REB", "AST", "STL", "BLK", "3PM", "TO"]}
            row["FG%"], row["FT%"] = tot.FGM / tot.FGA, tot.FTM / tot.FTA
            st.dataframe(pd.DataFrame([{"name": f"Team total: {len(played)} games played + {len(starts)} to start", **row}]),
                         hide_index=True, column_config=FMT)
            st.caption("Remaining games count only when your daily lineup would start him (see Daily lineup below), "
                       f"each as {model.P_PLAY:.0%} of a game. Games already played count in full, even if he sat on "
                       "your bench that day (the app can't see your past lineups).")
        team = v[v.Mine]

        team = v[v.Mine]
        per, pgv, gl = week_plan_inputs(tuple(cats), int(teams * roster), d["updated"], CODE)
        ws, we = this_week()
        starts = pd.DataFrame(daily_starts(set(team.index) - il, gl, pgv.to_dict(), v.pos.to_dict()),
                              columns=["id", "date", "slot"])
        st.subheader("Daily lineup")
        today = pd.Timestamp.now(tz="America/New_York").tz_localize(None).normalize()
        week_days = {f"{ws + pd.Timedelta(days=k):%a %m/%d}": ws + pd.Timedelta(days=k) for k in range(7)}
        pick = st.segmented_control("Day", list(week_days), key="lineup_day",
                                    default=next((k for k, dt in week_days.items() if dt == today), list(week_days)[0]))
        day = week_days.get(pick or "", today)
        stat_cols = ["PTS", "REB", "AST", "STL", "BLK", "3PM", "TO"]
        shot_cols = ["FG%", "FGA", "FT%", "FTA"]  # percentages next to their volume
        if day < today:  # done: what your players actually did
            g = d["games"]
            box = g[g.id.isin(team.index) & (g.day == (day - pd.to_datetime(d["schedule"].date).min()).days + 1)]
            if box.empty:
                st.info(f"None of your players played on {day:%A}.")
            else:
                box = box.assign(Player=box.id.map(team.name), MIN=box.MPG,
                                 **{"FG": box.FGM.astype(int).astype(str) + "-" + box.FGA.astype(int).astype(str),
                                    "FT": box.FTM.astype(int).astype(str) + "-" + box.FTA.astype(int).astype(str)})
                st.caption(f"{day:%A}: box scores (every game your players played, started or not).")
                st.dataframe(box[["Player", "MIN", *stat_cols, "FG", "FT"]], hide_index=True,
                             column_config={c: st.column_config.NumberColumn(format="%.0f") for c in ["MIN", *stat_cols]})
        else:
            on = starts[starts.date == day]
            playing = set(gl.id[(gl.date == day) & gl.id.isin(team.index)])
            team_plays = set(d["schedule"].team_id[pd.to_datetime(d["schedule"].date) == day])
            rows = []
            started_ids = set(on.id)
            sched = d["schedule"]
            opp = sched[pd.to_datetime(sched.date) == day].drop_duplicates("team_id").set_index("team_id").opp
            # matchup (DAL = home vs Dallas, @UTA = at Utah); injured or IL players with a game say so; blank = no game
            status = lambda i: ("IL" if i in il else opp.get(v.team_id.get(i), "") if i in playing else
                                f"OUT ({opp.get(v.team_id.get(i), '')})" if v.team_id.get(i) in team_plays else "")
            # spots nobody with a game can fill keep your usual player (best value that fits), so the list keeps
            # its shape every day; only spots with a game today are green
            by_slot = {sl: list(on.id[on.slot == sl]) for sl in set(SLOTS)}
            rest = [i for i in team.sort_values("value", ascending=False).index if i not in started_ids and i not in il
                    and i not in playing]
            rows, placed = [], set()
            for slot in SLOTS:  # one row per active slot, in Yahoo's order (the 3 Util spots take turns)
                pid = by_slot[slot].pop(0) if by_slot[slot] else next((i for i in rest if fits(v.pos.get(i), slot)), None)
                if pid in rest:
                    rest.remove(pid)
                placed.add(pid)
                st_ = status(pid) if pid else ""
                rows.append({"Slot": slot, "Player": team.name.get(pid, "(empty)") if pid else "(empty)", "Matchup": st_,
                             **{c: per.loc[pid, c] if pid in playing else None for c in stat_cols + shot_cols}})
            for i in team.sort_values("value", ascending=False).index:  # bench: whoever's left, playing first
                if i not in placed:
                    rows.append({"Slot": "IL" if i in il else "BN", "Player": team.name[i], "Matchup": status(i),
                                 **{c: per.loc[i, c] if i in playing else None for c in stat_cols + shot_cols}})
            lineup = pd.DataFrame(rows)
            bn = lineup.Slot.isin(["BN", "IL"])
            rank = lambda t: t.map(lambda x: 3 if x == "IL" else 1 if x.startswith("OUT") else 2 if x == "" else 0)
            lineup = pd.concat([lineup[~bn], lineup[bn].sort_values("Matchup", key=rank, kind="stable")])  # playing first
            n_open = len(SLOTS) - len(started_ids)
            st.caption(f"{day:%A}: {len(started_ids)} of your players {'plays and fills' if len(started_ids) == 1 else 'play and fill'} "
                       "the green spots (best first, by position). Stats are per-game projections." +
                       (f" {n_open} spot(s) have nobody playing: a streamer with a game this day would start there."
                        if n_open else ""))
            green = lineup.Player.map({team.name[i]: i in playing for i in team.index}).fillna(False) & ~lineup.Slot.isin(["BN", "IL"])
            st.dataframe(lineup.style.apply(lambda col: ["background-color: rgba(34,160,80,0.30)" if g else "" for g in green],
                                            subset=["Slot", "Player"])
                         .apply(lambda col: ["color: #999" if t == "IL" or t.startswith("OUT") else "" for t in col],
                                subset=["Matchup"])
                         .format({**{c: "{:.1f}" for c in stat_cols + ["FGA", "FTA"]}, "FG%": "{:.1%}", "FT%": "{:.1%}"},
                                 na_rep=""),
                         hide_index=True, height=35 * (len(lineup) + 1) + 3)

        stream_planner(v, team, il, cats, teams, roster, max_adds, moves, names.get(my_team) if synced else None)

with tab_match:
    if not synced or scoreboard is None or scoreboard.empty or my_team not in set(scoreboard.team_id):
        st.info("Connect your Yahoo league in the sidebar to see this week's matchup.")
    else:
        me_row = scoreboard[scoreboard.team_id == my_team].iloc[0]
        opp_row = scoreboard[scoreboard.team_id == me_row.opp_id].iloc[0]
        opp_name = names.get(int(me_row.opp_id), "Opponent")
        st.subheader(f"Week {me_row.week}: {names.get(my_team)} vs {opp_name}")
        active = lambda t: tuple(sorted(int(i) for i in rostered.id[(rostered.team_id == t) & ~rostered.slot.str.startswith("IL")]))
        today = pd.Timestamp.now(tz="America/New_York").tz_localize(None).normalize()
        c1, c2 = st.columns(2)
        give = c1.number_input("Most season value you'll give up per add", 0.0, 5.0, 0.5, 0.1, key="match_give",
                               help="Adds that cost more rest-of-season value than this aren't suggested")
        adds_left = max((max_adds if max_adds is not None else 4) - adds_used(moves, names.get(my_team), this_week()[0]), 0)
        c2.metric("Adds left this week", adds_left)
        bank = lambda r: tuple((c, float(r.get(c, 0) or 0)) for c in model.COUNT)
        base, p0, options, starts, opp_starts = matchup_plan(
            active(my_team), active(int(me_row.opp_id)), tuple(v.index[~v.Mine & ~v.Taken]), bank(me_row), bank(opp_row),
            tuple(cats), today, d["updated"], CODE, int(teams * roster), give, v.value)
        m1, m2, m3 = st.columns(3)
        m1.metric("Chance to win the matchup", f"{p0:.0%}", border=True)
        m2.metric("Categories expected", f"{base.p_win.sum():.1f} of {len(cats)}", border=True)
        m3.metric("Games left to start", f"{len(starts)} vs {len(opp_starts)}", border=True)
        tbl = pd.DataFrame({"Me so far": [me_row.get(c) for c in base.index], "Them so far": [opp_row.get(c) for c in base.index],
                            "Me projected": base.me, "Them projected": base.opp, "Win %": base.p_win}, index=base.index)
        for c in ("FG%", "FT%"):  # so-far percentages from made/attempted
            m, a = model.PCT[c]
            tbl.loc[c, "Me so far"] = me_row[m] / me_row[a] if me_row[a] else None
            tbl.loc[c, "Them so far"] = opp_row[m] / opp_row[a] if opp_row[a] else None
        nums = ["Me so far", "Them so far", "Me projected", "Them projected"]
        tbl.loc[["FG%", "FT%"], nums] = tbl.loc[["FG%", "FT%"], nums] * 100  # shown as percents: 46.83
        tbl[nums] = tbl[nums].astype(float).round(2)
        st.dataframe(tbl.reset_index(names="Category").style
                     .format("{:.2f}", subset=nums, na_rep="")
                     .format("{:.0%}", subset=["Win %"])
                     .apply(lambda col: [shade((x - 0.5) * 2) for x in col], subset=["Win %"]),
                     hide_index=True)
        st.caption("So far = Yahoo's totals (games in your active lineup). Projected = so far + each side's remaining "
                   f"games their daily lineup would start, each as {model.P_PLAY:.0%} of a game; your opponent is assumed "
                   "to start their best players. FG%/FT% come from projected makes and attempts, so a shooter moves them "
                   "only as much as his attempts allow. Win % has the game-to-game swings of each stat measured from "
                   "past seasons.")

        def why(cats_change):
            up = cats_change[cats_change > 0.02].sort_values(ascending=False)
            down = cats_change[cats_change < -0.02].sort_values()
            return ", ".join([f"{c} +{x:.0%}" for c, x in up.head(3).items()] + [f"{c} {x:.0%}" for c, x in down.head(2).items()])

        if options.empty:
            st.info("No start/bench switch or add improves your chances right now.")
        else:
            options = options.assign(gain=options.p - p0)
            st.markdown("**Who to start** (switches from the default lineup above that raise your chance to win)")
            sw = options[(options.kind == "start") & (options.gain > 0.005)].sort_values("gain", ascending=False)
            if sw.empty:
                st.caption("The default daily lineup (best players with a game) is already your best shot.")
            else:
                st.dataframe(pd.DataFrame({
                    "Day": sw.date.map(lambda x: f"{pd.Timestamp(x):%a %m/%d}"), "Start": sw["in"].map(v.name),
                    "Instead of": sw["out"].map(v.name), "Win chance": sw.p, "Change": sw.gain,
                    "Categories": sw.cats.map(why)}).head(10), hide_index=True,
                    column_config={"Win chance": st.column_config.NumberColumn(format="percent"),
                                   "Change": st.column_config.NumberColumn(format="%+.1%")})
            st.markdown(f"**Who to add** ({adds_left} add(s) left this week)")
            ad = options[(options.kind == "add") & (options.gain > 0.005)].sort_values("gain", ascending=False)
            if not adds_left:
                st.caption("No adds left this week.")
            elif ad.empty:
                st.caption("No free agent raises your chance to win within your season-value limit.")
            else:
                st.dataframe(pd.DataFrame({
                    "Add": ad["in"].map(v.name), "Team": ad["in"].map(v.team), "Drop": ad["out"].map(v.name),
                    "Win chance": ad.p, "Change": ad.gain, "Categories": ad.cats.map(why),
                    "Season value change": ad["in"].map(v.value).values - ad["out"].map(v.value).values}).head(10),
                    hide_index=True,
                    column_config={"Win chance": st.column_config.NumberColumn(format="percent"),
                                   "Change": st.column_config.NumberColumn(format="%+.1%"),
                                   "Season value change": st.column_config.NumberColumn(format="%+.2f")})
            st.caption("Changes are in your chance to win the whole matchup; Categories lists the biggest moves in each "
                       "category's win chance. A low-volume shooter barely moves FG%/FT% and costs counting stats, so "
                       "he won't show up here.")

with tab_waiver:
    team = v[v.Mine]
    if team.empty:
        st.info("Tick **Mine** on the Available Players tab first; pickups are compared against your roster.")
    else:
        worst = team.sort_values("value").iloc[0]
        st.caption(f"Free agents ranked by value. Your lowest-value player is **{worst['name']}** ({worst.value:.2f}). "
                   "Upgrade = value gained by swapping that player out (green = better than him, red = worse). "
                   "Category columns show the change per game.")
        fa = v[~v.Mine & ~v.Taken].copy()
        fa["Upgrade"] = (fa.value - worst.value).round(2)
        for c in cats:
            fa[f"Δ{c}"] = fa[f"z_{c}"] - worst[f"z_{c}"]
        f1, f2, f3 = st.columns([2, 2, 1])
        pos_filter = f1.multiselect("Position ", POSITIONS)
        team_filter = f2.multiselect("Team ", NBA_TEAMS)
        if pos_filter:
            fa = fa[pos_match(fa, pos_filter)]
        show = f3.number_input("Players shown", 10, 500, 100, step=10)
        if team_filter:
            fa = fa[fa.team.isin(team_filter)]
        num = ["Upgrade", "value", "opp_min", *[f"Δ{c}" for c in cats]]
        st.dataframe(fa[["Photo", "name", "Logo", "team", "pos", "Upgrade", "value", *(["RankChg"] if in_season else []), "opp_min",
                         *[f"Δ{c}" for c in cats], "Injury"]].head(show).style
                     .apply(lambda col: col.map(injury_badge), subset=["Injury"])
                     .apply(lambda col: (col / 3).map(shade), subset=["Upgrade"])
                     .format("{:+.2f}", subset=[c for c in num if c != "value"]).format("{:.2f}", subset=["value"])
                     .format("{:+.0f}", subset=["RankChg"] if in_season else []),
                     hide_index=True, column_config=FMT, height=600)
        # works from opening night: role = depth chart vs. preseason role until games pile up
        st.subheader("Minutes opening up: next 2 weeks")
        st.caption("Free agents moving into the starting lineup (ESPN depth chart, injured players skipped) who mostly came "
                   "off the bench lately. Backtest: about 4.5 more minutes over the next 2 weeks than their recent games "
                   "suggest (about 2.4 of it lasts the rest of the season if he keeps the job). Valued over their games in the next 14 days; often short-term streamers.")
        today = pd.Timestamp.now(tz="America/New_York").tz_localize(None).normalize()
        nxt = v[~v.Mine & ~v.Taken & (v.opp_min >= model.OPP_MIN_PER_START / 2) & (v.MPG > 0)].reset_index()  # at least half a start-share
        if nxt.empty:
            st.info("No free agent is moving into a starting lineup right now.")
        else:
            boost = (nxt.MPG + nxt.opp_min) / nxt.MPG
            nxt[model.COUNT] = nxt[model.COUNT].mul(boost, axis=0)
            nxt["MPG"] = nxt.MPG + nxt.opp_min
            nxt["wk2"] = model.games_between(nxt.id, d["injuries"], d["schedule"], d["players"], today,
                                             today + pd.Timedelta(days=13)).values
            nxt["GP"] = nxt.health * nxt.wk2
            allv = model.value(pd.concat([v[~v.index.isin(nxt.id)].reset_index(), nxt]), list(cats),
                               int(teams * roster)).set_index("id")
            nxt = allv.loc[nxt.id]
            nxt["pg_rank"] = nxt.value_pg.map(lambda x: int((allv.value_pg > x).sum()) + 1)
            nxt = nxt.sort_values("value", ascending=False).head(50)
            st.dataframe(nxt[["name", "team", "pos", "opp_min", "MPG", "wk2", "pg_rank", *[c for c in STAT_COLS if c not in ("GP", "MPG")], "Injury"]],
                         hide_index=True, column_config={**FMT, "wk2": st.column_config.NumberColumn("Games (2 wk)"),
                                                         "MPG": st.column_config.NumberColumn("MPG (boosted)", format="%.2f"),
                                                         "opp_min": st.column_config.NumberColumn("+Min (role)", format="%+.2f"),
                                                         **{c: st.column_config.NumberColumn(format="%.2f") for c in STAT_COLS
                                                            if c not in ("GP", "MPG", "FG%", "FT%")},
                                                         "pg_rank": st.column_config.NumberColumn("Per-game rank", help="Where his per-game "
                                                             "line while starting would rank among all players")})
    if moves is not None and len(moves):
        with st.expander(f"Recent league moves ({len(moves)})"):
            st.dataframe(moves.assign(time=moves.time.dt.strftime("%b %d %I:%M %p")), hide_index=True)
