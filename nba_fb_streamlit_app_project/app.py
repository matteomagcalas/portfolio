"""NBA fantasy draft board + waiver helper. Run: streamlit run app.py"""
import pandas as pd
import streamlit as st

import espn
import model

st.set_page_config(page_title="NBA Fantasy Draft Board", page_icon="🏀", layout="wide")


@st.cache_data(ttl=3600, show_spinner="Pulling latest ESPN data (about a minute the first time)...")
def data():
    espn.refresh()
    return espn.load()


@st.cache_data(show_spinner=False)
def board(cats, pool_size, updated):
    d = data()
    proj = model.project(d["stats"], espn.SEASON, injuries=d["injuries"], schedule=d["schedule"], players=d["players"])
    v = model.value(proj, list(cats), pool_size)
    inj = d["injuries"].drop_duplicates("id").set_index("id")
    v = v.merge(d["players"], on="id", how="left")
    status = inj.status.map({"Out": "O", "Day-To-Day": "DTD"}).fillna(inj.status)  # anything new stays as-is
    back = inj.return_date.map(lambda r: f"{pd.Timestamp(r):%m/%d}" if isinstance(r, str) else "")
    v["Injury"] = v.id.map((status + " " + back.where(~inj.season_out.astype(bool), "season")).str.strip()).fillna("")
    v["note"] = v.id.map(inj.note).fillna("")
    v["Rank"] = range(1, len(v) + 1)
    primary = v.primary.fillna(v.pos.str.split("/").str[0]).fillna("")
    v["PosRank"] = primary + (v.groupby(primary).cumcount() + 1).astype(str)
    return v.set_index("id")


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
    if "adp" in df:
        styles["Rank"] = ((df.adp - df.Rank) / 30).map(shade)  # full color at 30+ spots
    for c in cats:
        if c in df:
            styles[c] = (z.loc[df.index, f"z_{c}"] / 2.5).map(shade)  # full color at +/-2.5 sd
    return styles


SLOTS = ["PG", "SG", "G", "SF", "PF", "F", "C", "C", "Util", "Util"]  # Yahoo standard starting lineup
SLOT_OK = {"G": {"PG", "SG"}, "F": {"SF", "PF"}, "Util": {"PG", "SG", "SF", "PF", "C"}}


def fits(pos, slot):
    return bool(set(str(pos).split("/")) & SLOT_OK.get(slot, {slot}))


def fill_lineup(team):
    """Best players first, each into the narrowest open slot they qualify for.
    ponytail: greedy, can misplace a dual-eligible player; an assignment solver if it matters."""
    lineup = {i: None for i in range(len(SLOTS))}
    for pid, p in team.sort_values("value", ascending=False).iterrows():
        if p.wk_games == 0:
            continue  # out all week: can't fill a slot
        for i in sorted(lineup, key=lambda i: len(SLOT_OK.get(SLOTS[i], {SLOTS[i]}))):
            if lineup[i] is None and fits(p.pos, SLOTS[i]):
                lineup[i] = pid
                break
    return lineup


# ---- sidebar: league setup ----
with st.sidebar:
    st.header("League")
    fmt = st.selectbox("Format", list(model.CATS))
    teams = st.number_input("Teams", 4, 30, 10)
    roster = st.number_input("Roster spots per team", 5, 20, 13)
    cats = st.multiselect("Categories (remove one to punt it)", model.CATS[fmt], default=model.CATS[fmt])
    st.divider()
    d = data()
    st.caption(f"ESPN data updated {pd.Timestamp(d['updated']).tz_convert('US/Eastern'):%b %d, %I:%M %p} ET. "
               "Refreshes itself every few hours.")
    if st.button("Refresh now"):
        espn.refresh(force=True)
        st.cache_data.clear()
        st.rerun()

if not cats:
    st.warning("Pick at least one category.")
    st.stop()

v = board(tuple(cats), int(teams * roster), d["updated"])

# Draft state lives in the URL, so a page refresh or bookmark keeps your picks.
qp = st.query_params
mine = {int(x) for x in qp.get("mine", "").split(",") if x}
taken = {int(x) for x in qp.get("taken", "").split(",") if x}
v["Mine"], v["Taken"] = v.index.isin(mine), v.index.isin(taken)

STAT_COLS = ["GP", "MPG", "PTS", "REB", "AST", "STL", "BLK", "3PM", "FG%", "FT%", "TO"]
FMT = {c: st.column_config.NumberColumn(format="%.1f") for c in STAT_COLS}
FMT.update({"FG%": st.column_config.NumberColumn(format="%.3f"), "FT%": st.column_config.NumberColumn(format="%.3f"),
            "GP": st.column_config.NumberColumn(format="%.0f", help="Projected games played: games left after known "
                                                "injuries x expected availability from injury history"),
            "adp": st.column_config.NumberColumn("ADP", format="%.0f"),
            "age": st.column_config.NumberColumn("Age", format="%.0f"),
            "value": st.column_config.NumberColumn("Value", format="%.2f",
                                                   help="Season value over replacement: z-scores x projected games"),
            "Injury": st.column_config.TextColumn(width="small", help="O = out, DTD = day-to-day; date = expected return"),
            "Rank": st.column_config.NumberColumn(help="Green = we rank them ahead of ADP (a steal). Red = behind ADP."),
            "PosRank": st.column_config.TextColumn("Pos Rk", help="Rank among players at their primary position"),
            "name": "Player", "team": "Team", "pos": "Pos"})

st.title("🏀 NBA Fantasy Draft Board 2026-27")
tab_draft, tab_team, tab_waiver = st.tabs(["Draft board", "My team", "Waivers / pickups"])

with tab_draft:
    c1, c2, c3 = st.columns([2, 3, 1])
    search = c1.text_input("Search player")
    positions = c2.multiselect("Position", POSITIONS)
    hide = c3.toggle("Hide drafted", value=False)
    view = v
    if hide:
        view = view[~view.Mine & ~view.Taken]
    if search:
        view = view[view.name.str.contains(search, case=False, na=False)]
    if positions:
        view = view[pos_match(view, positions)]
    view = view.sort_values(["adp", "Rank"], na_position="last")  # market order; click a header to re-sort
    st.caption(f"Tick **Mine** for your picks and **Taken** for everyone else's. "
               f"{len(mine)} mine, {len(taken)} taken by others. Category colors: green = helps you vs. the "
               f"draftable pool, red = hurts (FG%/FT% weighted by attempts, high TO is red).")
    cols = ["Mine", "Taken", "adp", "Rank", "PosRank", "name", "team", "pos", "age", "value", *STAT_COLS, "Injury"]
    # key changes with the data so the editor never replays stale edits onto different rows
    edited = st.data_editor(view[cols].head(400).style.apply(board_style, z=v, cats=cats, axis=None), hide_index=True, height=650, column_config=FMT,
                            disabled=[c for c in cols if c not in ("Mine", "Taken")],
                            key=f"ed-{hash((tuple(view.index[:400]), frozenset(mine), frozenset(taken)))}")
    shown = set(view.index[:400])
    new_mine = (mine - shown) | set(view.index[:400][edited.Mine.values])
    new_taken = (taken - shown) | set(view.index[:400][edited.Taken.values])
    if new_mine != mine or new_taken != taken:
        qp["mine"] = ",".join(map(str, sorted(new_mine)))
        qp["taken"] = ",".join(map(str, sorted(new_taken)))
        st.rerun()

with tab_team:
    team = v[v.Mine]
    if team.empty:
        st.info("Tick **Mine** on the draft board to build your team.")
    else:
        st.subheader("Category strength")
        st.caption("Sum of your players' z-scores per category. Positive = above an average team's pace.")
        avg_team = v.head(int(teams * roster))[[f"z_{c}" for c in cats]].sum() / teams
        strength = team[[f"z_{c}" for c in cats]].sum() - avg_team * len(team) / roster
        for col, c in zip(st.columns(len(cats)), cats):
            col.metric(c, f"{strength[f'z_{c}']:+.1f}")
        st.dataframe(team[["Rank", "PosRank", "name", "team", "pos", "value", *STAT_COLS, "Injury"]]
                     .style.apply(board_style, z=v, cats=cats, axis=None), hide_index=True, column_config=FMT)

        # Fantasy week = Monday-Sunday; before opening night, use the opening week.
        start = max(pd.Timestamp.now().normalize(), pd.to_datetime(d["schedule"].date).min())
        week = (start - pd.Timedelta(days=start.weekday()), start + pd.Timedelta(days=6 - start.weekday()))
        v["wk_games"] = model.games_between(v.index, d["injuries"], d["schedule"], d["players"], start, week[1]).values
        team = v[v.Mine]
        lineup = fill_lineup(team)
        free = v[~v.Mine & ~v.Taken & (v.wk_games > 0)]

        st.subheader(f"Positional needs: week of {week[0]:%b %d} - {week[1]:%b %d}")
        st.caption("Your best lineup this week, slot by slot, against the best undrafted player who could fill it. "
                   "Players out all week leave their slot empty. Updates as injuries and schedules change.")
        rows = []
        for i, slot in enumerate(SLOTS):
            pid = lineup[i]
            cur = team.loc[pid] if pid is not None else None
            cands = free[free.pos.map(lambda p: fits(p, slot))]
            best = cands.iloc[0] if len(cands) else None
            cur_val = cur.value if cur is not None else 0.0
            rows.append({"Slot": slot, "Starter": cur["name"] if cur is not None else "EMPTY",
                         "Value": cur_val if cur is not None else None,
                         "Games this wk": cur.wk_games if cur is not None else 0,
                         "Best available": best["name"] if best is not None else "",
                         "Their value": best.value if best is not None else None,
                         "Upgrade": round(best.value - cur_val, 2) if best is not None else None})
        needs = pd.DataFrame(rows)
        bench = team.drop([p for p in lineup.values() if p is not None])
        top = needs[needs.Upgrade > 0].sort_values("Upgrade", ascending=False).drop_duplicates("Best available").head(3)
        if top.empty:
            st.success("No undrafted player beats any of your starters right now.")
        for _, r in top.iterrows():
            st.markdown(f"- **{r.Slot}**: {r['Best available']} would add **+{r.Upgrade:.2f}** "
                        f"over {r.Starter if r.Starter != 'EMPTY' else 'an empty slot'}")
        st.dataframe(needs.style.apply(lambda col: col.map(lambda x: shade(x / 3) if pd.notna(x) and x > 0 else ""),
                                       subset=["Upgrade"]), hide_index=True,
                     column_config={"Value": st.column_config.NumberColumn(format="%.2f"),
                                    "Their value": st.column_config.NumberColumn(format="%.2f"),
                                    "Upgrade": st.column_config.NumberColumn(format="%+.2f")})
        if len(bench):
            st.caption("Bench: " + ", ".join(f"{p['name']} ({p.wk_games} g)" for _, p in bench.iterrows()))

with tab_waiver:
    team = v[v.Mine]
    if team.empty:
        st.info("Tick **Mine** on the draft board first; pickups are compared against your roster.")
    else:
        worst = team.sort_values("value").iloc[0]
        st.caption(f"Free agents ranked by value. Your lowest-value player is **{worst['name']}** ({worst.value:.2f}). "
                   "Upgrade = value gained by swapping that player out. Category columns show the change per game.")
        fa = v[~v.Mine & ~v.Taken].copy()
        fa["Upgrade"] = (fa.value - worst.value).round(2)
        for c in cats:
            fa[f"Δ{c}"] = (fa[f"z_{c}"] - worst[f"z_{c}"]).round(1)
        fa = fa[fa.Upgrade > 0]
        pos_filter = st.multiselect("Position ", POSITIONS)
        if pos_filter:
            fa = fa[pos_match(fa, pos_filter)]
        st.dataframe(fa[["name", "team", "pos", "Upgrade", "value", *[f"Δ{c}" for c in cats], "Injury"]].head(50),
                     hide_index=True, column_config=FMT)
