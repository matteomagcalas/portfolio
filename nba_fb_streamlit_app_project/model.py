"""Projections and fantasy valuation.

Projection = per-minute rates x projected minutes x projected games.
  rates:   last 3 seasons, near-flat season weights, shrunk toward league rate by
           minutes played, adjusted for age, blended with ESPN's projected rates
  minutes: ESPN projected MPG blended with our history-based MPG
  games:   expected share of games (fit from median availability of prior seasons)
           x games left after the current injury's return date
Value = sum of category z-scores over the draftable pool, minus replacement level,
scaled by projected games.
"""
import numpy as np
import pandas as pd

COUNT = ["PTS", "REB", "AST", "STL", "BLK", "3PM", "TO", "FGM", "FGA", "FTM", "FTA"]
CATS = {"9-cat H2H": ["PTS", "REB", "AST", "STL", "BLK", "3PM", "FG%", "FT%", "TO"],
        "8-cat roto (no TO)": ["PTS", "REB", "AST", "STL", "BLK", "3PM", "FG%", "FT%"]}
SEASON_GAMES = 82
# Counting categories are harder to win than percentages; TO can be won by benching everyone.
CAT_WEIGHTS = {"FG%": 0.8, "FT%": 0.8, "TO": 0.5}  # unlisted categories = 1.0
# Next-season share of games played = a + b * median share over prior seasons. Fit on 25+ MPG
# players, 2023-24..2025-26: even 90%+ ironmen average ~78% the next year (injuries aren't predictable).
AVAIL_FIT = (0.505, 0.293)
ROOKIE_AVAIL = 0.74
# The fit tops out near 65 games; scale everyone so the most durable project to 75. A uniform
# scale leaves value rankings unchanged, it only moves the GP numbers shown.
MAX_GP = 75
AVAIL_SCALE = MAX_GP / 82 / sum(AVAIL_FIT)
# Actual / history-projected per-minute production by age, fit 2023-24..2025-26. Mixes aging with
# league trends (3PM keeps rising). Out-of-sample on 2025-26 it cut PTS/AST/3PM error 1.5-6%.
AGE_BINS = [0, 22, 25, 28, 31, 33, 99]
AGE_CURVE = pd.DataFrame({
    "PTS": [1.058, 1.043, 1.014, 0.990, 0.967, 0.962], "REB": [1.042, 1.002, 1.001, 0.972, 0.967, 0.910],
    "AST": [1.090, 1.109, 1.062, 1.035, 0.979, 0.990], "STL": [1.060, 1.062, 1.033, 1.070, 1.051, 1.046],
    "BLK": [1.019, 0.978, 0.961, 0.983, 0.979, 0.930], "3PM": [1.196, 1.092, 1.048, 1.071, 1.035, 1.073],
    "TO": [1.018, 1.018, 1.010, 0.963, 0.921, 0.974], "FGM": [1.049, 1.038, 1.009, 0.990, 0.973, 0.944],
    "FGA": [1.039, 1.028, 1.005, 0.992, 0.969, 0.969], "FTM": [1.024, 1.033, 1.014, 0.934, 0.889, 0.968],
    "FTA": [1.024, 1.025, 1.005, 0.938, 0.879, 0.965]})


def project(stats, target, weights=(1.0, 0.9, 0.8), shrink_min=600, espn_rate_blend=0.5,
            espn_min_blend=0.9, injuries=None, schedule=None, players=None,
            today=None, current_team=None, moved_rate_blend=0.5, age=None):
    """Per-game projections for season `target`, using only actual stats from earlier seasons.
    moved_rate_blend: ESPN rate weight for players who changed teams. Backtest (2025-26, 102
    movers) found leaning harder on ESPN slightly hurt, so it matches espn_rate_blend.
    age: Series id -> age during `target`; defaults to players.age."""
    hist = stats[(stats.source == "actual") & (stats.season < target) & (stats.season >= target - len(weights))
                 & (stats.GP > 0)].copy()
    hist["w"] = hist.season.map({target - 1 - i: w for i, w in enumerate(weights)})
    hist["MIN"] = hist.GP * hist.MPG
    for c in COUNT:
        hist[c] = hist[c] * hist.GP  # per-game -> season totals
    hist["avail"] = (hist.GP / SEASON_GAMES).clip(upper=1)

    tot = hist[COUNT + ["MIN"]].sum()
    league_rate = tot[COUNT] / tot.MIN  # minutes-weighted: garbage-time players barely move it

    wsum = hist[COUNT + ["MIN"]].mul(hist.w, axis=0).groupby(hist.id).sum()
    rate = wsum[COUNT].add(shrink_min * league_rate, axis=1).div(wsum.MIN + shrink_min, axis=0)
    out = pd.DataFrame(index=rate.index)
    g = hist.groupby("id")
    out["MPG"] = (hist.MPG * hist.w * hist.GP).groupby(hist.id).sum() / (hist.w * hist.GP).groupby(hist.id).sum()
    out["health"] = AVAIL_FIT[0] + AVAIL_FIT[1] * g.avail.median()
    if age is None and players is not None:
        age = players.set_index("id").age
    if age is not None:
        bucket = pd.cut(age.reindex(rate.index), AGE_BINS, labels=False)
        rate = rate * AGE_CURVE.reindex(bucket).fillna(1.0).set_axis(rate.index)

    espn = stats[(stats.source == "espn") & (stats.season == target) & (stats.MPG > 0)].set_index("id")
    espn_rate = espn[COUNT].div(espn.MPG, axis=0)
    out = out.reindex(out.index.union(espn.index))
    rate = rate.reindex(out.index)
    both = rate.index.intersection(espn_rate.index)
    if current_team is None and players is not None:
        current_team = players.set_index("id").team_id
    last_team = hist[hist.season == target - 1].set_index("id").get("team_id", pd.Series(dtype=float))
    now, before = current_team.reindex(both) if current_team is not None else None, last_team.reindex(both)
    moved = (now > 0) & (before > 0) & (now != before) if now is not None else pd.Series(False, index=both)
    a = pd.Series(np.where(moved, moved_rate_blend, espn_rate_blend), index=both)
    rate.loc[both] = rate.loc[both].mul(1 - a, axis=0) + espn_rate.loc[both].mul(a, axis=0)
    rookies = rate.index[rate.PTS.isna()]
    rate.loc[rookies] = espn_rate.loc[rookies]

    mpg_espn = espn.MPG.reindex(out.index)
    out["MPG"] = np.where(mpg_espn.notna() & out.MPG.notna(),
                          espn_min_blend * mpg_espn + (1 - espn_min_blend) * out.MPG,
                          out.MPG.fillna(mpg_espn))
    out["health"] = out.health.fillna(ROOKIE_AVAIL) * AVAIL_SCALE

    for c in COUNT:
        out[c] = rate[c] * out.MPG
    out["FG%"] = out.FGM / out.FGA
    out["FT%"] = out.FTM / out.FTA
    out["games"] = _games_left(out.index, injuries, schedule, players, today)  # games available
    out["GP"] = out.health * out.games  # expected games: what value uses
    return out.reset_index(names="id")


def _games_left(ids, injuries, schedule, players, today):
    """Games each player can play this season: 82, minus team games already played, minus games
    before their injury return date. (ESPN's schedule omits unscheduled NBA Cup games, so we
    count down from 82 rather than counting the listed schedule.) Players without a team get 0."""
    if schedule is None:  # backtests: no historical injury snapshot, assume a full season
        return pd.Series(SEASON_GAMES, index=ids)
    today = pd.Timestamp(today or pd.Timestamp.now().normalize())
    sched = schedule.assign(date=pd.to_datetime(schedule.date))
    team = players.set_index("id").team_id.reindex(ids)
    inj = injuries.drop_duplicates("id").set_index("id").reindex(ids)
    ret = pd.to_datetime(inj.return_date).fillna(today)
    left = []
    for t, r, out in zip(team, ret, inj.season_out.fillna(False).astype(bool)):
        games = sched.date[sched.team_id == t]
        missed = ((games >= today) & (games < r)).sum()
        left.append(0 if out or games.empty else max(0, SEASON_GAMES - (games < today).sum() - missed))
    return pd.Series(left, index=ids, dtype=float)


def games_between(ids, injuries, schedule, players, start, end):
    """Team games in [start, end] each player is healthy for (on/after injury return date)."""
    sched = schedule.assign(date=pd.to_datetime(schedule.date))
    sched = sched[(sched.date >= start) & (sched.date <= end)]
    team = players.set_index("id").team_id.reindex(ids)
    inj = injuries.drop_duplicates("id").set_index("id").reindex(ids)
    ret = pd.to_datetime(inj.return_date).fillna(pd.Timestamp(start))
    out = inj.season_out.fillna(False).astype(bool)
    return pd.Series([0 if o else int(((sched.team_id == t) & (sched.date >= r)).sum())
                      for t, r, o in zip(team, ret, out)], index=ids)


def value(proj, cats, pool_size, weights=None, iterations=3):
    """Z-score value per game and per season, measured against the top `pool_size` players.
    weights: {category: multiplier} applied when summing z-scores (default CAT_WEIGHTS)."""
    df = proj.dropna(subset=["PTS"]).copy()
    df = df[df.GP > 0]
    score = df.MPG * df.GP  # initial pool guess: who plays the most
    for _ in range(iterations):
        pool = df.loc[score.nlargest(pool_size).index]
        z = pd.DataFrame(index=df.index)
        for c in cats:
            if c in ("FG%", "FT%"):
                m, a = ("FGM", "FGA") if c == "FG%" else ("FTM", "FTA")
                pct = pool[m].sum() / pool[a].sum()
                col, pcol = df[m] - pct * df[a], pool[m] - pct * pool[a]  # volume-weighted impact
            else:
                col, pcol = df[c], pool[c]
            z[c] = (col - pcol.mean()) / pcol.std()
        if "TO" in cats:
            z["TO"] = -z["TO"]
        per_game = z.mul(pd.Series({c: (weights or CAT_WEIGHTS).get(c, 1.0) for c in cats})).sum(axis=1)
        replacement = per_game.loc[pool.index].min()
        score = (per_game - replacement) * df.GP / SEASON_GAMES
    df[[f"z_{c}" for c in cats]] = z.values
    df["value_pg"] = per_game.round(2)
    df["value"] = score.round(2)
    return df.sort_values("value", ascending=False)
