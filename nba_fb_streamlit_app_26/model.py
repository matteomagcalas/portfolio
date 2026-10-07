"""Projections and fantasy valuation.

Projection = per-minute rates x projected minutes x projected games.
  rates:   last 3 seasons weighted toward the most recent (1 / 0.6 / 0.3), shrunk toward league rate
           by minutes played, adjusted for age, blended with ESPN's projected rates
  minutes: our roster model (roster_minutes): a regression trained on past seasons that predicts MPG
           from a player's own minutes history, last season's starting role, this season's starting role
           (ESPN depth chart) and his current roster's competition. No ESPN projections.
           ESPN's MPG is only the fallback for players with no NBA minutes last season (rookies etc.).
  games:   expected share of games (fit from median availability of prior seasons)
           x games left after the current injury's return date
context: adjust() learns per-stat multipliers on those rates from trend (star step), trend x age,
         role change and team change, trained on how past projections missed.
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


def project(stats, target, weights=(1.0, 0.6, 0.3), shrink_min=300, espn_rate_blend=0.5,
            espn_min_blend=0.9, injuries=None, schedule=None, players=None,
            today=None, current_team=None, moved_rate_blend=0.5, age=None, roster_min=True, starter=None):
    """Per-game projections for season `target`, using only actual stats from earlier seasons.
    weights/shrink_min: backtest 2024-25 and 2025-26, (1, .6, .3) with 300 had the lowest per-minute error of
    the grid in both seasons and less pull toward league average for stars than near-flat (1, .9, .8) / 600.
    moved_rate_blend: ESPN rate weight for players who changed teams. Backtest (2025-26, 102
    movers) found leaning harder on ESPN slightly hurt, so it matches espn_rate_blend.
    age: Series id -> age during `target`; defaults to players.age.
    starter: id -> 1/0 projected starter (depth chart). Without it, minutes fall back to the ESPN blend.
    roster_min: replace the ESPN-blended MPG with roster_minutes() wherever it has a prediction. Backtest
    (2024-25 / 2025-26, starter = started half his games, a stand-in for the preseason depth chart):
    PTS error 2.33/2.51 -> 2.10/2.17, MPG error 3.80/4.07 -> 3.25/3.06, value rank 0.72/0.73 -> 0.77/0.75;
    starters who lost their spot: PTS bias +1.9/+3.1 -> -0.9/+0.6."""
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
    if roster_min and current_team is not None and starter is not None:  # needs the depth chart
        pl = players.set_index("id") if players is not None else pd.DataFrame(columns=["primary", "pos"])
        group = pl.primary.fillna(pl.pos.str.split("/").str[0]).map(GROUPS)
        mins = roster_minutes(stats, target, current_team, group, age if age is not None else pd.Series(dtype=float),
                              starter)
        out["MPG"] = mins.reindex(out.index).fillna(out.MPG)
    out["health"] = out.health.fillna(ROOKIE_AVAIL) * AVAIL_SCALE

    for c in COUNT:
        out[c] = rate[c] * out.MPG
    out["FG%"] = out.FGM / out.FGA
    out["FT%"] = out.FTM / out.FTA
    out["games"] = _games_left(out.index, injuries, schedule, players, today)  # games available
    out["GP"] = out.health * out.games  # expected games: what value uses
    return out.reset_index(names="id")


GROUPS = {"PG": "G", "SG": "G", "SF": "F", "PF": "F", "C": "C"}


def _minute_features(actual, T, roster, group, age, starter):
    """One row per rostered player with NBA minutes in season T-1. Uses seasons before T, plus `starter`
    (id -> 0..1): his starting role in T. Training: share of games started in T. Live: depth chart."""
    prev1 = actual[actual.season == T - 1].set_index("id")
    prev2 = actual[actual.season == T - 2].set_index("id")
    df = pd.DataFrame({"team": roster})
    df["mpg1"] = prev1.MPG.reindex(df.index).fillna(0)
    df["mpg2"] = prev2.MPG.reindex(df.index).fillna(0)
    df["gp1"] = prev1.GP.reindex(df.index).fillna(0) / SEASON_GAMES
    q = (prev1.PTS + prev1.REB + prev1.AST + 2 * (prev1.STL + prev1.BLK)) / prev1.MPG.where(prev1.MPG > 0)
    df["quality"] = q.reindex(df.index).fillna(q.quantile(0.25))  # production per minute
    last_team = prev1.team_id.reindex(df.index)
    df["moved"] = ((last_team != df.team) & last_team.notna()).astype(float)
    df["group"] = group.reindex(df.index).fillna("F")
    df["age"] = age.reindex(df.index).fillna(25)
    df["base"] = 0.7 * df.mpg1 + 0.3 * df.mpg2.where(df.mpg2 > 0, df.mpg1)
    df["gs1"] = prev1.GS.reindex(df.index).fillna(0)  # share of games started (ESPN per-game average)
    df["start"] = starter.reindex(df.index).fillna(0)
    df["benched"] = df.gs1 * (1 - df.start)  # started last season, not now
    df["promoted"] = (1 - df.gs1) * df.start
    # competition: last season's minutes held by better-per-minute teammates, at his position group and overall
    pairs = df.reset_index(names="id").merge(df.reset_index(names="mate"), on="team", suffixes=("", "_m"))
    pairs = pairs[(pairs.id != pairs.mate) & (pairs.quality_m > pairs.quality)]
    df["ahead_all"] = pairs.groupby("id").base_m.sum().reindex(df.index).fillna(0)
    df["ahead_group"] = pairs[pairs.group == pairs.group_m].groupby("id").base_m.sum().reindex(df.index).fillna(0)
    df["team_load"] = df.groupby("team").base.transform("sum")
    df["qrank"] = df.groupby("team").quality.rank(ascending=False)
    return df[df.mpg1 > 0]


def _design(df):
    x = df[["mpg1", "mpg2", "gp1", "quality", "moved", "age", "base", "ahead_group", "ahead_all", "team_load",
            "qrank", "gs1", "start", "benched", "promoted"]].astype(float)
    return np.c_[np.ones(len(x)), x.values, (x.age - 27) ** 2, x.base * x.ahead_group / 60,
                 x.base * x.benched, x.base * x.start]


def roster_minutes(stats, target, roster, group, age, starter):
    """Projected MPG for season `target` from minutes history + roster competition, no ESPN input.
    Linear regression fit on every earlier season that has a prior season (players with 10+ GP).
    roster: Series id -> team_id for `target`; group: id -> G/F/C; age: id -> age during `target`;
    starter: id -> 1 if projected to start in `target` (depth chart), else 0."""
    actual = stats[(stats.source == "actual") & (stats.season < target)]
    seasons = set(actual.season)
    train = []
    for T in sorted(seasons):
        if T - 1 not in seasons:
            continue
        cur = actual[(actual.season == T) & (actual.GP >= 10) & (actual.team_id > 0)].set_index("id")
        f = _minute_features(actual, T, cur.team_id, group, age - (target - T), (cur.GS >= 0.5).astype(float))
        train.append(f.assign(y=cur.MPG))
    train = pd.concat(train)
    beta = np.linalg.lstsq(_design(train), train.y.values, rcond=None)[0]
    f = _minute_features(actual, target, roster[roster > 0], group, age, starter)
    return pd.Series(np.clip(_design(f) @ beta, 0, 40), index=f.index)


ADJ_STATS = ["PTS", "REB", "AST", "STL", "BLK", "3PM", "TO", "FGM", "FGA", "FTM", "FTA"]


def _adj_features(actual, T, proj, team, starter, age):
    """Context the base projection can't see, per player, from seasons before T:
    trend (star step / decline), trend x youth, trend x age 30+, trend from an injury-shortened season
    (trusted less), role change. A flat "short last season" penalty overshot in both backtest seasons."""
    p = proj.set_index("id")
    prev1, prev2 = (actual[actual.season == T - k].set_index("id").reindex(p.index) for k in (1, 2))
    ag = age.reindex(p.index).fillna(26)
    young, old = ((25 - ag).clip(lower=0) / 5), ((ag - 30).clip(lower=0) / 5)
    f = pd.DataFrame(index=p.index)
    f["short1"] = 1 - (prev1.GP / SEASON_GAMES).clip(upper=1).fillna(1)  # games missed last season
    f["role"] = starter.reindex(p.index).fillna(0) - prev1.GS.fillna(0)  # + = promoted to starter
    f["moved"] = ((team.reindex(p.index) != prev1.team_id) & prev1.team_id.notna()).astype(float)
    f["min_change"] = ((p.MPG - prev1.MPG) / 10).fillna(0)
    out = {}
    for c in ADJ_STATS:
        r1, r2 = prev1[c] / prev1.MPG, prev2[c] / prev2.MPG
        both = (prev1.MPG >= 10) & (prev2.MPG >= 10)
        trend = np.log((r1 + 0.02) / (r2 + 0.02)).where(both, 0).clip(-0.7, 0.7).fillna(0)
        x = f.assign(trend=trend, trend_young=trend * young, trend_old=trend * old, trend_short=trend * f.short1)
        out[c] = x[["trend", "trend_young", "trend_old", "trend_short", "role", "moved", "min_change"]]
    return out


def adjust(stats, target, proj, team, starter, players, age=None, ridge=20.0, first_season=2024):
    """Learned per-stat multipliers on the per-minute rates in `proj`, from context features (_adj_features).
    Trained on earlier seasons: their base projection vs what each player actually did (log ratio of
    per-minute rates, weighted by minutes played). Ridge-regularized; multiplier capped at +/-25%."""
    if age is None:
        age = players.set_index("id").age
    actual = stats[(stats.source == "actual") & (stats.season < target)]
    X, Y, W = {c: [] for c in ADJ_STATS}, {c: [] for c in ADJ_STATS}, []
    for T in range(first_season, target):
        a = actual[actual.season == T].set_index("id")
        start_T = (a.GS >= 0.5).astype(float)
        base = project(stats[stats.season <= T], T, current_team=a.team_id, starter=start_T, players=players,
                       age=age - (target - T))
        base = base.set_index("id").reindex(a.index[(a.GP >= 20) & (a.MPG >= 12)]).dropna(subset=["PTS"])
        feats = _adj_features(actual, T, base.reset_index(names="id"), a.team_id, start_T, age - (target - T))
        W.append(a.GP[base.index] * a.MPG[base.index])
        for c in ADJ_STATS:
            X[c].append(feats[c].loc[base.index])
            Y[c].append(np.log((a[c][base.index] / a.MPG[base.index] + 0.02) / (base[c] / base.MPG + 0.02)))
    if not W:
        return proj
    w = pd.concat(W)
    feats = _adj_features(actual, target, proj, team, starter, age)
    out = proj.set_index("id").copy()
    for c in ADJ_STATS:
        x, y = pd.concat(X[c]), pd.concat(Y[c])
        xm = np.c_[np.ones(len(x)), x.values] * np.sqrt(w.values)[:, None]
        reg = ridge * np.eye(xm.shape[1]); reg[0, 0] = 0  # don't shrink the intercept
        beta = np.linalg.solve(xm.T @ xm + reg, xm.T @ (y.values * np.sqrt(w.values)))
        mult = np.exp(np.clip(np.c_[np.ones(len(feats[c])), feats[c].values] @ beta, -0.25, 0.25))
        out[c] = out[c] * pd.Series(mult, index=feats[c].index).reindex(out.index).fillna(1.0)
    out["FG%"], out["FT%"] = out.FGM / out.FGA, out.FTM / out.FTA
    return out.reset_index()


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
