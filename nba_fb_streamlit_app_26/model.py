"""Projections and fantasy valuation.

Projection = per-minute rates x projected minutes x projected games.
  rates:   last 3 seasons weighted toward the most recent (1 / 0.6 / 0.3), shrunk toward league rate
           by minutes played, adjusted for age, blended with ESPN's projected rates
  minutes: 80% our roster model (roster_minutes), 20% ESPN MPG. The roster model is a regression trained on past seasons that predicts MPG
           from a player's own minutes history, last season's starting role, this season's starting role
           (ESPN depth chart) and his current roster's competition. No ESPN projections.
           ESPN's MPG is only the fallback for players with no NBA minutes last season (rookies etc.).
  games:   expected share of games (fit from median availability of prior seasons)
           x games left after the current injury's return date
context: adjust() learns per-stat multipliers on those rates from trend (star step), trend x age,
         role change and team change, trained on how past projections missed.
Value = sum of category z-scores over the draftable pool, minus replacement level,
scaled by projected games, then softplus'd so it never goes below 0 (replacement level = about 0.7).
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
            today=None, current_team=None, moved_rate_blend=0.5, age=None, roster_min=True, starter=None,
            roster_espn_blend=0.2):
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
        # 20% ESPN MPG: tested 0/20/35/50% on 2024-25 and 2025-26; 20% lowered PTS error and raised value rank
        # in both (ESPN knows situations a few seasons of box scores can't: a star back from injury, a vacated role)
        mins = mins.reindex(out.index)
        mins = (1 - roster_espn_blend) * mins + roster_espn_blend * mpg_espn.where(mpg_espn.notna(), mins)
        out["MPG"] = mins.fillna(out.MPG)
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
    # minutes history weighted by recency AND games played, so an injury-shortened season counts less
    prev3 = actual[actual.season == T - 3].set_index("id")
    num = den = 0
    for w, pv in zip((1.0, 0.6, 0.3), (prev1, prev2, prev3)):
        gp = pv.GP.reindex(df.index).fillna(0) * w
        num = num + gp * pv.MPG.reindex(df.index).fillna(0); den = den + gp
    df["base"] = (num / den.where(den > 0)).fillna(df.mpg1)
    df["short_gap"] = (df.base - df.mpg1) * (1 - df.gp1)  # minutes his short last season undersold
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
            "qrank", "gs1", "start", "benched", "promoted", "short_gap"]].astype(float)
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


def project_by_role(stats, target, current_team, starter, players, injuries=None, schedule=None, age=None):
    """Depth-chart starters keep the ESPN-blended minutes and plain rates; only non-starters get the
    roster-minutes model and the learned context adjustment (bench players coming from bigger roles).
    Backtest 2024-25 / 2025-26: starters' PTS bias -0.10/-0.54 (adjusting everyone) -> +0.24/-0.24;
    bench bias stays fixed at -0.28/+0.08 (was +1.07/+1.53 before the roster model)."""
    kw = dict(current_team=current_team, players=players, injuries=injuries, schedule=schedule, age=age)
    plain = project(stats, target, roster_min=False, **kw).set_index("id")
    bench = adjust(stats, target, project(stats, target, starter=starter, **kw), current_team, starter, players,
                   age=age).set_index("id").reindex(plain.index)
    is_start = starter.reindex(plain.index).fillna(0) == 1
    return bench.where(~is_start & bench.PTS.notna(), plain).reset_index()


# In-season blend, fit by backtest_inseason.py on 2023-24..2025-26. K = minutes of play the preseason rate is worth,
# as (when running above it, when running below it): breakouts hold up more often than slumps (held-out: players
# 8+ PTS+REB+AST/game above projection kept 80% of it the rest of the season; one shared weight kept only 65% in
# the projection, separate weights 77%). tau = days for a game's weight to fall to 37%. Minutes: prior worth 2
# games when he's playing more, 8 when less, 30-day fade. Availability: prior worth 40 team games. Held-out
# rest-of-season PTS error, first month / months 2-3 / after: 2.69/2.58/2.91 (one weight) -> 2.65/2.53/2.81
# (with ROLE_ROS below); preseason-only was 3.07/3.28/3.90.
IN_SEASON = dict(K={"PTS": (200, 400), "REB": (200, 400), "AST": (200, 400), "STL": (800, 800), "BLK": (800, 400),
                    "3PM": (200, 400), "TO": (800, 400), "FGM": (200, 400), "FGA": (100, 200), "FTM": (400, 200),
                    "FTA": (400, 200)},
                 tau={"PTS": 60, "REB": 120, "AST": 60, "STL": 120, "BLK": 120, "3PM": 120, "TO": np.inf, "FGM": 60,
                      "FGA": 60, "FTM": 120, "FTA": 120},
                 Km=(2, 8), tau_m=30, Kh=40, new_mpg=10)
# Rest-of-season MPG per unit of promotion() on top of the blend: a fill-in back on the bench (starter returned)
# loses minutes, a player who keeps the job keeps them. Held out 2.1/2.5/2.7, all seasons 2.44.
ROLE_ROS = 2.44
# EXTRA next-2-week MPG per unit of promotion(), on top of ROLE_ROS (a fresh role change is biggest right away):
# fit on 2023-24..2025-26 checkpoints, "starting now" = started his team's next game, preseason role = started his
# first game. Held out 1.7/2.1/2.4; next-2-week minutes error for role changers 5.31/4.97/4.67 -> 4.89/4.67/4.52.
# All seasons 2.05, so ~4.5 MPG in total for the next 2 weeks. (Tried first: minutes vacated by injured teammates;
# spread over the whole bench it gave ~0.)
OPP_MIN_PER_START = 2.05


def _up_down(k, season, prior):
    """Prior weight: one number, or (above, below) = separate weights for running above / below the prior
    (backtest: big breakouts hold up more often than big drops)."""
    if not isinstance(k, tuple):
        return k
    return pd.Series(np.where(season > prior, k[0], k[1]), index=prior.index)


def in_season(prior, games, today, K, tau, Km, tau_m, Kh, new_mpg):
    """Rest-of-season per-game projection: the preseason `prior` updated with this season's `games` before `today`.
    Per-minute rates: prior rate counted as K[c] minutes of play (or (above, below) pair), plus every game's stats weighted by
    exp(-days ago / tau[c]); a hot week moves a stat only as far as the backtest says it predicts.
    Minutes: prior MPG counted as Km games, plus games weighted by exp(-days ago / tau_m).
    Availability: prior health counted as Kh team games, plus share of his team's games he played.
    prior: id, MPG, health, COUNT per game. games: id, day, team_id, MPG (that night's minutes), COUNT.
    Players with no prior (call-ups, undrafted rookies) start from new_mpg and the bench-player league rate."""
    g = games[games.day < today].astype({c: float for c in COUNT + ["MPG", "day"]})  # empty log reads as text
    p = prior.set_index("id")
    ids = p.index.union(g.id.unique())
    p = p.reindex(ids)
    bench = p[p.MPG < 20]
    bench_rate = bench[COUNT].sum() / bench.MPG.sum()
    rate0 = p[COUNT].div(p.MPG, axis=0)
    new = rate0.PTS.isna()
    rate0.loc[new] = bench_rate.values
    mpg0 = p.MPG.fillna(new_mpg)
    out = pd.DataFrame(index=ids)
    ago = today - g.day
    for c in COUNT:
        w = np.exp(-ago / tau[c])
        num = (g[c] * w).groupby(g.id).sum().reindex(ids).fillna(0)
        den = (g.MPG * w).groupby(g.id).sum().reindex(ids).fillna(0)
        k = _up_down(K[c], num / den.where(den > 0), rate0[c])
        out[c] = (k * rate0[c] + num) / (k + den)  # per-minute rate for now
    w = np.exp(-ago / tau_m)
    num = (g.MPG * w).groupby(g.id).sum().reindex(ids).fillna(0)
    den = w.groupby(g.id).sum().reindex(ids).fillna(0)
    km = _up_down(Km, num / den.where(den > 0), mpg0)
    out["MPG"] = (km * mpg0 + num) / (km + den)
    out[COUNT] = out[COUNT].mul(out.MPG, axis=0)
    # availability: games played / his current team's games so far (a traded player: since joining)
    last = g.sort_values("day").groupby("id").team_id.last()
    traded = g.groupby("id").team_id.nunique() > 1
    since = g[g.team_id == g.id.map(last)].groupby("id").day.min().where(traded, 0)
    team_days = g.drop_duplicates(["team_id", "day"])[["team_id", "day"]]
    gp = g[g.team_id == g.id.map(last)].groupby("id").size().reindex(ids).fillna(0)
    team_gp = pd.Series({i: ((team_days.team_id == t) & (team_days.day >= since[i])).sum()
                         for i, t in last.items()}).reindex(ids).fillna(0)
    h0 = p.health.fillna(ROOKIE_AVAIL * AVAIL_SCALE)
    out["health"] = (Kh * h0 + gp) / (Kh + team_gp)
    out["gp_so_far"] = g.groupby("id").size().reindex(ids).fillna(0)
    out = out.astype(float)  # an empty game log (before opening night) leaves object columns
    out["FG%"], out["FT%"] = out.FGM / out.FGA, out.FTM / out.FTA
    return out.reset_index(names="id")


def promotion(games, today, starter_now, tau_m, prior_start=None, k=4):
    """Starting now (1/0) minus his recent share of starts, weighted like the minutes blend: + = bench player
    moving into the lineup (injury ahead of him, trade, coach's call), - = starter moving to the bench.
    prior_start: preseason starter flag, worth k games (the minutes prior's weight), so a role change shows
    from opening night instead of waiting for games. Times OPP_MIN_PER_START = MPG change for the next 2 weeks."""
    g = games[games.day < today].astype({"day": float, "GS": float})
    w = np.exp(-(today - g.day) / tau_m)
    ids = g.id.unique() if prior_start is None else pd.Index(g.id.unique()).union(prior_start.index)
    s0 = (prior_start if prior_start is not None else pd.Series(dtype=float)).reindex(ids)
    num, den = (g.GS * w).groupby(g.id).sum().reindex(ids).fillna(0), w.groupby(g.id).sum().reindex(ids).fillna(0)
    recent = ((k * s0).fillna(0) + num) / (k * s0.notna() + den)
    return (starter_now.reindex(recent.index).fillna(0) - recent).dropna().rename("promo")


def with_role(proj, promo, mpg_per_start):
    """Shift minutes by mpg_per_start x promotion (+ = moving into the lineup, - = losing his spot) and scale the
    counting stats with them (same per-minute rates). proj indexed by id."""
    p = proj.copy()
    mpg = (p.MPG + promo.reindex(p.index).fillna(0) * mpg_per_start).clip(lower=0)
    p[COUNT] = p[COUNT].mul((mpg / p.MPG).where(p.MPG > 0, 1), axis=0)
    p["MPG"] = mpg
    return p


# Share of his team's games a rotation player who played the last one plays over the next 7 days (game logs:
# 0.900 / 0.884 / 0.881 in 2023-24 / 24-25 / 25-26). Rest days, new knocks: the injury report covers the rest.
P_PLAY = 0.89


def week_games(ids, games, schedule, injuries, players, start, end, opener):
    """Each player's remaining healthy games in [start, end]: (id, date) rows. A team's game is remaining until its
    box score is in; games before an injury return date don't count; out with no date or for the season = none."""
    day = lambda ts: (pd.Timestamp(ts) - opener).days + 1
    sched = schedule.assign(date=pd.to_datetime(schedule.date))
    sched["day"] = sched.date.map(day)
    sched = sched[(sched.day >= day(start)) & (sched.day <= day(end))]
    logged = games.astype({"day": float}).groupby("team_id").day.max()  # played once its box score is in
    sched = sched[sched.day > sched.team_id.map(logged).fillna(-1)]
    team = players.set_index("id").team_id.reindex(ids)
    inj = injuries.drop_duplicates("id").set_index("id").reindex(ids)
    ret = pd.to_datetime(inj.return_date)
    done = inj.season_out.fillna(False).astype(bool) | ((inj.status == "Out") & ret.isna())
    rows = pd.DataFrame({"id": ids, "team": team.values})[~done.values].merge(sched, left_on="team", right_on="team_id")
    rows = rows[rows.date >= rows.id.map(ret).fillna(pd.Timestamp.min)]
    return rows[["id", "date"]].reset_index(drop=True)


# Game-to-game spread of each stat, as variance / mean per game (pooled over players with 20+ games of 10+ minutes;
# 2024-25 / 2025-26 gave the same numbers). Points swing ~3x a Poisson count; threes, steals, blocks, turnovers ~1x.
VAR_RATIO = {"PTS": 3.0, "REB": 1.35, "AST": 1.25, "STL": 1.07, "BLK": 1.1, "3PM": 1.15, "TO": 1.02}
# Makes given attempts vs. a coin flip at his percentage (same seasons): FG 0.91, FT 1.0. So a shooter moves FG%/FT%
# only in proportion to his ATTEMPTS: a 90% free-throw shooter with 1 attempt a game barely moves it.
SHOT_VAR = {"FG%": 0.91, "FT%": 1.0}
PCT = {"FG%": ("FGM", "FGA"), "FT%": ("FTM", "FTA")}
# Projection error on top of game noise: a player's week total misses his projection by this share (FG%/FT%: this many
# percentage points), the same miss in all his games. Measured on next-7-day totals of 20+ MPG players at every 8-day
# checkpoint, 2023-24..2025-26 (early season and later came out alike).
PROJ_CV = {"PTS": 0.113, "REB": 0.096, "AST": 0.143, "STL": 0.152, "BLK": 0.199, "3PM": 0.164, "TO": 0.159,
           "FG%": 0.021, "FT%": 0.037}
# No category is ever called: injuries, streamers your opponent adds and lineup news can still flip it.
# Missed games come in runs (injury, rest): two games of the same player in a week are both missed or both played
# with this correlation (20+ games of 24+ MPG players, 2023-24..2025-26; 0.52-0.60 for 2-5 games a week).
RHO_MISS = 0.58
# What's still left early in the week: spread grows by this share with 6 days to go, fading to none with 2 left
# (fit on 8,100 replayed 10-on-10 matchups, 2023-24..2025-26; stable across halves). FT% swings most, FG% is on target.
EARLY = {"PTS": 0.22, "REB": 0.26, "AST": 0.16, "STL": 0.0, "BLK": 0.16, "3PM": 0.08, "TO": 0.18, "FG%": 0.02, "FT%": 0.30}
P_CAP = 0.01  # ponytail: a flat floor/cap, replace with a calibration fit once a few weeks of real results are in


def week_odds(banked, games, banked_opp, games_opp, cats, ids=None, ids_opp=None, days_left=2):
    """Chance to win each category this week and the matchup (most categories). banked: dict of totals so far
    (COUNT stats); games: array (games x COUNT) of per-game projections for each remaining game he'll START.
    Each game is played with P_PLAY. ids / ids_opp: the player behind each game row (projection error is shared by
    a player's games); None = each row its own player. days_left: days left in the week, today included (EARLY).
    Normal approximation per category; categories independent."""
    from math import erf, sqrt

    def side(b, g, who):
        g = np.asarray(g, dtype=float).reshape(-1, len(COUNT))
        who = np.arange(len(g)) if who is None else pd.factorize(pd.Series(list(who), dtype=object))[0]
        per = np.zeros((who.max() + 1 if len(g) else 0, len(COUNT)))
        np.add.at(per, who, P_PLAY * g)  # each player's expected remaining totals
        miss = P_PLAY * (1 - P_PLAY)  # variance of playing a game

        def runs(x):  # extra variance when a player's misses come together: covariance between his games
            s_ = np.zeros(len(per))
            np.add.at(s_, who, x)
            return miss * RHO_MISS * ((s_ ** 2).sum() - (x ** 2).sum())
        mu = {c: b.get(c, 0) + P_PLAY * g[:, k].sum() for k, c in enumerate(COUNT)}
        var = {c: (P_PLAY * VAR_RATIO.get(c, 1) * g[:, k] + P_PLAY * (1 - P_PLAY) * g[:, k] ** 2).sum()
               for k, c in enumerate(COUNT)}
        for pc, (m, a) in PCT.items():
            gm, ga = g[:, COUNT.index(m)], g[:, COUNT.index(a)]
            p_g = np.divide(gm, ga, out=np.zeros_like(gm), where=ga > 0)
            mu[pc] = mu[m] / mu[a] if mu[a] > 0 else 0.0
            dev = gm - mu[pc] * ga  # a missed game moves the % by how far his shooting is from the team's
            var[pc] = ((SHOT_VAR[pc] * P_PLAY * (ga * p_g * (1 - p_g)).sum() + miss * (dev ** 2).sum() + runs(dev)
                        + PROJ_CV[pc] ** 2 * (per[:, COUNT.index(a)] ** 2).sum()) / mu[a] ** 2 if mu[a] > 0 else 0.0)
        for k, c in enumerate(COUNT):
            if c in PROJ_CV:
                var[c] += PROJ_CV[c] ** 2 * (per[:, k] ** 2).sum() + runs(g[:, k])
        return mu, var

    (m1, v1), (m2, v2) = side(banked, games, ids), side(banked_opp, games_opp, ids_opp)
    rows = []
    for c in cats:
        diff = (m2[c] - m1[c]) if c == "TO" else (m1[c] - m2[c])  # fewer turnovers wins
        sd = sqrt(v1[c] + v2[c]) * (1 + EARLY.get(c, 0) * min(max(days_left - 2, 0), 4) / 4)
        p = (0.5 + 0.5 * erf(diff / (sd * sqrt(2)))) if sd > 0 else (1.0 if diff > 0 else 0.0 if diff < 0 else 0.5)
        rows.append({"cat": c, "me": m1[c], "opp": m2[c], "p_win": min(max(p, P_CAP), 1 - P_CAP)})
    out = pd.DataFrame(rows).set_index("cat")
    dist = np.array([1.0])  # number of categories won: Poisson-binomial
    for p in out.p_win:
        dist = np.convolve(dist, [1 - p, p])
    n = len(cats)
    win = dist[n // 2 + 1:].sum()
    tie = dist[n // 2] if n % 2 == 0 else 0.0
    return out, win + tie / 2


def week(proj, games, schedule, injuries, players, start, end, opener):
    """Projected category TOTALS for a fantasy week [start, end]: what each player already did this week plus,
    for each of his remaining healthy games (week_games), P_PLAY x his per-game projection (proj, which should
    include any role boost). FG%/FT% from the summed makes and attempts."""
    day = lambda ts: (pd.Timestamp(ts) - opener).days + 1
    g = games.astype({"day": float})
    played = g[(g.day >= day(start)) & (g.day <= day(end))]
    p = proj.set_index("id") if "id" in proj else proj
    actual = played.groupby("id")[COUNT + ["MPG"]].sum().reindex(p.index).fillna(0)
    left = week_games(p.index, games, schedule, injuries, players, start, end, opener).groupby("id").size()
    left = left.reindex(p.index).fillna(0).astype(int)  # healthy games left: shown
    exp = left * P_PLAY  # expected games played: what the totals use
    out = actual[COUNT].add(p[COUNT].fillna(0).mul(exp, axis=0))
    out["g_played"] = played.groupby("id").size().reindex(p.index).fillna(0).astype(int)
    out["g_left"] = left
    out["GP"] = out.g_played + exp
    out["MPG"] = (actual.MPG + p.MPG.fillna(0) * exp) / out.GP.where(out.GP > 0)
    out["FG%"], out["FT%"] = out.FGM / out.FGA, out.FTM / out.FTA
    return out


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
        done = games.empty or r > games.max()  # back after the last game = out for the season
        left.append(0 if out or done else max(0, SEASON_GAMES - (games < today).sum() - missed))
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
    df = proj.dropna(subset=["PTS"]).copy()  # GP 0 (out for the season) stays listed, with value 0
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
        # softplus per game, THEN times the share of games played: about equal to value over replacement for
        # good players, slides toward 0 (never below) under replacement, and always scales with availability,
        # so a player out most of the year is worth little no matter how good he is per game.
        score = np.logaddexp(0, per_game - replacement) * df.GP / SEASON_GAMES
    df[[f"z_{c}" for c in cats]] = z.values
    df["value_pg"] = per_game.round(2)
    df["value"] = score.round(2)
    return df.assign(_s=score).sort_values("_s", ascending=False).drop(columns="_s")  # exact order, not rounded ties
