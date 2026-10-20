"""In-season backtest: replay 2023-24, 2024-25 and 2025-26 every 8 days. At each checkpoint, update the preseason
projection with the games played so far (model.in_season) and score it against what each player did the REST of
the season. Fits model.IN_SEASON (how much a game counts, how fast old games fade) and checks it on the season
left out of the fit. Run: python backtest_inseason.py"""
import itertools

import numpy as np
import pandas as pd

import espn
import model

SEASONS = [2024, 2025, 2026]
DAYS = range(8, 160, 8)
K_GRID = [0, 50, 100, 200, 400, 800, 1600, 3200, 6400, np.inf]  # prior worth this many minutes
TAU_GRID = [np.inf, 120, 60, 30, 15]  # days for a game's weight to fall to 37%
KM_GRID = [0, 2, 4, 8, 16, 32, np.inf]  # prior MPG worth this many games
KH_GRID = [0, 5, 10, 20, 40, 80, np.inf]  # prior health worth this many team games
CATS = model.CATS["9-cat H2H"]


def prior_for(d, T):
    """Preseason projection for season T as the app would have made it: history + ESPN preseason, team and
    starting role from each player's first game (stand-in for the opening-night depth chart)."""
    first = espn.games(T).sort_values("day").groupby("id").first()
    age = d["players"].set_index("id").age - (espn.SEASON - T)
    p = model.project_by_role(d["stats"], T, first.team_id, first.GS, d["players"], age=age)
    p["health"] = p.health / model.AVAIL_SCALE  # raw share of games, same scale as in-season shares
    return p


def checkpoints(d):
    """(season, day, prior, games, rest-of-season actuals per player with 5+ games left)."""
    for T in SEASONS:
        g, prior = espn.games(T), prior_for(d, T)
        for D in DAYS:
            rest = g[g.day >= D]
            n = rest.groupby("id").size()
            a = rest.groupby("id")[model.COUNT + ["MPG"]].mean()[n >= 5]
            team_left = rest.drop_duplicates(["team_id", "day"]).groupby("team_id").size()
            last = g[g.day < D].sort_values("day").groupby("id").team_id.last()
            a["gp"] = n
            a["share"] = (n / rest.groupby("id").team_id.last().map(team_left)).clip(upper=1)
            a["share"] = a.share.where(~a.index.isin(last.index), n / last.map(team_left).reindex(a.index)).clip(upper=1)
            yield T, D, prior, g, a


def sums(g, D, tau, cols):
    """Recency-weighted totals of `cols` and weights per player for games before day D."""
    g = g[g.day < D]
    w = np.exp(-(D - g.day) / tau)
    return g[cols].mul(w, axis=0).groupby(g.id).sum(), w.groupby(g.id).sum()


def _best_k(r0, num, den, act, scale, wt, grid):
    """Best prior weight k for blended = (k*r0 + num) / (k + den), separately for rows running above and below
    the prior. Returns ((k_up, err_up), (k_down, err_down)); err = weighted squared error of blended*scale vs act."""
    up = (num / np.where(den > 0, den, np.nan)) > r0
    out = []
    for rows in (up, ~up):
        best = None
        for k in grid:
            r = r0 if np.isinf(k) else (k * r0 + num) / (k + den)
            e = np.nansum((r * scale - act)[rows] ** 2 * wt[rows])
            if best is None or e < best[1]:
                best = (k, e)
        out.append(best)
    return out


def fit(cps, split=True):
    """Grid search each stat's prior weight and fade (tau) on rest-of-season error, then the same for minutes,
    then Kh for availability. split: separate weights for players running above vs below their prior.
    Errors weighted by rest-of-season games (a guy who plays 60 more games matters more than one who plays 5)."""
    K, tau = {}, {}
    data = {}
    for t in TAU_GRID:
        parts = []
        for T, D, prior, g, a in cps:
            p = prior.set_index("id").reindex(a.index)
            s, wsum = sums(g, D, t, model.COUNT + ["MPG"])
            s, wsum = s.reindex(a.index).fillna(0), wsum.reindex(a.index).fillna(0)
            parts.append(pd.DataFrame({**{f"r0_{c}": p[c] / p.MPG for c in model.COUNT}, **{f"n_{c}": s[c] for c in model.COUNT},
                                       "den": s.MPG, "m0": p.MPG, "mnum": s.MPG, "mden": wsum, "mpg": a.MPG, "gp": a.gp,
                                       **{f"a_{c}": a[c] for c in model.COUNT}}))
        data[t] = pd.concat(parts).dropna(subset=["m0"])
    pick = (lambda r: (r[0][0], r[1][0])) if split else None
    for c in model.COUNT:
        best = None
        for t, df in data.items():
            res = _best_k(df[f"r0_{c}"].values, df[f"n_{c}"].values, df.den.values, df[f"a_{c}"].values, df.mpg.values,
                          df.gp.values, K_GRID)
            if not split:  # one k for everyone: re-score the grid on all rows
                errs = [sum(_best_k(df[f"r0_{c}"].values, df[f"n_{c}"].values, df.den.values, df[f"a_{c}"].values,
                                    df.mpg.values, df.gp.values, [k])[i][1] for i in (0, 1)) for k in K_GRID]
                res = ((K_GRID[int(np.argmin(errs))], min(errs)), (K_GRID[int(np.argmin(errs))], 0))
            err = res[0][1] + res[1][1]
            if best is None or err < best[0]:
                best = (err, (res[0][0], res[1][0]) if split else res[0][0], t)
        K[c], tau[c] = best[1], best[2]
    best = None
    for t, df in data.items():
        args = (df.m0.values, df.mnum.values, df.mden.values, df.mpg.values, np.ones(len(df)), df.gp.values)
        if split:
            res = _best_k(*args, KM_GRID)
            err, km = res[0][1] + res[1][1], (res[0][0], res[1][0])
        else:
            errs = [sum(r[1] for r in _best_k(*args, [k])) for k in KM_GRID]
            err, km = min(errs), KM_GRID[int(np.argmin(errs))]
        if best is None or err < best[0]:
            best = (err, km, t)
    Km, tau_m = best[1], best[2]
    fin = lambda x: tuple(float(min(v, 1e12)) for v in x) if isinstance(x, tuple) else float(min(x, 1e12))
    K, Km = {c: fin(k) for c, k in K.items()}, fin(Km)
    best = None
    for kh in KH_GRID:
        err = wsum = 0
        for T, D, prior, g, a in cps:
            h = model.in_season(prior, g, D, K, tau, Km, tau_m, min(kh, 1e12), 10).set_index("id")
            e = (h.health.reindex(a.index) - a.share) ** 2
            ok = e.notna()
            err, wsum = err + e[ok].sum(), wsum + ok.sum()
        if best is None or err / wsum < best[0]:
            best = (err / wsum, kh)
    return dict(K=K, tau=tau, Km=Km, tau_m=tau_m, Kh=float(min(best[1], 1e12)), new_mpg=10)


def role(g, D, params):
    """promotion() as the app computes it, with past-season stand-ins: 'starting now' = started his team's next game,
    preseason role = started his first game."""
    nxt = g[g.day >= D].sort_values("day").groupby("id").first().GS
    first = g.sort_values("day").groupby("id").GS.first()
    return model.promotion(g, D, nxt, params["tau_m"], first)


def predict(T, D, prior, g, params, role_beta=0.0):
    p = model.in_season(prior, g, D, **params).set_index("id")
    return model.with_role(p, role(g, D, params), role_beta) if role_beta else p


def score(cps, params, role_beta=0.0):
    """Per checkpoint: rest-of-season per-game error by stat, and how well season value ranks the top 150."""
    out = []
    for T, D, prior, g, a in cps:
        p = predict(T, D, prior, g, params, role_beta).reindex(a.index).dropna(subset=["PTS"])
        act = a.loc[p.index]
        row = {"season": T, "day": D}
        for c in ["PTS", "REB", "AST", "STL", "BLK", "3PM", "TO", "MPG"]:
            row[c] = np.sqrt(np.average((p[c] - act[c]) ** 2, weights=act.gp))
        p["GP"], act = p.health * 82, act.assign(GP=act.share * 82)
        pv = model.value(p.reset_index(names="id"), CATS, 156).set_index("id").value
        av = model.value(act.reset_index(names="id"), CATS, 156).set_index("id").value
        top = av.nlargest(150).index
        row["value_rank"] = pv[top].rank().corr(av[top].rank())
        out.append(row)
    return pd.DataFrame(out)


def role_fit(cps, params):
    """Rest-of-season MPG change per unit of promotion (on top of the blend), least squares weighted by games left."""
    x, y, w = [], [], []
    for T, D, prior, g, a in cps:
        p = model.in_season(prior, g, D, **params).set_index("id")
        f = pd.DataFrame({"promo": role(g, D, params), "r": a.MPG - p.MPG, "n": a.gp}).dropna()
        x.append(f.promo); y.append(f.r); w.append(f.n)
    x, y, w = pd.concat(x), pd.concat(y), pd.concat(w)
    return float(np.sum(x * y * w) / np.sum(x * x * w))


def breakouts(cps, params, role_beta=0.0):
    """Players 8+ PTS+REB+AST/game above (below) their preseason projection after 8+ games: share of the gap the
    model keeps vs the share that held the rest of the season."""
    rows = []
    for T, D, prior, g, a in cps:
        if D not in (24, 40, 64, 96):
            continue
        p = predict(T, D, prior, g, params, role_beta)
        pre = prior.set_index("id")
        before = g[g.day < D]
        td = before.groupby("id")[["PTS", "REB", "AST"]].mean().sum(axis=1)
        f = pd.DataFrame({"pre": pre.PTS + pre.REB + pre.AST, "todate": td, "model": p.PTS + p.REB + p.AST,
                          "rest": a.PTS + a.REB + a.AST, "n": a.gp, "played": before.groupby("id").size()}).dropna()
        rows.append(f[f.played >= 8])
    df = pd.concat(rows)
    out = {}
    for name, sel in [("up 8+", df.todate - df.pre >= 8), ("down 8+", df.todate - df.pre <= -8)]:
        x = df[sel]
        m = lambda c: np.average(x[c], weights=x.n)
        gap = m("todate") - m("pre")
        out[name] = {"players": len(x), "model keeps": (m("model") - m("pre")) / gap, "held": (m("rest") - m("pre")) / gap,
                     "model": m("model"), "actual rest": m("rest")}
    return pd.DataFrame(out).T


def opportunity(cps, params, role_beta=0.0):
    """Next-14-day minutes for players moving into / out of the starting lineup: fit MPG per unit of
    model.promotion on two seasons, score on the third. 'Starting now' in the past = started his team's next game
    (the live app reads ESPN's depth chart with injured players skipped)."""
    rows = []
    for T, D, prior, g, a in cps:
        p = predict(T, D, prior, g, params, role_beta)  # the rest-of-season projection, role included
        after = g[(g.day >= D) & (g.day < D + 14)]
        nxt = g[g.day >= D].sort_values("day").groupby("id").first().GS
        n = after.groupby("id").size()
        first = g.sort_values("day").groupby("id").GS.first()  # preseason role stand-in, as in prior_for
        f = pd.DataFrame({"promo": model.promotion(g, D, nxt, params["tau_m"], first), "pred": p.MPG,
                          "y": after.groupby("id").MPG.mean(), "n": n}).dropna()
        rows.append(f[f.n >= 2].assign(season=T))
    df = pd.concat(rows).reset_index(drop=True)
    for held in SEASONS:
        tr, te = df[df.season != held], df[df.season == held]
        b = np.sum(tr.promo * (tr.y - tr.pred) * tr.n) / np.sum(tr.promo ** 2 * tr.n)
        m = te.promo.abs() > 0.5
        rm = lambda e, k: np.sqrt(np.average(e[k] ** 2, weights=te.n[k]))
        print(f"{held}: {b:.2f} MPG per start-share; next-14d MPG error, role changers "
              f"{rm(te.y - te.pred, m):.2f} -> {rm(te.y - te.pred - b * te.promo, m):.2f}, everyone "
              f"{rm(te.y - te.pred, te.n > 0):.2f} -> {rm(te.y - te.pred - b * te.promo, te.n > 0):.2f}")
    return np.sum(df.promo * (df.y - df.pred) * df.n) / np.sum(df.promo ** 2 * df.n)


def main():
    d = espn.load()
    cps = list(checkpoints(d))
    results, bo = [], []
    for held in SEASONS:  # fit on the other two seasons, score on the held-out one
        train, test = [c for c in cps if c[0] != held], [c for c in cps if c[0] == held]
        sym, asym = fit(train, split=False), fit(train, split=True)
        beta = role_fit(train, asym)
        print(held, "fit:", asym, "role MPG per start-share:", round(beta, 2))
        for name, p, b in [("current (one weight)", sym, 0.0), ("A: separate up/down", asym, 0.0),
                           ("A+B: plus role", asym, beta)]:
            results.append(score(test, p, b).assign(method=name))
            bo.append(breakouts(test, p, b).assign(method=name, season=held))
    r = pd.concat(results)
    r["phase"] = pd.cut(r.day, [0, 30, 80, 200], labels=["first month", "months 2-3", "after"])
    cols = ["PTS", "REB", "AST", "STL", "BLK", "3PM", "TO", "MPG", "value_rank"]
    print("\nRest-of-season error (RMSE per game; value_rank = rank correlation, higher better), held-out seasons:")
    print(r.groupby(["phase", "method"], observed=True)[cols].mean().round(3).to_string())
    b = pd.concat(bo).reset_index(names="group")
    print("\nBreakouts / drops, held-out seasons (share of the gap kept by the model vs share that held):")
    print(b.groupby(["group", "method"])[["players", "model keeps", "held", "model", "actual rest"]].mean().round(2).to_string())
    asym = fit(cps, split=True)
    print("\nAll seasons fit (ship this):", asym, "role (rest of season):", round(role_fit(cps, asym), 2))


if __name__ == "__main__":
    main()
