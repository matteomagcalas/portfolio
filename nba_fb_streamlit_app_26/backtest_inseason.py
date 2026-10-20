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


def fit(cps):
    """Grid search each stat's (K, tau) on per-minute rate error, then Km/tau_m on MPG, Kh on availability.
    Errors weighted by rest-of-season games (a guy who plays 60 more games matters more than one who plays 5)."""
    K, tau = {}, {}
    rows = []
    for T, D, prior, g, a in cps:
        p = prior.set_index("id")
        for t in TAU_GRID:
            s, _ = sums(g, D, t, model.COUNT + ["MPG"])
            rows.append((T, D, t, p, s, a))
    for c in model.COUNT:
        best = None
        for k, t in itertools.product(K_GRID, TAU_GRID):
            err = wsum = 0
            for T, D, tt, p, s, a in rows:
                if tt != t:
                    continue
                r0 = (p[c] / p.MPG).reindex(a.index)
                num, den = s[c].reindex(a.index).fillna(0), s.MPG.reindex(a.index).fillna(0)
                if np.isinf(k):
                    r = r0
                else:
                    r = ((k * r0).fillna(0) + num) / (k * r0.notna() + den).where(lambda x: x > 0)
                    r = r.where(r0.notna() | (den > 0))
                e = (r * a.MPG - a[c]) ** 2 * a.gp
                ok = e.notna()
                err, wsum = err + e[ok].sum(), wsum + a.gp[ok].sum()
            if best is None or err / wsum < best[0]:
                best = (err / wsum, k, t)
        K[c], tau[c] = best[1], best[2]
    best = None
    for km, t in itertools.product(KM_GRID, TAU_GRID):
        err = wsum = 0
        for T, D, prior, g, a in cps:
            gg = g[g.day < D]
            w = np.exp(-(D - gg.day) / t)
            num, den = (gg.MPG * w).groupby(gg.id).sum().reindex(a.index), w.groupby(gg.id).sum().reindex(a.index)
            m0 = prior.set_index("id").MPG.reindex(a.index)
            m = m0 if np.isinf(km) else (km * m0 + num) / (km + den)
            e = (m - a.MPG) ** 2 * a.gp
            ok = e.notna()
            err, wsum = err + e[ok].sum(), wsum + a.gp[ok].sum()
        if best is None or err / wsum < best[0]:
            best = (err / wsum, km, t)
    Km, tau_m = best[1], best[2]
    best = None
    for kh in KH_GRID:
        err = wsum = 0
        for T, D, prior, g, a in cps:
            h = model.in_season(prior, g, D, {c: min(k, 1e12) for c, k in K.items()}, tau, min(Km, 1e12), tau_m, min(kh, 1e12), 10).set_index("id")
            e = (h.health.reindex(a.index) - a.share) ** 2
            ok = e.notna()
            err, wsum = err + e[ok].sum(), wsum + ok.sum()
        if best is None or err / wsum < best[0]:
            best = (err / wsum, kh)
    fin = lambda x: float(min(x, 1e12))  # inf = ignore this season's games: a huge prior weight does the same
    return dict(K={c: fin(k) for c, k in K.items()}, tau=tau, Km=fin(Km), tau_m=tau_m, Kh=fin(best[1]), new_mpg=10)


def score(cps, params):
    """Per checkpoint: rest-of-season per-game error by stat, and how well season value ranks the top 150."""
    out = []
    for T, D, prior, g, a in cps:
        p = model.in_season(prior, g, D, **params).set_index("id").reindex(a.index).dropna(subset=["PTS"])
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


def opportunity(cps, params):
    """Next-14-day minutes for players moving into / out of the starting lineup: fit MPG per unit of
    model.promotion on two seasons, score on the third. 'Starting now' in the past = started his team's next game
    (the live app reads ESPN's depth chart with injured players skipped)."""
    rows = []
    for T, D, prior, g, a in cps:
        p = model.in_season(prior, g, D, **params).set_index("id")
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
    prior_only = dict(K={c: 1e12 for c in model.COUNT}, tau={c: np.inf for c in model.COUNT}, Km=1e12, tau_m=np.inf,
                      Kh=1e12, new_mpg=10)
    to_date = dict(K={c: 1e-9 for c in model.COUNT}, tau={c: np.inf for c in model.COUNT}, Km=1e-9, tau_m=np.inf,
                   Kh=1e-9, new_mpg=10)
    last_15 = dict(K={c: 1e-9 for c in model.COUNT}, tau={c: 7 for c in model.COUNT}, Km=1e-9, tau_m=7,
                   Kh=1e-9, new_mpg=10)
    results = []
    for held in SEASONS:  # fit on the other two seasons, score on the held-out one
        params = fit([c for c in cps if c[0] != held])
        test = [c for c in cps if c[0] == held]
        for name, p in [("preseason only", prior_only), ("season to date only", to_date),
                        ("recent games only", last_15), ("blend (fit on other seasons)", params)]:
            results.append(score(test, p).assign(method=name))
        print(held, "fit:", params)
    r = pd.concat(results)
    r["phase"] = pd.cut(r.day, [0, 30, 80, 200], labels=["first month", "months 2-3", "after"])
    cols = ["PTS", "REB", "AST", "STL", "BLK", "3PM", "TO", "MPG", "value_rank"]
    print("\nRest-of-season error (RMSE per game; value_rank = rank correlation, higher better), held-out seasons:")
    print(r.groupby(["phase", "method"], observed=True)[cols].mean().round(3).to_string())
    print("\nAll seasons fit (ship this):", fit(cps))


if __name__ == "__main__":
    main()
