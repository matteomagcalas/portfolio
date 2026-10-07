"""Backtest: project 2025-26 from earlier seasons, score against 2025-26 actuals.
Compares our model to ESPN's own 2025-26 preseason projection and to "repeat last
season" (roughly what the 2025 R notebook did). Run: python backtest.py"""
import itertools

import numpy as np
import pandas as pd

import espn
import model

TARGET = 2026
STATS = ["PTS", "REB", "AST", "STL", "BLK", "3PM", "TO", "MPG", "FG%", "FT%"]
CATS = model.CATS["9-cat H2H"]


def score(proj, actual, ids):
    """Per-stat correlation for regulars, plus rank correlation of season value."""
    a = actual[(actual.GP >= 20) & (actual.MPG >= 15) & actual.id.isin(ids)].set_index("id")
    p = proj.set_index("id").reindex(a.index)
    keep = p.PTS.notna()
    corr = {c: np.corrcoef(p.loc[keep, c], a.loc[keep, c])[0, 1] for c in STATS}
    av = model.value(actual, CATS, 130).set_index("id").value
    pv = model.value(proj, CATS, 130).set_index("id").value
    top = av.nlargest(150).index.intersection(pv.index)  # how well we order the players who mattered
    corr["value_rank"] = pv[top].rank().corr(av[top].rank())
    return corr


def pct(df):
    return df.assign(**{"FG%": df.FGM / df.FGA, "FT%": df.FTM / df.FTA})


def main():
    d = espn.load()
    stats = d["stats"]
    age = d["players"].set_index("id").age - (espn.SEASON - TARGET)  # age during the backtest season
    actual = pct(stats[(stats.source == "actual") & (stats.season == TARGET)])
    team = actual.set_index("id").team_id  # the team each player actually played for that season
    projs = {
        "repeat last season": pct(stats[(stats.source == "actual") & (stats.season == TARGET - 1)]),
        "ESPN 2025-26 proj": pct(stats[(stats.source == "espn") & (stats.season == TARGET)]),
        "ours (default)": model.project(stats, TARGET, current_team=team, age=age, players=d["players"]),
        "ours, ESPN-blend minutes": model.project(stats, TARGET, current_team=team, age=age, roster_min=False),
        "ours, no ESPN input": model.project(stats, TARGET, espn_rate_blend=0, current_team=team, age=age,
                                             players=d["players"]),
    }
    ids = set.intersection(*(set(p.id) for p in projs.values()))  # same players for every method
    print(pd.DataFrame({k: score(p, actual, ids) for k, p in projs.items()}).T.round(3).to_string())

    last = stats[(stats.source == "actual") & (stats.season == TARGET - 1)].set_index("id").team_id.reindex(team.index)
    movers = set(team.index[(team > 0) & (last > 0) & (team != last)]) & ids
    print(f"\nPlayers who changed teams ({len(movers)}): accuracy by how far their production leans on ESPN")
    rows = {}
    for mb in [0.5, 0.65, 0.8, 0.95]:
        s = score(model.project(stats, TARGET, current_team=team, moved_rate_blend=mb, age=age), actual, movers)
        rows[mb] = {c: s[c] for c in STATS if c != "MPG"}
    print(pd.DataFrame(rows).T.round(3).assign(mean=lambda d: d.mean(axis=1).round(3)).to_string())

    print("\nTuning (value_rank / mean stat corr):")
    grid = []
    for w, rb, mb in itertools.product([(1, .9, .8), (1, .7, .5), (1, 1, 1), (1, .8)],
                                       [0.3, 0.5, 0.7], [0.5, 0.7, 0.9]):
        s = score(model.project(stats, TARGET, weights=w, espn_rate_blend=rb, espn_min_blend=mb, current_team=team, age=age), actual, ids)
        grid.append({"weights": w, "rate_blend": rb, "min_blend": mb, "value_rank": s["value_rank"],
                     "mean_corr": np.mean([s[c] for c in STATS])})
    print(pd.DataFrame(grid).sort_values("mean_corr", ascending=False).round(3).head(10).to_string(index=False))


if __name__ == "__main__":
    main()
