"""Run: python test_model.py"""
import pandas as pd

import model


def line(id, season, GP, MPG, FGM, FGA, **kw):
    base = dict(PTS=15, REB=5, AST=3, STL=1, BLK=0.5, TO=2, FTM=2, FTA=2.5)
    return dict(id=id, season=season, source="actual", GP=GP, MPG=MPG, FGM=FGM, FGA=FGA, **{**base, **kw}, **{"3PM": 1.5})


stats = pd.DataFrame(
    [line(i, s, 70, 30, 6, 13) for i in range(2, 40) for s in (2024, 2025, 2026)]
    + [line(1, s, 70, 30, 9, 15) for s in (2024, 2025, 2026)]       # 60% on 15 shots
    + [line(99, s, 70, 30, 1.8, 3) for s in (2024, 2025, 2026)]      # 60% on 3 shots
    + [line(50, 2024, 80, 30, 6, 13), line(50, 2025, 20, 30, 6, 13), line(50, 2026, 78, 30, 6, 13)])  # one injury year

proj = model.project(stats, 2027).set_index("id")
v = model.value(proj.reset_index(), model.CATS["9-cat H2H"], 30).set_index("id")
assert v.loc[1, "z_FG%"] > 2 * v.loc[99, "z_FG%"], "volume should matter for FG%"
assert proj.loc[50, "GP"] > proj.loc[2, "GP"] - 3, "one injury season shouldn't sink availability"

schedule = pd.DataFrame({"team_id": 1, "date": pd.date_range("2026-10-21", periods=82, freq="2D").strftime("%Y-%m-%d")})
players = pd.DataFrame({"id": [1, 2, 3], "team_id": 1})
injuries = pd.DataFrame({"id": [2, 3], "status": "Out", "return_date": ["2026-11-20", "2027-01-01"],
                         "season_out": [False, True], "note": ""})
left = model._games_left(pd.Index([1, 2, 3]), injuries, schedule, players, "2026-10-01")
assert list(left) == [82, 67, 0], left
print("ok")
