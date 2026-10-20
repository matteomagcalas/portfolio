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
# in-season: one 40-point game shouldn't turn a 10-point scorer into a star, a full season of 20s should move him most of the way
prior = pd.DataFrame({"id": [7], "MPG": [25.0], "health": [0.8], **{c: [10.0 if c == "PTS" else 2.0] for c in model.COUNT}})
def game(day, pts, mins=25.0):
    return {"id": 7, "day": day, "team_id": 1, "MPG": mins, **{c: (pts if c == "PTS" else 2.0) for c in model.COUNT}}
one = model.in_season(prior, pd.DataFrame([game(1, 40)]), 2, **model.IN_SEASON).iloc[0]
season = model.in_season(prior, pd.DataFrame([game(d, 20) for d in range(1, 120, 2)]), 120, **model.IN_SEASON).iloc[0]
none = model.in_season(prior, pd.DataFrame([game(1, 40)]).iloc[:0], 1, **model.IN_SEASON).iloc[0]
assert 10 < one.PTS < 15, one.PTS
assert season.PTS > 17, season.PTS
assert abs(none.PTS - 10) < 1e-9, none.PTS
# weekly totals: A scored 15 in game 1 of 3 (10 ppg projected); B scored 0, then ruled out past the week
opener = pd.Timestamp("2026-10-20")
sched = pd.DataFrame({"team_id": 1, "date": ["2026-10-20", "2026-10-22", "2026-10-24"]})
wk_games = pd.DataFrame([{**game(1, 15), "id": 7}, {**game(1, 0), "id": 8}])
proj = pd.DataFrame({"id": [7, 8], "MPG": 25.0, **{c: 10.0 if c == "PTS" else 2.0 for c in model.COUNT}})
inj = pd.DataFrame({"id": [8], "status": ["Out"], "return_date": ["2026-11-03"], "season_out": [False], "note": ""})
pl = pd.DataFrame({"id": [7, 8], "team_id": 1})
wk = model.week(proj, wk_games, sched, inj, pl, "2026-10-19", "2026-10-25", opener)
assert abs(wk.loc[7, "PTS"] - (15 + 2 * model.P_PLAY * 10)) < 1e-9, wk.loc[7, "PTS"]
assert wk.loc[8, "PTS"] == 0 and wk.loc[8, "g_left"] == 0, wk.loc[8]
print("ok")
