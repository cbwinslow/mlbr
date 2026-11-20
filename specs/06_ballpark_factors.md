# Spec 06: Empirical Multi-Year Regressed Ballpark Factors (2002–2025)

## 1. Mathematical Objective & Theoretical Grounding
Traditional baseball statistics and advanced metrics conflate a player's intrinsic true-talent ability with the environmental and geometric characteristics of the ballparks in which they play (altitude, air density, wall distances, foul territory, outfield wall height, wind patterns).

Following foundational sabermetric theory (Bill James, Tom Tango, David Smyth, Dan Szymborski):
- Raw single-season park scoring ratios are severely contaminated by small sample variance ($N \approx 81$ home games per team, scheduled against non-random divisional opponents).
- A 1-year unregressed ratio over-reacts to flukish weather or pitching staff extremes.
- We formulate a strictly causal, multi-year ($3$-year rolling weighted $3/2/1$) regressed empirical Park Factor engine across 24 seasons ($2002–2025$) for all 30 Major League venues.

Target Metrics:
1. `pf_runs`: Overall Run Park Factor ($100 = \text{neutral}$)
2. `pf_hr`: Home Run Park Factor
3. `pf_hr_rhb`, `pf_hr_lhb`: Handedness-specific Home Run Factors
4. `pf_so`: Strikeout Park Factor
5. `pf_bb`: Walk Park Factor
6. `pf_babip`: Hit on Contact (BABIP) Park Factor

## 2. Input Features & Data Lake Query
- `v_retrosheet_games`: Game scores, dates, park identifiers (`PARK_ID`), home and road team identifiers (`HOME_TEAM_ID`, `AWAY_TEAM_ID`).
- `v_retrosheet_plays`: 16.2M+ plate appearances with `EVENT_CD` (HR=23, SO=3, BB=14/15, hits=20..22, BIP=2, 18, 19, 20..22) and batter handedness (`BAT_HAND_CD`).
- `data/raw/mlbmeta/park_dimensions.parquet`: Venue name, city, elevation, azimuth, wall distances, roof type, and turf type.

## 3. Mathematical Formulation & Empirical Shrinkage
For team $i$ playing in home park $p$ during season $t$:
Let $M_{\text{home, } p}$ be the metric total for both teams in home games at park $p$, and $G_{\text{home}}$ be the number of home games.
Let $M_{\text{road, } i}$ be the metric total for both teams in road games played by team $i$, and $G_{\text{road}}$ be road games.

1. **Scoring Ratio:**
$$\text{Ratio} = \frac{M_{\text{home, } p} / G_{\text{home}}}{M_{\text{road, } i} / G_{\text{road}}}$$

2. **League Symmetry Adjustment ($N = 30$ teams):**
$$\text{PF}_{\text{raw}} = \frac{N \cdot \text{Ratio}}{N - 1 + \text{Ratio}} \times 100$$

3. **3-Year Rolling Regressed Empirical Factor:**
Weighting across $(t, t-1, t-2)$ with weights $(3, 2, 1)$:
$$\text{PF}_{\text{3yr}} = \frac{3 \cdot \text{PF}_{\text{raw}, t} \cdot G_t + 2 \cdot \text{PF}_{\text{raw}, t-1} \cdot G_{t-1} + 1 \cdot \text{PF}_{\text{raw}, t-2} \cdot G_{t-2}}{3 G_t + 2 G_{t-1} + 1 G_{t-2}}$$
With empirical shrinkage denominator $M$:
$$\text{PF}_{\text{reg}} = 100 + (\text{PF}_{\text{3yr}} - 100) \times \frac{G_{\text{eff}}}{G_{\text{eff}} + M}$$
Where $M = 40$ games for runs, $M = 1500$ PA for HR, and $M = 2000$ PA for SO/BB/BABIP.

## 4. Implementation Contracts & Deliverables
- Executable R Pipeline: `models/06_ballpark_factors_empirical.R`
- Gold Materialized Dataset: `data/processed/ballpark_factors_2002_2025.parquet`
  - ADR-001 ZSTD Parquet sorted by `(season, park_id)`
  - Schema: `season`, `park_id`, `venue_name`, `team_id`, `elevation_ft`, `roof_type`, `g_home`, `g_road`, `pf_runs_raw`, `pf_runs`, `pf_hr`, `pf_hr_rhb`, `pf_hr_lhb`, `pf_so`, `pf_bb`, `pf_babip`.
- Preserved 4-Asset Dark Cyber Visualizations:
  1. `pf01_park_factor_runs_scatter.*` (Run Factor vs Altitude & Raw Ratio)
  2. `pf02_handedness_hr_asymmetry.*` (LHB vs RHB HR Asymmetry Frontier)

## 5. Verification & Acceptance Criteria
1. Row count: Exactly 30 MLB primary venues per season (plus neutral-site games identified and partitioned cleanly).
2. Monotonicity & Invariants: All `pf_*` columns strictly finite, non-null, within plausible ranges ($50 \le \text{PF} \le 160$).
3. Known Park Ground Truth:
   - Coors Field (`DEN02`): `pf_runs >= 120.0` across 3-year regressed seasons.
   - Great American Ball Park (`CIN09`): `pf_hr >= 110.0` (active venue code CIN09).
   - Oracle Park (`SFO03`): `pf_hr <= 90.0`.
   - Yankee Stadium (`NYC21`): `pf_hr_lhb >= pf_hr_rhb + 8.0` index points (3-year regressed short porch asymmetry).
4. Out-of-Time Stability: Year-over-year correlation $r(\text{PF}_t, \text{PF}_{t+1}) \ge 0.75$ for continuous parks.

## 6. Experiment Registry & Metadata
- ID: `EXP-006-BALLPARK-FACTORS`
- Tier: Level 3 Foundation / Ballpark Environmental Priors
- Status: VALIDATED upon successful run and audit pass.
