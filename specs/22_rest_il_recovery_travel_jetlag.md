# Spec 22: Starter Rest-Day Recovery, IL-Return Biomechanics & Circadian Travel Jet-Lag (`EXP-022`)

## 1. Mathematical Objective & Theoretical Grounding

Levels 2 and 3 of `mlbr` model player true talent (`EXP-004`, `EXP-007`), park factors (`EXP-006`), weather/umpires/framing (`EXP-012`), and rolling 3-day bullpen fatigue (`EXP-013`). Two critical physiological and logistical variables remain unmodeled: **starting pitcher recovery/IL-return ramps** and **team circadian jet-lag from cross-country travel** (Winter et al. 2009, *Sleep*; Song et al. 2017, *PNAS*; Lichtman & Tango rest-day studies).

### Part A: Within-Pitcher Rest-Day Recovery & Post-IL Ramp (`2018–2025`, `n ~ 33,500` Starts)
For each starting pitcher $p$ in season $s$ on date $t$, let $\Delta d_{p,t} = t - t_{\text{prev}}$ be exact days of rest since his prior regular-season start (zero cross-season leakage). We classify outings into five rest regimes (`4d Short`, `5d Normal`, `6d Extra`, `7–14d Extended`, `15+d IL Return`) plus post-IL ramp sequence (`IL Return #1`, `IL Return #2`, `IL Return #3`).

To eliminate selection bias (aces pitch on 5d rest more often than depth starters), we estimate **within-pitcher-season fixed-effect deltas**:

$$\Delta y_{p,s,t} = y_{p,s,t} - \bar{y}_{p,s}$$

across fastball velocity (`ff_velo`), strike/zone rate (`zone_pct`), `CSW%`, pitch count (`n_pitches`), and batters faced (`n_bf`). We then validate strictly chronologically (`2018–2023` train $\to$ locked `2024–2025` OOS) whether adding rest/IL-return indicators to prior-start workload improves OOS `BF` RMSE and `SO` RMSE (especially on IL-return starts).

### Part B: Haversine Travel Distance & Circadian Jet-Lag (`2015–2025`, `n ~ 45,000+` Team-Games)
Joining `v_retrosheet_games` with exact ballpark coordinates `(lat, lon)` from `park_dimensions.parquet`, we compute exact great-circle **Haversine travel miles** between consecutive team game venues and approximate **circadian time-zone shift**:

$$\Delta \text{TZ}_{c,t} = \text{round}\!\left(\frac{\text{lon}_t - \text{lon}_{t-1}}{15^\circ}\right) \in \{-3, -2, -1, 0, +1, +2, +3\}\text{ hours}$$

Testing **Eastward Travel ($\Delta \text{TZ} \ge +2\text{h}$, losing sleep)** vs. **Westward Travel ($\Delta \text{TZ} \le -2\text{h}$)** vs. **No-Rest Red-Eye Travel (`days_between == 1` & `miles >= 1000`)** on team scoring (`runs_for`, `runs_against`) and win rate (`win_fl`), controlling for home-field advantage and club season win%, strictly trained on `2015–2023` and tested OOS on locked `2024–2025`.

## 2. Input Features & Zero-Copy DuckDB Lake Query
- **Part A:** `v_statcast_features` (`2018–2025`, `game_type = 'R'`), starter outings matched via first-inning starter identification (`inning = 1` & `at_bat_number` minimum per half-inning, capturing even short 30-pitch IL-return ramps without survivorship bias!).
- **Part B:** `v_retrosheet_games` (`2015–2025`) joined with `data/raw/park_dimensions.parquet` (or `data/raw/mlb_meta/park_dimensions.parquet` resolved dynamically).

## 3. Target Variables & Loss Functions
- **Part A:** Within-pitcher $\Delta \text{Velo}$ (mph), $\Delta \text{CSW\%}$, $\Delta \text{Pitches}$, $\Delta \text{BF}$; locked `2024–2025` OOS `BF` RMSE and `SO` RMSE overall and on IL-return starts (`15+d`).
- **Part B:** Club run differential (`runs_for - runs_against`), `runs_for`, and win probability Brier/Log-Loss on locked `2024–2025` OOS (`p < 0.05` two-sided hypothesis tests).

## 4. Validation Strategy (Zero Temporal Leakage)
- Part A: `2018–2023` train $\to$ locked `2024–2025` OOS (`n ~ 9,600` starts).
- Part B: `2015–2023` train $\to$ locked `2024–2025` OOS (`n ~ 9,700` team-games).

## 5. Implementation Blueprint
- **Script:** `models/22_rest_il_recovery_travel_jetlag.R` (`--smoke` / `--full`)
- **Gold Tables:** `data/processed/pitcher_rest_il_recovery_2018_2025.parquet`, `data/processed/team_travel_jetlag_2015_2025.parquet`, `data/processed/rest_travel_oos_ablation_2024_2025.parquet`
- **Visualizations (Dark Cyber 4-Asset):** `rst01_pitcher_rest_velo_csw_curve`, `rst02_il_return_pitch_count_ramp`, `rst03_circadian_jetlag_win_run_impact`
- **Study & Lab:** `site/analyses/22_rest_il_recovery_travel_jetlag.qmd`, `workbooks/lab22_rest_il_recovery_travel_jetlag_workbook.R`, `site/workbooks/lab22_rest_il_recovery_travel_jetlag_workbook.qmd`
- **Tests & ADR:** `pkg/mlbr/tests/testthat/test-core.R`, `DECISIONS.md` (`ADR-018`), `experiments/registry.json` (`EXP-022-REST-IL-RECOVERY-TRAVEL-JETLAG`).
