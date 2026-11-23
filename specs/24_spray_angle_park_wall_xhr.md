# Spec 24: Batted-Ball Spray-Angle, Pull-Air% & Ballpark Wall-Geometry xHR Physics (EXP-024)

## 1. Objective & Q-CRISP Hierarchy
- **Level:** Level 2 (Batted-Ball Physics, Ballpark Wall Geometry & Batter Power Skill).
- **Business & Sabermetric Problem:** Standard Statcast `xSLG` / `xHR` models rely solely on Exit Velocity (`launch_speed`) and Launch Angle (`launch_angle`), ignoring BOTH horizontal spray direction (pulled fly balls clear `330 ft` foul-pole corners whereas dead-center requires `400+ ft`) and exact stadium-specific fence geometry (`left_line_ft` through `right_line_ft` + `elevation_ft`).
- **Core Hypothesis:** Incorporating horizontal spray angle (`theta`), pull-signed angle (`theta_pull`), continuous piecewise-linear fence distance `wall_dist_ft(theta; venue, season)`, and ballpark `elevation_ft` strictly beats both EV+LA-only (`M0`) and scalar park-factor (`M2`) `xHR` models on locked `2025` out-of-sample Log-Loss, Brier Score, and ROC AUC across `124,395` batted balls.

## 2. Data & Strict Zero-Leakage Protocol
- **Population:** `900,589` regular-season Statcast balls in play (`type = 'X'`, complete `hc_x, hc_y, launch_speed, launch_angle`) across `2018–2025`, joined `100.0%` (`0` unmatched games) via `v_mlbmeta_schedule.venue_id` to `data/raw/mlbmeta/park_dimensions.parquet` (`league = 'mlb'`).
- **Chronological Split:**
  - **Train:** `2018–2023` (`~653k` BIP)
  - **Validation / Early Stopping:** `2024` (`123,779` BIP)
  - **Locked Out-of-Sample Test:** `2025` (`124,395` BIP, `5,616` HRs)
- **Zero-Leakage Invariant:** Scalar venue HR factors (`venue_hr_rate_tr`) and XGBoost hyperparameters/trees never touch `2025`.

## 3. 4-Model Ablation Ladder (XGBoost Binary Log-Loss, seed = 42)
1. `M0_EV_LA_Only`: `launch_speed`, `launch_angle`.
2. `M1_Plus_Spray_Angle`: `+ spray_angle, pull_angle, abs_spray_angle, stand_L`.
3. `M2_Plus_Scalar_Park_HR`: `+ venue_hr_rate_tr` (strictly `2018–2023` empirical venue HR/BIP).
4. `M3_Full_Wall_Geometry_Elevation`: `+ wall_dist_ft` (exact piecewise-linear fence distance at ball's spray angle $\theta$), `elevation_ft`, `is_dome`.

## 4. Deliverables & Verification
- Gold ADR-001 ZSTD Parquet tables in `data/processed/`:
  - `xhr_wall_geometry_oos_ablation_2025.parquet`
  - `batter_pull_air_xhr_stability_2024_2025.parquet`
  - `park_wall_xhr_effects_2025.parquet`
- Dark Cyber 4-Asset figures (`xhr01`, `xhr02` + interactive Plotly HTML, `xhr03`; `.rds < 5.0 MB`).
- 7-section Q-CRISP Study 24 + runnable Lab 24 (`24/24` parity), `ADR-020`, unit tests in `test-core.R`, independent `sabermetrics-method-auditor`, and full 7-part verification gate.
