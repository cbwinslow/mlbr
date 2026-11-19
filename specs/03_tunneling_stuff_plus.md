# Specification 03: Commit-Point Pitch Tunneling & Platoon-Conditioned Stuff+ / Pitching+

- **Experiment ID:** `EXP-003-TUNNELING-STUFF-PLUS`
- **Architecture Tier:** Level 1 (Atomic Pitch Physics & Quality) → Level 2 (True-Talent Pitcher Projections)
- **Authoritative Script:** `models/03_tunneling_stuff_plus.R`
- **Published Study:** `site/analyses/03_tunneling_stuff_plus.qmd`
- **Canonical SQL:** `sql/02_pitch_tunneling.sql` (exact $y_0 = 50\text{ ft}$ reconstruction)

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Literature Foundation
- **Pitch Tunneling (Harry Pavlidis & Jonathan Judge, *Baseball Prospectus*, 2017):** Batters require ~150 ms of visual processing + ~150 ms of swing execution, fixing the swing-decision commit point at approximately $y_{\text{commit}} = 23.8\text{ ft}$ from the front edge of home plate (equivalently ~167 ms before arrival on a 95 mph fastball). Deception is maximized when two consecutive pitches occupy nearly identical 3D space at $y_{\text{commit}}$ (small **Tunnel Distance**, $d_{\text{tunnel}}$) and diverge sharply between $y_{\text{commit}}$ and home plate (large **Plate Separation**, $d_{\text{plate}}$, and high **Break-to-Tunnel Ratio**).
- **Stuff+ / Location+ / Pitching+ Decomposition (Eno Sarris, Max Bay, Cameron Grove, *The Athletic* / *FanGraphs*):** Pitch quality separates cleanly into:
  1. **Stuff+:** Intrinsic physical characteristics independent of where the pitch finishes (`release_speed`, `ivb_in`, `hb_glove_in`, `release_spin_rate`, `spin_axis`, `release_extension`, `release_pos_x_arm`, `release_pos_z`, `arm_angle_deg`, fastball velocity/movement differentials, and pitcher/batter handedness context `matchup`).
  2. **Location+:** Execution and situational placement (`plate_x_norm`, `plate_z`, normalized strike-zone coordinates, `vaa`, `haa_away`, `balls`, `strikes`, `attack_zone`).
  3. **Pitching+ (Combined + Tunneling):** Joint physical + location + pairwise sequence deception (`tunnel_dist_in`, `plate_sep_in`, `post_tunnel_break_in`, `break_to_tunnel_ratio`, `delta_velo_mph`, `delta_flight_ms`, `is_repeat_pitch`).
- **Expected Run Value Target (Tom Tango, *The Book*; RE24):** Evaluating every pitch by its expected change in run expectancy (`pitcher_run_exp = -delta_run_exp`) unifies called strikes, whiffs, balls, and batted-ball quality (`xwoba`) on a single additive run scale directly consumable by Level 2–5 models.

### 1.2 Exact Commit-Point Kinematics (Bug Fix over Naive Release Shortcut)
In Statcast/Hawk-Eye 9-parameter kinematics (Alan Nathan), $(v_{x0}, v_{y0}, v_{z0})$ are defined at **$y_0 = 50.0\text{ ft}$**, whereas `(release_pos_x, release_pos_z)` are measured at `release_pos_y` ($\approx 54.5\text{ ft}$). Integrating forward from `release_pos_x` using $t_{\text{dec}}$ (which begins at $y_0 = 50.0\text{ ft}$) omits ~4.5 ft of flight and introduces a **2.87-inch mean (4.69-inch p95)** coordinate error at $y = 23.8\text{ ft}$ (verified empirically over 50,000 2024 Hawk-Eye pitches).

Exact reconstruction uses home-plate coordinates `(plate_x, plate_z)` at $y_p = 1.417\text{ ft}$:
$$t_p = \frac{-\sqrt{v_{y0}^2 + 2 a_y (1.417 - 50.0)} - v_{y0}}{a_y}, \qquad t_{\text{dec}} = \frac{-\sqrt{v_{y0}^2 + 2 a_y (23.8 - 50.0)} - v_{y0}}{a_y}$$
$$x_{50} = \text{plate\_x} - v_{x0} t_p - \frac{1}{2} a_x t_p^2, \qquad z_{50} = \text{plate\_z} - v_{z0} t_p - \frac{1}{2} a_z t_p^2$$
$$x_{\text{dec}} = x_{50} + v_{x0} t_{\text{dec}} + \frac{1}{2} a_x t_{\text{dec}}^2, \qquad z_{\text{dec}} = z_{50} + v_{z0} t_{\text{dec}} + \frac{1}{2} a_z t_{\text{dec}}^2$$

For pitch $N$ following pitch $N-1$ by the **same pitcher** (`prev_pitcher = pitcher`) within `(game_pk, at_bat_number)`:
$$d_{\text{tunnel}} = 12 \sqrt{(x_{\text{dec}, N} - x_{\text{dec}, N-1})^2 + (z_{\text{dec}, N} - z_{\text{dec}, N-1})^2}\text{ (inches)}$$
$$d_{\text{plate}} = 12 \sqrt{(\text{plate\_x}_N - \text{plate\_x}_{N-1})^2 + (\text{plate\_z}_N - \text{plate\_z}_{N-1})^2}\text{ (inches)}$$
$$\text{Break-to-Tunnel Ratio} = \frac{d_{\text{plate}}}{\max(d_{\text{tunnel}}, 1.0)}$$

### 1.3 Standardized Plus-Scale Calibration
For each model $\hat{r}_i$ predicting `pitcher_run_exp` (positive = pitcher-favorable runs per pitch), standardize against the **training-era** pitch-level mean $\mu_{\text{train}}$ and pitcher-season (min 250 pitches) standard deviation $\sigma_{\text{pit, train}}$:
$$\text{Stuff+}_i = 100 + 10 \cdot \frac{\hat{r}_i - \mu_{\text{train}}}{\sigma_{\text{pit, train}}}$$
So `100` = MLB average pitcher, `110` = +1 SD above average, and `10` points = 1 pitcher-level SD (matching industry Stuff+ conventions).

---

## 2. Input Features & Data Lake Query

All extraction reads zero-copy from Silver `v_statcast_features` (ZSTD sorted Parquet, `2015–2026`, 9.17M total pitches; 8.67M valid kinematics + run value) with per-pitcher-season fastball anchor baselines computed **strictly within-season / expanding prior-to-date** (zero future leakage):

```sql
WITH fb_anchor AS (
  -- Pitcher primary fastball baseline (FF/SI/FC) computed strictly from prior games or same-game prior pitches
  SELECT
    game_pk, at_bat_number, pitch_number,
    AVG(CASE WHEN pitch_type IN ('FF', 'SI', 'FC') THEN release_speed END)
      OVER (PARTITION BY pitcher, game_year ORDER BY game_date, game_pk, at_bat_number, pitch_number
            ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pitcher_fb_velo_to_date,
    AVG(CASE WHEN pitch_type IN ('FF', 'SI', 'FC') THEN ivb_in END)
      OVER (PARTITION BY pitcher, game_year ORDER BY game_date, game_pk, at_bat_number, pitch_number
            ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pitcher_fb_ivb_to_date,
    AVG(CASE WHEN pitch_type IN ('FF', 'SI', 'FC') THEN hb_glove_in END)
      OVER (PARTITION BY pitcher, game_year ORDER BY game_date, game_pk, at_bat_number, pitch_number
            ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pitcher_fb_hb_to_date
  FROM v_statcast_features
  WHERE game_year >= 2015
)
-- Joined by immutable composite key (game_pk, at_bat_number, pitch_number)
-- to v_statcast_features and sql/02_pitch_tunneling.sql
```

### Feature Blocks
| Block | Features | Used In |
| :--- | :--- | :--- |
| **A. Physical Stuff** | `release_speed`, `effective_speed`, `release_spin_rate`, `spin_axis`, `ivb_in`, `hb_glove_in`, `hb_away_in`, `release_pos_x_arm`, `release_pos_z`, `release_extension`, `arm_angle_deg` (NA-native in TrackMan), `delta_fb_velo`, `delta_fb_ivb`, `delta_fb_hb`, `pitch_type` (one-hot/integer), `is_same_hand`, `p_throws`, `stand` | Stuff+, Pitching+ |
| **B. Command & Context** | `plate_x_norm`, `plate_z`, `sz_norm_z = (plate_z - sz_bot) / NULLIF(sz_top - sz_bot, 0)`, `vaa`, `haa_away`, `balls`, `strikes`, `outs_when_up`, `n_thruorder_pitcher` | Location+, Pitching+ |
| **C. Sequence Tunneling** | `tunnel_dist_in`, `plate_sep_in`, `post_tunnel_break_in`, `break_to_tunnel_ratio`, `delta_velo_mph`, `delta_flight_ms`, `is_repeat_pitch` (NA on pitch 1 of PA or pitcher change) | Tunnel+ / Pitching+ |

---

## 3. Target Variable & Loss Function

### Primary Target (Expected Run Value)
- **Target:** `y = pitcher_run_exp` (continuous; positive = pitcher-favorable run value per pitch).
- **Primary Loss:** Squared Error (`reg:squarederror`, **RMSE** and out-of-sample $R^2$ in runs/100 pitches).
- **Secondary Diagnostic Target (Deception / Whiff Lift):** Binary `is_whiff` conditioned on `is_swing = 1`, evaluated with **Log-Loss**, **Brier Score**, and **ROC AUC** specifically to isolate the incremental value of Commit-Point Tunneling (Block C) on swing-and-miss deception.
- **Downstream True-Talent Stability Metric:** Pitcher-level year-over-year (2024 → 2025, min 300 pitches) Pearson $r$ and predictive correlation with out-of-sample pitcher `K%`, `CSW%`, and `FIP` (validating that Stuff+ stabilizes faster than raw ERA/wOBA).

---

## 4. Validation Strategy (Strict Chronological Splits)

Zero future leakage across all training, early stopping, calibration, and fastball-anchor windows:
- **Training Set:** `2015–2023` (`6,299,030` valid pitches across TrackMan + Hawk-Eye).
  - *Ablation Branch (Hawk-Eye Only):* Train `2020–2023` (`2,585,278` valid pitches, 90.7% `arm_angle_deg` coverage) to test whether TrackMan pre-2020 tracking shifts help or hurt 2024–2026 generalization.
- **Validation / Early-Stopping & Calibration Set:** `2024` (`765,880` valid pitches). Used exclusively for `xgboost` early stopping, hyperparameter tuning, and Plus-scale $(\mu, \sigma)$ lock-in.
- **Locked Out-of-Time Test Set:** `2025–2026` (`1,610,620` valid pitches). Untouched until final evaluation.

---

## 5. Baseline Comparison

Every model tier must strictly beat simpler baselines on the locked `2025–2026` test set:
1. **Baseline 0 (Naive Pitch-Type Mean):** Empirical mean `pitcher_run_exp` by `(pitch_type, matchup)` from `2015–2023`.
2. **Baseline 1 (Linear / Ridge `glmnet`):** Regularized linear model on Block A (Stuff) and Blocks A+B (Pitching) — quantifies the non-linear interaction gain from gradient boosted trees.
3. **Model 1 (`Stuff+` — `xgboost` `hist`):** Block A only (`max_depth = 6`, `eta = 0.03`, `subsample = 0.8`, `colsample_bytree = 0.8`, `min_child_weight = 200`).
4. **Model 2 (`Pitching+` Base — `xgboost` `hist`):** Blocks A + B.
5. **Model 3 (`Pitching+` + Tunneling — `xgboost` `hist`):** Blocks A + B + C (quantifies exact incremental $\Delta\text{RMSE}$, $\Delta\text{Log-Loss}$ on whiffs, and pitcher-level YoY predictive lift from commit-point tunneling).

---

## 6. Implementation Blueprint

### 6.1 Code & Silver/Gold Artifacts
1. **Canonical SQL (`sql/02_pitch_tunneling.sql`):** Fixed exact $y_0 = 50\text{ ft}$ reconstruction + mid-AB pitcher guard + Break-to-Tunnel Ratio (completed).
2. **Silver Materialization (`R/materialize_tunneling.R` or extension of `R/materialize_features.R`):** Materialize pitch-level tunneling metrics to ADR-001 ZSTD sorted Hive Parquet (`data/features/statcast_tunneling/`) keyed by `(game_pk, at_bat_number, pitch_number)` and mount as `v_statcast_tunneling` in `pkg/mlbr/R/features.R`.
3. **Authoritative Modeling Script (`models/03_tunneling_stuff_plus.R`):**
   - Extracts full-population `2015–2026` features via `mlbr::lake_duckdb()`.
   - Trains Baselines 0–1 + `xgboost` (`hist`) Models 1–3 + Hawk-Eye `2020–2023` ablation.
   - Scores full `2020–2026` population and writes Gold pitcher-season Stuff+/Location+/Pitching+/Tunnel+ leaderboard to `data/processed/pitcher_stuff_plus.parquet` (ZSTD).
   - Logs all validated formulas, metrics, and ablation results via `mlbr::lake_log_experiment("EXP-003-TUNNELING-STUFF-PLUS", ...)`.
4. **Dark Cyber 4-Asset Visual Suite (`models/stuff01_*` .. `models/stuff05_*`):**
   - `stuff01_tunnel_geometry_whiff_surface`: 2D surface of `tunnel_dist_in` vs `post_tunnel_break_in` colored by whiff rate.
   - `stuff02_sequence_deception_matrix`: Pitch-pair (`FF -> SL`, `FF -> CH`, `SI -> ST`, etc.) Tunnel Distance vs. $\Delta$Whiff% by platoon (`Same-Hand` vs `Opp-Hand`).
   - `stuff03_ablation_oos_lift`: Out-of-time (`2025–2026`) RMSE & Whiff Log-Loss lift across Baseline 0, Ridge, Stuff+, Pitching+, Pitching+Tunneling, and Hawk-Eye ablation.
   - `stuff04_pitcher_stuff_vs_location_interactive`: Interactive `plotly` scatter of 2025–2026 MLB pitchers (`Stuff+` vs `Location+`, sized by pitches, colored by `Pitching+`, hover names from `v_chadwick_register`).
   - `stuff05_yoy_stability_curve`: Split-half / YoY stabilization curve (`Stuff+` vs `K%`, `CSW%`, `FIP`, `ERA`).
5. **Quarto Publication (`site/analyses/03_tunneling_stuff_plus.qmd`):** Full 7-Section Q-CRISP study + `quarto render site/` + closeout verification (`mlbr-verifier`, `mlbr-pipeline-auditor`, `mlbr-study-reviewer`).
