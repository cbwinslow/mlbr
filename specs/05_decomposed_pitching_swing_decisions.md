# Specification 05: Context-Neutral Decomposed Pitching+ & Pitch-Level Swing Decision Value (`EXP-005-DECOMPOSED-PITCHING-DECISIONS`)

## 1. Objective & Scientific Hypotheses
Study 03 (`EXP-003-TUNNELING-STUFF-PLUS`) trained single-head `xgboost` models directly on raw single-pitch `pitcher_run_exp` (`-delta_run_exp`), reaching `r = -0.473` forecasting `2025` `SIERA` (narrowly behind FanGraphs `Pitching+` at `r = -0.488`). Because raw `delta_run_exp` mixes pitch quality with base-out leverage (e.g., a bases-loaded walk costs `~0.85` runs vs. `~0.29` bases empty) and batted-ball BABIP noise, a **Markov-decomposed probability engine** that estimates pitch-outcome probabilities separately and prices them via context-neutral 12-count linear weights should eliminate base-out variance AND simultaneously yield a first-principles **Pitch-Level Batter Process Engine** (`Decision+`, `BatControl+`, `Contact+`, `BatterProcess+`) replacing Study 04's box-zone `Zone-Chase (Z-O%)` proxy.

**Testable Hypotheses:**
1. **H1 (Base-Out Variance Elimination):** Replacing raw `delta_run_exp` regression with a 5-head Markov probability decomposition (`p_swing`, `p_cstr|take`, `p_whiff|swing`, `p_foul|contact`, `E[xwOBA|BIP]`) priced via `2015–2023` context-neutral 12-count linear weights improves out-of-sample `2024 -> 2025` `SIERA` and `FIP` forecasting over Study 03 (`r = -0.473` SIERA) and rivals or beats commercial FanGraphs `Pitching+` (`r = -0.488`).
2. **H2 (Tunneling Isolation in `p_whiff|swing` & `p_swing`):** Feeding `v_statcast_tunneling` features (`tunnel_dist_in`, `plate_sep_in`, `post_tunnel_break_in`, `break_to_tunnel_ratio`, `delta_velo_mph`, `delta_flight_ms`) directly into the `p_swing` and `p_whiff|swing` heads yields measurable logloss reduction and positive `Pitching+Tunnel` predictive lift.
3. **H3 (Hawk-Eye Era Homogeneity Ablation):** Training pure-physics `Decomp Stuff+` strictly on Hawk-Eye seasons (`2020–2023`) vs. full TrackMan+Hawk-Eye (`2015–2023`) tests whether sensor homogeneity improves `2024 -> 2025` OOS `Stuff+` validity.
4. **H4 (Pitch-Level Batter Decision Value Over Expected):** Defining zero-mean pitch-level Decision Value Over Expected, $\text{decision\_voe}_i = (I_{\text{swing},i} - \hat{p}_{\text{sw},i})(\text{EV}_{\text{swing},i} - \text{EV}_{\text{take},i})$, plus `BatControl+` (whiff avoidance over expected) and `Contact+` (xwOBA over expected) achieves higher year-over-year stability and stronger out-of-sample `2024 -> 2025` / `2025 -> 2026` `wOBA`, `BB%`, and `K%` forecasting than aggregate `Zone-Chase (Z-O%)` alone.

---

## 2. Data Sources, Population Scope & Zero-Leakage Temporal Splits
- **Primary Silver Views:** `v_statcast_features` (`14.64M` pitches, ADR-001 ZSTD Sorted) LEFT JOINED on `(game_pk, at_bat_number, pitch_number)` with `v_statcast_tunneling` (`10.20M` sequential pairs, ADR-001 ZSTD Sorted).
- **Population Filter:** Strictly Regular Season (`game_type = 'R'`), `balls BETWEEN 0 AND 3`, `strikes BETWEEN 0 AND 2`, `stand IN ('R', 'L')`, `p_throws IN ('R', 'L')`, valid kinematics (`release_speed IS NOT NULL`, `plate_x IS NOT NULL`, `plate_z IS NOT NULL`, `sz_top > sz_bot`).
- **Evaluation Targets:** `v_fangraphs_pitch` (`TBF >= 100` / `IP >= 25`) and `v_fangraphs_bat` (`PA >= 100`), plus `2026` untouched Regular-Season Statcast outcomes and Study 03 Gold `pitcher_stuff_plus.parquet` for apples-to-apples head-to-head comparison.
- **Strict Chronological Splits (Identical to Study 03 for Exact Parity):**
  - `train_insample`: `2015–2023` (primary) and `2020–2023` (Hawk-Eye ablation) — fit 12-count Markov RV table, BIP `xwOBA -> RV` linear map, and all 5 `xgboost` heads.
  - `val_calibration`: `2024` — early stopping (`early_stopping_rounds = 40`), 100-scale standardization (`mean = 100`, `sd = 10`), and zero-leakage calibration.
  - `test_oos`: `2025` (locked FanGraphs OOS) and `2026` (locked untouched Statcast Regular-Season OOS).

---

## 3. Mathematical Formulation

### 3.1 Context-Neutral 12-Count Linear Weights (`2015–2023` Train Only)
For each count state $c = (b, s) \in \{0..3\} \times \{0..2\}$, compute empirical mean `delta_run_exp` (batter perspective) strictly on `2015–2023` Regular-Season pitches:
- $\Delta\text{RV}_{\text{ball}}(b, s) = \overline{\Delta\text{RE}}(\text{take, ball} \mid b, s)$
- $\Delta\text{RV}_{\text{strike}}(b, s) = \overline{\Delta\text{RE}}(\text{called strike or whiff} \mid b, s)$
- $\Delta\text{RV}_{\text{foul}}(b, s) = \overline{\Delta\text{RE}}(\text{foul} \mid b, s)$ (strictly `0.0` when $s = 2$)
- Context-neutral BIP run value map fit via OLS on `2015–2023` Regular-Season BIPs with finite `xwoba`:
  $$\Delta\text{RV}_{\text{BIP}}(b, s, \widehat{\text{xwOBA}}) = \alpha_{b,s} + \beta \cdot \widehat{\text{xwOBA}}$$

### 3.2 Five-Head Decomposed Expected Value
For every pitch $i$ in count $(b_i, s_i)$, the 5 `xgboost` heads output:
- $\hat{p}_{\text{sw},i} = P(\text{swing} \mid X_i)$
- $\hat{p}_{\text{cs},i} = P(\text{called strike} \mid \text{take}, X_i)$
- $\hat{p}_{\text{wh},i} = P(\text{whiff} \mid \text{swing}, X_i)$
- $\hat{p}_{\text{fl},i} = P(\text{foul} \mid \text{swing, contact}, X_i)$
- $\widehat{\text{xwOBA}}_i = \mathbb{E}[\text{xwOBA} \mid \text{BIP}, X_i]$

Conditional expected batter run values:
$$\text{EV}_{\text{take},i} = \hat{p}_{\text{cs},i}\,\Delta\text{RV}_{\text{strike}}(b_i,s_i) + (1 - \hat{p}_{\text{cs},i})\,\Delta\text{RV}_{\text{ball}}(b_i,s_i)$$
$$\text{EV}_{\text{contact},i} = \hat{p}_{\text{fl},i}\,\Delta\text{RV}_{\text{foul}}(b_i,s_i) + (1 - \hat{p}_{\text{fl},i})\,\Delta\text{RV}_{\text{BIP}}(b_i,s_i,\widehat{\text{xwOBA}}_i)$$
$$\text{EV}_{\text{swing},i} = \hat{p}_{\text{wh},i}\,\Delta\text{RV}_{\text{strike}}(b_i,s_i) + (1 - \hat{p}_{\text{wh},i})\,\text{EV}_{\text{contact},i}$$
$$\widehat{\text{xRV}}_{\text{bat},i} = (1 - \hat{p}_{\text{sw},i})\,\text{EV}_{\text{take},i} + \hat{p}_{\text{sw},i}\,\text{EV}_{\text{swing},i}$$

### 3.3 Pitcher Grades (`Decomp Stuff+`, `Location+`, `Pitching+`, `Pitching+Tunnel`)
- **`Decomp Stuff+`:** Fits the 5 heads using ONLY pure physical/aerodynamic features + pitcher FF reference deltas + platoon (`p_throws`, `stand`, `is_same_hand`) — zero location or count. Scored in context-neutral `0-0` (or marginal count-averaged) linear weights so pure stuff has zero count-state leakage!
- **`Decomp Pitching+`:** Full physics + location (`plate_x_norm`, `plate_z`, `plate_z_rel = (plate_z - sz_bot) / (sz_top - sz_bot)`, `attack_zone`) + count (`balls`, `strikes`).
- **`Decomp Pitching+Tunnel`:** Adds `v_statcast_tunneling` features (`has_tunnel`, `tunnel_dist_in`, `plate_sep_in`, `post_tunnel_break_in`, `break_to_tunnel_ratio`, `delta_velo_mph`, `delta_flight_ms`, `is_repeat_pitch`) into `p_swing` and `p_whiff|swing`.
- **`Decomp Location+` (Pure Command):** Standalone 5-head Markov model on `feats_loc` (`plate_x_norm`, `plate_z`, `sz_norm_z`, count, outs, TTO, platoon, pitch type — zero velocity/spin/break), martingale count-centered and standardized on `2024` validation (`mean = 100`, `sd = 10`).

### 3.4 Pitch-Level Batter Process Decomposition (Zero-Mean Over Expected)
Using the locked full-context `Decomp Pitching+Tunnel` probability surface:
1. **Decision Value Over Expected (`Decision+`):**
   $$\text{decision\_voe}_i = (I_{\text{swing},i} - \hat{p}_{\text{sw},i}) \cdot (\text{EV}_{\text{swing},i} - \text{EV}_{\text{take},i})$$
2. **Bat Control / Whiff Avoidance Over Expected (`BatControl+`):**
   $$\text{bat\_ctrl\_rv}_i = I_{\text{swing},i} \cdot (\hat{p}_{\text{wh},i} - I_{\text{whiff},i}) \cdot (\text{EV}_{\text{contact},i} - \Delta\text{RV}_{\text{strike}}(b_i, s_i))$$
3. **Contact Quality Over Expected (`Contact+`):**
   $$\text{contact\_rv}_i = I_{\text{bip},i} \cdot \beta \cdot (\text{xwOBA}_i - \widehat{\text{xwOBA}}_i)$$
4. **Total Batter Process Value (`BatterProcess+`):**
   $$\text{process\_rv}_i = \text{decision\_voe}_i + \text{bat\_ctrl\_rv}_i + \text{contact\_rv}_i$$
   Summed per batter-season (`rv_per_100 = 100 * sum(rv) / n_pitches`, sum-before-divide!) and standardized to `100 ± 10` on `2024` qualified batters (`>= 1,000` pitches seen).

---

## 4. Feature Contracts & Engineering Constraints
- Strictly use `mlbr::default_lake()` and `mlbr::lake_duckdb()` with `SET memory_limit = '12GB'`.
- Free large matrices (`rm(...)` + `gc()`) between `xgboost` heads on `cbwdellr720` (~29 GB available RAM).
- Use `xgboost` `3.2.1.1` API (`xgb.params()`, `evals = list(train = ..., val = ...)`, `xgb.attr(bst, "best_iteration")`).
- Enforce fail-fast assertions on row counts, join parity, logloss/RMSE finiteness, `eval_split` tagging, zero NAs in Gold outputs, and `.rds < 5 MB` (using `force()` + top-level plotting functions).

---

## 5. Evaluation & Acceptance Criteria
1. **Pitcher Head-to-Head (`2024 -> 2025` Locked OOS, $N \ge 250$ qualified pitchers `TBF >= 100`):** Report Pearson $r$, exposure-weighted $r$, and `wRMSE` forecasting `2025` `SIERA`, `FIP`, `K%`, `BB%` for `Study 03 Pitching+`, `Decomp Pitching+`, `Decomp Pitching+Tunnel`, `Hawk-Eye Decomp Stuff+`, and commercial FanGraphs `Pitching+`.
2. **Batter Process Validity (`2024 -> 2025` & `2025 -> 2026` Locked OOS, `PA >= 150`):** Report YoY stability ($r_{\text{YoY}}$) and incremental `wOBA`/`BB%`/`K%`/`xwOBA` predictive $R^2$ / `wRMSE` of `Decision+`, `BatControl+`, `Contact+`, and `BatterProcess+` over Study 04's `Zone-Chase (Z-O%)` proxy and raw `xwOBA`.
3. **Honest Reporting:** Report both wins AND any negative ablations (e.g., if Hawk-Eye `2020–2023` trails `2015–2023` due to smaller training volume, state it clearly).

---

## 6. Deliverables & Closeout Contract (Including Mandatory 1:1 Work-Along Lab)
- **Authoritative Pipeline:** `models/05_decomposed_pitching_swing_decisions.R`
- **Gold ZSTD Parquets (`data/processed/`):**
  - `pitcher_decomp_pitching_plus.parquet`
  - `batter_process_metrics.parquet`
  - `decomp_count_rv_table.parquet`
  - `decomp_benchmark_ablation.parquet`
- **Dark Cyber 4-Asset Visual Suite (`models/decomp01_*` .. `models/decomp05_*`):**
  1. `decomp01_markov_count_rv_matrix` — 12-Count Context-Neutral Transition Value & Swing/Take Surface
  2. `decomp02_pitcher_ablation_vs_study03_fg` — `2024 -> 2025` OOS Forecasting Ladder (`Study 03` vs `Decomp` vs `FanGraphs`)
  3. `decomp03_pitcher_decomp_explorer` *(Static `.png` + Interactive Plotly `.html`)* — `Decomp Stuff+` vs `Decomp Location+` Frontier (`2025`/`2026 test_oos`)
  4. `decomp04_batter_process_validity` — YoY Stability & Incremental OOS Lift (`Decision+`, `BatControl+`, `Contact+` vs `Z-O%` & `xwOBA`)
  5. `decomp05_batter_decision_vs_contact_explorer` *(Static `.png` + Interactive Plotly `.html`)* — `Decision+` vs `Contact+` Batter Archetype Frontier (`2025`/`2026 test_oos`)
- **Q-CRISP 7-Section Study:** `site/analyses/05_decomposed_pitching_decisions.qmd`
- **Mandatory Companion Work-Along Lab 05:** `workbooks/lab05_decomposed_pitching_decisions_workbook.R` + `site/workbooks/lab05_decomposed_pitching_decisions_workbook.qmd`
- **Registry & Verification:** `EXP-005-DECOMPOSED-PITCHING-DECISIONS` logged via `lake_log_experiment()`, `generate_agent_manifest.R` **6/6** (confirming 5/5 numbered studies ↔ 5/5 work-along labs), `proof_lake.R` 28/28, `quarto render site/` 23/23, and independent read-only audits (`sabermetrics-method-auditor` + `silent-failure-hunter`).
