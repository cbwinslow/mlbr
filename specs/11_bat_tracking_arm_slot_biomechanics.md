# Specification 11: Hawk-Eye Bat-Tracking (`1.15M` Swings) & Pitcher Arm-Slot (`arm_angle`) Biomechanics

- **Experiment ID:** `EXP-011-BAT-TRACKING-ARM-SLOT-BIOMECHANICS`
- **Architecture Tier:** Level 1B / Level 2 — Biomechanical Process Sensors & True-Talent Expansion
- **Status:** `SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - `v_statcast` (`2020–2026`, Hawk-Eye `bat_speed`, `swing_length`, `attack_angle`, `swing_path_tilt`, `intercept_*`, `arm_angle`)
  - `v_fangraphs_bat`, `v_chadwick_register` (for OOS `2025` ISO/wOBA projection lift validation)
- **Downstream Consumers:** `EXP-004`/`EXP-007` true-talent batter projections, `EXP-003`/`EXP-005` pitch quality (`Stuff+`), `EXP-014` Level 4 v2 lineup-aware Markov simulator.

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Collision Physics, Squared-Up Rate & Blast Rate (MLB Hawk-Eye)
For every competitive regular-season swing (`TRY_CAST(bat_speed AS DOUBLE) >= 60.0` mph), theoretical maximum exit velocity from bat-ball collision mechanics is:
$$\text{MaxEV} = 1.23 \cdot v_{\text{bat}} + 0.2116 \cdot v_{\text{release}}$$
A batted ball is **Squared-Up** when $\text{SqUp} = \text{EV} / \text{MaxEV} \ge 0.80$, and is a **Blast** when:
$$\text{SqUp} \ge 0.80 \quad \text{AND} \quad v_{\text{bat}} + 100 \cdot \text{SqUp} \ge 164$$
**Ideal Attack Angle** is defined as $\theta_{\text{attack}} \in [5^\circ, 20^\circ]$, aligning the barrel plane with incoming pitch vertical approach angles.

### 1.2 Pitcher Arm-Slot Expected Break (`ivb_above_slot`)
Fitting a strictly chronological pre-test (`2020–2023`) quadratic expectation of four-seam/sinker induced vertical break ($\text{IVB} = 12 \cdot \text{pfx\_z}$ inches) on pitcher `arm_angle` ($\phi$):
$$\widehat{\text{IVB}}(\phi) = \beta_0 + \beta_1 \phi + \beta_2 \phi^2, \qquad \text{IVB}_{\text{above\_slot}} = \text{IVB} - \widehat{\text{IVB}}(\phi)$$
Low-slot pitchers ($\phi < 25^\circ$) with positive $\text{IVB}_{\text{above\_slot}}$ generate flatter vertical approach angles and higher fastball whiff rates than raw velocity and raw IVB predict.

---

## 2. Validation & Benchmark Strategy
- **YoY Reliability (`2024 -> 2025`, `>= 150` competitive swings):** Compare year-over-year stability $r$ of `bat_speed`, `swing_length`, `attack_angle`, `blast_rate`, and `arm_angle` against traditional `wOBA`, `ISO`, and `BABIP`.
- **Chronological OOS Projection Lift (`2023->2024` train, `2024->2025` locked OOS test):** Test whether adding `bat_speed` + `blast_rate` + `swing_length` to prior-year `ISO`/`wOBA` reduces weighted OOS RMSE on `2025` FanGraphs `ISO` and `wOBA`.
- **OOS Fastball Whiff% Ablation (`2020–2023` train, `2024–2025` locked OOS test):** Compare Log-Loss and ROC AUC of `Whiff ~ velo + ivb` vs. `Whiff ~ velo + ivb + arm_angle + ivb_above_slot`.

---

## 3. Implementation Deliverables
- Model: `models/11_bat_tracking_arm_slot_biomechanics.R` (`--smoke` / `--full`)
- Gold Parquets: `data/processed/batter_bat_tracking_biomechanics_2023_2026.parquet`, `data/processed/pitcher_arm_angle_biomechanics_2020_2026.parquet`, `data/processed/bat_tracking_oos_ablation_metrics.parquet`
- Study & Lab: `site/analyses/11_bat_tracking_arm_slot_biomechanics.qmd`, `workbooks/lab11_bat_tracking_arm_slot_biomechanics_workbook.R`, `site/workbooks/lab11_bat_tracking_arm_slot_biomechanics_workbook.qmd`
- Visuals: `models/bio01_*`, `models/bio02_*`, `models/bio03_*` (Dark Cyber 4-asset suites, `< 5.0 MB` `.rds`).
