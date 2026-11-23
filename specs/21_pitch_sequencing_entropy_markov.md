# Spec 21: Pitch Sequencing, Count-State Markov Transitions, Eye-Level Shifts & Pitcher Predictability Entropy (`EXP-021`)

## 1. Mathematical Objective & Theoretical Grounding

Studies `EXP-001` through `EXP-005` established single-pitch aerodynamics (`VAA/HAA`), 4-quadrant platoon break (`hb_away_in`), commit-point pitch tunneling (`y_0 = 50 \to 23.8` ft), and 5-head Decomposed Pitching+. However, hitting is a **sequential perceptual-motor task**: batters update their swing timing and spatial expectations dynamically based on the preceding pitch's velocity, vertical eye-level (`prev_plate_z`), and the pitcher's count-specific predictability (Gray 2002, *Journal of Experimental Psychology*; Pavlidis & Judge 2016; Tango et al. *The Book*).

In **EXP-021 (Level 1C)**, we isolate the pure marginal value of **multi-pitch sequencing and information-theoretic unpredictability** above raw pitch physics and location across the full Hawk-Eye / Statcast population (`5.8M+` consecutive pitch pairs):

1. **Count-Conditioned Pitch-Mix Shannon Entropy ($H_{\text{count}}$):**
   For pitcher $p$ in season $s$, across the 12 ball-strike counts $c \in \{0\text{-}0, \dots, 3\text{-}2\}$ with pitch-type shares $P(t \mid p, s, c)$:
   $$H(p, s, c) = -\sum_{t} P(t \mid p, s, c)\,\log_2 P(t \mid p, s, c), \qquad H_{\text{count}}(p, s) = \sum_{c} w_{p,s,c}\,H(p, s, c)$$
   where $w_{p,s,c}$ is the pitcher's empirical pitch share in count $c$. Higher $H_{\text{count}}$ (in bits) denotes lower predictability given the count.
2. **First-Order Markov Pitch-Group Transition & Eye-Level Shift Geometry:**
   Categorizing pitches into `Fastball (FF/SI/FC)`, `Breaking (SL/ST/CU/KC)`, and `Offspeed (CH/FS)`, we quantify exact transition-level Whiff%, CSW%, Chase%, xwOBA, and `RV/100`, alongside vertical eye-level change $\Delta z = z_N - z_{N-1}$ (ft), horizontal shift $\Delta x_{\text{norm}} = x_{\text{norm}, N} - x_{\text{norm}, N-1}$, and velocity differential $\Delta v = v_N - v_{N-1}$ (mph).
3. **Controlled Out-of-Sample Sequencing Lift (`Seq+`):**
   Training strictly chronologically on `2020–2023` Hawk-Eye regular-season paired swings, tuning on `2024`, and locking `2025` OOS (`~330,000+` paired swings), we compare two `xgboost` (`hist`) binary logistic whiff models:
   - **`M1_Physics_Location_Count`:** `release_speed`, `ivb_in`, `hb_away_in`, `vaa`, `plate_x_norm`, `plate_z`, `balls`, `strikes`, `is_same_hand`, `pitch_group`.
   - **`M2_Plus_Sequencing_Entropy`:** `M1` + `is_repeat_pitch`, `prev_pitch_group`, `delta_velo_mph`, `delta_plate_z`, `delta_plate_x_norm`, `tunnel_dist_in`, `post_tunnel_break_in`, and strictly prior-season `prior_count_entropy`.
   Pitcher **Sequence+ (`Seq+`)** isolates pure sequencing skill orthogonal to stuff/location:
   $$\text{Seq+}_{p, s} = 100 + 10 \cdot \frac{\overline{\Delta \hat{p}}_{p,s} - \mu_{2024}}{\sigma_{2024}}, \qquad \Delta \hat{p}_i = \hat{p}_{\text{M2}, i} - \hat{p}_{\text{M1}, i}$$

## 2. Input Features & Zero-Copy DuckDB Lake Query
Zero-copy DuckDB join between `v_statcast_tunneling` (`5.84M` pairs `2015–2025`) and `v_statcast_features` (with window `LAG(plate_z)` / `LAG(plate_x_norm)` over `(game_pk, at_bat_number)` ordered by `pitch_number`) + `v_chadwick_register` for player names.

## 3. Target Variable & Loss Functions
- **Primary Target:** `is_whiff` on paired regular-season swings (`is_swing = 1`).
- **Evaluation Metrics (Locked `2025` OOS, full population, zero subsampling):** Binary Log-Loss, Brier Score, ROC AUC, and paired bootstrap / DeLong significance of `M2` vs `M1`.
- **Reliability Metrics:** Year-over-year (`2024 -> 2025`) Pearson $r$ for `H_count` (Shannon Entropy) and `Seq+` (`>= 200` paired swings).

## 4. Validation Strategy (Zero Temporal Leakage)
- `2020–2023` Hawk-Eye regular-season paired swings (`train`) $\to$ `2024` (`validation` & `Seq+` standardization lock) $\to$ `2025` (`locked OOS test`). Pitcher `prior_count_entropy` strictly uses `season - 1`.

## 5. Acceptance Criteria
1. `M2_Plus_Sequencing_Entropy` strictly beats `M1_Physics_Location_Count` on locked `2025` OOS Whiff Log-Loss, Brier Score, and ROC AUC across all `~300k+` paired swings.
2. Pitch-mix Shannon Entropy $H_{\text{count}}$ exhibits strong YoY reliability (`r > 0.75`) and non-repeat vertical eye-level changes (`Fastball High -> Breaking Low`) show measurable whiff lift over repeat pitches.

## 6. Implementation Blueprint
- **Script:** `models/21_pitch_sequencing_entropy_markov.R` (`--smoke` / `--full`)
- **Gold Tables:** `data/processed/pitch_markov_transitions_2015_2025.parquet`, `data/processed/pitcher_sequencing_entropy_2020_2025.parquet`, `data/processed/sequencing_model_ablation_2025.parquet`
- **Visualizations (Dark Cyber 4-Asset):** `seq01_markov_pitch_transition_whiff_matrix`, `seq02_eye_level_shift_velo_whiff_surface`, `seq03_pitcher_entropy_vs_seq_plus_leaderboard`
- **Study & Lab:** `site/analyses/21_pitch_sequencing_entropy_markov.qmd`, `workbooks/lab21_pitch_sequencing_entropy_markov_workbook.R`, `site/workbooks/lab21_pitch_sequencing_entropy_markov_workbook.qmd`
- **Tests & ADR:** `pkg/mlbr/tests/testthat/test-core.R`, `DECISIONS.md` (`ADR-017`), `experiments/registry.json` (`EXP-021-PITCH-SEQUENCING-ENTROPY-MARKOV`).
