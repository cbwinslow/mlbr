# Spec 23: Live In-Game Bayesian Win Probability, Pre-Game Prior Decay & Leverage Index (EXP-023)

## 1. Objective & Q-CRISP Hierarchy
- **Level:** Level 4 (Live In-Game State-Space Probability & Dynamic Market Valuation).
- **Business & Sabermetric Problem:** Static textbook RE24 Win Expectancy (`EXP-009`) treats every team as an identical `.500` club in an average `8.8`-run scoring environment. In live in-game pricing (and real-time bullpen/pinch-hit strategy), a `65%` pre-game favorite tied `0-0` in the 2nd inning is NOT a `53%` coin flip, and a `2-run` lead in an `11.5`-total Coors Field game is significantly less safe than in a `7.0`-total pitcher's duel.
- **Core Hypothesis:** Combining a strictly leak-free `2015–2023` empirical RE24 base Win Expectancy matrix with an inning-decayed Bayesian log-odds update from `EXP-014` (`p_m6_ml`, `exp_total_m6`, `bp_fatigue_adv_home`) strictly improves locked `2025` out-of-sample play-by-play Brier Score and Log-Loss across Innings 1–9+ relative to both static RE24 WE and non-decayed flat priors.

## 2. Data & Strict Zero-Leakage Protocol
- **Base Empirical WE Matrix (`2015–2023` Train Only):** Built strictly from `v_retrosheet_plays` (`2015–2023`, `~1.6M` plays) joined to final game outcomes (`v_retrosheet_games`), excluding All-Star games (`ALS`/`NLS`) and tie games. Sparse extreme states are Empirical-Bayes shrunk toward a smooth `2015–2023` logistic score-diff/inning curve.
- **Chronological Split for Bayesian Update Parameters:**
  - **Train:** `2024` regular season (`2,429` games, `~185k` plays joined to `EXP-014` `unified_pro_game_f5_totals_2024_2025.parquet`).
  - **Locked OOS Test:** `2025` regular season (`2,430` games, `~185k` plays).
- **Zero-Leakage Invariant:** Neither the `2015–2023` base WE table nor the `2024` Bayesian decay/context parameters ever touch `2025` outcomes.

## 3. Model Ladder
1. `M0_Static_RE24_WE`: Pure `2015–2023` empirical RE24 Win Expectancy.
2. `M1_Static_Plus_Flat_Prior`: Adds constant `EXP-014` pre-game log-odds (`logit(p_m6_ml)`) without inning decay.
3. `M2_Bayesian_Decayed_Prior`: Profile-MLE decay exponent $\gamma$ weighting pre-game log-odds by remaining regulation game fraction $f_{\text{rem}}^{\gamma}$.
4. `M3_Full_Context_Live_WE`: Adds run-environment lead compression `(exp_total_m6 - mu_tot) * score_diff * sqrt(f_rem)` and late-inning bullpen fatigue differential `bp_fatigue_adv_home * I(inning >= 6)`.

## 4. Deliverables & Verification
- Gold ADR-001 ZSTD Parquet tables in `data/processed/`:
  - `live_we_base_matrix_2015_2023.parquet`
  - `live_we_oos_ablation_2025.parquet`
  - `live_we_by_inning_2025.parquet`
  - `live_we_top_clutch_plays_2025.parquet`
- Dark Cyber 4-Asset figures (`lwe01`, `lwe02` + interactive HTML, `lwe03`; `.rds < 5.0 MB`).
- 7-section Q-CRISP Study 23 + runnable Lab 23 (`1:1` parity), `ADR-019`, unit tests in `test-core.R`, independent `sabermetrics-method-auditor` audit, and full 7-part verification gate.
