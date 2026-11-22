# Spec 19: Joint Ordered/Multinomial Total Bases (`0–4+ TB`) & Exponential Half-Life State-Space Decay (`EXP-019`)

## 1. Mathematical Objective & Theoretical Grounding

This specification executes two high-impact upgrades to `mlbr`'s Level 5 player-prop engines (`EXP-016` and `EXP-017`):

### Part A: Joint Ordered & Multinomial Total Bases Distribution (`0, 1, 2, 3, 4+ TB`)
In `EXP-017`, batter Home Run (`HR >= 1`) and Total Bases (`TB >= 2`) were modeled via separate binary logistic regressions. Pricing an entire sportsbook Total Bases ladder (`Over 0.5 TB`, `Over 1.5 TB`, `Over 2.5 TB`, `Over 3.5 TB`) via independent binary logits risks non-monotonic probability crossings ($P(\text{TB} \ge k+1) > P(\text{TB} \ge k)$) and ignores shared latent offensive power.

We model the 5-category discrete Total Bases outcome $Y_i = \min(\text{game\_tb}_i, 4) \in \{0, 1, 2, 3, 4+\}$ jointly via:
1. **Proportional Odds Ordered Logit (`M1_Ordered_Logit`, McCullagh 1980 via `MASS::polr`):**
   $$\text{logit}\,P(Y_i \le k \mid \mathbf{x}_i) = \alpha_k - \boldsymbol{\beta}^\top \mathbf{x}_i, \qquad k \in \{0, 1, 2, 3\}, \quad \alpha_0 < \alpha_1 < \alpha_2 < \alpha_3$$
2. **Multinomial Softmax Logit (`M2_Multinomial_Logit` via `nnet::multinom`):**
   $$P(Y_i = k \mid \mathbf{x}_i) = \frac{\exp(\boldsymbol{\gamma}_k^\top \mathbf{x}_i)}{\sum_{m=0}^4 \exp(\boldsymbol{\gamma}_m^\top \mathbf{x}_i)}, \qquad \boldsymbol{\gamma}_0 = \mathbf{0}$$
   allowing non-proportional power/environment effects (e.g., `bat_speed`, `blast_rate`, `pf_hr_hand`, `wind_out_mph` lifting `TB >= 4` / home-run states disproportionately more than `TB = 1` singles) while **algebraically guaranteeing** strict monotonicity across every ladder rung:
   $$1 \ge P(\text{TB} \ge 1) \ge P(\text{TB} \ge 2) \ge P(\text{TB} \ge 3) \ge P(\text{TB} \ge 4) \ge 0$$

### Part B: Exponential Half-Life State-Space Decay for Pitcher Strikeout Props (`EXP-016` Upgrade)
In `EXP-016`, in-season pitcher form used a flat 30-day boxcar window (`t - 30` to `t - 1`), assigning equal weight to a start 29 days ago and a start 5 days ago while dropping pitches discontinuously on Day 31. We replace the boxcar with a continuous **Exponential Half-Life Decay Kernel** (strictly causal, `d < t`, up to 90 days back):

$$w(\Delta t; H) = 2^{-\Delta t / H}, \qquad \Delta t = \text{DATEDIFF}(\text{'day'}, d, t) \in [1, 90]$$

$$\text{CSW}_{H}(p, t) = \frac{\sum_{d < t} w(\Delta t; H)\,\text{CSW}_{p,d} + M_{\text{CSW}} \cdot 0.290}{\sum_{d < t} w(\Delta t; H)\,P_{p,d} + M_{\text{CSW}}}, \qquad \text{K\%}_{H}(p, t) = \frac{\sum_{d < t} w(\Delta t; H)\,\text{SO}_{p,d} + M_K \cdot 0.224}{\sum_{d < t} w(\Delta t; H)\,\text{PA}_{p,d} + M_K}$$

Half-life $H \in \{7, 14, 21, 30\}$ days is selected strictly on `2024` training NegBin NLL (zero `2025` peeking) and benchmarked against `EXP-016`'s flat 30-day Negative Binomial model (`M4_Flat30d_NegBin`) on locked `2025` OOS (`n = 4,859` starts).

## 2. Input Features & Zero-Copy DuckDB Lake Query
- **Part A:** `data/processed/batter_hr_tb_prop_backtest_2024_2025.parquet` (`n = 87,462` starting batter-games; `2024` train `43,722`, `2025` OOS `43,740`).
- **Part B:** `data/processed/dynamic_statespace_negbin_kprops_2024_2025.parquet` (`n = 9,714` starts) joined via zero-copy DuckDB SQL against daily pitcher pitch-level summaries aggregated from `v_statcast_features` (`2024–2025`, `game_type = 'R'`).

## 3. Target Variables & Proper Scoring Rules
- **Part A (`2025` OOS):**
  - 5-Class Multi-Category Log-Loss (NLL): $-\frac{1}{N}\sum \log \hat{p}_{i, y_i}$
  - Ranked Probability Score (RPS): $\frac{1}{4N}\sum_{i=1}^N \sum_{k=0}^3 (\hat{F}_i(k) - \mathbb{I}(y_i \le k))^2$
  - Threshold Log-Loss & ROC AUC across all 4 ladders: `TB >= 1`, `TB >= 2`, `TB >= 3`, `TB >= 4`
  - Monotonicity Violations Count (must equal `0`).
- **Part B (`2025` OOS):**
  - Exact Negative Binomial Count NLL, Strikeout RMSE, `Over 5.5 K` Brier/AUC, and `Over 7.5 K` tail Brier.

## 4. Validation Strategy (Zero Temporal Leakage)
- **Strict Chronological Split:** All Part A parameters (`MASS::polr`, `nnet::multinom`) and Part B half-life selection $H^*$ + `MASS::glm.nb` parameters are fit strictly on `2024` and frozen on locked `2025`. Every exponential decay query enforces `prev.game_date < cur.game_date`.

## 5. Acceptance Criteria
1. Part A: `0` monotonicity violations across all `43,740` locked `2025` batter-games; both `M1_Ordered_Logit` and `M2_Multinomial_Logit` strictly beat `M0_League_BaseRate` on 5-class NLL, RPS, and all 4 threshold AUCs (`TB >= 1..4`).
2. Part B: Report exact locked `2025` OOS comparison of optimal `2024`-selected half-life $H^*$ vs. `EXP-016` flat 30-day window (`M4_Flat30d_NegBin`) honestly across RMSE, Count NLL, `Over 5.5 K` AUC, and `Over 7.5 K` Brier.

## 6. Implementation Blueprint
- **Script:** `models/19_ordered_tb_exp_decay_statespace.R` (`--smoke` / `--full`)
- **Gold Tables:** `data/processed/ordered_tb_ladder_backtest_2024_2025.parquet` (stores locked `2025` OOS ladder predictions `n = 43,740` with empirical `2024` conditional tail mean $E[\text{TB} \mid \text{TB} \ge 4] = 4.937$), `data/processed/ordered_tb_ablation_metrics_2025.parquet`, `data/processed/exp_decay_kprop_ablation_2025.parquet`
- **Visualizations (Dark Cyber 4-Asset):** `ord01_tb_ladder_probabilities_2025`, `ord02_ordered_vs_multinomial_ablation`, `ord03_exp_halflife_kprop_curve`
- **Study & Lab:** `site/analyses/19_ordered_tb_exp_decay_statespace.qmd`, `workbooks/lab19_ordered_tb_exp_decay_statespace_workbook.R`, `site/workbooks/lab19_ordered_tb_exp_decay_statespace_workbook.qmd`
- **Tests & ADR:** `pkg/mlbr/tests/testthat/test-core.R`, `DECISIONS.md` (`ADR-015`), `experiments/registry.json` (`EXP-019-ORDERED-TB-EXP-DECAY-STATESPACE`).
