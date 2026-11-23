# Spec 20: Interior Half-Life Sweep, Dynamic In-Season Starter Workload (`BF/GS`) & Multivariate Copula-Kelly Staking (`EXP-020`)

## 1. Mathematical Objective & Theoretical Grounding

This specification executes two high-leverage quantitative upgrades directly resolving the follow-up findings from `EXP-018` and `EXP-019`:

### Part A: Interior Half-Life Sweep (`H = 14..180d`) & Dynamic In-Season Starter Workload (`BF/GS`)
In `EXP-019`, `H* = 30d` sat on the upper boundary of `{7, 14, 21, 30}` while expected innings/batters-faced (`eb_bf_gs`) remained static at `season - 1`. Because expected strikeouts satisfy $E[\text{SO}] = \text{K\%} \times \text{BF/GS}$, pitch-count ramps (IL returns, opener/bulk-guy transitions, stamina breakouts) create large in-season `BF/GS` shifts that static `season - 1` priors miss.

We expand the strictly causal (`d.gdate < s.gdate`, up to 180 days within-season) exponential decay kernel $w(\Delta t; H) = 2^{-\Delta t / H}$ across $H \in \{14, 30, 45, 60, 90, 180\}$ days (selecting $H^*$ strictly on `2024` NegBin training NLL) and construct an exponentially-decayed in-season starter workload feature shrunk toward the pitcher's prior-season `eb_bf_gs`:

$$\text{BF\_GS}_{H}(p, t) = \frac{\sum_{d < t} w(\Delta t; H)\,\text{BF}_{p,d} + M_{\text{GS}} \cdot \text{eb\_bf\_gs}_{p, s-1}}{\sum_{d < t} w(\Delta t; H) + M_{\text{GS}}}, \qquad M_{\text{GS}} = 4.0 \text{ starts}$$

### Part B: Multivariate Correlated Kelly Portfolio Staking (`EXP-010` + `EXP-018`)
When betting multiple markets inside the same MLB game (Full-Game ML, Starting Pitcher Over/Under 5.5 K, and Game Over/Under 8.5 Runs), naive independent Fractional Kelly sizes each leg via $f_j^{\text{ind}} = c \cdot \frac{\mu_j}{\sigma_j^2}$, ignoring off-diagonal return covariances $\Sigma_{jk} = O_j O_k (P_{jk} - p_j p_k)$ where $P_{jk} = C_{\text{Gauss}}(p_j, p_k; \hat{\rho}_{jk, 2024})$ comes from our `2024`-trained Gaussian Copula (`EXP-018`).

By second-order Multivariate Kelly optimization (Thorp 2006; MacLean, Thorp & Ziemba 2011), the covariance-adjusted stake vector solves:

$$\max_{\mathbf{f} \in [0, f_{\max}]^m} \; \mathbf{f}^\top \boldsymbol{\mu} - \frac{1}{2c} \mathbf{f}^\top \boldsymbol{\Sigma} \mathbf{f}$$

scaling down positively correlated multi-market tickets (`Home ML + Home SP O5.5 K`, $\rho > 0$) to compress portfolio drawdown while preserving allocation on negatively correlated natural hedges (`Home SP O5.5 K + Game Over 8.5`, $\rho < 0$).

## 2. Input Features & Zero-Copy DuckDB Lake Query
- **Part A:** `data/processed/dynamic_statespace_negbin_kprops_2024_2025.parquet` (`n = 9,714` starts) joined via DuckDB SQL against daily pitcher summaries from `v_statcast_features` (`2024–2025`, `game_type = 'R'`) including `n_pa` (batters faced per outing).
- **Part B:** `data/processed/unified_pro_game_f5_totals_2024_2025.parquet` (`n = 4,859` games) joined with `EXP-020` Part A K-Prop predictions and `2024`-trained Gaussian Copula correlation matrix $\hat{\mathbf{R}}_{2024}$ across `{Home ML, Home SP O5.5 K, Game Over 8.5}`.

## 3. Target Variables & Evaluation Metrics
- **Part A (Locked `2025` OOS, `n = 4,859` starts):** Exact Negative Binomial Count NLL, Strikeout RMSE, `Over 5.5 K` Brier & ROC AUC, and `Over 7.5 K` tail Brier.
- **Part B (Locked `2025` OOS, `n = 2,430` games, `-105` / `2.38%` vig transparent synthetic benchmark per ADR-009):** Realized Cumulative Log-Growth, ROI%, Daily Sharpe Ratio, Maximum Drawdown (%), and Same-Game Stake Reduction (%) on positively correlated tickets.

## 4. Validation Strategy (Zero Temporal Leakage)
- All half-life selection ($H^*$), `MASS::glm.nb` parameters, `Over 8.5` calibration, and $3 \times 3$ Gaussian Copula correlations $\hat{\mathbf{R}}_{2024}$ are fit strictly on `2024` and frozen on locked `2025`. Daily slate sizing uses strictly start-of-day bankroll $W_t$ (zero intraday look-ahead).

## 5. Acceptance Criteria
1. Part A: Identify exact interior/plateau half-life $H^*$ strictly on `2024` training NLL; dynamic in-season `bf_gs_exp` strictly improves locked `2025` OOS Strikeout RMSE, Count NLL, and `Over 5.5 K` AUC over `EXP-019` (`M5d_ExpDecay_H30d`).
2. Part B: Multivariate Copula-Kelly (`c = 0.25`) strictly reduces portfolio volatility / Maximum Drawdown on positively correlated same-game portfolios relative to Independent Quarter-Kelly while preserving positive risk-adjusted Sharpe Ratio.

## 6. Implementation Blueprint
- **Script:** `models/20_dynamic_workload_multivariate_kelly.R` (`--smoke` / `--full`)
- **Gold Tables:** `data/processed/dynamic_workload_kprop_ablation_2025.parquet`, `data/processed/multivariate_kelly_backtest_2025.parquet`, `data/processed/multivariate_kelly_summary_2025.parquet`
- **Visualizations (Dark Cyber 4-Asset):** `mvk01_interior_halflife_workload_ablation`, `mvk02_multivariate_vs_independent_kelly_equity`, `mvk03_copula_stake_haircut_by_correlation`
- **Study & Lab:** `site/analyses/20_dynamic_workload_multivariate_kelly.qmd`, `workbooks/lab20_dynamic_workload_multivariate_kelly_workbook.R`, `site/workbooks/lab20_dynamic_workload_multivariate_kelly_workbook.qmd`
- **Tests & ADR:** `pkg/mlbr/tests/testthat/test-core.R`, `DECISIONS.md` (`ADR-016`), `experiments/registry.json` (`EXP-020-DYNAMIC-WORKLOAD-MULTIVARIATE-KELLY`).
