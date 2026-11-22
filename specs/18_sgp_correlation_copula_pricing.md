# Spec 18: Same-Game Parlay (SGP) Correlation Matrix & Gaussian/Clayton Copula Pricing Engine (`EXP-018`)

## 1. Mathematical Objective & Theoretical Grounding

Sportsbook Same-Game Parlays (SGPs) combine multiple markets from a single MLB contest (e.g., Full-Game Moneyline, First-5-Innings ML, Game Total Runs, Starting Pitcher Strikeout Props, and Batter HR / Total Bases Props). Because all legs share the same underlying realization of game physics, plate appearances, and run scoring, naive independence multiplication:

$$P_{\text{ind}}(A_1 \cap \dots \cap A_d) = \prod_{j=1}^{d} p_j$$

systematically misprices correlated scripts:
- **Positive correlation** (e.g., `Home ML + F5 Home Win`, `Batter HR >= 1 + Batter's Team ML`, `Batter TB >= 2 + Game Over 8.5`, `Teammate Stack TB >= 2`) causes naive independence to **underprice** joint hit probabilities (offering +EV if unadjusted, or leaking edge if over-taxed by opaque bookmaker SGP hold).
- **Negative correlation** (e.g., `Home SP Over 5.5 K + Game Over 8.5 Runs`, `Batter TB >= 2 + Opposing SP Over 5.5 K`) causes naive independence to **overprice** joint hit probabilities.

By **Sklar's Theorem (1959)** (see Joe 2014, *Dependence Modeling with Copulas*; Nelsen 2006, *An Introduction to Copulas*), any multivariate joint distribution with known marginal exceedance probabilities $(p_1, \dots, p_d) \in (0, 1)^d$ can be uniquely decoupled from its dependence structure via a copula $C: [0,1]^d \to [0,1]$:

$$P(Y_1 = 1, \dots, Y_d = 1) = C(p_1, \dots, p_d; \boldsymbol{\theta})$$

subject to the **Fréchet-Hoeffding bounds**:
$$\max\Big(0, \sum_{j=1}^d p_j - (d - 1)\Big) \le C(p_1, \dots, p_d) \le \min(p_1, \dots, p_d)$$

We evaluate two canonical copula families strictly out-of-sample against naive independence:
1. **Gaussian Copula (`M1_Gaussian_Copula`):**
   Maps marginal probabilities to standard normal latent thresholds $z_j = \Phi^{-1}(p_j)$ with latent correlation matrix $\mathbf{R}$:
   $$P_{\text{Gauss}}(Y_1 = 1, \dots, Y_d = 1 \mid \mathbf{R}) = \Phi_d(z_1, \dots, z_d; \mathbf{R})$$
   Exhibits symmetric tail behavior ($\lambda_L = \lambda_U = 0$) and generalizes seamlessly to $d \ge 3$ leg SGPs via `mvtnorm::pmvnorm`.
2. **Clayton Copula (`M2_Clayton_Copula`, with 90°-rotation for negative dependence):**
   - For positively correlated legs ($\hat{\rho}_{2024} \ge 0$), the exceedance-tail Clayton copula ($\theta > 0$):
     $$C_{\theta}^{\text{Cl}}(p_1, p_2) = \left(p_1^{-\theta} + p_2^{-\theta} - 1\right)^{-1/\theta}$$
     concentrates dependence in the joint occurrence tail ($\lambda_L = 2^{-1/\theta} > 0$), testing whether rare baseball events (e.g., `HR >= 1 + Team Win`) co-occur more strongly in the extreme tail than a Gaussian copula predicts.
   - For negatively correlated legs ($\hat{\rho}_{2024} < 0$), the 90°-rotated Clayton copula ($\theta > 0$):
     $$C_{\theta}^{90^\circ}(p_1, p_2) = p_2 - C_{\theta}^{\text{Cl}}(1 - p_1, p_2)$$
     captures asymmetric negative dependence while strictly preserving marginal probabilities and Fréchet-Hoeffding bounds.

## 2. Input Features & Data Lake Query

All marginals and realized outcomes are sourced strictly from previously validated Gold ADR-001 ZSTD Parquet tables joined via zero-copy DuckDB SQL (`mlbr::lake_duckdb()`):
- **Full-Game ML (`p_m6_ml`, `home_win`), F5 ML (`p_m6_f5`, `f5_home_win`), & Totals (`exp_total_m6`, `actual_total_runs`):** `data/processed/unified_pro_game_f5_totals_2024_2025.parquet` (`EXP-014`, `n = 4,859` games).
  - Pre-game `Over 8.5 Runs` marginal $p_{\text{Over8.5}} = P(\text{actual\_total\_runs} \ge 9 \mid \text{exp\_total\_m6})$ is calibrated strictly on `2024` via logistic regression on `exp_total_m6` and frozen on locked `2025`.
- **Starting Pitcher Strikeout Props (`p_m4_o55`, `y_over_5_5`):** `data/processed/dynamic_statespace_negbin_kprops_2024_2025.parquet` (`EXP-016`, `n = 9,714` starts).
- **Batter HR & Total Bases Props (`p_hr_m2`, `y_hr`, `p_tb2_m2`, `y_tb2`, `slot`, `bat_home`):** `data/processed/batter_hr_tb_prop_backtest_2024_2025.parquet` (`EXP-017`, `n = 87,462` batter-games).

## 3. Target Variables, Copula Estimation & Loss Functions

### Parameter Estimation (Strictly on `2024` Training Data)
For each 2-leg SGP pair $(A, B)$, let $P_{11,i}(\psi) = C(p_{A,i}, p_{B,i}; \psi)$, $P_{10,i} = p_{A,i} - P_{11,i}$, $P_{01,i} = p_{B,i} - P_{11,i}$, and $P_{00,i} = 1 - p_{A,i} - p_{B,i} + P_{11,i}$.
Both $\hat{\rho}_{\text{Gauss}} \in (-0.98, 0.98)$ and $\hat{\theta}_{\text{Clayton}} \in (10^{-4}, 25)$ are estimated by maximizing the exact bivariate Bernoulli log-likelihood on `2024`:

$$\ell(\psi) = \sum_{i \in 2024} \Big[ y_{11,i}\log P_{11,i} + y_{10,i}\log P_{10,i} + y_{01,i}\log P_{01,i} + y_{00,i}\log P_{00,i} \Big]$$

For the 3-leg SGP (`Home ML + Home Batter TB >= 2 + Game Over 8.5`), the $3 \times 3$ correlation matrix $\hat{\mathbf{R}}_{2024}$ is assembled from the three pairwise `2024` Gaussian MLE correlations (projected to nearest positive-definite correlation matrix if needed) and evaluated via exact 3D integration (`mvtnorm::pmvnorm`).

### Locked `2025` OOS Evaluation Metrics
For each SGP archetype on locked `2025` (`y_parlay = I(Y_A = 1 & Y_B = 1)`):
1. **Parlay Binary Log-Loss:** $-\frac{1}{N}\sum [y_i \log \hat{p}_i + (1-y_i)\log(1-\hat{p}_i)]$
2. **Parlay Brier Score:** $\frac{1}{N}\sum (\hat{p}_i - y_i)^2$
3. **Calibration Bias (Pricing Error):** $\text{Bias} = \bar{\hat{p}} - \bar{y}$ (in percentage points) and **Relative Pricing Multiplier** $\bar{\hat{p}}_{\text{copula}} / \bar{\hat{p}}_{\text{ind}}$.
4. **Full 4-Cell Joint Log-Loss** (evaluating all four outcomes $00, 01, 10, 11$ simultaneously).

## 4. Validation Strategy (Zero Temporal Leakage)
- **Training Split:** `2024` regular season (`2,429` games; `43,722` batter-games). All `Over 8.5` marginal parameters, Gaussian $\hat{\rho}$, and Clayton $\hat{\theta}$ are fit strictly on `2024`.
- **Locked OOS Test Split:** `2025` regular season (`2,430` games; `43,740` batter-games). Zero refitting or peeking at `2025` correlations.

## 5. Baseline Comparison & Acceptance Criteria
- **Baseline:** `M0_Naive_Independence` ($p_A \cdot p_B$, or $p_1 p_2 p_3$ for 3-leg SGP).
- **Acceptance Criteria:**
  1. All copula joint probabilities strictly obey Fréchet-Hoeffding bounds $\max(0, p_A + p_B - 1) \le \hat{p}_{11} \le \min(p_A, p_B)$.
  2. On strongly correlated positive SGPs (`Home ML + F5 Home Win`, `Batter HR >= 1 + Team ML`, `Batter TB >= 2 + Game Over 8.5`) and negatively correlated SGPs (`Home SP O5.5 K + Game Over 8.5`, `Batter TB >= 2 + Opp SP O5.5 K`), both Gaussian and Clayton copulas strictly improve locked `2025` OOS Parlay Log-Loss/Brier (positive scripts) and 4-cell Joint NLL (negative scripts, isolating dependence from seasonal marginal drift) vs. `M0_Naive_Independence`.
  3. On the 3-leg canonical SGP (`Home ML + Home Batter TB >= 2 + Game Over 8.5`), the 3D Gaussian Copula strictly beats 3-leg naive independence on locked `2025` OOS Log-Loss and reduces calibration bias by > 80%.

## 6. Implementation Blueprint
- **Model Script:** `models/18_sgp_correlation_copula_pricing.R` (`--smoke` / `--full` CLI)
- **Gold Outputs:**
  - `data/processed/sgp_empirical_correlation_matrix_2024_2025.parquet`
  - `data/processed/sgp_copula_backtest_2024_2025.parquet` (stores locked `2025` OOS evaluation rows scored by `2024`-trained copulas across all 7 two-leg SGPs)
  - `data/processed/sgp_copula_ablation_metrics_2025.parquet`
- **Visualizations (Dark Cyber 4-Asset):** `sgp01_correlation_matrix_heatmap`, `sgp02_gaussian_vs_clayton_copula_pricing`, `sgp03_sgp_oos_calibration_ablation`
- **Study & Lab:** `site/analyses/18_sgp_correlation_copula_pricing.qmd`, `workbooks/lab18_sgp_correlation_copula_pricing_workbook.R`, `site/workbooks/lab18_sgp_correlation_copula_pricing_workbook.qmd`
- **Tests & ADR:** `pkg/mlbr/tests/testthat/test-core.R`, `DECISIONS.md` (`ADR-014`), `experiments/registry.json` (`EXP-018-SGP-CORRELATION-COPULA-PRICING`).
