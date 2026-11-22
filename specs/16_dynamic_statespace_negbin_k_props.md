# Specification 16: Dynamic In-Season State-Space Talent Updates & Negative Binomial Overdispersed Strikeout Prop Pricing

- **Experiment ID:** `EXP-016-DYNAMIC-STATESPACE-NEGBIN-K-PROPS`
- **Architecture Tier:** Level 5D — Real-Time In-Season Bayesian State-Space Updates & Overdispersed Count Pricing
- **Status:** `SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - Silver Tier: `data/features/statcast_biomechanics/` (`4.55M` pitches, `2020–2026`)
  - Gold Tier: `sp_strikeout_prop_backtest_2024_2025.parquet` (`EXP-015`), `unified_pro_game_f5_totals_2024_2025.parquet` (`EXP-014`)
  - Bronze Tier: `v_statcast` (`2024–2025` pitch-by-pitch `CSW%`, `xwOBA`, `game_date`)
- **Downstream Consumers:** Live in-season dynamic K-prop (`Over 4.5 / 5.5 / 6.5 / 7.5 K`) and game-winner pricing.

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Negative Binomial Overdispersion for Strikeout Counts
Because starting pitcher strikeouts exhibit extra-Poisson variance ($\text{Var}(\text{SO}) > \mathbb{E}[\text{SO}]$) driven by early hooks, blowouts, and deep ace outings, we model $\text{SO}_i \sim \text{NegBin}(\mu_i, \theta)$ with variance $\mu_i + \mu_i^2 / \theta$ (`MASS::glm.nb`, $\theta$ fit strictly on `2024`):
$$P(\text{SO}_i > k + 0.5) = 1 - F_{\text{NB}}(k;\; \mu_i, \hat{\theta})$$

### 1.2 Zero-Leakage In-Season State-Space Updates (`date < t`)
For every starter $p$ and club $c$ strictly prior to game date $t$ (`g2.game_date BETWEEN t - 30 AND t - 1`), compute Empirical Bayes shrunk trailing 30-day `CSW%` ($M = 150$ pitches), trailing 30-day club `xwOBA` ($M = 200$ BIP), and trailing 30-day competitive `bat_speed` ($M = 150$ swings) from Silver `statcast_biomechanics/`.

---

## 2. Implementation Deliverables
- Silver Materialization: `R/materialize_biomechanics_framing.R` (`data/features/statcast_biomechanics/`)
- Model: `models/16_dynamic_statespace_negbin_k_props.R` (`--smoke` / `--full`)
- Gold Parquets: `dynamic_statespace_negbin_kprops_2024_2025.parquet`, `dynamic_negbin_ablation_metrics_2025.parquet`
- Study & Lab: `site/analyses/16_dynamic_statespace_negbin_k_props.qmd`, `workbooks/lab16_dynamic_statespace_negbin_k_props_workbook.R`, `site/workbooks/lab16_dynamic_statespace_negbin_k_props_workbook.qmd`
- Visuals: `models/dyn01_*`, `models/dyn02_*`, `models/dyn03_*` (Dark Cyber 4-asset suites, `< 5.0 MB` `.rds`).
