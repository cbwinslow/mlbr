# Specification 07: PECOTA k-NN Similarity Comps, ZiPS Archetype Multi-Year Aging, Defense/Baserunning EDA & Meta-Ensemble Consensus

- **Experiment ID:** `EXP-007-PECOTA-ZIPS-ENSEMBLE`
- **Architecture Tier:** Level 2B (Complete Player Projection Gamut — Comps, Archetype Aging, Multi-Year Horizons & Consensus Ensemble)
- **Status:** `SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - Bronze/Silver Views: `v_fangraphs_bat` (`1960–2025`, 32,447 batter-seasons), `v_fangraphs_pitch` (`1960–2025`, 28,144 pitcher-seasons), `v_fangraphs_fielder` (`2002–2025`, `OAA`, `DRS`, `UZR_150`, `Defense`), `v_lahman_people` (`height`, `weight`, `bats`, `throws`, `birthYear`, `birthMonth`), `v_chadwick_register`
  - Level 2 Gold Tables: `data/processed/batter_projections_true_talent.parquet` (`EXP-004`), `data/processed/pitcher_projections_true_talent.parquet` (`EXP-004`)
- **Downstream Consumers:** Level 3 Team BaseRuns & Pythagenpat (`EXP-008`), Level 4 Plate-Appearance Monte Carlo Simulator (`EXP-009`), Level 5 Market Valuation (`EXP-010`).

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Completing the Five Pillars of Sabermetric Projection
`EXP-004` established **Tom Tango's Marcel** (`5/4/3` & `3/2/1`), **Midpoint Delta-Method Aging**, and **THE BAT / OOPSY-style Statcast Physics Ridge**. To cover the full gamut of modern baseball forecasting, `EXP-007` adds the remaining three pillars:
1. **Non-Pitch Exploratory Data Analysis (Defense, Baserunning & Archetype Aging):** Quantifies year-over-year reliability ($r_{t,t+1}$) and aging decay of `OAA`, `DRS`, `UZR/150`, `wBsR`, and `SB%` across `v_fangraphs_fielder` (`2002–2025`), proving why defensive value peaks near age 22–24 while plate discipline peaks near 29–31.
2. **ZiPS-Style Archetype Multi-Year Projections (Dan Szymborski):** Uses a 4-year recency-weighted baseline (`8/5/4/3` weights), position/archetype-specific Delta-Method aging curves (`C`, `Up-the-Middle [SS/2B/CF]`, `Corners/DH` for batters; `SP_Power`, `SP_Finesse`, `RP` for pitchers), and projects **5-year career trajectories** ($T, T+1, \dots, T+4$).
3. **PECOTA-Style Nearest-Neighbor ($k$-NN) Similarity Engine (Nate Silver):** Matches every target player-season against the full `1960–2025` historical pool ($60,591$ player-seasons) using era-neutralized seasonal $z$-scores, exact July-1 baseball age, physical anthropometrics (`height`, `weight`, `BMI`), handedness, and position group. Computes Gaussian kernel weights $w_j = \exp(-d_{ij}^2 / (2\tau^2))$ over the top $k = 50$ strictly historical comps ($\text{season}_j \le T - 2$) to derive:
   - Non-parametric empirical **10th / 25th / 50th / 75th / 90th percentile outcomes** (`p10`–`p90`),
   - **Breakout & Collapse Probabilities**,
   - **Attrition / Survival Probability** ($\Pr(\text{active at } T+1 \text{ with } \ge 50\text{ PA/TBF})$), directly measuring veteran survivorship bias,
   - Named **Top-5 Historical Comps** for every active 2026 player.
4. **ATC / Consensus Meta-Ensemble (`M5_Ensemble`):** Combines `B2_Marcel`, `M_ZiPS`, `M_PECOTA`, and `M4_Statcast_Physics` via strictly chronological non-negative convex stacking ($\sum w_m = 1, w_m \ge 0$) learned strictly on seasons $\le T - 1$.

### 1.2 Exact Formulas
- **Era-Neutralized Seasonal Z-Score (for 1960–2025 Comp Matching):**
  $$z_{i,s}^{(f)} = \frac{x_{i,s}^{(f)} - \mu_{s,g}^{(f)}}{\sigma_{s,g}^{(f)}}$$
  where $\mu_{s,g}^{(f)}$ and $\sigma_{s,g}^{(f)}$ are exposure-weighted seasonal mean and standard deviation within role/league $g$.
- **PECOTA Weighted Similarity Distance:**
  $$d_{ij} = \sqrt{\sum_{f} \alpha_f \big(z_{i,T-1}^{(f)} - z_{j,s}^{(f)}\big)^2 + \alpha_{\text{age}} (\text{Age}_i - \text{Age}_j)^2 + \alpha_{\text{pos}} \mathbb{I}(\text{PosGrp}_i \neq \text{PosGrp}_j) + \alpha_{\text{hand}} \mathbb{I}(\text{Hand}_i \neq \text{Hand}_j)}$$
- **Zero-Leakage Comp Constraint:** When predicting season $T$ from observed season $T-1$, candidate historical comps must satisfy $s \le T - 2$ so their subsequent $s + 1$ outcome is strictly $\le T - 1$.
- **Exact Baseball Innings Conversion:** $\text{IP}_{\text{true}} = \lfloor \text{IP} \rfloor + \text{round}(10 \cdot (\text{IP} - \lfloor \text{IP} \rfloor)) / 3$.

---

## 2. Input Features & Data Lake Queries
- `v_fangraphs_bat` (`1960–2025`, `PA >= 50`, excluding pitchers): `wOBA`, `K_pct`, `BB_pct`, `ISO`, `BABIP`, `OBP`, `SLG`, `PA`, `SB`, `CS`, `wBsR`, `Fielding`, `Positional`, `WAR`, `position`.
- `v_fangraphs_pitch` (`1960–2025`, `TBF >= 50`): `IP`, `TBF`, `G`, `GS`, `SO`, `BB`, `HR`, `FIP`, `ERA`, `K_pct`, `BB_pct`, `WAR`.
- `v_fangraphs_fielder` (`2002–2025`, `Inn >= 100`): `Pos`, `Inn`, `DRS`, `UZR`, `UZR_150`, `OAA`, `Defense`, `ARM`, `RngR`.
- `v_lahman_people` joined via `v_chadwick_register`: `height`, `weight`, `bats`, `throws`, `birthYear`, `birthMonth`.

---

## 3. Target Variables & Loss Functions
- **Batters:** Next-season `wOBA` (headline offensive value) evaluated via exposure-weighted RMSE, MAE, and weighted Pearson $r$.
- **Pitchers:** Next-season `FIP` (headline pitching value) evaluated via exposure-weighted RMSE, MAE, and weighted Pearson $r$.
- **Distribution Calibration:** Empirical coverage of survival-conditioned (`next_PA/TBF >= 50`) PECOTA `[p10, p90]` 80% intervals on locked 2025 out-of-sample outcomes (target: $76\%\text{--}84\%$).

---

## 4. Validation Strategy & `--smoke` → `--full` Execution Protocol
- **Stage A (`--smoke`):** Runs end-to-end on target years `2024:2026` with a capped comp search window in `< 25 seconds` to validate all schemas, joins, assertions, and plots.
- **Stage B (`--full`):** Runs on the **entire 1960–2025 historical comp pool** (`60,591` player-seasons), evaluates full `2016–2025` chronological rolling OOS (10 seasons) + locked `2024 -> 2025` OOS shootout, and materializes 5-year (`2026–2030`) career trajectories and top-5 historical comps for every active 2026 player.

---

## 5. Baseline Comparison Ladder
1. `B2_Marcel` (EXP-004 Tango Marcel)
2. `M_ZiPS_Archetype` (8/5/4/3 + Position/Archetype Delta-Method Aging)
3. `M_PECOTA_kNN` (1960–2025 Similarity-Weighted $k$-NN Comps)
4. `M4_Statcast_Physics` (EXP-004 Statcast + Process Engine Ridge)
5. `M5_Consensus_Ensemble` (Chronological Convex Stack of all 4 paradigms)

---

## 6. Implementation Blueprint & Deliverables
- **Model Script:** `models/07_pecota_zips_ensemble.R` (supports `--smoke` and `--full` CLI flags)
- **Gold ZSTD Parquets (`data/processed/`):**
  - `defense_baserunning_eda_2002_2025.parquet`
  - `pecota_knn_comps_2026.parquet` (named top-5 historical comps + similarity scores + breakout/collapse/survival probabilities)
  - `zips_5yr_trajectories_2026_2030.parquet` (5-year archetype-aged career paths)
  - `ensemble_projections_2016_2026.parquet` (full 5-paradigm comparison & consensus forecasts)
  - `ensemble_backtest_metrics.parquet`
- **4-Asset Dark Cyber Visuals (`models/pz01_*` .. `pz04_*` + `site/assets/`):**
  1. `pz01_defense_baserunning_stabilization_aging.*` (Non-Pitch EDA: YoY stability & position-archetype aging curves)
  2. `pz02_pecota_knn_percentile_fan.*` (PECOTA `p10`–`p90` percentile fan & breakout/collapse frontier + interactive Plotly)
  3. `pz03_zips_5yr_career_trajectories.*` (5-year multi-season career curves for marquee stars by archetype)
  4. `pz04_five_paradigm_oos_shootout.*` (Head-to-head chronological RMSE shootout: Marcel vs. ZiPS vs. PECOTA vs. Statcast Physics vs. Consensus Ensemble)
- **Curriculum:** `site/analyses/07_pecota_zips_ensemble.qmd` + `workbooks/lab07_pecota_zips_ensemble_workbook.R` + `site/workbooks/lab07_pecota_zips_ensemble_workbook.qmd` (3+ mentor checkpoints).
- **Registry:** `EXP-007-PECOTA-ZIPS-ENSEMBLE`.
