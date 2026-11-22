# Specification 15: Daily 9-Man Starting Lineups, Handedness Park Asymmetry & Starting Pitcher Strikeout (`SO`) Prop Pricing Engine

- **Experiment ID:** `EXP-015-DAILY-LINEUPS-PLATOON-K-PROPS`
- **Architecture Tier:** Level 4C / Level 5C — Daily Lineup Platoon Matchups & Player Prop Pricing
- **Status:** `SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - `v_retrosheet_plays` (`2024–2025`, 89,118 starting lineup slots + realized starter `SO` & `BF`)
  - `v_fangraphs_bat`, `v_fangraphs_pitch`, `v_chadwick_register` (strictly `season - 1` true-talent profiles)
  - `ballpark_factors_2002_2025.parquet` (`pf_hr_lhb`, `pf_hr_rhb`, `pf_so`), `pitcher_arm_angle_biomechanics_2020_2026.parquet`, `catcher_framing_csaa_2021_2025.parquet`
- **Downstream Consumers:** Pitcher Strikeout Prop (`Over/Under 4.5 / 5.5 / 6.5 K`) pricing, F5 & Full-Game lineup adjustments.

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Exact 9-Man Starting Lineup Weighted Skill & Handedness Park Factor
For each game's starting 9 batters ($b = 1 \dots 9$) with lineup-order PA weights $w_b \in [1.10, \dots, 0.90]$ ($\sum w_b = 9$) and strictly prior-season (`season - 1`) EB-shrunk skill ($M = 150$ PA):
$$\text{LineupK\%} = \frac{\sum_{b=1}^9 w_b \cdot \text{K\%}_{b, t-1}^{\text{EB}}}{9}, \qquad \text{LineupPF}_{\text{HR}} = \frac{\sum_{b=1}^9 w_b \cdot \big(\mathbb{I}(L_b)\text{PF}_{\text{HR,LHB}} + \mathbb{I}(R_b)\text{PF}_{\text{HR,RHB}}\big)}{9}$$

### 1.2 Poisson Starting Pitcher Strikeout (`SO`) Prop Model
Model expected starter strikeouts $\lambda_i = \mathbb{E}[\text{SO}_i]$ strictly chronologically (`2024` train $\rightarrow$ locked `2025` OOS):
$$\log(\lambda_i) = \beta_0 + \beta_1 \log(\widehat{\text{K\%}}_{\text{SP}, t-1} \cdot \widehat{\text{BF}}_{\text{SP}, t-1}) + \beta_2 \text{OppLineupK\%}_i + \beta_3 \text{IVB}_{\text{slot}, t-1} + \beta_4 \text{PF}_{\text{SO}} + \beta_5 \Delta K_{\text{ump}} + \beta_6 \text{CSAA}_{c, t-1}$$
Exact discrete prop probabilities follow directly from the Poisson CDF:
$$P(\text{SO}_i > 5.5) = 1 - \sum_{k=0}^{5} \frac{e^{-\lambda_i} \lambda_i^k}{k!}$$

---

## 2. Implementation Deliverables
- Model: `models/15_daily_lineups_platoon_k_props.R` (`--smoke` / `--full`)
- Gold Parquets: `daily_lineups_platoon_2024_2025.parquet`, `sp_strikeout_prop_backtest_2024_2025.parquet`, `sp_k_prop_ablation_metrics_2025.parquet`
- Study & Lab: `site/analyses/15_daily_lineups_platoon_k_props.qmd`, `workbooks/lab15_daily_lineups_platoon_k_props_workbook.R`, `site/workbooks/lab15_daily_lineups_platoon_k_props_workbook.qmd`
- Visuals: `models/kprop01_*`, `models/kprop02_*`, `models/kprop03_*` (Dark Cyber 4-asset suites, `< 5.0 MB` `.rds`).
