# Specification 17: Batter Home Run (`HR >= 1`) & Total Bases (`TB >= 2`) Prop Pricing Engine

- **Experiment ID:** `EXP-017-BATTER-HR-TOTAL-BASES-PROPS`
- **Architecture Tier:** Level 5E — Batter Power Prop Pricing (`v_statcast_biomechanics` + Handedness Parks + Weather Carry)
- **Status:** `SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - `v_retrosheet_plays` (`2024–2025` regular-season starting batter-games, `EVENT_CD` 20/21/22/23 for `1B/2B/3B/HR`)
  - `v_statcast_biomechanics` (Silver 4.55M pitches: prior-season `bat_speed_dbl`, `is_blast` rate, `attack_angle_dbl`)
  - `v_fangraphs_bat`, `v_fangraphs_pitch` (strictly `season - 1` EB-shrunk batter `ISO`/`HR%`/`SLG` and opposing SP `HR%`)
  - `ballpark_factors_2002_2025.parquet` (`pf_hr_lhb`, `pf_hr_rhb`, `pf_runs`), `game_weather_umpire_environment_2015_2025.parquet` (`temp_f`, `wind_out_mph`)
- **Downstream Consumers:** Batter Home Run (`Yes/No`) & Over 1.5 Total Bases (`TB >= 2`) prop markets.

---

## 1. Mathematical Objective & Theoretical Grounding

For every starting batter-game $i$ (`BAT_LINEUP_ID` $1 \dots 9$) in `2024–2025` pure regular-season MLB games (`n ~ 87,400`), define binary targets $y_{\text{HR}} = \mathbb{I}(\text{HR}_i \ge 1)$ and $y_{\text{TB2}} = \mathbb{I}(\text{TB}_i \ge 2)$ where $\text{TB}_i = 1\cdot\text{1B} + 2\cdot\text{2B} + 3\cdot\text{3B} + 4\cdot\text{HR}$. Fit strictly chronologically on `2024` (`n ~ 43,700`) and evaluate strictly OOS on locked `2025` (`n ~ 43,700`):
$$\text{logit}(p_i) = \beta_0 + \beta_1 \text{ISO}_{t-1}^{\text{EB}} + \beta_2 \text{BatSpeed}_{t-1} + \beta_3 \text{BlastRate}_{t-1} + \beta_4 \text{Slot}_i + \beta_5 \text{Platoon}_i + \beta_6 \text{PF}_{\text{HR,hand}} + \beta_7 \text{Temp}_F + \beta_8 \text{WindOut} + \beta_9 \text{OppSP\_HR\%}_{t-1}$$

---

## 2. Implementation Deliverables
- Silver View: `mlbr::lake_register_biomechanics()` (`v_statcast_biomechanics`, 17th DuckDB view)
- Model: `models/17_batter_hr_total_bases_props.R` (`--smoke` / `--full`)
- Gold Parquets: `batter_hr_tb_prop_backtest_2024_2025.parquet`, `batter_hr_tb_ablation_metrics_2025.parquet`
- Study & Lab: `site/analyses/17_batter_hr_total_bases_props.qmd`, `workbooks/lab17_batter_hr_total_bases_props_workbook.R`, `site/workbooks/lab17_batter_hr_total_bases_props_workbook.qmd`
- Visuals: `models/bprop01_*`, `models/bprop02_*`, `models/bprop03_*` (Dark Cyber 4-asset suites, `< 5.0 MB` `.rds`).
