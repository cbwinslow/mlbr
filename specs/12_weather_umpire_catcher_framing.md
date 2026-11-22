# Specification 12: Environmental & Officiating Alpha — Game-Time Weather/Wind Carry, Home-Plate Umpire Strike Zones & Catcher Framing (`CSAA`)

- **Experiment ID:** `EXP-012-WEATHER-UMPIRE-CATCHER-FRAMING`
- **Architecture Tier:** Level 1C / Level 3B — Environmental Carry, Officiating & Receiving Alpha
- **Status:** `SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - `v_retrosheet_games` (`2015–2025`, `25,640` games with `TEMP_PARK_CT`, `WIND_SPEED_PARK_CT`, `WIND_DIRECTION_PARK_CD`, `BASE4_UMP_ID`)
  - `v_statcast` (`2021–2025`, `1.77M` called pitches with `fielder_2`, `plate_x`, `plate_z`, `sz_top`, `sz_bot`)
  - `data/processed/ballpark_factors_2002_2025.parquet`, `data/processed/game_simulation_backtest_2024_2025.parquet`
- **Downstream Consumers:** `EXP-014` Level 4 v2 Weather/Umpire/Framing-Adjusted Game & Totals Simulator.

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Game-Time Weather & Wind Carry (`2015–2025`)
Define signed wind carry `wind_out_mph` (`+speed` for Retrosheet codes `1, 2, 3` blowing out to LF/CF/RF; `-speed` for `5, 6, 7` blowing in; `0` for `0, 4, 8` dome/crosswind/calm) and outdoor temperature `temp_f` (`TEMP_PARK_CT`). Fit strictly chronologically on `2015–2023`:
$$\widehat{R}_{\text{env}} = \beta_0 + \beta_1 \text{PF}_{\text{runs}} + \beta_2 (\text{temp\_f} - 70) + \beta_3 \text{wind\_out\_mph} + \beta_4 \mathbb{I}(\text{day\_game})$$
Evaluate strictly OOS on locked `2024–2025` (`4,859` games) as an additive adjustment on top of `EXP-009`'s `exp_total_runs`.

### 1.2 Home-Plate Umpire Empirical Bayes Run Factor
For each home-plate umpire $u$ (`BASE4_UMP_ID`) in pre-test `2015–2023`, compute park-adjusted excess runs per game $\bar{d}_u$ over $n_u$ games and shrink toward zero with reliability $M = 80$:
$$\Delta R_u^{\text{EB}} = \frac{n_u}{n_u + 80} \cdot \bar{d}_u$$

### 1.3 Catcher Framing: Called Strikes Above Average (`CSAA`)
Normalize vertical location into batter-specific zone height $z_{\text{norm}} = (\text{plate\_z} - \text{sz\_bot}) / (\text{sz\_top} - \text{sz\_bot})$. Estimate expected called-strike probability $\hat{p}_{\text{CS}}(x, z_{\text{norm}}, \text{stand})$ strictly on pre-test `2021–2023` called pitches via a 2D empirical strike-zone grid (`24 × 24` cells over `x ∈ [-1.8, 1.8]`, `z_norm ∈ [-0.5, 1.5]`), then score every `2021–2025` called pitch:
$$\text{CSAA}_{c,t} = \sum_{i \in (c,t)} \big(y_i - \hat{p}_{\text{CS}, i}\big), \qquad \text{FramingRuns}_{c,t} = 0.125 \cdot \text{CSAA}_{c,t}$$

---

## 2. Implementation Deliverables
- Model: `models/12_weather_umpire_catcher_framing.R` (`--smoke` / `--full`)
- Gold Parquets: `game_weather_umpire_environment_2015_2025.parquet`, `umpire_zone_factors_2015_2025.parquet`, `catcher_framing_csaa_2021_2025.parquet`
- Study & Lab: `site/analyses/12_weather_umpire_catcher_framing.qmd`, `workbooks/lab12_weather_umpire_catcher_framing_workbook.R`, `site/workbooks/lab12_weather_umpire_catcher_framing_workbook.qmd`
- Visuals: `models/env01_*`, `models/env02_*`, `models/env03_*` (Dark Cyber 4-asset suites, `< 5.0 MB` `.rds`).
