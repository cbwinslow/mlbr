# Specification 14: Unified Pro Multi-Market Engine (Full-Game ML, F5, Totals) & Exact 12-Team MLB Playoff Bracket Simulator

- **Experiment ID:** `EXP-014-UNIFIED-PRO-GAME-F5-TOTALS-BRACKET`
- **Architecture Tier:** Level 4 v2 / Level 5 v2 — Multi-Signal Unified Pricing & Exact 12-Team Postseason Bracket
- **Status:** `SPEC READY FOR IMPLEMENTATION`
- **Upstream Dependencies:**
  - `game_simulation_backtest_2024_2025.parquet` (`EXP-009`), `team_standings_projections_2026.parquet` (`EXP-008`)
  - `pitcher_arm_angle_biomechanics_2020_2026.parquet` (`EXP-011`)
  - `game_weather_umpire_environment_2015_2025.parquet` (`EXP-012`)
  - `team_bullpen_fatigue_daily_2018_2025.parquet` (`EXP-013`)
  - `v_retrosheet_plays` (`2024–2025` for realized First-5-Innings F5 scores)
- **Downstream Consumers:** Live multi-market pricing (ML, F5, Totals, Pennant & World Series Futures).

---

## 1. Mathematical Objective & Theoretical Grounding

### 1.1 Unified Multi-Signal Pre-Game Pricing (Train `2024` -> Locked OOS `2025`)
Combine Level 4 base probabilities (`M3`, `M4`) with prior-season starter arm-slot deception (`EXP-011`, strictly `season - 1`), pre-game weather/umpire carry (`EXP-012`), and zero-leakage trailing 3-day bullpen fatigue (`EXP-013`):
$$\text{logit}(p_{\text{ML}}) = \beta_0 + \beta_1 \text{logit}(p_{\text{M3}}) + \beta_2 \text{logit}(p_{\text{M4}}) + \beta_3 \Delta\text{IVB}_{\text{slot}} + \beta_4 \Delta\text{BP}_{\text{fatigue}}$$
$$\widehat{T}_{\text{total}} = \gamma_0 + \gamma_1 T_{\text{EXP-009}} + \gamma_2 \Delta R_{\text{weather}} + \gamma_3 \Delta R_{\text{ump}} + \gamma_4 \big(\Delta R_{\text{BP,H}} + \Delta R_{\text{BP,A}}\big)$$

### 1.2 Exact 12-Team MLB Playoff Bracket Simulation (`50,000` Seasons)
For each simulated 2026 season, seed AL and NL (`#1, #2` division winners earn First-Round Byes; `#3` vs `#6` and `#4` vs `#5` play Best-of-3 Wild Card; Division Series Best-of-5; LCS Best-of-7; World Series Best-of-7) using exact Log5 single-game win probabilities with home-field advantage (`+0.035`). Guarantees $\sum_{\text{AL}} P(\text{Pennant}) = 1$, $\sum_{\text{NL}} P(\text{Pennant}) = 1$, and $\sum_{30} P(\text{WS}) = 1$.

---

## 2. Implementation Deliverables
- Model: `models/14_unified_pro_game_f5_totals_bracket.R` (`--smoke` / `--full`)
- Gold Parquets: `unified_pro_game_f5_totals_2024_2025.parquet`, `unified_pro_multi_market_metrics.parquet`, `mlb_2026_12team_bracket_ws_odds.parquet`
- Study & Lab: `site/analyses/14_unified_pro_game_f5_totals_bracket.qmd`, `workbooks/lab14_unified_pro_game_f5_totals_bracket_workbook.R`, `site/workbooks/lab14_unified_pro_game_f5_totals_bracket_workbook.qmd`
- Visuals: `models/pro01_*`, `models/pro02_*`, `models/pro03_*` (Dark Cyber 4-asset suites, `< 5.0 MB` `.rds`).
