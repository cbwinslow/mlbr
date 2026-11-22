# MLBR QUANTITATIVE PLATFORM — AGENT ONBOARDING & MILESTONE HANDOFF GUIDE

> **For Next Session (Gemini / Claude):** This document contains the exact verified project state (`HEAD` green in GitHub Actions CI), operating invariants, and step-by-step execution blueprint for the final hierarchical tier (**Level 5: `EXP-010` Market Edge & Prediction Market Valuation**).

---

## 1. Project Location & Core Invariants
- **Remote Canonical Host:** `cbwdellr720` (`192.168.10.1`, Ubuntu 24.04, 40 cores, 125 GB RAM, `/mnt/storage/Rprojects/mlbr`).
- **NO LOCAL PROJECT FILES ON WINDOWS:** Windows client `cbwwin` is strictly an SSH client. Never clone or write project files locally.
- **PowerShell SSH Invocation Safety:** Always strip Windows UTF-8 BOM (`\ufeff`) and `\r` when piping bash over SSH:
  ```powershell
  $script = @'
  cd /mnt/storage/Rprojects/mlbr
  # your commands
  '@
  $script | ssh cbwdellr720 "cat | sed $'1s/^\xef\xbb\xbf//' | tr -d '\r' | bash"
  ```
- **Two-Stage Dev-to-Full Protocol (ADR-006):** Every new model script MUST support `--smoke` (fast `< 25s` schema/assertion verification) and `--full` (100% full-population Gold ZSTD Parquet materialization).
- **Dynamic Lake Pathing & DuckDB Hygiene:** Always use `mlbr::default_lake()`, `SET memory_limit = '12GB';`, and `on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)`.
- **1:1 Study <-> Lab Curriculum Parity:** Every numbered Q-CRISP study (`site/analyses/XX_*.qmd`) MUST have a companion runnable RStudio script (`workbooks/labXX_*.R`) and rendered guide (`site/workbooks/labXX_*.qmd`) with 3+ mentor checkpoints (enforced by `R/generate_agent_manifest.R`).
- **Dark Cyber 4-Asset Mandate:** Every figure preserves `.png` (300 DPI Dark Cyber `#090d16`), `.rds` (`< 5.0 MB` via top-level plotting functions with `force()`), `.parquet`, and `.html` (interactive Plotly where applicable).
- **Separate Builders from Auditors:** Always run an independent read-only audit (`sabermetrics-method-auditor`) before committing and pushing.

---

## 2. Verified Baseline (All 16 Completed & Audited Experiments — Full 5-Tier Hierarchy + Pro Signal Expansion Complete!)

All 16 experiments (`EXP-001` through `EXP-016`) are completed, independently audited, logged in `experiments/registry.json`, verified across `180+` `testthat` unit tests + `45/45` Quarto pages, and green in GitHub Actions CI:

| Exp ID | Tier | Name | Key Validated Result | Study & Lab |
|:---|:---:|:---|:---|:---|
| **EXP-001** | 1 | Aerodynamics & Approach Angles | VAA/HAA & Magnus lift predict swing-and-miss above raw velocity | `01_pitch_quality_eda.qmd` / `lab01` |
| **EXP-002** | 1 | 4-Quadrant Platoon Physics | Platoon break symmetry & Pitcher Vulnerability Index (PVI) | `02_platoon_physics.qmd` / `lab02` |
| **EXP-003** | 1 | Pitch Tunneling & Stuff+ | Pavlidis/Judge break-to-tunnel ratio ($y_0 = 50 \to 23.8$ ft), split-half $r = 0.975$ | `03_tunneling_stuff_plus.qmd` / `lab03` |
| **EXP-005** | 1 | Decomposed Pitching+ & Process Engine | 5-head Markov pricing ($r = -0.504$ w/ SIERA) + Batter Decision+ ($r = 0.846$ YoY) | `05_decomposed_pitching_decisions.qmd` / `lab05` |
| **EXP-004** | 2 | True-Talent Projections & WAR | Beats Tango Marcel ($p = 1.000$ SO, $p = 0.983$ wOBA), 80% CI coverage ($82.8\%$), Full WAR | `04_player_projections.qmd` / `lab04` |
| **EXP-006** | 3 | Empirical 3-Year Ballpark Factors | 720 park-seasons (2002–2025), Coors 121.6 R, Great American 132.1 HR, Yankee +8.9 LHB HR | `06_ballpark_factors.qmd` / `lab06` |
| **EXP-007** | 2B | PECOTA k-NN, ZiPS 5-Yr & Defense EDA | 70,967 comps (1960–2025), OAA YoY $r = 0.506 >$ DRS $0.431 >$ UZR $0.374$, Exact KKT M5 Ensemble | `07_pecota_zips_ensemble.qmd` / `lab07` |
| **EXP-008** | 3 | BaseRuns, Pythagenpat & 2026 Standings | 720 regular seasons BaseRuns $r = 0.982$, next-yr RMSE `0.0670`, 10k MC w/ club PECOTA uncertainty | `08_team_baseruns_pythagenpat.qmd` / `lab08` |
| **EXP-009** | 4 | 24 Base-Out Markov Game Simulator & WE | 4,859 OOS games (2024–2025) Brier `0.2446` (`M3` `0.2439`), Cal Slope `0.971`, `+0.086 R/G` total bias, 5,372-state WE | `09_game_monte_carlo_simulator.qmd` / `lab09` |
| **EXP-010** | 5 | Market Edge, CLV & Prediction Markets | Locked 2025 OOS (`n=2,430`): `M5` Cal Slope `1.043`, Eighth-Kelly `+2.07%` ROI / `29.5%` Max DD vs Full Kelly `78.4%` (`-105`), Polymarket `+91.7%` overround audit | `10_market_edge_prediction_valuation.qmd` / `lab10` |
| **EXP-011** | 1B | Hawk-Eye Bat-Tracking & Arm-Slot | 1.04M swings (`2023–2026`), Arm Angle YoY `r=0.963`, Bat Speed `r=0.923`, `-3.66%` OOS ISO RMSE, `+0.0302` OOS 4-Seam Whiff AUC | `11_bat_tracking_arm_slot_biomechanics.qmd` / `lab11` |
| **EXP-012** | 3B | Weather/Wind, Umpires & Framing CSAA | 25,639 games (`+0.33 R/G` per `+10°F`), 1.77M called pitches zero-sum CSAA (Bailey `+23.8 R`), `-0.0091 R/G` OOS Totals RMSE | `12_weather_umpire_catcher_framing.qmd` / `lab12` |
| **EXP-013** | 3C | Pitcher Stamina & Bullpen Fatigue | 1.44M starter fastballs (`-0.43 mph` at P61–80; `+0.35 RV/100` at P101+) & 35,812 team-game zero-leakage 3-day bullpen fatigue | `13_pitcher_stamina_bullpen_fatigue.qmd` / `lab13` |
| **EXP-014** | 5B | Unified Pro Multi-Market & 12-Team Bracket | Locked 2025 OOS F5 AUC `0.5846`, Totals RMSE `-0.0118 R/G`, 50,000 exact 12-team brackets (`NYY 12.91%`, `LAD 10.62%` WS) | `14_unified_pro_game_f5_totals_bracket.qmd` / `lab14` |
| **EXP-015** | 5C | Daily 9-Man Lineups & SP Strikeout Props | 89,118 lineup slots, 4,859 locked-2025 OOS starts: `Over 5.5 K` AUC `0.6638` (`+0.0228` lift), SO RMSE `2.3285` | `15_daily_lineups_platoon_k_props.qmd` / `lab15` |
| **EXP-016** | 5D | Silver Biomech + Dynamic State-Space NegBin K-Props | 4.55M Silver pitches + NegBin (`theta=46.9`) + 30d `CSW%` state-space: `Over 5.5 K` AUC `0.6779`, Brier `0.2095`, `Over 7.5 K` Brier `0.1133` | `16_dynamic_statespace_negbin_k_props.qmd` / `lab16` |

---

## 3. Immediate Next Milestone: `EXP-010` (Level 5 Market Edge, Pricing Efficiency & Prediction Market Valuation)

### Specification
- Spec file: `specs/10_market_edge_prediction_valuation.md` (Already drafted and ready!)
- Upstream Gold Tables Ready to Consume:
  - `data/processed/game_simulation_backtest_2024_2025.parquet` (`4,859` OOS games with `p_b2`, `p_m3`, `p_m4`, `p_m4_rl15`, `exp_total_runs`, `home_win`)
  - `data/processed/team_standings_projections_2026.parquet` (`30` clubs with `mean_wins`, `p10_wins`, `p90_wins`, `roster_sigma_logit`, `div_prob`, `playoff_prob`)
  - `data/raw/markets/` (archived Polymarket / Kalshi public orderbook snapshots if present)

### Execution Steps for Next Session:
1. **Create Model Script:** `models/10_market_edge_prediction_valuation.R` (`--smoke` -> `--full`)
   - Build a calibrated **Level 4 Game Fair-Value Blend** (`M3_Starter_Team_Log5` + `M4_PA_Markov_Simulator`) strictly out-of-sample (train blend weight on `2024`, test strictly on locked `2025` regular season!).
   - Evaluate Expected Value (`EV`), edge thresholds (`+1%`, `+2%`, `+3%`, `+5%`), Closing Line Value (`CLV`), and **Staking Regimes** (Flat 1u, Full Kelly, Quarter Kelly `c = 0.25`, Eighth Kelly `c = 0.125`) across `2024–2025` games (using live archived market snapshots from `data/raw/markets/` if available AND a realistic vig-adjusted synthetic market benchmark / `B2_PriorYr_Pythag` consensus line for full 4,859-game stress testing).
   - Price **2026 Season Win-Total & Playoff Prediction Markets** (Kalshi / Polymarket fair prices + Kelly sizing) directly from `team_standings_projections_2026.parquet`!
   - Output Gold Parquets: `data/processed/market_valuation_backtest_2024_2025.parquet` and `data/processed/market_2026_futures_valuation.parquet`.
   - Preserve Dark Cyber 4-asset figures: `models/mkt01_*`, `models/mkt02_*`, `models/mkt03_*` (including interactive Plotly bankroll & EV frontier).
   - Log `EXP-010-MARKET-EDGE-PREDICTION-VALUATION` via `mlbr::lake_log_experiment()`.
2. **Create Study 10 & Lab 10:**
   - `site/analyses/10_market_edge_prediction_valuation.qmd` (7-section Q-CRISP)
   - `workbooks/lab10_market_edge_prediction_valuation_workbook.R` & `site/workbooks/lab10_market_edge_prediction_valuation_workbook.qmd` (3+ mentor checkpoints)
3. **Update Site, ADRs & Tests:**
   - Add Study 10 & Lab 10 to `site/_quarto.yml` and `site/gallery.qmd`, add **ADR-009** to `DECISIONS.md`, and add `EXP-010` Kelly/EV/Brier invariants to `pkg/mlbr/tests/testthat/test-core.R`.
4. **Independent Audit & Full Closeout Gate:** Run `sabermetrics-method-auditor`, fix any findings, pass all 6 deterministic checks below, commit, and push!

---

## 4. Optional Post-Level-5 Enhancements (Backlog)
- **Daily Lineup & Platoon Wiring for `EXP-009`:** Integrate `data/raw/mlb_lineups/` daily 9-man batting orders + handedness park factors (`pf_hr_lhb`/`pf_hr_rhb`) into `models/09_game_monte_carlo_simulator.R`.
- **Game-Time Weather & Air Density (`rho`):** Add temperature/humidity/barometric pressure density adjustments to `models/06_ballpark_factors_empirical.R`.
- **Pitcher Stamina & Velocity-Decay EDA:** Model in-game fastball velocity drop by pitch count and days of rest (`days_rest`) from `v_statcast_features`.

---

## 5. Deterministic Verification Gate (Must Pass Before Any Git Push)
```bash
Rscript R/proof_lake.R                            # Must pass 28/28
Rscript -e "mlbr::proof_crosswalks()"             # Must pass 5/5
Rscript R/generate_agent_manifest.R               # Must pass 6/6 (confirms 1:1 Study-Lab parity across 01..10)
Rscript -e 'devtools::test("pkg/mlbr")'           # Must pass all unit tests (113+ tests, 0 fails)
pytest tests/                                     # Must pass 2/2 integration tests
R CMD check pkg/mlbr --no-manual --no-vignettes   # Must have 0 errors
quarto render site/                               # Must render all pages cleanly with exit code 0
```
