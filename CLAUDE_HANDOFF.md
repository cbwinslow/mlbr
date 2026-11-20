# MLBR QUANTITATIVE PLATFORM — AGENT ONBOARDING & MILESTONE HANDOFF GUIDE

> **For Claude / Next AI Developer:** This document contains the exact project state, operating invariants, and step-by-step roadmap for executing the remaining quantitative milestones in the `mlbr` Sabermetrics Data Lake & Quantitative Prediction Platform.

---

## 1. Project Location & Core Invariants
- **Remote Canonical Host:** `cbwdellr720` (`192.168.10.1`, Ubuntu 24.04, 40 cores, 125 GB RAM, `/mnt/storage/Rprojects/mlbr`).
- **NO LOCAL PROJECT FILES ON WINDOWS:** Windows client `cbwwin` is strictly an SSH client. Never clone or write project files locally.
- **PowerShell SSH Invocation Safety:** When piping bash scripts from Windows PowerShell over SSH, PowerShell transmits a UTF-8 BOM byte (`\ufeff`) on stdin. Always strip it:
  ```powershell
  $script = @'
  cd /mnt/storage/Rprojects/mlbr
  # your commands
  '@
  $script | ssh cbwdellr720 "cat | sed $'1s/^\xef\xbb\xbf//' | tr -d '\r' | bash"
  ```
- **Dynamic Lake Pathing:** Always call `mlbr::default_lake()` (never hardcode paths).
- **DuckDB Session Hygiene:** Always execute `DBI::dbExecute(con, "SET memory_limit = '12GB';")` and close connections with `on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)`.
- **1:1 Study <-> Lab Curriculum Parity:** Every numbered Q-CRISP research study (`site/analyses/0X_*.qmd`) MUST have a companion runnable RStudio script (`workbooks/lab0X_*.R`) and rendered lab guide (`site/workbooks/lab0X_*.qmd`) with 3+ mentor checkpoints (enforced by `R/generate_agent_manifest.R`).
- **Dark Cyber 4-Asset Mandate:** Every saved visualization MUST preserve:
  1. `.png` at 300 DPI Dark Cyber (`#090d16` background, cyan `#38bdf8`, emerald `#10b981`, amber `#f59e0b`, rose `#f43f5e`).
  2. `.rds` native R object stripped of parent closure environments (`< 5.0 MB` file size).
  3. `.parquet` processed data table.
  4. `.html` interactive widget where applicable.

---

## 2. Verified Baseline (Completed & Validated Experiments)

All 6 experiments are fully completed, peer-audited (`SHIP` + `PASS`), and verified green in GitHub Actions CI:

| Exp ID | Tier | Name | Key Finding / Validated Metric | Study & Lab |
|:---|:---:|:---|:---|:---|
| **EXP-001** | 1 | Aerodynamics & Approach Angles | VAA/HAA and Magnus lift predict swing-and-miss above raw velocity | `01_pitch_quality_eda.qmd` / `lab01` |
| **EXP-002** | 1 | 4-Quadrant Platoon Physics | Platoon break symmetry & Pitcher Vulnerability Index (PVI) | `02_platoon_physics.qmd` / `lab02` |
| **EXP-003** | 1 | Pitch Tunneling & Stuff+ | Pavlidis/Judge break-to-tunnel ratio ($y_0 = 50 \to 23.8$ ft), split-half $r = 0.975$ | `03_tunneling_stuff_plus.qmd` / `lab03` |
| **EXP-005** | 1 | Decomposed Pitching+ & Process Engine | 5-head Markov pricing ($r = -0.504$ with SIERA) + Batter Decision+ ($r = 0.846$ YoY) | `05_decomposed_pitching_decisions.qmd` / `lab05` |
| **EXP-004** | 2 | True-Talent Projections & WAR | Beats Tango Marcel ($p = 1.000$ SO, $p = 0.983$ wOBA), 80% CI coverage ($82.8\%$), Full WAR | `04_player_projections.qmd` / `lab04` |
| **EXP-006** | 3 | Empirical 3-Year Ballpark Factors | 720 park-seasons (2002-2025), Coors 121.6 R, Great American 132.1 HR, Yankee +8.9 LHB HR | `06_ballpark_factors.qmd` / `lab06` |

---

## 3. Immediate Next Milestone: EXP-007 (Level 3 Team BaseRuns & Standings Simulator)

### Specification
- Spec file: `specs/07_team_baseruns_pythagenpat.md` (Already drafted and ready!)

### Execution Steps for Next Agent:
1. **Create Model Script:** `models/07_team_baseruns_pythagenpat.R`
   - Ingest `v_retrosheet_games` and `v_retrosheet_plays` (`BAT_EVENT_FL = 'T'`) for 2002–2025.
   - Implement David Smyth's BaseRuns: $R = \frac{A \cdot B}{B + C} + D$.
   - Implement Smyth/Patriot dynamic Pythagenpat exponent: $\gamma = \left(\frac{R + RA}{G}\right)^{0.287}$.
   - Integrate Gold Ballpark Factors from `data/processed/ballpark_factors_2002_2025.parquet` to calculate park-neutral BaseRuns.
   - Aggregate Level 2 True-Talent Player Projections from `data/processed/*_projections_true_talent.parquet` to construct projected 2026 team runs scored and allowed.
   - Run 10,000 Monte Carlo schedule simulations over `v_mlbmeta_schedule` to compute projected division titles, wild cards, and win totals.
   - Output Gold Parquet: `data/processed/team_baseruns_pythagenpat_2002_2025.parquet` and `data/processed/team_standings_projections_2026.parquet`.
   - Preserve 4-asset figures: `models/br01_*`, `models/br02_*`, `models/br03_*`.
   - Log experiment `EXP-007-TEAM-BASERUNS-PYTHAGENPAT` using `mlbr::lake_log_experiment()`.
2. **Create Q-CRISP Study:** `site/analyses/07_team_baseruns_pythagenpat.qmd` (7-section template).
3. **Create Companion Student Lab:** `workbooks/lab07_team_baseruns_pythagenpat_workbook.R` and `site/workbooks/lab07_team_baseruns_pythagenpat_workbook.qmd` with 3+ mentor checkpoints.
4. **Update Site Navigation:** Add Study 07 and Lab 07 to `site/_quarto.yml` and `site/gallery.qmd`.

---

## 4. Downstream Milestones: EXP-008 & EXP-009

- **EXP-008: Level 4 Plate-Appearance Markov Monte Carlo Game Simulator & Live Win Probability**
  - Spec: `specs/08_game_monte_carlo_simulator.md` (Already drafted and ready!)
  - Core methods: Log5 matchup pricing, 24 base-out discrete state transitions, pitcher stamina/TTO penalties, bullpen sequencing, WPA / Win Expectancy trajectories.
- **EXP-009: Level 5 Market Edge, Pricing Efficiency & Prediction Market Valuation**
  - Spec: `specs/09_market_edge_prediction_valuation.md` (Already drafted and ready!)
  - Core methods: Polymarket & Kalshi orderbook pricing, fractional Kelly bankroll management, Closing Line Value (CLV) backtesting, Brier score calibration.

---

## 5. Deterministic Verification Gate (Must Run Before Any Git Push)

Before committing any milestone, run this exact sequence on `cbwdellr720`:
```bash
Rscript R/proof_lake.R                          # Must pass 28/28
Rscript -e "mlbr::proof_crosswalks()"           # Must pass 5/5
Rscript R/generate_agent_manifest.R             # Must pass 6/6 (confirms 1:1 Study-Lab parity)
Rscript -e 'devtools::test("pkg/mlbr")'         # Must pass all unit tests (73+ tests)
pytest tests/                                   # Must pass 2/2 integration tests
R CMD check pkg/mlbr --no-manual --no-vignettes # Must have 0 errors
quarto render site/                             # Must render all pages cleanly with exit code 0
```
