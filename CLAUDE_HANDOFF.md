# MLBR QUANTITATIVE PLATFORM — AGENT ONBOARDING & MILESTONE HANDOFF GUIDE

> **MANDATORY FIRST-STEP OPERATING CONTRACT (READ BEFORE DOING ANYTHING):**
> Before running any modeling script, editing any file, or answering design questions, the incoming AI agent **MUST read ALL of the following documents in full** on `cbwdellr720:/mnt/storage/Rprojects/mlbr` (zero truncation) and obey every invariant inside them:
> 1. `CLAUDE.md` (Grand Architecture, Medallion & Full-Population Mandate, 7-Section Q-CRISP Template, **Mandatory 3-Part Mentoring Response Format: Concrete Recommendations + Pros & Cons + Ways to Improve**, Dark Cyber visual standard)
> 2. `AGENTS.md` (Core contract, DOX hierarchy, Closeout & Verification Steps 1–7 including updating nearest owning `AGENTS.md`)
> 3. `site/AGENTS.md`, `pkg/mlbr/AGENTS.md`, `specs/AGENTS.md` (Directory-level contracts)
> 4. `.agents/skills/qcrisp-study/SKILL.md` & `.agents/scripts/check_guardrails.py` (Study + Lab DuckDB SQL & guardrail requirements)
> 5. `DECISIONS.md` (`ADR-001` through `ADR-017`) & `experiments/registry.json` (Never re-derive or drift from validated formulas)

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
- **Two-Stage Dev-to-Full Protocol (ADR-006):** Every new model/materialization script MUST support `--smoke` (fast `< 25s` schema/assertion verification) and `--full` (100% full-population Gold/Silver ZSTD Parquet materialization).
- **Dynamic Lake Pathing & DuckDB Hygiene:** Always use `mlbr::default_lake()`, `SET memory_limit = '12GB';`, and `on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)`. Never hardcode `/mnt/storage/Rprojects/mlbr` in `.R`/`.py`/`.sql` files (enforced by `.agents/scripts/check_guardrails.py`).
- **1:1 Study <-> Lab Curriculum Parity:** Every numbered Q-CRISP study (`site/analyses/XX_*.qmd`, strictly following `CLAUDE.md §6` 7-Section Template) MUST ship with a runnable RStudio script (`workbooks/labXX_*.R` containing first-principles math, zero-copy DuckDB SQL, Dark Cyber diagnostic PNG, and 3 Mentor Checkpoints) AND companion rendered guide (`site/workbooks/labXX_*.qmd`) wired into `site/_quarto.yml`, `site/gallery.qmd`, and `site/AGENTS.md` (mechanically enforced by `R/generate_agent_manifest.R`).
- **Dark Cyber 4-Asset Mandate:** Every figure preserves `.png` (300 DPI Dark Cyber `#090d16`), `.rds` (`< 5.0 MB` via top-level plotting functions with `force()`), `.parquet`, and `.html` (interactive Plotly where applicable).
- **Separate Builders from Auditors:** Always run an independent read-only audit (`sabermetrics-method-auditor`) on all new models/tests/studies, resolve any findings, and pass the full verification gate before committing and pushing.
- **Mandatory Mentoring Output Format (`CLAUDE.md §7`):** Every response to the user MUST include: **(1) Concrete Recommendations**, **(2) Pros & Cons Analysis**, and **(3) Ways to Improve**.

---

## 2. Verified Baseline (All 21 Completed & Audited Experiments — 17 Native DuckDB Views!)

All 21 experiments (`EXP-001` through `EXP-021`) and 17 Architecture Decision Records (`ADR-001` through `ADR-017`) are completed, independently audited (`PASS`), logged in `experiments/registry.json`, verified across **`234/234` `testthat` unit tests**, **`28/28` `proof_lake`**, **`5/5` `proof_crosswalks`**, **`6/6` `generate_agent_manifest`** (`21/21` Study–Lab parity), **`2/2` `pytest`**, and **`55/55` rendered Quarto pages**:

- **17 Native Zero-Copy DuckDB Views (`mlbr::lake_duckdb()`):** `v_statcast`, `v_retrosheet_plays`, `v_retrosheet_games`, `v_retrosheet_subs`, `v_retrosheet_rosters`, `v_fangraphs_bat`, `v_fangraphs_pitch`, `v_fangraphs_fielder`, `v_chadwick_register`, `v_lahman_batting`, `v_lahman_pitching`, `v_lahman_people`, `v_ncaa_pbp`, `v_mlbmeta_schedule`, `v_statcast_features`, `v_statcast_tunneling`, and **`v_statcast_biomechanics`** (`4.55M` Silver pitches `2020–2026`).

| Exp ID | Tier | Name | Key Validated Result | Study & Lab |
|:---|:---:|:---|:---|:---|
| **EXP-001** | 1 | Aerodynamics & Approach Angles | VAA/HAA & Magnus lift predict swing-and-miss above raw velocity | `01_pitch_quality_eda.qmd` / `lab01` |
| **EXP-002** | 1 | 4-Quadrant Platoon Physics | Platoon break symmetry & Pitcher Vulnerability Index (PVI) | `02_platoon_physics.qmd` / `lab02` |
| **EXP-003** | 1 | Pitch Tunneling & Stuff+ | Pavlidis/Judge break-to-tunnel ratio ($y_0 = 50 \to 23.8$ ft), split-half $r = 0.975$ | `03_tunneling_stuff_plus.qmd` / `lab03` |
| **EXP-004** | 2 | True-Talent Projections & WAR | Beats Tango Marcel ($p = 1.000$ SO, $p = 0.983$ wOBA), 80% CI coverage ($82.8\%$), Full WAR | `04_player_projections.qmd` / `lab04` |
| **EXP-005** | 1 | Decomposed Pitching+ & Process Engine | 5-head Markov pricing ($r = -0.504$ w/ SIERA) + Batter Decision+ ($r = 0.846$ YoY) | `05_decomposed_pitching_decisions.qmd` / `lab05` |
| **EXP-006** | 3 | Empirical 3-Year Ballpark Factors | 720 park-seasons (2002–2025), Coors 121.6 R, Great American 132.1 HR, Yankee +8.9 LHB HR | `06_ballpark_factors.qmd` / `lab06` |
| **EXP-007** | 2B | PECOTA k-NN, ZiPS 5-Yr & Defense EDA | 70,967 comps (1960–2025), OAA YoY $r = 0.506 >$ DRS $0.431 >$ UZR $0.374$, Exact KKT M5 Ensemble | `07_pecota_zips_ensemble.qmd` / `lab07` |
| **EXP-008** | 3 | BaseRuns, Pythagenpat & 2026 Standings | 720 regular seasons BaseRuns $r = 0.982$, next-yr RMSE `0.0670`, 10k MC w/ club PECOTA uncertainty | `08_team_baseruns_pythagenpat.qmd` / `lab08` |
| **EXP-009** | 4 | 24 Base-Out Markov Game Simulator & WE | 4,859 OOS games (2024–2025) Brier `0.2446` (`M3` `0.2439`), Cal Slope `0.971`, `+0.086 R/G` total bias, 5,372-state WE | `09_game_monte_carlo_simulator.qmd` / `lab09` |
| **EXP-010** | 5 | Market Edge, CLV & Prediction Markets | Locked 2025 OOS (`n=2,430`): `M5` Cal Slope `1.043`, Eighth-Kelly `+2.07%` ROI / `29.5%` Max DD vs Full Kelly `78.4%` (`-105`), Polymarket `+91.7%` overround audit | `10_market_edge_prediction_valuation.qmd` / `lab10` |
| **EXP-011** | 1B | Hawk-Eye Bat-Tracking & Arm-Slot | 1.04M swings (`2023–2026`), Arm Angle YoY `r=0.963`, Bat Speed `r=0.923`, `-3.66%` OOS ISO RMSE, `+0.0302` OOS 4-Seam Whiff AUC | `11_bat_tracking_arm_slot_biomechanics.qmd` / `lab11` |
| **EXP-012** | 3B | Weather/Wind, Umpires & Framing CSAA | 25,639 games (`+0.323 R/G` per `+10°F`, `+0.188 R/G` per `+10 mph` wind out), 1.77M called pitches zero-sum CSAA (Bailey `+23.8 R`), `-0.0091 R/G` OOS Totals RMSE | `12_weather_umpire_catcher_framing.qmd` / `lab12` |
| **EXP-013** | 3C | Pitcher Stamina & Bullpen Fatigue | 1.44M starter fastballs (`-0.43 mph` at P61–80; `+0.35 RV/100` at P101+) & 35,812 team-game zero-leakage 3-day bullpen fatigue | `13_pitcher_stamina_bullpen_fatigue.qmd` / `lab13` |
| **EXP-014** | 5B | Unified Pro Multi-Market & 12-Team Bracket | Locked 2025 OOS F5 AUC `0.5846`, Totals RMSE `-0.0118 R/G`, 50,000 exact 12-team brackets (`NYY 12.91%`, `LAD 10.62%` WS) | `14_unified_pro_game_f5_totals_bracket.qmd` / `lab14` |
| **EXP-015** | 5C | Daily 9-Man Lineups & SP Strikeout Props | 89,118 lineup slots, 4,859 locked-2025 OOS regular-season starts: `Over 5.5 K` AUC `0.6638` (`+0.0228` lift), SO RMSE `2.3285` | `15_daily_lineups_platoon_k_props.qmd` / `lab15` |
| **EXP-016** | 5D | Silver Biomech + Dynamic State-Space NegBin K-Props | 4.55M Silver pitches + NegBin (`theta=46.9`) + 30d `CSW%` state-space: `Over 5.5 K` AUC `0.6779`, Brier `0.2095`, `Over 7.5 K` Brier `0.1133` | `16_dynamic_statespace_negbin_k_props.qmd` / `lab16` |
| **EXP-017** | 5E | Batter HR & Total Bases Props (`v_statcast_biomechanics`) | 17th DuckDB view + 43,740 locked-2025 starts: `HR>=1` AUC `0.6155` (`+0.0195`), `TB>=2` AUC `0.5695` (`+0.0277`) | `17_batter_hr_total_bases_props.qmd` / `lab17` |

---

## 3. Recommended Next Milestones (For Fresh Session)
1. **Live Non-Synthetic CLV & Closing-Line Backtesting (`EXP-020`):** Once `mlbr-market-archive.timer` accumulates 2026 regular-season Polymarket, Kalshi, and confirmed lineup snapshots under `data/raw/markets/`, evaluate realized non-synthetic CLV and SGP hold across all 6 priced markets.
2. **Multivariate Kelly Portfolio Staking & Conditional Copula (`rho(x)`):** Wire `EXP-018` correlation matrix directly into `EXP-010` Kelly sizer for simultaneous same-game ticket risk caps, and test slot/park-conditioned correlation parameters.
3. **Passive Daily Odds & Lineup Archiving Automation (`DONE` — `mlbr-market-archive.timer` enabled at `16:30` & `22:30 UTC`):** Monitor accumulated `data/raw/markets/` snapshots and run live non-synthetic CLV backtest once 2026 regular-season sample accrues.
4. **CRAN-Ready `sabermetricsR` Extraction (`pkg/mlbr` already achieves `0 ERRORS | 0 WARNINGS | 0 NOTEs` via `R CMD build && R CMD check --no-manual`):** When ready, extract the pure vectorized math from Studies `01–18` into a standalone CRAN-ready `sabermetricsR` package.

---

## 4. Deterministic Verification Gate (Must Pass Before Any Git Push)
```bash
Rscript R/proof_lake.R                            # Must pass 28/28
Rscript -e "mlbr::proof_crosswalks()"             # Must pass 5/5
Rscript R/generate_agent_manifest.R               # Must pass 6/6 (confirms 1:1 Study-Lab parity across 01..NN)
Rscript -e 'devtools::test("pkg/mlbr")'           # Must pass all unit tests (191+ tests, 0 fails)
pytest tests/                                     # Must pass 2/2 integration tests
R CMD check pkg/mlbr --no-manual --no-vignettes   # Must have 0 errors
quarto render site/                               # Must render all pages cleanly with exit code 0
```
