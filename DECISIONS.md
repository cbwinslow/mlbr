# Architecture Decision Records (`mlbr`)

Concise log of lasting architectural, storage, and methodological decisions for `mlbr`. Add a new ADR whenever a non-obvious design choice is settled so future sessions do not re-litigate it.

---

## ADR-001: ZSTD-Compressed, Sorted, Hive-Partitioned Apache Parquet + DuckDB/Arrow Compute
- **Status:** Accepted (2026-11-18)
- **Context:** The `mlbr` lake stores 14.64M+ Statcast pitches and 125+ seasons across Retrosheet, FanGraphs, Chadwick, Lahman, and NCAA. Monolithic `.duckdb` files introduce single-writer lock contention across R, Python, and Quarto processes, while `.sql` dumps are slow and bloated.
- **Decision:** Standardize on Apache Parquet with **ZSTD compression** (`ROW_GROUP_SIZE = 100000`), Hive-partitioned by `game_year`, and sorted by `(pitcher, game_date, game_pk, at_bat_number, pitch_number)`. Mount via zero-copy DuckDB views (`mlbr::lake_duckdb()`) and Arrow/Polars (`pyarrow` / `polars.scan_parquet`). Reserve Arrow IPC/Feather strictly for ephemeral scratch IPC.
- **Consequences:** Sub-50ms analytical queries (~39ms on `v_statcast_features`) via min-max row-group pruning, concurrent multi-engine reads (R + Python), and compact disk footprint.

## ADR-002: Medallion Architecture & Full-Population Silver Materialization
- **Status:** Accepted
- **Context:** Ad-hoc feature engineering inside notebooks causes formula drift, inconsistent sample filters, and slow repeated recomputation.
- **Decision:** Enforce strict Bronze (`data/raw/`, 100% zero-drop schema fidelity), Silver (`data/features/`, full-population materialized features keyed by `game_pk`, `at_bat_number`, `pitch_number`), and Gold (`data/processed/`, `data/models/`) tiers. Validated metrics must be codified in `R/materialize_*.R` and `sql/`.
- **Consequences:** Every downstream study, workbook, and model inherits identical, audited feature definitions over the full population.

## ADR-003: Strict Chronological Validation & 4-Asset Preservation
- **Status:** Accepted
- **Context:** Baseball forecasting models are highly vulnerable to temporal leakage (training on future seasons/dates) and unreproducible figures.
- **Decision:** All predictive models must use strictly chronological train/validation/test splits. Every published figure/model must preserve four assets (`.png` at 300 DPI Dark Cyber, `.rds` native R object, `.parquet`/`.csv` aggregated plot data, and `.html` interactive widget where applicable) and log results into `experiments/registry.json`.
- **Consequences:** Full auditability, restyle-without-requery capability, and zero silent regressions in reported findings.

## ADR-004: Empirical Multi-Year Regressed Ballpark Factors & Handedness Asymmetry Decomposition
- **Status:** Accepted (2026-11-20)
- **Context:** Single-season home-vs-road scoring and home run ratios suffer from severe sample noise (~81 home games per season) and schedule imbalance. Furthermore, symmetric park factors mask critical handedness biases caused by asymmetric outfield dimensions (e.g. Yankee Stadium short right porch vs. Oracle Park deep right-center triples alley).
- **Decision:** Derive 3-year rolling weighted (3/2/1) empirical park factors from `v_retrosheet_games` and `v_retrosheet_plays` (`BAT_EVENT_FL = 'T'`, 16.2M+ PAs across 2000-2025) with Empirical Bayes pseudo-count smoothing and event-specific shrinkage denominators ($M = 40$ games for runs, $M = 1500$ PAs for HR, $M = 800$ PAs for LHB/RHB HR splits, $M = 2000$ PAs for SO, BB, and BABIP). Enrich with exact physical geometry, elevation, and roof type from `park_dimensions.parquet`.
- **Consequences:** Provides stable, non-noisy environmental priors (`pf_runs`, `pf_hr`, `pf_hr_rhb`, `pf_hr_lhb`, `pf_so`, `pf_bb`, `pf_babip`) across 720 park-seasons (2002-2025) stored as Gold ADR-001 ZSTD Parquet (`data/processed/ballpark_factors_2002_2025.parquet`) for Level 3 team run creation/prevention and Level 4 Monte Carlo simulation.

## ADR-005: Full-Spectrum True-Talent WAR Decomposition & Residual Sensor Ridge Blending
- **Status:** Accepted (2026-11-20)
- **Context:** True-talent player projections require combining long-term historical aging and stabilization baselines (Tango Marcel + Carleton reliability M) with high-frequency modern physical tracking sensors (Hawk-Eye mlbr Stuff+, Decomposed Pitching+, and Batter Process Engine). Blending sensors of radically different sample lengths directly in a single regression causes L2 over-shrinkage, while standard WAR accounting can inadvertently double-count positional adjustments if FanGraphs `Defense` is combined naively with positional runs.
- **Decision:** Employ a 2-stage orthogonal architecture: Stage 1 establishes multi-year Marcel rates and zero-sum midpoint Delta-Method aging curves; Stage 2 applies a compact zero-mean Residual Ridge learning orthogonal adjustments from modern physics sensors. In WAR accounting, isolate pure fielding runs `b.Fielding AS fld_runs` so that $\text{proj\_def\_runs} = \text{proj\_fld\_runs} + \text{proj\_pos\_runs}$ and $\text{proj\_war} = (\text{proj\_wraa} + \text{proj\_bsr\_runs} + \text{proj\_fld\_runs} + \text{proj\_pos\_runs} + \text{proj\_repl\_runs}) / 9.77$ strictly adheres to FanGraphs WAR without double-counting.
- **Consequences:** Out-of-sample weighted RMSE significantly outperforms Marcel and naive persistences on both pitchers ($p = 1.000$) and batters ($p = 0.983$), while producing calibrated exposure-dependent 80% uncertainty bands and mathematically coherent True-Talent WAR.

## ADR-006: Complete 5-Paradigm Projection Gamut (PECOTA k-NN, ZiPS Archetype Aging, Meta-Ensemble) & Two-Stage `--smoke` -> `--full` Protocol
- **Status:** Accepted (2026-11-21)
- **Context:** While Statcast physical tracking (`EXP-004`) dominates next-season point-estimate RMSE, complete baseball forecasting also requires non-parametric empirical uncertainty fans (`p10–p90`), attrition/survival probabilities, interpretable historical player comps, and multi-year 5-season career trajectories. In addition, rapid iteration on 70k–14.6M-row scripts benefits from a fast smoke-test mode prior to full-lake materialization.
- **Decision:** Standardize a two-stage CLI contract (`--smoke` for `< 25s` schema/assertion verification, `--full` for 100% full-population Gold Parquet materialization) and implement vectorized BLAS PECOTA k-NN similarity matching (`1960–2025` era-neutralized z-scores, strict `comp_season <= T - 2` anti-leakage guard), ZiPS `8/5/4/3` archetype-specific midpoint Delta-Method aging with 5-year horizons (`2026–2030`), and a chronological M5 convex meta-ensemble.
- **Alternatives Considered:** External QP packages (`quadprog`) or finite-difference `BFGS` (not chosen: `BFGS` hit `maxit=100` on flat collinear wOBA MSE surfaces, whereas exact active-set KKT enumeration over all 15 faces of the 4-simplex guarantees $10^{-16}$ global optimality in `< 0.5 ms` with zero external dependencies; revisit only if stacking $m > 10$ models). Matching PECOTA on unregressed single-season deltas (not chosen: double-regresses stars by ~30 pts wOBA; replaced by symmetric 4-year `8/5/4/3` candidate baselines).
- **Consequences:** Delivers all five canonical sabermetric projection paradigms in one unified Gold tier while proving empirically that Statcast physics captures 100% of point-estimate weight and PECOTA/ZiPS provide orthogonal distributional and multi-year career intelligence.

## ADR-007: Level 3 Non-Linear BaseRuns, Dynamic Pythagenpat & Two-Layer Schedule Monte Carlo
- **Status:** Accepted (2026-11-21)
- **Context:** Aggregating player projections into team win totals requires accounting for non-linear run creation (baserunners × advancement), era-specific run environments, ballpark asymmetry, and both game-level binomial variance and pre-season true-talent projection uncertainty.
- **Decision:** Compute David Smyth BaseRuns directly from Retrosheet atomic play events (`2002–2025`), apply Smyth/Patriot dynamic Pythagenpat (`gamma = ((RS+RA)/G)^0.287`), bridge Level 2 `M5` Consensus player projections + `EXP-006` Ballpark Factors into 2026 park-adjusted team runs, and simulate 10,000 full 2,430-game schedules with pre-season logit talent perturbation (`sigma = 0.14`) and neutral HFA (`0.535`).
- **Alternatives Considered:** Fixed Pythagorean exponent (`gamma = 1.83`, not chosen because it misprices extreme high/low run environments) or binomial-only schedule simulation without talent perturbation (not chosen because it understates pre-season win-total credible intervals by ~40%; revisit once Level 4 PA-level simulator is wired).
- **Consequences:** Proves BaseRuns Pythagenpat strictly outperforms actual win% and actual-runs Pythagorean in next-season forecasting (`0.06515` vs `0.07082` RMSE) while generating calibrated 2026 MLB playoff probabilities summing exactly to 6 division titles and 12 playoff berths.

## ADR-008: Level 4 Exact 24 Base-Out Markov Half-Inning Convolution & Zero-Leakage Win Expectancy
- **Status:** Accepted (2026-11-21)
- **Context:** Simulating 4,859 historical MLB games via naive play-by-play Monte Carlo (10,000 paths/game = 48.6M games) is slow and introduces Monte Carlo sampling variance into Brier/Log-Loss backtests.
- **Decision:** Extract exact empirical 24 base-out transition tensors strictly from pre-test `2015–2023` Retrosheet PAs (`1.57M` plays), propagate exact half-inning run distributions analytically across starter TTO 1/2/3 innings, bullpen innings, and extra-inning ghost-runner (`bases = 2`) states, and convolve 9 innings via exact discrete convolution parallelized across 8 cores (`53.5s` for all `4,859` games).
- **Alternatives Considered:** Pure stochastic Monte Carlo game paths (not chosen for pre-game backtest pricing due to $O(1/\sqrt{N})$ sampling noise and 20× slower runtime; reserved for interactive single-game trajectory visualization).
- **Consequences:** Delivers deterministic, zero-variance Moneyline, Run-Line (`-1.5`), and Over/Under Total pricing across all `4,859` regular-season `2024–2025` games (`Brier = 0.24451`, `Cal Slope = 0.949`) plus a 5,372-state empirical Win Expectancy matrix ready for Level 5 market valuation (`EXP-010`).

## ADR-009: Level 5 Chronological Fair-Value Blending, Fractional Kelly Risk Caps & Prediction Market Overround Auditing
- **Status:** Accepted (2026-11-21)
- **Context:** Translating Level 4 pre-game probabilities (`M3_Starter_Team_Log5`, `M4_PA_Markov_Simulator`) and Level 3 season standings (`team_standings_projections_2026.parquet`) into capital allocation exposes portfolios to estimation error, correlated same-day MLB slates, and severe early-season prediction-market overround (`+91.7%` raw sum in archived Polymarket 2027 WS contracts).
- **Decision:** Fit the convex Level 4 fair-value blend weight strictly chronologically (`2024` regular season Log-Loss minimization $\rightarrow$ frozen `w_M3 = 0.453, w_M4 = 0.547` on locked `2025` OOS), size simultaneous-day MLB slates off start-of-day bankroll $W_t$ with zero intraday look-ahead, enforce single-game (`5%`) and daily slate (`35%`) exposure caps, recommend Eighth-Kelly (`c = 0.125`) or Quarter-Kelly (`c = 0.25`) over Full Kelly, and explicitly audit multi-outcome overround ($\nu = \sum q_i^{\text{mid}} - 1$) and bid-ask spreads on archived Polymarket/Kalshi orderbooks.
- **Alternatives Considered:** Full Kelly (`c = 1.0`, rejected: produced `78.4%` max drawdown on locked `2025` OOS vs. `29.5%` for Eighth-Kelly under exact `-105/-105` vig) and unadjusted mid-price comparisons on raw futures books (rejected: ignores `+91.7%` overround and `7–8¢` median bid-ask spread drag).
- **Consequences:** Completes the 5-tier hierarchical `mlbr` platform (`EXP-001` through `EXP-010`) with reproducible Gold valuation tables (`market_valuation_backtest_2024_2025.parquet`, `market_staking_summary_2024_2025.parquet`, `market_2026_futures_valuation.parquet`), transparent disclosure of synthetic vs. live archived market benchmarks, and zero-leakage portfolio risk controls.

## ADR-010: Pro-Grade Signal Expansion (Hawk-Eye Bat-Tracking, Arm-Slot Ride, Weather/Umpire/CSAA, Bullpen Fatigue) & Exact 12-Team MLB Playoff Bracket
- **Status:** Accepted (2026-11-21)
- **Context:** While `EXP-001` through `EXP-010` completed the 5-tier hierarchy, modern MLB front offices and sharp syndicates extract substantial additional signal from Hawk-Eye bat-tracking (`1.04M` competitive swings), pitcher release `arm_angle`, game-time temperature/wind carry, home-plate umpire strike zones, catcher framing (`CSAA`), rolling 3-day bullpen exhaustion, First-5-Innings (F5) markets, and MLB's asymmetric 12-team playoff bracket (Top-2 First-Round Byes).
- **Decision:** Standardize four strictly chronological research engines (`EXP-011`..`EXP-014`): (1) competitive bat-tracking (`>= 60 mph`) & pre-test quadratic `arm_angle` slot-ride (`ivb_above_slot`), (2) zero-sum season-centered 2D strike-zone Catcher Framing (`CSAA`) removing secular 2021–2025 umpire zone-tightening drift + weather/umpire carry, (3) zero-leakage (`g2.game_date < g1.game_date`) rolling 3-day bullpen workload fatigue, and (4) unified multi-market (ML/F5/Totals) pricing paired with an exact 50,000-season 12-team MLB playoff bracket simulator guaranteeing exact AL/NL pennant (`1.000`), bye (`4.00`), and WS (`1.000`) probability sums.
- **Consequences:** Delivers `+0.0302` OOS 4-seam Whiff AUC (`EXP-011`), `-3.66%` OOS ISO RMSE (`EXP-011`), `+23.8` Framing Runs for Patrick Bailey (`EXP-012`), `-0.0118 R/G` locked-2025 OOS Totals RMSE (`EXP-014`), `0.5846` F5 OOS AUC (`EXP-014`), and exact bye-adjusted 2026 World Series probabilities (`NYY 12.91%`, `LAD 10.62%`).

---

## Template for New ADRs
```markdown
## ADR-NNN: <Short Title>
- **Status:** Proposed | Accepted | Superseded by ADR-XXX
- **Context:** <Problem and constraints>
- **Decision:** <What we chose>
- **Consequences:** <Trade-offs, invariants, and follow-up work>
```
