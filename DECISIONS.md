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

## ADR-011: Exact Daily 9-Man Starting Lineup Extraction & Multi-Factor Poisson Pitcher Strikeout (`SO`) Prop Pricing
- **Status:** Accepted (2026-11-21)
- **Context:** Team-level offensive ratings obscure daily 9-man lineup strikeout propensity, handedness platoon stacking, and venue-specific HR asymmetry (`pf_hr_lhb` vs `pf_hr_rhb`), which are primary drivers of sharp MLB player prop markets.
- **Decision:** Extract exact 1-through-9 starting batting orders directly from `v_retrosheet_plays` (`89,118` lineup slots across `2024–2025`), apply strictly prior-season (`season - 1`) Empirical Bayes batter/pitcher priors ($M = 150$ PA/BF), and price starting pitcher strikeouts via a strictly chronological Poisson GLM (`2024` train $\rightarrow$ locked `2025` OOS `n = 4,859` starts) incorporating opposing 9-man lineup K%, arm-slot ride (`EXP-011`), park `pf_so` (`EXP-006`), pre-test umpire K-factors (`2015–2023`), and prior-season catcher framing (`EXP-012`).
- **Consequences:** Delivers `Over 5.5 K` locked-2025 OOS ROC AUC `0.6638` (`+0.0228` over starter-prior-only `0.6409`) and SO RMSE `2.3285` (`-0.0257` vs starter-only), while demonstrating honestly that daily lineup composition concentrates its strongest incremental alpha in player props rather than raw full-game moneylines.

## ADR-012: Pitch-Level Silver Biomechanics Materialization & Negative Binomial Dynamic State-Space K-Prop Pricing
- **Status:** Accepted (2026-11-21)
- **Context:** Starting pitcher strikeout counts exhibit `28%` extra-Poisson overdispersion (`Var/Mean = 1.28`), while static `season - 1` priors miss in-season pitcher breakouts and velocity/command slumps. Simultaneously, pitch-level `ivb_above_slot`, Alan Nathan `MaxEV`/`is_blast`, and zero-sum `csaa_pitch` needed permanent Silver materialization per ADR-002.
- **Decision:** Materialize `data/features/statcast_biomechanics/` (`4.55M` pitches `2020–2026`, Hive-partitioned ZSTD Parquet) via `R/materialize_biomechanics_framing.R`, and upgrade Strikeout Props to a `MASS::glm.nb` Negative Binomial model incorporating strictly pre-game (`date < t`) trailing 30-day starter `CSW%` ($M = 150$) and `K%` ($M = 60$).
- **Consequences:** Improves locked-2025 OOS (`n = 4,859` starts) SO RMSE `2.3285 -> 2.3068`, exact Count NLL `2.2457 -> 2.2327`, `Over 5.5 K` Brier `0.21295 -> 0.20948` / AUC `0.6638 -> 0.6779`, and `Over 7.5 K` tail Brier `0.11499 -> 0.11333`, while cleanly documenting that 30-day club offensive streaks (`xwOBA` + `bat_speed`) add variance rather than signal to Full-Game ML.

## ADR-013: Native Silver View `v_statcast_biomechanics` (17th Lake View) & Batter HR / Total Bases Prop Pricing
- **Status:** Accepted (2026-11-21)
- **Context:** Promoting `data/features/statcast_biomechanics/` (`4.55M` pitches) to a first-class zero-copy DuckDB view eliminates repeated `read_parquet()` path boilerplate across studies and unlocks batter-level power prop pricing (`HR >= 1` and `TB >= 2`).
- **Decision:** Export `mlbr::lake_register_biomechanics()` and auto-mount `v_statcast_biomechanics` inside `mlbr::lake_duckdb()` (17th view), and price batter `HR >= 1` and `TB >= 2` props via strictly chronological logistic models (`2024` train `n = 43,722` $\rightarrow$ locked `2025` OOS `n = 43,740`) combining `season - 1` Silver `bat_speed`/Blast Rate, batting-order slot, platoon, handedness `pf_hr_hand`, weather/wind carry, and opposing SP `HR%`.
- **Consequences:** Lifts locked-2025 OOS `HR >= 1` ROC AUC `0.5960 -> 0.6155` (`-0.00311` Log-Loss) and `TB >= 2` ROC AUC `0.5418 -> 0.5695` (`-0.00422` Log-Loss), expanding `mlbr` to six validated out-of-sample MLB markets across 17 studies and 17 companion labs.

## ADR-014: Same-Game Parlay (SGP) Within-Game Correlation Matrix & Gaussian/Clayton Copula Pricing
- **Status:** Accepted (2026-11-22)
- **Context:** Pricing multi-leg MLB Same-Game Parlays (SGPs) via naive independence ($\prod p_j$) introduces severe mispricing (`-18.0 pp` on `Home ML + F5 Home`, `-5.04 pp` on `Batter TB 2+ + Game Over 8.5`, `-3.81 pp` on 3-leg `Home ML + Home TB 2+ + Over 8.5`) because all legs share a single realization of game physics and scoring.
- **Decision:** Implement pure-R vectorized 2D/3D Gaussian Copulas (exact Plackett 1954 16-point Gauss-Legendre quadrature, verified `< 1e-6` vs `mvtnorm::pmvnorm`) alongside Clayton & 90°-Rotated Clayton Copulas in `models/18_sgp_correlation_copula_pricing.R`, fit strictly via `2024` 4-cell bivariate Bernoulli MLE and evaluated OOS on locked `2025` across 8 canonical SGP scripts uniting all 6 validated `mlbr` markets (`EXP-014`, `EXP-016`, `EXP-017`).
- **Consequences:** Cuts locked-2025 OOS Log-Loss on `Home ML + F5 Home` (`0.756 -> 0.680`, `1.56x` mult), `Batter HR + Team ML` (`0.268 -> 0.265`, `1.29x`), `Batter TB 2+ + Over 8.5` (`0.521 -> 0.512`, `1.23x`), and 3-leg `Home ML + Home TB 2+ + Over 8.5` (`0.378 -> 0.369`, `-80.2%` pricing bias), while proving that Gaussian and Clayton deliver near-identical 2-leg accuracy and Gaussian generalizes cleanly to $d \ge 3$ legs.

## ADR-015: Joint Multinomial Softmax Total Bases Ladder (`0–4+ TB`) & Continuous Exponential Half-Life Decay (`H* = 30d`)
- **Status:** Accepted (2026-11-22)
- **Context:** Separate binary logits for retail Total Bases ladders (`TB >= 1, 2, 3, 4`) risk probability crossings, while proportional odds (`MASS::polr`) constrains power covariates (`bat_speed`, `blast_rate`, `pf_hr_hand`) to act identically on singles (`TB = 1`) and home runs (`TB >= 4`). Meanwhile, `EXP-016` used a discontinuous flat 30-day boxcar window for in-season pitcher `CSW%` and `K%`.
- **Decision:** Adopt a joint 5-class Multinomial Softmax model (`nnet::multinom`) over `{0, 1, 2, 3, 4+ TB}` guaranteeing zero monotonicity violations across all 4 ladder rungs, and upgrade pitcher K-Prop state-space features to a strictly causal (`d < t`, 90-day lookback) exponential decay kernel with `2024`-selected half-life `H* = 30d` (`w = 2^(-dt/30)`).
- **Consequences:** Improves locked-2025 OOS (`n = 43,740`) 5-class TB Log-Loss `1.4518 -> 1.4429 (POLR) -> 1.4388 (Multinomial)`, RPS `0.1891 -> 0.1865`, and `TB >= 4` AUC `0.5884 -> 0.6003` with `0` monotonicity violations; simultaneously lifts locked-2025 OOS (`n = 4,859`) K-Prop Count NLL `2.2327 -> 2.2303`, RMSE `2.3068 -> 2.3002`, and `Over 5.5 K` AUC `0.6779 -> 0.6827` (`theta: 46.89 -> 51.52`; `H* = 30d` sits on the upper boundary of the `{7, 14, 21, 30}` grid, so testing `H = 45–60d` is logged as a follow-up).

## ADR-016: Causal In-Season Starter Workload (`BF/GS`) State-Space & Multivariate Copula-Kelly Portfolio Sizing
- **Status:** Accepted (2026-11-22)
- **Context:** `EXP-019` updated in-season pitcher `CSW%`/`K%` dynamically (`H* = 30d` on `{7..30}` boundary) while holding starter workload (`eb_bf_gs`) frozen at `season - 1`, leaving substantial pitch-count/IL-ramp variance unexplained. Simultaneously, multi-market same-game portfolios (`ML + K-Props + Totals`) sized independently ignore within-game covariance quantified in `EXP-018`.
- **Decision:** Expand the strictly causal (`d < t`, same-season) exponential half-life sweep to `{14..180d}`, add causal exponentially-decayed prior-start workload `bf_gs_h60` (`M_GS = 4.0` starts shrunk toward `eb_bf_gs`, zero reliever contamination) to `MASS::glm.nb`, and solve exact box-constrained Multivariate Copula-Kelly QPs (`max f'mu - (1/(2c)) f'Sigma f`, `f in [0, 0.05]^m`, `35%` daily slate cap) using `EXP-018`'s `2024` Gaussian Copula covariance matrix.
- **Consequences:** Lifts NegBin dispersion `theta` from `51.78 -> 84.32` (cutting unexplained strikeout overdispersion by ~40%), lowers locked-2025 OOS (`n = 4,859`) Count NLL `2.2301 -> 2.2260`, SO RMSE `2.2999 -> 2.2899`, and `Over 5.5 K` AUC `0.6827 -> 0.6849`; Multivariate Copula-Kelly haircuts positively correlated same-game tickets by `-7.98%`, boosts negatively correlated hedges by `+11.64%`, and lowers daily portfolio volatility (`7.48% -> 7.41%`, with single-path Max DD `32.61% -> 33.95%` from hedge boosting).

## ADR-017: Multi-Pitch Sequencing, Count-Conditioned Shannon Entropy & Orthogonal `Seq+` (Level 1C)
- **Status:** Accepted (2026-11-23)
- **Context:** Raw pitch-group Markov transition rates confound sequencing with count selection and single-pitch stuff (`Breaking -> Breaking` hits `35.2%` raw whiff% mainly in two-strike counts). Isolating true multi-pitch deception requires residualizing against full single-pitch aerodynamics, location, and count.
- **Decision:** Compute full-population (`5.47M` pairs, `2015–2025`) Markov transitions, 2D eye-level shifts (`delta_plate_z × delta_velo_mph`), and count-conditioned pitch-mix Shannon Entropy $H_{\text{count}}(p, s)$, and train strictly chronological `xgboost hist` whiff models (`2020–2023` train $\to$ `2024` val $\to$ locked `2025` OOS `n = 279,640` paired swings) to define orthogonal pitcher `Seq+ = 100 + 10 * z(mean(p_M2 - p_M1))` locked to `2024` scale.
- **Consequences:** Proves multi-pitch sequencing strictly beats full single-pitch physics+location+count on locked `2025` OOS Whiff Log-Loss (`0.46171 -> 0.46091`), Brier (`0.14813 -> 0.14782`), and ROC AUC (`0.76945 -> 0.77087`), while establishing Count Entropy (`YoY r = 0.8428`) and `Seq+` (`YoY r = 0.5936`) as repeatable Level 1C pitcher traits (`Tarik Skubal #1 SP at 130.5 Seq+`).

---

## Template for New ADRs
```markdown
## ADR-NNN: <Short Title>
- **Status:** Proposed | Accepted | Superseded by ADR-XXX
- **Context:** <Problem and constraints>
- **Decision:** <What we chose>
- **Consequences:** <Trade-offs, invariants, and follow-up work>
```
