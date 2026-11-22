# site — Multi-Page Quarto Documentation & Analysis Gallery (AGENTS.md)

## Purpose
`site` contains the Quarto website source code, configuration, visual assets, and machine learning analysis gallery for the `mlbr` platform. It builds into `/mnt/storage/Rprojects/mlbr/docs` for publication on `cloudcurio.cc` and GitHub Pages.

## Ownership
- Owned by `cbwinslow`.
- Maintained as an interactive, publication-ready data science site inspired by scikit-learn's `auto_examples` gallery.

## Local Contracts
- **Build Engine:** Quarto (`/usr/local/bin/quarto` or `/usr/lib/rstudio-server/bin/quarto/bin/quarto`).
- **Target Output Directory:** `../docs` (relative to `site/`).
- **Domain & CNAME:** `cloudcurio.cc`.
- **Navigation & Layout:**
  - Left-docked, collapsible sidebar with structured category grouping.
  - Global full-text search integrated into sidebar/overlay.
  - Automatic sitemap generation enabled (`sitemap: true`).
  - Analysis gallery listing with cards, categories, tags, and reading/execution times.
- **Reproducibility:** All code chunks use `library(mlbr)` and DuckDB zero-copy views. R plots use high-resolution svg/png (`fig-format: retina`, `fig-width: 8`, `fig-height: 5`).

## Work Guidance
- Site layout:
  - `_quarto.yml`: Root project configuration and sidebar navigation.
  - `index.qmd`: Executive dashboard, interactive lake status table, quickstart code.
  - `setup.qmd`: Installation and environment configuration (RStudio Server + local R).
  - `architecture.qmd`: Parquet partition layout, DuckDB zero-copy mounting, rate-limit safety rules.
  - `gallery.qmd`: Scikit-learn style card gallery of all studies.
  - `analyses/*.qmd`: Machine learning studies:
    - `pitch_clustering.qmd`: Unsupervised pitch clustering with K-Means & GMM.
    - `pitch_classification.qmd`: Multi-class pitch type classifier with tidymodels Random Forest (`ranger`).
    - `xwoba_surface.qmd`: 2D Non-linear GAM surface on Exit Velocity & Launch Angle.
    - `run_expectancy.qmd`: 24-state Run Expectancy (RE24) matrix from 23.8M Retrosheet plays.
    - `01_pitch_quality_eda.qmd`: Study 01 — Aerodynamics, VAA/HAA, and whiff lift.
    - `02_platoon_physics.qmd`: Study 02 — 4-Quadrant Platoon Physics, break symmetry, and PVI.
    - `03_tunneling_stuff_plus.qmd`: Study 03 — Exact commit-point pitch tunneling (`v_statcast_tunneling`), Stuff+/Location+/Pitching+, and stabilization curves.
    - `04_player_projections.qmd`: Study 04 — Tier 2 Chronological True-Talent Player Projections (Marcel, Zero-Sum Delta-Method Aging, Stage-2 Residual Physics & 80% Uncertainty Bands).
    - `05_decomposed_pitching_decisions.qmd`: Study 05 — Context-Neutral 5-Head Markov Decomposed Pitching+ & Pitch-Level Batter Process Engine (`Decision+`, `BatControl+`, `Contact+`).
    - `06_ballpark_factors.qmd`: Study 06 — Empirical 3-Year Regressed Ballpark Factors & Handedness Asymmetry (`EXP-006`).
    - `07_pecota_zips_ensemble.qmd`: Study 07 — PECOTA k-NN Comps, ZiPS 5-Yr Archetype Aging, Defense EDA & KKT Meta-Ensemble (`EXP-007`).
    - `08_team_baseruns_pythagenpat.qmd`: Study 08 — David Smyth Complex BaseRuns, Dynamic Pythagenpat & 2026 10k-Season Schedule Simulator (`EXP-008`).
    - `09_game_monte_carlo_simulator.qmd`: Study 09 — Level 4 24 Base-Out Markov Game Simulator & Empirical Win Expectancy (`EXP-009`).
    - `10_market_edge_prediction_valuation.qmd`: Study 10 — Level 5 Market Edge, CLV, Fractional Kelly & Prediction Market Valuation (`EXP-010`).
    - `11_bat_tracking_arm_slot_biomechanics.qmd`: Study 11 — Hawk-Eye Bat-Tracking (1.04M Swings) & Pitcher Arm-Slot Biomechanics (`EXP-011`).
    - `12_weather_umpire_catcher_framing.qmd`: Study 12 — Weather/Wind Carry, Umpire Zones & Catcher Framing CSAA (`EXP-012`).
    - `13_pitcher_stamina_bullpen_fatigue.qmd`: Study 13 — Pitcher Stamina, Velocity Decay & Bullpen Fatigue Engine (`EXP-013`).
    - `14_unified_pro_game_f5_totals_bracket.qmd`: Study 14 — Unified Pro Multi-Market Engine (ML, F5, Totals) & Exact 12-Team Playoff Bracket (`EXP-014`).
    - `15_daily_lineups_platoon_k_props.qmd`: Study 15 — Daily 9-Man Starting Lineups, Platoon Parks & Pitcher Strikeout Prop Engine (`EXP-015`).
    - `16_dynamic_statespace_negbin_k_props.qmd`: Study 16 — Dynamic In-Season State-Space Updates & Negative Binomial Overdispersed K-Props (`EXP-016`).
  - `workbooks/*.qmd`: Companion Student Work-Along Labs (1:1 parity with every numbered Q-CRISP study: `lab01`–`lab16`).
- Build command: `quarto render site/` from project root.

## Verification
- Test compilation: `cd /mnt/storage/Rprojects/mlbr && quarto render site/` must execute with exit code 0.
- Verify output: Check that `/mnt/storage/Rprojects/mlbr/site/docs/index.html` and analysis HTML files exist and render without markdown/code syntax errors.

## Child DOX Index
None.
