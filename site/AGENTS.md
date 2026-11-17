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
- Build command: `quarto render site/` from project root.

## Verification
- Test compilation: `cd /mnt/storage/Rprojects/mlbr && quarto render site/` must execute with exit code 0.
- Verify output: Check that `/mnt/storage/Rprojects/mlbr/docs/index.html` and analysis HTML files exist and render without markdown/code syntax errors.

## Child DOX Index
None.
