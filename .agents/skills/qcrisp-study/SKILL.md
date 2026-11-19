---
name: qcrisp-study
description: >-
  Use when designing, running, auditing, or publishing a new mlbr quantitative study or lab workbook
  following the Q-CRISP lifecycle, 7-Section Study Template, 4-Asset Preservation, and Quarto publication.
---

# mlbr Q-CRISP Study Playbook

## 1. Spec First
- Create or update `specs/<NN>_<study_name>.md` defining: hypothesis, mathematical formulas, loss functions, strict chronological train/test splits (zero temporal leakage), and acceptance criteria.
- Review against `DECISIONS.md` and `experiments/registry.json` to inherit verified formulas.

## 2. Full-Population Analysis & Feature Promotion
- Query via `mlbr::lake_duckdb()` (`v_statcast_features`) across the full lake population (14.64M+ pitches).
- Never subsample final metrics; restrict sampling only to interactive exploration or browser canvas safety.
- Promote validated new features into `R/materialize_features.R` and `sql/01_statcast_features.sql`.

## 3. Dark Cyber 4-Asset Visualization Standard
Every figure saved to `models/` (and `site/assets/` as needed) MUST preserve:
1. `.png` at 300 DPI â€” Dark Cyber theme (`#090d16` canvas, `#111827` surface, `#1e293b` gridlines; accents `#38bdf8`, `#10b981`, `#f59e0b`, `#f43f5e`; zero white backgrounds).
2. `.rds` â€” Native R `ggplot`/`plotly` object.
3. `.parquet` (or `.csv`) â€” Exact aggregated plot data for auditability.
4. `.html` â€” Interactive standalone `plotly`/`htmlwidgets` widget where applicable.

## 4. Registry, Manifest & Site Closeout
1. Log all findings (including negative/rejected results) via `mlbr::lake_log_experiment()` into `experiments/registry.json`.
2. Sync manifests: `Rscript R/generate_agent_manifest.R`.
3. Render site: `quarto render site/` and confirm exit code 0.
4. Run `mlbr-study-reviewer` and `mlbr-verifier` before committing.
