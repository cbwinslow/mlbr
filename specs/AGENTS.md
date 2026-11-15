# Model Specifications & SDD Architecture (specs/AGENTS.md)

This directory houses the formal engineering specifications for the 5-tier predictive modeling hierarchy in `mlbr`.

## Specification Guidelines
Every model spec in this directory must explicitly document:
1. **Mathematical Objective & Theoretical Grounding**: Literature citations (Tango, Szymborski, Silver, Smyth, Bill James, academic papers).
2. **Input Features & Data Lake Query**: Explicit DuckDB SQL extraction query against zero-copy views.
3. **Target Variable & Loss Function**: Exact definition of outcome, event codes, and evaluation metric (e.g. Log-Loss, Brier Score, ROC AUC, RMSE).
4. **Validation Strategy**: Strict chronological / temporal splitting to prevent look-ahead bias (e.g., train 2018-2023, validate 2024, test 2025-2026).
5. **Baseline Comparison**: Simple empirical baseline (e.g., league average, linear logistic regression, naive Marcel).
6. **Implementation Blueprint**: Target R scripts in `models/` and package functions in `pkg/mlbr/R/`.
EOF
