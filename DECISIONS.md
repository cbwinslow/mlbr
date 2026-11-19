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

---

## Template for New ADRs
```markdown
## ADR-NNN: <Short Title>
- **Status:** Proposed | Accepted | Superseded by ADR-XXX
- **Context:** <Problem and constraints>
- **Decision:** <What we chose>
- **Consequences:** <Trade-offs, invariants, and follow-up work>
```
