# mlbr: Interactive Learning & Development Guide

Welcome! This guide is your home base for learning how the `mlbr` data lake, R package, DuckDB zero-copy queries, and machine learning pipelines work from first principles.

---

## 1. Quick Access Links

| Service | Address | Notes |
|---|---|---|
| **RStudio Server** | [http://192.168.10.1:8787](http://192.168.10.1:8787) | Your primary interactive coding environment. Open `mlbr.Rproj`. |
| **Documentation & Analysis Site** | [http://192.168.10.1:8088](http://192.168.10.1:8088) | Live local preview of your Quarto site with all charts & tables. |
| **Active Project Path** | `/mnt/storage/Rprojects/mlb` | Root workspace directory on `cbwdellr720`. |
| **Data Lake Storage** | `/mnt/storage/Rprojects/mlb/data/raw/` | Partitioned Apache Parquet datasets (34M+ rows). |

---

## 2. The Mental Model: How It All Fits Together

```
                  ┌───────────────────────────────┐
                  │ 1. Raw Parquet Files on Disk  │
                  │    (data/raw/<source>/*.parquet)
                  └───────────────┬───────────────┘
                                  │
                                  ▼
                  ┌───────────────────────────────┐
                  │ 2. DuckDB Zero-Copy Engine    │
                  │    - Direct pointer to files  │
                  │    - No bulk import into RAM  │
                  │    - SIMD vectorized queries  │
                  └───────────────┬───────────────┘
                                  │
                                  ▼
                  ┌───────────────────────────────┐
                  │ 3. mlbr R Package             │
                  │    - LakeManager (R6)         │
                  │    - lake_duckdb(), proof()   │
                  │    - dplyr / dbplyr integration│
                  └───────────────┬───────────────┘
                                  │
                                  ▼
                  ┌───────────────────────────────┐
                  │ 4. Quarto Analytics Gallery   │
                  │    - site/*.qmd files         │
                  │    - Compiles to site/docs/   │
                  │    - Published at :8088       │
                  └───────────────────────────────┘
```

---

## 3. Learning Milestones (Step-by-Step)

### Milestone 1: Your First Zero-Copy Query
*Goal: Understand how DuckDB queries 34M rows in milliseconds without running out of RAM.*
1. Open RStudio Server at `http://192.168.10.1:8787`.
2. Open `mlbr.Rproj`.
3. In the R console, connect to the lake:
   ```r
   library(mlbr)
   library(dplyr)
   
   con <- lake_duckdb()
   ```
4. Query the fastest pitches in baseball history using `dplyr`:
   ```r
   tbl(con, "statcast") %>%
     filter(release_speed > 103) %>%
     select(player_name, pitch_type, release_speed, game_date) %>%
     arrange(desc(release_speed)) %>%
     head(10) %>%
     collect()
   ```

### Milestone 2: Exploring Historical Retrosheet Plays
*Goal: Query 150+ years of baseball history (1871–2025).*
1. Query Barry Bonds' home runs in 2001 or Shohei Ohtani's 50/50 season in 2024 directly from play-by-play.
2. Inspect the base-out state codes: `BASE1_RUN_ID`, `BASE2_RUN_ID`, `BASE3_RUN_ID`.

### Milestone 3: Building a New Analysis Page
*Goal: Create your own custom Quarto study in `site/analyses/`.*
1. Create a new file: `site/analyses/my_study.qmd`.
2. Write a question (e.g. "Do left-handed batters perform better against right-handed sinkers?").
3. Write the DuckDB query + ggplot2 chart.
4. Run `cd /mnt/storage/Rprojects/mlb/site && quarto render my_study.qmd`.
5. View it live in your browser at `http://192.168.10.1:8088/analyses/my_study.html`!

### Milestone 4: Expanding the `mlbr` Package
*Goal: Add your own custom R function to `pkg/mlbr/R/`.*
1. Add a new helper (e.g. `calculate_woba(df)` or `pitch_arsenal(player_id)`).
2. Add roxygen2 tags (`#' @export`, `#' @param`).
3. Reinstall and run tests (`proof_lake()`).

---

## 4. Helpful Cheatsheet Commands

- **Check Data Lake Ingest Progress:**
  ```bash
  tail -n 15 /tmp/baseball-ingest.log
  ```
- **Run the 28-Point Invariant Proof Suite:**
  ```r
  mlbr::proof_lake()
  ```
- **Recompile the Documentation Site:**
  ```bash
  cd /mnt/storage/Rprojects/mlb/site && quarto render
  ```
