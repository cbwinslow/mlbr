# mlbr - Production Sabermetrics Data Lake & Machine Learning Platform

Production baseball intelligence workspace, high-performance Apache Parquet data lake, and machine learning research platform hosted on `cbwdellr720` (40 CPU cores, 125 GB RAM).

* **Canonical Lake Root:** `/mnt/storage/Rprojects/mlbr`
* **RStudio Server (Direct NIC):** [http://192.168.10.1:8787](http://192.168.10.1:8787) (Auto-logout disabled, 365-day session persistence)
* **Quarto Documentation & ML Gallery:** [http://192.168.10.1:8088](http://192.168.10.1:8088)

---

## 1. Data Lake Inventory & Granularity

Every dataset is stored at the finest raw atomic granularity (per pitch, per play, per player-day) in Snappy-compressed Apache Parquet format. Raw columns are 100% preserved with zero dropping or lossy downcasting.

| Domain | Parquet Path | Granularity | Seasons | Total Records | Compressed Size |
|:---|:---|:---|:---:|:---:|:---:|
| **Statcast (MLB Savant)** | `data/raw/statcast/` | Per-pitch (119 metrics) | 2008–2026 | **14,643,543 pitches** | ~2.07 GB |
| **Retrosheet Plays** | `data/raw/retrosheet/` | Per-play (160 fields) | 1900–2025 | **23,818,512 plays** | ~775 MB |
| **FanGraphs Boards** | `data/raw/fangraphs/` | Bat, Pitch, Field, Team | 1901–2025 | **322,391 rows** | ~131 MB |
| **Chadwick Register** | `data/raw/chadwick/` | Universal Player Crosswalk | All eras | **527,990 players** | ~37 MB |
| **NCAA Play-by-Play** | `data/raw/ncaa/` | Per-play College Baseball | Modern eras | **914,428 plays** | ~26 MB |
| **MLB Metadata & Views** | `data/raw/mlbmeta/` | Schedules, Standings, Draft | 2008–2026 | **475,276 rows** | ~17 MB |
| **Lahman Database** | `data/raw/lahman/` | 31 Relational Tables | 1871–2025 | **706,563 rows** | ~11 MB |
| **ESPN Summaries** | `data/raw/espn/` | Daily Scoreboards | 2002–2026 | **27,383 rows** | ~3 MB |
| **Total Lake Footprint** | | | **125+ Years** | **~41.5 Million Rows** | **~3.07 GB** |

---

## 2. Zero-Copy Architecture & DuckDB Virtual Views

Rather than importing 41+ million records into memory or maintaining complex database servers, `mlbr` uses memory-mapped Apache Parquet with DuckDB vectorization.

```r
library(mlbr)
con <- lake_duckdb() # Mounts 14 zero-copy views in ~15 milliseconds

# Query millions of pitches in sub-second time
tbl(con, "v_statcast") %>%
  filter(pitch_type == "FF", release_speed >= 100) %>%
  count(pitcher, sort = TRUE)
```

### Available DuckDB Views:
* `v_statcast` — Full pitch-by-pitch tracking feed (2008–2026)
* `v_retrosheet_plays` — Complete play-by-play events (1900–2025)
* `v_retrosheet_games` — Game-level box scores and environmental metadata
* `v_retrosheet_subs` & `v_retrosheet_rosters` — Lineup substitutions and roster files
* `v_fangraphs_bat` & `v_fangraphs_pitch` — Historical sabermetric leaderboards (WAR, wOBA, FIP)
* `v_chadwick_register` — Universal player ID mapping (`key_mlbam`, `key_retro`, `key_bbref`, `key_fangraphs`)
* `v_lahman_people`, `v_lahman_batting`, `v_lahman_pitching` — Biographical and historical tables
* `v_mlbmeta_schedule` — Official MLB game schedule (2008–2026)

---

## 3. Universal Crosswalk System

Link players, games, and franchises across disparate systems without name mismatch issues:

```r
library(mlbr)

# Player ID Lookup
crosswalk_player("Shohei Ohtani")
# Returns: key_mlbam: 660271 | key_retro: ohtas001 | key_bbref: ohtansh01

# Bidirectional Game ID Translation
crosswalk_game("BOS202304150", to = "game_pk")
# Returns: 718576 (Statcast game_pk)

crosswalk_game(718576, to = "GAME_ID")
# Returns: "BOS202304150" (Retrosheet GAME_ID)

# Franchise Translation
team_crosswalk() # 33 franchises mapped across Retrosheet, Statcast, and BBRef
```

---

## 4. Verification & Validation Suites

The lake features automated verification routines ensuring complete data fidelity:

```bash
# 1. Automated 28-point empirical invariant test suite
Rscript R/proof_lake.R

# 2. Universal crosswalk invariant test suite
Rscript -e "mlbr::proof_crosswalks()"

# 3. Read-only deep lake audit & truncation check
Rscript R/validate_lake.R

# 4. AI Agent Manifest & Semantic Invariant runner
Rscript R/generate_agent_manifest.R
```

---

## 5. Machine-Readable AI Agent Context

Automated manifests are kept continuously synchronized in `data/manifest/`:
* `data/manifest/lake_manifest.json` — Complete schema catalog, column categories, and null rates.
* `data/manifest/agent_context.json` — Compact payload for LLM system prompt injection.
* `data/manifest/LAKE_AGENT_PROMPT.md` — Copy-paste system instructions for Claude, ChatGPT, and Gemini.

---

## 6. Machine Learning & Modeling

Modeling scripts are organized in `models/`:
* `models/01_whiff_model.R` — Expected Whiff Probability (`xWhiff`) and Stuff+ modeling using Logistic Regression, Random Forest (`ranger`), and XGBoost.
* Interactive execution in RStudio Server: [http://192.168.10.1:8787](http://192.168.10.1:8787).
