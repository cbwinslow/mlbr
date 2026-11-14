# mlb - Baseball Data Lake & Analysis Project (cbwdellr720)

Personal baseball analysis workspace and comprehensive data lake on `cbwdellr720`.
Data lives on the RAID volume at `/mnt/storage/Rprojects/mlbr`.
Open the project from cbwwin's browser via **http://192.168.10.1:8787** -> Files -> navigate here -> open `mlb.Rproj`.

## Data Sources & Granularity (Raw stays Raw)

Raw files are always kept at the finest available granularity (per pitch, per play, per player-day); aggregation happens in R, never in the files.

| Source | Raw Directory | Granularity | Coverage & Scope | Status |
|--------|---------------|-------------|------------------|--------|
| **Retrosheet (GitHub mirror)** | `data/raw/retrosheet/` | **Per-play, per-game, per-sub, rosters** | 1871–2025 (all regular season, postseason, ASG) | **Complete** (622 files, 23.8M plays) |
| **Statcast (MLB Savant)** | `data/raw/statcast/` | **Per-pitch** | 2008–2025 (adaptive month/week/day ladder) | **Active background ingest** (~5.5M+ pitches) |
| **FanGraphs** | `data/raw/fangraphs/` | Player-season, team-season, constants/parks/projections | 1901–2025 | **Complete** (425 files, 322k rows) |
| **Chadwick Register** | `data/raw/chadwick/` | Universal player ID crosswalk (MLBAM, Retro, B-Ref, FG) | All players historical & active | **Complete** (528k rows) |
| **NCAA Baseball** | `data/raw/ncaa/` | Teams, schedules, play-by-play, conference groups | Modern eras (sportsdataverse repository) | **Complete** (8 tables, 914k rows) |
| **MLB Metadata & Models** | `data/raw/mlbmeta/` | Schedules, drafts, park dimensions, RE24, WE, WPA, OAA, stuff+, framing | 2008–2026 + static references | **Complete** (81 tables, 475k rows) |
| **Lahman Database** | `data/raw/lahman/` | Player-season, historical awards, appearances | 1871–present (31 tables) | **Complete** (706k rows) |
| **ESPN Scoreboards** | `data/raw/espn/` | Season scoreboards (MLB + NCAA) | 2002–2026 | **Complete** (32 files, 27k rows) |
| **MiLB Statcast** | `data/raw/milb/` | **Per-pitch (Minors)** | 2021–2025 | Ready (runs post-Statcast) |
| **Baseball-Reference** | `data/raw/bbref/` | **Per player-day** (batting & pitching) | 2015–2025 (strict 12s rate limit) | Ready (runs post-Statcast) |

## The `baseballLake` R Package (`pkg/baseballLake`)

The project contains a standalone, documented, CRAN-compliant R package (`pkg/baseballLake`) installed in `/mnt/storage/R/library`. It passes `R CMD check` with **0 errors and 0 warnings**.

### Key Functions
- `lake_status(lake)`: Fast footer-only audit of file counts, row counts, and disk usage across every raw source without memory overhead.
- `validate_lake(lake)`: Deep integrity verification: checks for 25k cap truncation flags, interior day gaps, retrosheet per-year play coverage, and table inventory.
- `lake_ingest(lake, sources=...)`: Idempotent orchestrator for the entire lake with built-in rate limiting and safety serialization.
- Individual modular fetchers: `statcast_ingest()`, `retrosheet_github_ingest()`, `fangraphs_ingest()`, `lahman_ingest()`, `chadwick_ingest()`, `ncaa_ingest()`, `mlb_meta_ingest()`, `milb_ingest()`, `espn_ingest()`, `bbref_ingest()`.

### Loading and Usage in R
```r
.libPaths(c("/mnt/storage/R/library", .libPaths()))
library(baseballLake)

# Check lake inventory
lake_status("/mnt/storage/Rprojects/mlbr")

# Run integrity validation
validate_lake("/mnt/storage/Rprojects/mlbr")

# Query Statcast or Retrosheet lazily with Arrow
library(arrow)
library(dplyr)
plays <- open_dataset("/mnt/storage/Rprojects/mlbr/data/raw/retrosheet")
pitches <- open_dataset("/mnt/storage/Rprojects/mlbr/data/raw/statcast")
```

## Running Pipeline & Ingest

- Check running ingests: `ps aux | grep -i '[R]script'`
- Ingest log: `/tmp/baseball-ingest.log` (monitors background downloads)
- Full CLI runner: `Rscript R/ingest_all.R`
