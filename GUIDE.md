# mlbr: Interactive Learning & Development Guide

Welcome! This guide is your hands-on home base for working along with the `mlbr` data lake, DuckDB zero-copy views, Q-CRISP studies, and Student Work-Along Labs in RStudio Server.

---

## 1. Quick Access Links

| Service | Address | Notes |
|---|---|---|
| **RStudio Server** | [http://192.168.10.1:8787](http://192.168.10.1:8787) | Primary interactive coding environment. Open `mlbr.Rproj`. |
| **Local Documentation & Gallery** | [http://192.168.10.1:8088](http://192.168.10.1:8088) | Live local preview served directly from `/mnt/storage/Rprojects/mlbr/site/docs/`. |
| **GitHub Pages Research Site** | [https://cbwinslow.github.io/mlbr/](https://cbwinslow.github.io/mlbr/) | Public auto-deployed Quarto site from `site/docs/`. |
| **Canonical Project Root** | `/mnt/storage/Rprojects/mlbr` | Single canonical workspace on `cbwdellr720` (40 cores, 125 GB RAM). |

---

## 2. Two-Track Workflow: Studies vs. Work-Along Labs

Every numbered Q-CRISP study ships with **1:1 paired deliverables** (enforced mechanically by `R/generate_agent_manifest.R`):

| Level | Q-CRISP Study (`site/analyses/`) | Work-Along Lab (`workbooks/` → `site/workbooks/`) | Key Concepts & Gold Tables |
|:---:|:---|:---|:---|
| **1** | `01_pitch_quality_eda.qmd` (`EXP-001`) | `workbooks/lab01_aerodynamics_workbook.R` | Trajectory physics, VAA/HAA, IVB, `v_statcast_features` (`14.64M` pitches) |
| **1** | `02_platoon_physics.qmd` (`EXP-002`) | `workbooks/lab02_platoon_physics_workbook.R` | `RvR`/`RvL`/`LvL`/`LvR`, `hb_away_in`, TTO decay, Pitcher `PVI` |
| **1** | `03_tunneling_stuff_plus.qmd` (`EXP-003`) | `workbooks/lab03_tunneling_stuff_plus_workbook.R` | `y0 = 50 -> 23.8 ft` tunneling (`v_statcast_tunneling`, `10.20M` pairs), `Stuff+`/`Location+`/`Pitching+` |
| **2** | `04_player_projections.qmd` (`EXP-004`) | `workbooks/lab04_player_projections_workbook.R` | `IP_true`, Tango Marcel, Delta-Method aging, `Pulled Air%`, `Zone-Chase (Z-O%)`, 80% CIs (`2016–2027`) |

---

## 3. Quickstart: Zero-Copy DuckDB Over the Full Lake

Open RStudio Server (`http://192.168.10.1:8787`), open `mlbr.Rproj`, and run:

```r
library(mlbr)
library(DBI)
library(dplyr)

con <- lake_duckdb() # Mounts all 16 Bronze + Silver zero-copy views in ~15 ms

# Example 1: Query 2025 Regular-Season Four-Seam VAA & Whiff% from Silver (14.64M rows)
tbl(con, "v_statcast_features") %>%
  filter(game_type == "R", game_year == 2025, pitch_type == "FF", is_swing == 1) %>%
  summarise(swings = n(), mean_velo = mean(release_speed, na.rm = TRUE),
            mean_vaa = mean(vaa, na.rm = TRUE), whiff_pct = mean(is_whiff, na.rm = TRUE)) %>%
  collect()

# Example 2: Inspect tightest Fastball -> Sweeper tunnels from Silver (10.20M pairs)
tbl(con, "v_statcast_tunneling") %>%
  filter(game_type == "R", game_year == 2025, prev_pitch_type == "FF", pitch_type == "ST") %>%
  summarise(pairs = n(), mean_tunnel_in = mean(tunnel_dist_in, na.rm = TRUE),
            mean_btr = mean(break_to_tunnel_ratio, na.rm = TRUE)) %>%
  collect()

DBI::dbDisconnect(con, shutdown = TRUE)
```

---

## 4. Verification & Rebuild Cheatsheet

```bash
# 28-point data lake invariant suite
Rscript R/proof_lake.R

# Universal player/team/game crosswalk checks (5/5)
Rscript -e "mlbr::proof_crosswalks()"

# Semantic invariants + 1:1 Study <-> Work-Along Lab parity check (6/6)
Rscript R/generate_agent_manifest.R

# Rebuild Quarto site into site/docs/ (served live at :8088 and GitHub Pages)
quarto render site/
```
