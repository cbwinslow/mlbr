# ==============================================================================
# mlbr Student Work-Along Lab 12: Weather/Wind Carry, Home-Plate Umpire Zones
#                                 & Catcher Framing (CSAA)
# ==============================================================================
# Companion to: site/workbooks/lab12_weather_umpire_catcher_framing_workbook.qmd
# Reference Study: site/analyses/12_weather_umpire_catcher_framing.qmd (EXP-012)
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: First-Principles Weather Carry & Framing Run Value Math
# ------------------------------------------------------------------------------
weather_delta_hand <- function(temp_f, wind_out_mph, b_temp = 0.0323, b_wind = 0.0188) {
  b_temp * (temp_f - 70.0) + b_wind * wind_out_mph
}
cat("--- Part 1: Wrigley Field Summer Wind-Out vs. April Wind-In Carry ---\n")
cat(sprintf("90F + 15 mph Wind Out: %+.2f R/G | 45F + 15 mph Wind In: %+.2f R/G (Swing = %.2f R/G!)\n\n",
            weather_delta_hand(90, 15), weather_delta_hand(45, -15),
            weather_delta_hand(90, 15) - weather_delta_hand(45, -15)))

# ------------------------------------------------------------------------------
# Part 2: Zero-Copy DuckDB SQL Audit & Gold EXP-012 Tables
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(arrow)
  library(data.table)
  library(ggplot2)
})

lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

wx_p <- file.path(lake, "data/processed/game_weather_umpire_environment_2015_2025.parquet")
sql_wx <- sprintf("
  SELECT
    CASE WHEN temp_f < 55 THEN '<55F'
         WHEN temp_f < 70 THEN '55-69F'
         WHEN temp_f < 85 THEN '70-84F'
         ELSE '85F+' END AS temp_band,
    COUNT(*) AS n_games,
    ROUND(AVG(actual_total_runs), 2) AS avg_total_runs
  FROM read_parquet('%s')
  GROUP BY temp_band
  ORDER BY avg_total_runs
", wx_p)
cat("--- Part 2A: Zero-Copy DuckDB SQL Scoring by Temperature Band (2015-2025) ---\n")
print(DBI::dbGetQuery(con, sql_wx))

cat_dt <- as.data.table(arrow::read_parquet(file.path(lake, "data/processed/catcher_framing_csaa_2021_2025.parquet")))
cat("\n--- Part 2B: Top 6 MLB Catchers by 2025 Framing Runs ---\n")
print(head(cat_dt[season == 2025L & n_called >= 1000L][order(-framing_runs),
      .(player_name, n_called, csaa = round(csaa, 1), csaa_1k = round(csaa_per_1000, 1),
        framing_runs = round(framing_runs, 1))], 6))

# ------------------------------------------------------------------------------
# Part 3: Diagnostic Chart — Catcher Framing 2024 -> 2025 YoY Persistence
# ------------------------------------------------------------------------------
c24 <- cat_dt[season == 2024L & n_called >= 1000L, .(catcher_mlbam, player_name, cs24 = csaa_per_1000)]
c25 <- cat_dt[season == 2025L & n_called >= 1000L, .(catcher_mlbam, cs25 = csaa_per_1000)]
cy  <- merge(c24, c25, by = "catcher_mlbam")

p_diag <- ggplot(cy, aes(x = cs24, y = cs25)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#64748b", linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#64748b", linewidth = 0.6) +
  geom_point(color = "#10b981", size = 3.0, alpha = 0.88) +
  geom_smooth(method = "lm", se = TRUE, color = "#38bdf8", fill = "#38bdf8", alpha = 0.16) +
  geom_text(data = cy[abs(cs25) > 15 | abs(cs24) > 15], aes(label = player_name),
            vjust = -0.8, size = 2.8, color = "#f8fafc", fontface = "bold", check_overlap = TRUE) +
  labs(
    title = "Lab 12 Diagnostic: Catcher Framing Persistence (2024 -> 2025 CSAA / 1,000 Called)",
    subtitle = sprintf("n = %d qualified catchers | Out-of-sample YoY r = %.3f | Elite framers repeat skill year after year",
                       nrow(cy), stats::cor(cy$cs24, cy$cs25)),
    x = "2024 CSAA per 1,000 Called Pitches",
    y = "2025 CSAA per 1,000 Called Pitches",
    caption = "Source: mlbr Lab 12 (EXP-012 Catcher Framing Engine)"
  ) +
  mlbr::theme_cyber(base_size = 11)

out_png <- file.path(lake, "site/assets/lab12_catcher_framing_yoy.png")
ggsave(out_png, p_diag, width = 10.5, height = 5.6, dpi = 300, bg = "#090d16")
cat(sprintf("\nSaved Lab 12 diagnostic chart to %s\n", out_png))

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Complete in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (Wrigley Wind Swing):
#   Filter `game_weather_umpire_environment_2015_2025.parquet` to `park_id == "CHI11"` (Wrigley Field).
#   Compare mean `actual_total_runs` when `wind_out_mph >= 10` vs. `wind_out_mph <= -10`.
#
# CHECKPOINT 2 (Why Zero-Sum Season Centering Is Mandatory for Framing):
#   Explain why comparing 2025 called pitches to an uncentered 2021-2023 strike-zone grid
#   creates a negative bias across all 2025 catchers as MLB umpire zones tightened.
#
# CHECKPOINT 3 (Umpire Variance vs. Shrinkage):
#   In `umpire_zone_factors_2015_2025.parquet`, compare `sd(raw_rpg_dev)` vs. `sd(ump_rpg_eb)`
#   for umpires with >= 60 games. How much noise does Empirical Bayes M=80 remove?
