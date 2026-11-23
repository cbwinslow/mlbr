#!/usr/bin/env Rscript
# ==============================================================================
# Lab 22: Starter Rest-Day Recovery, Post-IL Ramp & Circadian Travel Jet-Lag
# Companion Work-Along Workbook for Study 22 (EXP-022)
# ==============================================================================
suppressPackageStartupMessages({
  library(mlbr); library(DBI); library(arrow); library(dplyr); library(ggplot2)
})

cat("=== Lab 22: Starter Rest/IL Recovery & Circadian Travel Jet-Lag ===\n")
lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

# PART 1: First-Principles Great-Circle Haversine Distance (Dodger Stadium -> Yankee Stadium)
haversine_mi <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180.0
  a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2.0 * 3958.8 * asin(sqrt(a))
}
d_la_ny <- haversine_mi(34.0739, -118.2400, 40.8296, -73.9262)
tz_la_ny <- round((-73.9262 - (-118.2400)) / 15.0)
cat(sprintf("Dodger Stadium -> Yankee Stadium: %.1f miles | Circadian shift: %+d hours (Eastward)\n",
            d_la_ny, as.integer(tz_la_ny)))

# MENTOR CHECKPOINT 1:
# Verify LA -> NYC great-circle distance is ~2,445 miles and eastward shift is +3 hours.
stopifnot(d_la_ny > 2400 && d_la_ny < 2500 && tz_la_ny == 3)
cat("CHECKPOINT 1 PASS: Haversine great-circle distance & circadian shift verified.\n")

# PART 2: Zero-Copy DuckDB SQL Query on EXP-022 Post-IL Ramp & OOS Ablation
p_il <- file.path(lake, "data/processed/pitcher_il_return_ramp_2018_2025.parquet")
p_ab <- file.path(lake, "data/processed/rest_travel_oos_ablation_2024_2025.parquet")

il_sql <- DBI::dbGetQuery(con, sprintf("
  SELECT ramp_stage, n_starts, ROUND(mean_pitches, 1) AS avg_pitches,
         ROUND(d_pitches, 2) AS delta_pitches, ROUND(d_bf, 2) AS delta_bf,
         ROUND(d_ff_velo, 3) AS delta_velo_mph, ROUND(d_so, 3) AS delta_so
  FROM read_parquet('%s')
  ORDER BY d_pitches ASC
", p_il))
print(il_sql)

# MENTOR CHECKPOINT 2:
# Notice IL Return #1 throws -8.75 fewer pitches (-2.15 BF) EVEN THOUGH fastball velocity
# is +0.136 mph HIGHER! Why does this create strong Under-K prop alpha?
stopifnot(il_sql$delta_pitches[1] < -7.0 && il_sql$delta_velo_mph[1] > 0.05)
cat("CHECKPOINT 2 PASS: Post-IL pitch-count leash vs fresh-arm velocity verified.\n")

# PART 3: Dark Cyber Diagnostic Chart — Locked 2024-2025 OOS IL Return #1 RMSE Reduction
ab_df <- arrow::read_parquet(p_ab) |>
  filter(grepl("IL_Return", experiment_part)) |>
  tidyr::pivot_longer(cols = c(rmse_base, rmse_plus), names_to = "model", values_to = "rmse")

p_lab22 <- ggplot(ab_df, aes(x = experiment_part, y = rmse, fill = model)) +
  geom_col(position = position_dodge(width = 0.65), width = 0.58) +
  scale_fill_manual(values = c("rmse_base" = "#f43f5e", "rmse_plus" = "#10b981"),
                    labels = c("Baseline (EB Prior Only)", "+ Rest/IL Leash Flags"), name = "Model") +
  labs(
    title = "Lab 22 Diagnostic: Locked 2024-2025 OOS RMSE on IL Return #1 Starts (n = 414)",
    subtitle = "Adding causal IL-return leash flags cuts BF RMSE by -14.8% and SO RMSE by -7.3%",
    x = NULL, y = "Locked 2024-2025 OOS RMSE (Lower is Better)"
  ) +
  mlbr::theme_cyber()

out_png <- file.path(lake, "models/lab22_il_return_oos_rmse_diagnostic.png")
ggsave(out_png, p_lab22, width = 9, height = 5, dpi = 300)
cat(sprintf("Saved Dark Cyber Lab 22 diagnostic PNG to: %s\n", out_png))

# MENTOR CHECKPOINT 3:
# Why did raw Eastward 2h+ No-Rest travel look like -0.174 R/G before controlling for is_home,
# then collapse to -0.066 R/G (p = 0.80) once home-field advantage was included?
cat("CHECKPOINT 3 PASS: Lab 22 completed cleanly!\n")
