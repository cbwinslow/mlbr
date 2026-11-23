#!/usr/bin/env Rscript
# ==============================================================================
# Lab 24: Batted-Ball Spray-Angle, Pull-Air% & Ballpark Wall-Geometry xHR
#         (EXP-024 Companion Workbook)
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(arrow)
  library(dplyr)
  library(ggplot2)
  library(mlbr)
})

cat("=== Lab 24: Spray-Angle, Pull-Air% & Park-Wall Geometry xHR ===\n")
lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB'")

# ------------------------------------------------------------------------------
# CHECKPOINT 1: Verify Exact Statcast Spray-Angle & Piecewise-Linear Wall Math
# ------------------------------------------------------------------------------
spray_deg <- function(hc_x, hc_y) atan2(hc_x - 125.42, 198.27 - hc_y) * (180.0 / pi)
ang_cf <- spray_deg(125.42, 50.0)
ang_rf <- spray_deg(200.0, 123.69)
cat(sprintf("Dead CF spray angle: %.2f deg | RF line spray angle: %+.2f deg\n", ang_cf, ang_rf))
stopifnot(abs(ang_cf) < 0.01, ang_rf > 40.0)
cat("CHECKPOINT 1 PASS: Statcast spray-angle geometry verified.\n")

# ------------------------------------------------------------------------------
# CHECKPOINT 2: Verify Locked 2025 OOS Monotonic Improvement (M3 < M2 < M1 < M0)
# ------------------------------------------------------------------------------
p_ab <- file.path(lake, "data/processed/xhr_wall_geometry_oos_ablation_2025.parquet")
ab <- DBI::dbGetQuery(con, sprintf("
  SELECT model, n_features, n_oos_bip,
         ROUND(oos_logloss, 5) AS oos_logloss,
         ROUND(oos_brier, 5) AS oos_brier,
         ROUND(oos_auc, 5) AS oos_auc
  FROM read_parquet('%s')
", p_ab))
print(ab)
stopifnot(
  ab$oos_logloss[4] < ab$oos_logloss[3],
  ab$oos_logloss[3] < ab$oos_logloss[2],
  ab$oos_logloss[2] < ab$oos_logloss[1],
  ab$oos_auc[4] > 0.994
)
cat("CHECKPOINT 2 PASS: Strict monotonic 2025 OOS Log-Loss reduction M3 < M2 < M1 < M0 verified.\n")

# ------------------------------------------------------------------------------
# CHECKPOINT 3: Dark Cyber Park-Wall HR Boost Diagnostic (Top 5 vs Bottom 5)
# ------------------------------------------------------------------------------
p_pk <- file.path(lake, "data/processed/park_wall_xhr_effects_2025.parquet")
pk <- arrow::read_parquet(p_pk)
pk_ext <- bind_rows(head(pk, 5), tail(pk, 5))

p_diag <- ggplot(pk_ext, aes(x = reorder(venue_name, wall_hr_delta), y = wall_hr_delta, fill = wall_hr_delta > 0)) +
  geom_hline(yintercept = 0, color = "#f8fafc", linetype = "dashed") +
  geom_col(width = 0.65, alpha = 0.92, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%+.1f HR", wall_hr_delta)),
            hjust = ifelse(pk_ext$wall_hr_delta >= 0, -0.15, 1.15), color = "#f8fafc", size = 3.4, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#f43f5e")) +
  labs(
    title = "Lab 24 Diagnostic: Top 5 & Bottom 5 2025 MLB Parks by Wall-Geometry HR Delta",
    subtitle = "M3 (Exact Wall Distance + Elevation) minus M1 (Park-Neutral Spray xHR) across 2025 BIP",
    x = NULL, y = "Net 2025 Expected Home Runs Added / Removed by Stadium Wall & Altitude",
    caption = "mlbr Lab 24 | Great American Ball Park (+51.0 HR) vs Kauffman Stadium (-40.9 HR)"
  ) +
  mlbr::theme_cyber()

out_png <- file.path(lake, "models/lab24_park_wall_hr_delta_diagnostic.png")
ggsave(out_png, p_diag, width = 10, height = 5.5, dpi = 300)
cat(sprintf("Saved Dark Cyber Lab 24 diagnostic PNG to: %s\n", out_png))
cat("CHECKPOINT 3 PASS: Lab 24 completed cleanly!\n")
