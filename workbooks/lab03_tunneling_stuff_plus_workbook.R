# ==============================================================================
# mlbr Student Work-Along Lab 03: Commit-Point Pitch Tunneling & Stuff+ / Loc+
# ==============================================================================
# Companion to: site/workbooks/lab03_tunneling_stuff_plus_workbook.qmd
# Reference Study: site/analyses/03_tunneling_stuff_plus.qmd (EXP-003-TUNNELING-STUFF-PLUS)
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Run line-by-line with Ctrl + Enter and complete the 3 MENTOR CHECKPOINTS!
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: Exact y0 = 50 ft -> 23.8 ft Commit-Point Tunneling Kinematics
# ------------------------------------------------------------------------------
# Batter swing commitment occurs ~175 ms before plate arrival, at y_commit = 23.8 ft
# from home plate (y_plate = 1.417 ft). Statcast velocity/acceleration vectors
# (vx0, vy0, vz0, ax, ay, az) are anchored at y0 = 50.0 ft!
#
# To find the exact (x, z) position of a pitch at y_target (23.8 ft or 1.417 ft):
#   delta_y = y_target - 50.0   (negative, since pitch moves toward y = 0)
#   vy_t    = -sqrt(vy0^2 + 2 * ay * delta_y)
#   t       = (vy_t - vy0) / ay
#   x(t)    = x0 + vx0 * t + 0.5 * ax * t^2
#   z(t)    = z0 + vz0 * t + 0.5 * az * t^2

pos_at_y <- function(x0, z0, vx0, vy0, vz0, ax, ay, az, y_target) {
  dy   <- y_target - 50.0
  vy_t <- -sqrt(vy0^2 + 2 * ay * dy)
  t    <- (vy_t - vy0) / ay
  list(
    t = t,
    x = x0 + vx0 * t + 0.5 * ax * t^2,
    z = z0 + vz0 * t + 0.5 * az * t^2
  )
}

# Compare a 96 mph Four-Seamer followed by an 86 mph Gyro Slider from the SAME release tunnel:
ff_commit <- pos_at_y(x0 = -1.80, z0 = 5.80, vx0 = 4.5, vy0 = -140.0, vz0 = -4.2, ax = -8.5,  ay = 30.0, az = -14.0, y_target = 23.8)
sl_commit <- pos_at_y(x0 = -1.82, z0 = 5.78, vx0 = 3.2, vy0 = -125.5, vz0 = -1.5, ax =  6.0,  ay = 24.0, az = -28.0, y_target = 23.8)

ff_plate  <- pos_at_y(x0 = -1.80, z0 = 5.80, vx0 = 4.5, vy0 = -140.0, vz0 = -4.2, ax = -8.5,  ay = 30.0, az = -14.0, y_target = 1.417)
sl_plate  <- pos_at_y(x0 = -1.82, z0 = 5.78, vx0 = 3.2, vy0 = -125.5, vz0 = -1.5, ax =  6.0,  ay = 24.0, az = -28.0, y_target = 1.417)

tunnel_dist_in <- 12.0 * sqrt((sl_commit$x - ff_commit$x)^2 + (sl_commit$z - ff_commit$z)^2)
plate_sep_in   <- 12.0 * sqrt((sl_plate$x  - ff_plate$x)^2  + (sl_plate$z  - ff_plate$z)^2)
btr            <- plate_sep_in / max(tunnel_dist_in, 0.5)   # Pavlidis/Judge Break-to-Tunnel Ratio

cat("--- Part 1: Hand-Calculated Commit-Point Tunneling (FF -> SL) ---\n")
cat(sprintf("FF flight time: commit = %.1f ms | plate = %.1f ms\n", 1000 * ff_commit$t, 1000 * ff_plate$t))
cat(sprintf("SL flight time: commit = %.1f ms | plate = %.1f ms\n", 1000 * sl_commit$t, 1000 * sl_plate$t))
cat(sprintf("Commit-point separation (y = 23.8 ft): %.2f inches\n", tunnel_dist_in))
cat(sprintf("Home-plate separation   (y = 1.417 ft): %.2f inches\n", plate_sep_in))
cat(sprintf("Break-to-Tunnel Ratio (BTR):           %.2fx late divergence!\n\n", btr))


# ------------------------------------------------------------------------------
# Part 2: Querying Silver v_statcast_tunneling (10.20M Pairs) & Gold Stuff+
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(arrow)
  library(dplyr)
  library(ggplot2)
})

run_lab03 <- function() {
  lake <- mlbr::default_lake()
  con  <- mlbr::lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")

  cat("Querying 2024-2026 Regular-Season FF -> Offspeed/Breaking tunnel bins (v_statcast_tunneling)...\n")
  tunnel_bins <- DBI::dbGetQuery(con, "
    SELECT
      pitch_type,
      FLOOR(LEAST(tunnel_dist_in, 17.99) / 2.0) * 2.0 + 1.0 AS tunnel_bin_in,
      COUNT(*) AS pairs,
      ROUND(100.0 * SUM(is_whiff) / NULLIF(SUM(is_swing), 0), 2) AS whiff_pct,
      ROUND(AVG(break_to_tunnel_ratio), 2) AS avg_btr
    FROM v_statcast_tunneling
    WHERE game_type = 'R'
      AND game_year BETWEEN 2024 AND 2026
      AND prev_pitch_type = 'FF'
      AND pitch_type IN ('SL', 'ST', 'CH', 'CU')
      AND tunnel_dist_in BETWEEN 0 AND 18
      AND is_swing = 1
    GROUP BY pitch_type, tunnel_bin_in
    HAVING COUNT(*) >= 150
    ORDER BY pitch_type, tunnel_bin_in;
  ")
  print(head(tunnel_bins, 16), row.names = FALSE)

  cat("\nLoading Gold pitcher_stuff_plus.parquet (STRICT eval_split == 'test_oos' for 2025)...\n")
  sp_path <- file.path(lake, "data/processed/pitcher_stuff_plus.parquet")
  stopifnot(file.exists(sp_path))
  sp_df <- as.data.frame(arrow::read_parquet(sp_path))
  sp25 <- sp_df[sp_df$game_year == 2025L & sp_df$eval_split == "test_oos" & sp_df$n_pitches >= 1000, ]
  cat(sprintf("Qualified 2025 out-of-sample pitchers (>= 1,000 pitches): %d\n", nrow(sp25)))

  cat("\nTop 10 Out-of-Sample 2025 Pitchers by mlbr Pitching+ (Stuff+ & Location+ decomposed):\n")
  cols <- intersect(c("pitcher_name", "n_pitches", "stuff_plus", "location_plus", "pitching_plus", "pitching_tunnel_plus"), names(sp25))
  print(head(sp25[order(-sp25$pitching_plus), cols], 10), row.names = FALSE)

  # ----------------------------------------------------------------------------
  # Part 3: Dark Cyber Lab Diagnostic Plot (4-Asset Preservation)
  # ----------------------------------------------------------------------------
  make_lab03_plot <- function(df) {
    force(df)
    ggplot(df, aes(x = tunnel_bin_in, y = whiff_pct, color = pitch_type)) +
      geom_line(linewidth = 1.15) +
      geom_point(aes(size = pairs), alpha = 0.9) +
      scale_color_manual(values = c("SL" = "#38bdf8", "ST" = "#10b981", "CH" = "#f59e0b", "CU" = "#f43f5e")) +
      scale_size_continuous(range = c(2.2, 5.2), guide = "none") +
      scale_x_continuous(breaks = seq(1, 17, by = 2)) +
      labs(
        title = "Lab 03 Diagnostic: Tight Fastball Tunnels Amplify Secondary Whiff Rates (2024-2026)",
        subtitle = "Exact y0 = 50 -> 23.8 ft commit-point separation after a 4-Seam Fastball (v_statcast_tunneling, game_type = 'R')",
        x = "Commit-Point Tunnel Separation from Prior Four-Seamer (inches)",
        y = "Swinging Strike Rate on Swings (Whiff%)",
        color = "Follow-Up Pitch",
        caption = "Source: mlbr Silver v_statcast_tunneling (10.20M pairs) | Student Lab 03"
      ) +
      mlbr::theme_cyber(base_size = 11) +
      theme(legend.position = "top")
  }

  p_lab03 <- make_lab03_plot(tunnel_bins)
  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")
  mlbr::save_plot_assets(p_lab03, "lab03_tunnel_whiff_decay", data = tunnel_bins,
                         width = 11.5, height = 6.8, dir = models_dir)
  stopifnot(file.copy(file.path(models_dir, "lab03_tunnel_whiff_decay.png"),
                      file.path(assets_dir, "lab03_tunnel_whiff_decay.png"), overwrite = TRUE))
  cat("\nLab 03 completed cleanly! Inspect models/lab03_tunnel_whiff_decay.png\n")
}

run_lab03()

# ==============================================================================
# MENTOR CHECKPOINTS (Try these in RStudio Server, then ask me to review!)
# ==============================================================================
# CHECKPOINT 1: In Part 1, change the slider's release height `z0` from `5.78`
#   to `5.40` (a 4.5-inch release tell). How much does `tunnel_dist_in` worsen
#   and how much does `break_to_tunnel_ratio` drop?
# CHECKPOINT 2: Modify the DuckDB query in Part 2 to filter to `prev_pitch_type = 'SI'`
#   (Sinker) followed by `'SL'`, `'ST'`, or `'CH'`. Compare Sinker-tunnel whiff
#   curves against Four-Seam tunnels.
# CHECKPOINT 3: Inspect `data/processed/stuff_plus_stabilization_curve.parquet`.
#   At how many pitches does `mlbr Stuff+` exceed r = 0.95 self-correlation vs.
#   `Location+`? Explain why pitch physics stabilizes faster than command.
