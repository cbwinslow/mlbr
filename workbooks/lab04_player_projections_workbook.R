# ==============================================================================
# mlbr Student Work-Along Lab 04: Marcel Projections, Delta Aging & Physics Priors
# ==============================================================================
# Companion to: site/workbooks/lab04_player_projections_workbook.qmd
# Reference Study: site/analyses/04_player_projections.qmd (EXP-004-PLAYER-PROJECTIONS)
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Run line-by-line with Ctrl + Enter and try the 3 MENTOR CHECKPOINTS at the end!
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: Hand-Calculated Projection Building Blocks (Outs, Marcel, Uncertainty)
# ------------------------------------------------------------------------------
# Step 1.1: Exact Baseball Outs Notation Conversion
# FanGraphs & MLB box scores encode 1/3 and 2/3 innings as .1 and .2!
# Treating 186.2 IP as 186.20 decimal innings introduces systematic error in HR/9 and FIP.
ip_to_true <- function(ip) {
  floor(ip) + round((ip - floor(ip)) * 10) / 3.0
}

cat("--- Part 1A: Exact Baseball Outs Conversion ---\n")
sample_ip <- c(186.0, 186.1, 186.2, 62.2)
print(data.frame(box_ip = sample_ip, ip_true = round(ip_to_true(sample_ip), 4)), row.names = FALSE)

# Step 1.2: Hand-Calculated Era-Adjusted Marcel + Tango Direct Age Factor
# Example: 27-year-old hitter with 3 seasons of wOBA and PA (5/4/3 recency weights, M = 2200)
marcel_woba_example <- function(woba_vec, pa_vec, lg_woba_vec, lg_target, age_proj,
                                weights = c(5, 4, 3), M = 2200) {
  # Weighted excess above seasonal league average (sum-before-divide!)
  num   <- sum(weights * pa_vec * (woba_vec - lg_woba_vec))
  denom <- sum(weights * pa_vec) + M
  base_reg <- lg_target + num / denom
  # Tom Tango (2004) direct age multiplier (Step 5)
  tango_m <- if (age_proj <= 29) 1 + 0.006 * (29 - age_proj) else 1 / (1 + 0.003 * (age_proj - 29))
  list(reg_woba = base_reg, reliability = sum(weights * pa_vec) / denom,
       tango_mult = tango_m, aged_woba = base_reg * tango_m)
}

ex_vet <- marcel_woba_example(c(0.380, 0.365, 0.355), c(600, 580, 550),
                              c(0.312, 0.310, 0.315), lg_target = 0.312, age_proj = 27)
ex_rok <- marcel_woba_example(c(0.380, 0.000, 0.000), c(180, 0, 0),
                              c(0.312, 0.310, 0.315), lg_target = 0.312, age_proj = 23)

cat("\n--- Part 1B: Marcel Shrinkage on Established Star vs. 180-PA Rookie ---\n")
cat(sprintf("Established 27yo (1,730 PA): reliability = %.1f%% | regressed = %.3f | Tango-aged = %.3f\n",
            100 * ex_vet$reliability, ex_vet$reg_woba, ex_vet$aged_woba))
cat(sprintf("Hot 23yo Rookie  (180 PA):   reliability = %.1f%% | regressed = %.3f | Tango-aged = %.3f\n",
            100 * ex_rok$reliability, ex_rok$reg_woba, ex_rok$aged_woba))

# Step 1.3: Exposure-Calibrated 80% Projection Uncertainty Band
# Formula: CI_80 = proj +/- 1.2816 * sqrt(sigma_skill^2 + sigma_noise^2 / exposure)
ci80_width <- function(exposure, sigma_skill = 0.024, sigma_noise = 0.35) {
  se <- sqrt(sigma_skill^2 + (sigma_noise^2) / exposure)
  1.2816 * se
}
cat("\n--- Part 1C: How 80% wOBA Uncertainty Band Narrows With Playing Time ---\n")
for (pa in c(150, 350, 600)) {
  cat(sprintf("  Proj PA = %3d -> 80%% half-width = +/- %.3f wOBA\n", pa, ci80_width(pa)))
}
cat("\n")


# ------------------------------------------------------------------------------
# Part 2: Inspecting Gold Tier 2 Projections, Aging Curves & Backtest Metrics
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

make_lab04_plot <- function(df) {
  force(df)
  ggplot(df, aes(y = player_label, x = proj_woba)) +
    geom_segment(aes(x = proj_woba_lo80, xend = proj_woba_hi80, yend = player_label),
                 color = "#38bdf8", linewidth = 2.2, alpha = 0.55) +
    geom_point(aes(x = obs_t1_woba), color = "#f43f5e", size = 2.8, shape = 17, alpha = 0.9) +
    geom_point(color = "#10b981", size = 3.6) +
    scale_x_continuous(labels = function(x) sprintf(".%03d", round(x * 1000))) +
    labs(
      title = "Lab 04 Diagnostic: Top 20 Projected 2026 Batters — 80% True-Talent Bands vs. 2025 Observed wOBA",
      subtitle = "Green dot = mlbr M4 Projected 2026 wOBA | Cyan bar = Calibrated 80% CI | Red triangle = Observed 2025 wOBA",
      x = "wOBA (Observed 2025 vs. Projected 2026 True Talent)",
      y = NULL,
      caption = "Source: mlbr Gold batter_projections_true_talent.parquet (EXP-004) | Student Lab 04"
    ) +
    mlbr::theme_cyber(base_size = 10.5)
}

run_lab04 <- function() {
  lake <- mlbr::default_lake()
  con  <- mlbr::lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")

  # Query 2025 pulled air-ball rate directly from Silver v_statcast_features!
  cat("Querying 2025 Pulled Air-Ball% leaders directly from v_statcast...\n")
  pull_air_25 <- DBI::dbGetQuery(con, "
    SELECT
      CONCAT(c.name_first, ' ', c.name_last) AS batter_name,
      COUNT(*) AS bip,
      ROUND(100.0 * AVG(CASE
        WHEN CAST(launch_angle AS DOUBLE) >= 10 AND (
          (stand = 'R' AND DEGREES(ATAN2(CAST(hc_x AS DOUBLE) - 125.42, 198.27 - CAST(hc_y AS DOUBLE))) < -15.0) OR
          (stand = 'L' AND DEGREES(ATAN2(CAST(hc_x AS DOUBLE) - 125.42, 198.27 - CAST(hc_y AS DOUBLE))) >  15.0)
        ) THEN 1.0 ELSE 0.0 END), 1) AS pulled_air_pct,
      ROUND(AVG(TRY_CAST(estimated_woba_using_speedangle AS DOUBLE)), 3) AS bip_xwoba
    FROM v_statcast f
    JOIN v_chadwick_register c ON f.batter = c.key_mlbam
    WHERE f.game_type = 'R'
      AND f.game_year = 2025
      AND TRY_CAST(f.hc_x AS DOUBLE) IS NOT NULL AND TRY_CAST(f.hc_y AS DOUBLE) IS NOT NULL
      AND TRY_CAST(f.launch_angle AS DOUBLE) IS NOT NULL
      AND f.stand IN ('R', 'L')
    GROUP BY c.name_first, c.name_last
    HAVING COUNT(*) >= 250
    ORDER BY pulled_air_pct DESC, bip DESC
    LIMIT 10;
  ")
  print(pull_air_25, row.names = FALSE)

  # Load Gold EXP-004 Tables
  proc <- file.path(lake, "data/processed")
  pg <- as.data.frame(arrow::read_parquet(file.path(proc, "pitcher_projections_true_talent.parquet")))
  bg <- as.data.frame(arrow::read_parquet(file.path(proc, "batter_projections_true_talent.parquet")))
  bm <- as.data.frame(arrow::read_parquet(file.path(proc, "projections_backtest_metrics.parquet")))

  cat("\nLocked 2024 -> 2025 Out-of-Sample Pitcher FIP & Batter wOBA Ablation Ladder:\n")
  ladder <- bm[bm$track == "Track3_LockedOOS_2025" &
                 ((bm$domain == "Pitcher" & bm$metric == "fip") |
                  (bm$domain == "Batter"  & bm$metric == "woba")),
               c("domain", "metric", "model", "n_eval", "w_rmse", "w_r", "cal_slope", "p_beats_b2")]
  print(ladder, row.names = FALSE)

  cat("\nTop 8 Active 2026 Starting Pitchers with 80% Uncertainty Intervals:\n")
  sp26 <- pg[pg$projection_year == 2026L & pg$proj_role == "SP" & pg$tbf_t1 >= 50 & pg$proj_tbf >= 550, ]
  sp26 <- sp26[order(sp26$proj_fip), ]
  print(head(sp26[, c("player_name", "team_prev", "age_proj", "obs_t1_era", "proj_fip", "proj_fip_lo80", "proj_fip_hi80", "stuff_plus_t1")], 8), row.names = FALSE)

  # ----------------------------------------------------------------------------
  # Part 3: Dark Cyber Uncertainty Band Diagnostic Chart (4-Asset Preservation)
  # ----------------------------------------------------------------------------
  top20_bat <- bg[bg$projection_year == 2026L & bg$pa_t1 >= 50 & bg$proj_pa >= 480, ]
  top20_bat <- head(top20_bat[order(-top20_bat$proj_woba), ], 20)
  top20_bat$player_label <- factor(top20_bat$player_name, levels = rev(top20_bat$player_name))


  p_lab04 <- make_lab04_plot(top20_bat)
  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")
  mlbr::save_plot_assets(p_lab04, "lab04_top20_batter_uncertainty_bands", data = top20_bat,
                         width = 11.5, height = 7.4, dir = models_dir)
  stopifnot(file.copy(file.path(models_dir, "lab04_top20_batter_uncertainty_bands.png"),
                      file.path(assets_dir, "lab04_top20_batter_uncertainty_bands.png"), overwrite = TRUE))
  cat("\nLab 04 completed cleanly! Inspect models/lab04_top20_batter_uncertainty_bands.png\n")
}

run_lab04()

# ==============================================================================
# MENTOR CHECKPOINTS (Try these in RStudio Server, then ask me to review!)
# ==============================================================================
# CHECKPOINT 1: In Part 1B, change `M` from 2200 to 250 (our pre-2016 calibrated
#   reliability denominator for Batter K%). Why does a 180-PA rookie's K% get
#   nearly 80% trust while their wOBA gets only ~29% trust?
# CHECKPOINT 2: Load `data/processed/projections_aging_curves.parquet` and plot
#   `cum_delta_smooth` vs `naive_centered` for `group == "SP"` and `metric == "FBv (mph)"`.
#   Quantify the exact survivorship-bias gap in mph at Age 38.
# CHECKPOINT 3: Filter `pitcher_projections_true_talent.parquet` to
#   `projection_year == 2027L & played_2026 == 0L` (injured/missed-2026 pitchers).
#   Inspect how their 2027 projections decay by age without double-counting priors.
