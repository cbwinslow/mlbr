# ==============================================================================
# mlbr Student Work-Along Lab 06: Empirical Multi-Year Regressed Ballpark Factors
# ==============================================================================
# Companion to: site/workbooks/lab06_ballpark_factors_workbook.qmd
# Reference Study: site/analyses/06_ballpark_factors.qmd (EXP-006-BALLPARK-FACTORS)
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Run line-by-line with Ctrl + Enter and try the 3 MENTOR CHECKPOINTS at the end!
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: Hand-Calculated Park Factor Math & Shrinkage Mechanics
# ------------------------------------------------------------------------------
# Step 1.1: Raw Single-Season Scoring Ratio & 30-Team League Symmetry
calc_raw_pf <- function(r_home, g_home, r_road, g_road, n_teams = 30.0) {
  ratio <- (r_home / g_home) / (r_road / g_road)
  (n_teams * ratio) / (n_teams - 1.0 + ratio) * 100.0
}

cat("--- Part 1A: Raw Scoring Ratio & League Symmetry Adjustment ---\n")
# Example: Team with 450 home runs in 81 games, 360 road runs in 81 games
ex_raw <- calc_raw_pf(450, 81, 360, 81)
cat(sprintf("Home: 450 R in 81 G | Road: 360 R in 81 G -> Ratio = %.3f | Raw 1-Yr PF = %.1f\n",
            (450/81)/(360/81), ex_raw))

# Step 1.2: 3-Year Rolling Regressed Empirical Bayes Shrinkage
# Weights: 3 for year T, 2 for T-1, 1 for T-2; Shrinkage denominator M = 40 games
shrink_3yr_pf <- function(raw_vec, g_vec, weights = c(3, 2, 1), M = 40.0) {
  w_pf <- sum(weights * raw_vec * g_vec) / sum(weights * g_vec)
  tot_g <- sum(g_vec)
  pf_reg <- 100.0 + (w_pf - 100.0) * (tot_g / (tot_g + M))
  list(weighted_3yr_pf = w_pf, regressed_pf = pf_reg, tot_games = tot_g)
}

cat("\n--- Part 1B: 3-Year Weighted Regression & Small-Sample Shrinkage ---\n")
# Example: Noisy venue with raw PFs c(127.5, 108.0, 112.0) across 81 home games each
ex_reg <- shrink_3yr_pf(c(127.5, 108.0, 112.0), c(81, 81, 81))
cat(sprintf("Year T raw: %.1f | 3-Yr Weighted: %.1f | Shrunk Regressed PF: %.1f (M = 40 G)\n",
            127.5, ex_reg$weighted_3yr_pf, ex_reg$regressed_pf))

# ------------------------------------------------------------------------------
# Part 2: Querying Gold Ballpark Factors & Altitude Geometry
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(arrow)
  library(data.table)
  library(ggplot2)
})

run_lab06 <- function(lake = NULL) {
  if (is.null(lake)) lake <- mlbr::default_lake()
  gold_path <- file.path(lake, "data/processed/ballpark_factors_2002_2025.parquet")
  if (!file.exists(gold_path)) {
    stop(sprintf(
      "Gold ballpark factors parquet missing: %s\nPlease run 'Rscript models/06_ballpark_factors_empirical.R' first to materialize it.",
      gold_path
    ))
  }

  pf <- as.data.table(arrow::read_parquet(gold_path))

  cat("\n--- Part 2: 2024 Top 5 Run-Inflating & Run-Suppressing Ballparks ---\n")
  pf24 <- pf[season == 2024L]
  setorder(pf24, -pf_runs)
  print(head(pf24[, .(rank = 1:.N, venue_name, park_id, elevation_ft, pf_runs_raw, pf_runs, pf_hr)], 5))

  cat("\nBottom 5 Run-Suppressing Ballparks (2024):\n")
  print(tail(pf24[, .(venue_name, park_id, elevation_ft, pf_runs_raw, pf_runs, pf_hr)], 5))

  cat("\n--- Handedness Home Run Asymmetry Leaders (2024, LHB vs RHB) ---\n")
  pf24[, hr_asym := pf_hr_lhb - pf_hr_rhb]
  setorder(pf24, -hr_asym)
  cat("Top 3 Parks Favoring Left-Handed Power (Short Right Porches):\n")
  print(head(pf24[, .(venue_name, park_id, pf_hr_lhb, pf_hr_rhb, hr_asym)], 3))

  cat("\nTop 3 Parks Favoring Right-Handed Power:\n")
  print(tail(pf24[, .(venue_name, park_id, pf_hr_lhb, pf_hr_rhb, hr_asym)], 3))

  # ----------------------------------------------------------------------------
  # Part 3: Dark Cyber Diagnostic Chart (4-Asset Preservation)
  # ----------------------------------------------------------------------------
  pf24$park_label <- factor(pf24$park_id, levels = pf24$park_id[order(pf24$pf_runs)])
  p_lab06 <- ggplot(pf24, aes(x = pf_runs, y = park_label, color = elevation_ft)) +
    geom_vline(xintercept = 100, color = "#64748b", linetype = "dashed") +
    geom_point(size = 3.5) +
    scale_color_viridis_c(option = "plasma", name = "Elevation (ft)") +
    theme_minimal(base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = "#090d16", color = NA),
      panel.background  = element_rect(fill = "#090d16", color = NA),
      panel.grid.major  = element_line(color = "#1e293b", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      text              = element_text(color = "#f8fafc"),
      axis.text         = element_text(color = "#94a3b8"),
      legend.background = element_rect(fill = "#0f172a", color = "#1e293b")
    ) +
    labs(
      title = "Lab 06: 3-Year Regressed Run Park Factors by MLB Venue (2024)",
      subtitle = "Coors Field (DEN02, 5,183 ft) commands the league ceiling at 122.2; Seattle (SEA03) suppresses runs to 85.9",
      x = "3-Year Regressed Run Park Factor (pf_runs)",
      y = "Venue (PARK_ID)"
    )

  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")
  mlbr::save_plot_assets(p_lab06, "lab06_ballpark_factors_summary", data = pf24,
                         width = 11.5, height = 7.4, dir = models_dir)
  file.copy(file.path(models_dir, "lab06_ballpark_factors_summary.png"),
            file.path(assets_dir, "lab06_ballpark_factors_summary.png"), overwrite = TRUE)

  cat("\nLab 06 completed cleanly! Inspect models/lab06_ballpark_factors_summary.png\n")
}

run_lab06()

# ==============================================================================
# MENTOR CHECKPOINTS (Try these in RStudio Server, then ask me to review!)
# ==============================================================================
# CHECKPOINT 1: In Part 1B, change `M` from 40 to 120 (pure multi-season shrinkage).
#   Why does a smaller `M` respond faster to major stadium renovations (e.g. moving
#   in the fences at Camden Yards or Comerica Park) while a larger `M` guards against flukes?
# CHECKPOINT 2: Load `data/processed/ballpark_factors_2002_2025.parquet` and compute
#   the year-over-year correlation `r(pf_runs[t], pf_runs[t+1])` across continuous parks.
#   Compare that against the raw 1-year ratio correlation `r(pf_runs_raw[t], pf_runs_raw[t+1])`.
# CHECKPOINT 3: Inspect `NYC21` (Yankee Stadium) vs `BOS07` (Fenway Park).
#   Why does Fenway have a high BABIP factor (`pf_babip > 105`) and moderate HR factor,
#   while Yankee Stadium has an extreme LHB home run factor and neutral BABIP?
