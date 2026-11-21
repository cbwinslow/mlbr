# ==============================================================================
# mlbr Student Work-Along Lab 08: David Smyth BaseRuns, Dynamic Pythagenpat &
#                                 10,000-Season 2026 MLB Standings Simulation
# ==============================================================================
# Companion to: site/workbooks/lab08_team_baseruns_pythagenpat_workbook.qmd
# Reference Study: site/analyses/08_team_baseruns_pythagenpat.qmd (EXP-008)
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Run line-by-line with Ctrl + Enter and complete the 3 MENTOR CHECKPOINTS!
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: Hand-Calculated BaseRuns, Dynamic Pythagenpat & Log5 Game Math
# ------------------------------------------------------------------------------
calc_baseruns_hand <- function(ab, h, d2, d3, hr, bb, ibb, hbp, sb, cs, gdp) {
  tb <- h + d2 + 2 * d3 + 3 * hr
  A  <- h + bb + hbp - hr - 0.5 * ibb
  B  <- (1.4 * tb - 0.6 * h - 3 * hr + 0.1 * (bb + hbp - ibb) + 0.9 * (sb - cs - gdp)) * 1.1
  C  <- ab - h + cs + gdp
  D  <- hr
  list(A = A, B = B, C = C, D = D, baseruns = (A * B) / (B + C) + D)
}

calc_pythagenpat_hand <- function(rs, ra, g = 162) {
  gamma <- ((rs + ra) / g)^0.287
  wpct  <- (rs^gamma) / (rs^gamma + ra^gamma)
  list(gamma = gamma, wpct = wpct, wins = wpct * g)
}

cat("--- Part 1: Hand-Calculated BaseRuns & Dynamic Pythagenpat ---\n")
# Example elite offense: 1,450 H (300 2B, 25 3B, 230 HR), 580 BB (20 IBB), 65 HBP, 110 SB, 25 CS, 4,320 outs
ex_bsr <- calc_baseruns_hand(5520, 1450, 300, 25, 230, 580, 20, 65, 110, 25, 110)
cat(sprintf("BaseRuns Components: A=%.1f, B=%.1f, C=%.0f, D=%.0f -> Est. Runs = %.1f\n",
            ex_bsr$A, ex_bsr$B, ex_bsr$C, ex_bsr$D, ex_bsr$baseruns))
ex_py <- calc_pythagenpat_hand(rs = 810, ra = 680, g = 162)
cat(sprintf("Pythagenpat (810 RS, 680 RA): gamma = %.3f -> Win%% = %.3f -> Exp. Wins = %.1f\n\n",
            ex_py$gamma, ex_py$wpct, ex_py$wins))

# ------------------------------------------------------------------------------
# Part 2: Inspecting Gold EXP-008 Team BaseRuns & 2026 Playoff Odds
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(arrow)
  library(data.table)
  library(ggplot2)
})

make_lab08_plot <- function(df) {
  force(df)
  ggplot(df, aes(x = reorder(team_bbref, playoff_prob), y = playoff_prob * 100, fill = league)) +
    geom_col(width = 0.72, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.1f%%", 100 * playoff_prob)), hjust = -0.15, color = "#f8fafc", size = 2.8, fontface = "bold") +
    coord_flip(ylim = c(0, 102)) +
    facet_wrap(~division, scales = "free_y") +
    scale_fill_manual(values = c("AL" = "#38bdf8", "NL" = "#10b981"), name = "League") +
    labs(
      title = "Lab 08 Diagnostic: 2026 MLB Playoff Probabilities by Division (10,000 MC Seasons)",
      subtitle = "Exact 6-team-per-league MLB playoff format (3 Division Winners + 3 Wild Cards per league; sums to 12.0 teams)",
      x = NULL, y = "Total Playoff Qualification Probability (%)",
      caption = "Source: mlbr Gold team_standings_projections_2026.parquet (EXP-008) | Student Lab 08"
    ) +
    mlbr::theme_cyber(base_size = 10)
}

run_lab08 <- function(lake = NULL) {
  if (is.null(lake)) lake <- mlbr::default_lake()
  proc <- file.path(lake, "data/processed")
  req <- c("team_baseruns_pythagenpat_2002_2025.parquet",
           "team_pythagenpat_backtest_metrics.parquet",
           "team_standings_projections_2026.parquet")
  missing <- req[!file.exists(file.path(proc, req))]
  if (length(missing) > 0L) {
    stop(sprintf("Missing EXP-008 Gold files (%s). Run 'Rscript models/08_team_baseruns_pythagenpat.R --full' first.",
                 paste(missing, collapse = ", ")))
  }

  hist_dt <- as.data.table(arrow::read_parquet(file.path(proc, "team_baseruns_pythagenpat_2002_2025.parquet")))
  oos_dt  <- as.data.table(arrow::read_parquet(file.path(proc, "team_pythagenpat_backtest_metrics.parquet")))
  s26_dt  <- as.data.table(arrow::read_parquet(file.path(proc, "team_standings_projections_2026.parquet")))

  cat("--- Part 2A: Year T -> Year T+1 Out-of-Time Team Win% Forecasting (2002-2025) ---\n")
  print(oos_dt)

  cat("\n--- Part 2B: Top 6 & Bottom 6 Projected 2026 MLB Clubs (10,000 MC Seasons) ---\n")
  setorder(s26_dt, -mean_wins)
  print(head(s26_dt[, .(franchise_name, league, division, proj_rs = round(proj_rs), proj_ra = round(proj_ra),
                        mean_wins, p10_wins, p90_wins, div_prob, playoff_prob)], 6))
  cat("\nBottom 6 Projected Clubs:\n")
  print(tail(s26_dt[, .(franchise_name, league, division, proj_rs = round(proj_rs), proj_ra = round(proj_ra),
                        mean_wins, p10_wins, p90_wins, div_prob, playoff_prob)], 6))

  # Part 3: Dark Cyber Diagnostic Chart (4-Asset Preservation)
  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")
  dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)

  p_lab08 <- make_lab08_plot(s26_dt)
  mlbr::save_plot_assets(p_lab08, "lab08_division_playoff_odds", data = s26_dt,
                         width = 12.0, height = 7.2, dir = models_dir)
  file.copy(file.path(models_dir, "lab08_division_playoff_odds.png"),
            file.path(assets_dir, "lab08_division_playoff_odds.png"), overwrite = TRUE)
  cat("\nLab 08 completed cleanly! Inspect models/lab08_division_playoff_odds.png\n")
  invisible(s26_dt)
}

# ------------------------------------------------------------------------------
# MENTOR CHECKPOINTS (Try these exercises in RStudio Server!)
# ------------------------------------------------------------------------------
# CHECKPOINT 1 (BaseRuns vs. Sequencing Luck):
#   Query `hist_dt[season == 2024][order(-luck_wins)]`. Which 2024 MLB teams
#   outperformed their BaseRuns Pythagenpat expectation by the most wins, and
#   how did those clubs perform in 2025?
#
# CHECKPOINT 2 (Dynamic Pythagenpat Exponent Across Eras):
#   Compare mean `pyth_gamma` in a low-scoring era (e.g., 2014) vs. a high-scoring
#   era (e.g., 2019). Why does a +100 run differential produce MORE wins in a
#   low-run environment than in a high-run environment?
#
# CHECKPOINT 3 (Division Difficulty & Schedule Strength):
#   Compare the 2026 NL East (Braves, Phillies, Mets) vs. NL Central or AL West
#   in `s26_dt`. Why do the 87.5-win Phillies have ~31% division odds while the
#   85.4-win Mariners have ~39% division odds?
# ------------------------------------------------------------------------------

if (!interactive()) {
  run_lab08()
}
