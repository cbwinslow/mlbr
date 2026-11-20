# ==============================================================================
# mlbr Student Work-Along Lab 02: 4-Quadrant Platoon Physics, TTO Decay & PVI
# ==============================================================================
# Companion to: site/workbooks/lab02_platoon_physics_workbook.qmd
# Reference Study: site/analyses/02_platoon_physics.qmd (EXP-002-PLATOON-PHYSICS)
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Run line-by-line with Ctrl + Enter and try the 3 MENTOR CHECKPOINTS at the end!
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: Hand-Calculated Batter-Perspective Break Symmetry & PVI Math
# ------------------------------------------------------------------------------
# Statcast pfx_x is measured from the CATCHER'S perspective:
#   +pfx_x = toward 1st base (catcher's right)
#   -pfx_x = toward 3rd base (catcher's left)
#
# To compare RvR, RvL, LvL, and LvR on ONE unified physical axis ("away from
# the batter's eyes"), we flip the sign based on batter handedness (`stand`):
#   hb_away_in = +pfx_x * 12  for RHB (1st-base side is AWAY from a righty)
#   hb_away_in = -pfx_x * 12  for LHB (3rd-base side is AWAY from a lefty)

compute_hb_away <- function(pfx_x_ft, stand) {
  ifelse(stand == "R", pfx_x_ft * 12.0, -pfx_x_ft * 12.0)
}

demo_pitches <- data.frame(
  scenario  = c("RHP Sweeper vs RHB (RvR)", "RHP Sweeper vs LHB (RvL)",
                "LHP Sweeper vs LHB (LvL)", "LHP Sweeper vs RHB (LvR)"),
  p_throws  = c("R", "R", "L", "L"),
  stand     = c("R", "L", "L", "R"),
  pfx_x_ft  = c(1.25, 1.25, -1.25, -1.25)   # 15 inches of glove-side sweep!
)
demo_pitches$hb_away_in <- compute_hb_away(demo_pitches$pfx_x_ft, demo_pitches$stand)

cat("--- Part 1A: Unified Batter-Perspective Break (hb_away_in) ---\n")
print(demo_pitches, row.names = FALSE)
cat("Key insight: Same-handed sweepers (RvR, LvL) BOTH yield +15.0 in (breaking AWAY),\n")
cat("while opposite-handed sweepers (RvL, LvR) BOTH yield -15.0 in (breaking IN toward the barrel).\n\n")

# Step 1.2: Hand-Calculating Empirical Bayes Pitcher Platoon Vulnerability Index (PVI)
# Raw split: delta_xwoba_raw = xwOBA_opp - xwOBA_same  (positive = worse vs opp-hand batters)
# Effective harmonic exposure: n_eff = 2 * n_same * n_opp / (n_same + n_opp)
# Empirical Bayes shrinkage (M = 800 pitches): PVI = (n_eff / (n_eff + 800)) * delta_raw
calc_pvi <- function(xwoba_same, xwoba_opp, n_same, n_opp, M = 800) {
  n_eff <- (2 * n_same * n_opp) / (n_same + n_opp)
  raw_delta <- xwoba_opp - xwoba_same
  shrink_w  <- n_eff / (n_eff + M)
  list(n_eff = n_eff, raw_delta = raw_delta, shrink_w = shrink_w, pvi_xwoba = shrink_w * raw_delta)
}

ex_small <- calc_pvi(0.280, 0.360, n_same = 120, n_opp = 120)
ex_large <- calc_pvi(0.280, 0.360, n_same = 1800, n_opp = 1800)
cat("--- Part 1B: Why Harmonic-Mean Shrinkage (M = 800) Protects Against Noise ---\n")
cat(sprintf("Small sample (120 vs 120): raw = %+.3f | weight = %.1f%% -> PVI = %+.3f\n",
            ex_small$raw_delta, 100 * ex_small$shrink_w, ex_small$pvi_xwoba))
cat(sprintf("Large sample (1800 vs 1800): raw = %+.3f | weight = %.1f%% -> PVI = %+.3f\n\n",
            ex_large$raw_delta, 100 * ex_large$shrink_w, ex_large$pvi_xwoba))


# ------------------------------------------------------------------------------
# Part 2: Querying Full-Population Silver v_statcast_features (14.64M Pitches)
# ------------------------------------------------------------------------------
suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(arrow)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

run_lab02 <- function() {
  lake <- mlbr::default_lake()
  con  <- mlbr::lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")

  cat("Querying all 4 platoon quadrants across modern Regular-Season Statcast (2020-2026)...\n")
  quad_summary <- DBI::dbGetQuery(con, "
    SELECT
      matchup,
      pitch_type,
      COUNT(*) AS pitches,
      ROUND(AVG(hb_away_in), 2) AS avg_hb_away_in,
      ROUND(100.0 * SUM(is_csw) / COUNT(*), 2) AS csw_pct,
      ROUND(100.0 * SUM(is_whiff) / NULLIF(SUM(is_swing), 0), 2) AS whiff_pct,
      ROUND(SUM(xwoba) / NULLIF(COUNT(xwoba), 0), 3) AS xwoba
    FROM v_statcast_features
    WHERE game_type = 'R'
      AND game_year BETWEEN 2020 AND 2026
      AND pitch_type IN ('FF', 'SI', 'SL', 'ST', 'CH', 'CU')
      AND matchup IN ('RvR', 'RvL', 'LvL', 'LvR')
    GROUP BY matchup, pitch_type
    ORDER BY pitch_type, matchup;
  ")
  print(head(quad_summary, 24), row.names = FALSE)

  cat("\nLoading Gold Study 02 Pitcher PVI leaderboard (2024-2026)...\n")
  pvi_path <- file.path(lake, "data/processed/pitcher_pvi_2024_2026.parquet")
  stopifnot(file.exists(pvi_path))
  pvi_df <- as.data.frame(arrow::read_parquet(pvi_path))
  cat(sprintf("Loaded %s qualified pitchers from Gold PVI table.\n", format(nrow(pvi_df), big.mark = ",")))

  cat("\nTop 8 Most Platoon-Vulnerable Pitchers (highest positive PVI_xwoba = struggle vs opp hand):\n")
  print(head(pvi_df[order(-pvi_df$pvi_xwoba), c("pitcher_name", "p_throws", "n_same", "n_opp", "pvi_xwoba", "pvi_csw")], 8), row.names = FALSE)

  cat("\nTop 8 Most Reverse-Split / Platoon-Proof Pitchers (lowest PVI_xwoba):\n")
  print(head(pvi_df[order(pvi_df$pvi_xwoba), c("pitcher_name", "p_throws", "n_same", "n_opp", "pvi_xwoba", "pvi_csw")], 8), row.names = FALSE)

  # ----------------------------------------------------------------------------
  # Part 3: Dark Cyber Lab Diagnostic Plot (4-Asset Preservation)
  # ----------------------------------------------------------------------------
  quad_summary$matchup <- factor(quad_summary$matchup, levels = c("RvR", "LvL", "RvL", "LvR"))
  quad_summary$handed_match <- ifelse(quad_summary$matchup %in% c("RvR", "LvL"), "Same-Handed", "Opposite-Handed")

  make_lab02_plot <- function(df) {
    force(df)
    ggplot(df, aes(x = avg_hb_away_in, y = whiff_pct, color = matchup, shape = handed_match)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#475569", linewidth = 0.6) +
      geom_point(size = 4.2, alpha = 0.92) +
      geom_text(aes(label = pitch_type), nudge_y = 1.6, size = 3.1, fontface = "bold", show.legend = FALSE) +
      scale_color_manual(values = c("RvR" = "#38bdf8", "LvL" = "#10b981", "RvL" = "#f59e0b", "LvR" = "#f43f5e")) +
      labs(
        title = "Lab 02 Diagnostic: Batter-Perspective Break (hb_away_in) vs. Whiff% Across All 4 Quadrants",
        subtitle = "2020-2026 Regular-Season Statcast (v_statcast_features) | Positive hb_away_in breaks AWAY from the batter",
        x = "Mean Horizontal Break Away from Batter (hb_away_in, inches)",
        y = "Swinging Strike Rate on Swings (Whiff%)",
        color = "Quadrant", shape = "Platoon Matchup",
        caption = "Source: mlbr Data Lake (v_statcast_features) | Student Lab 02"
      ) +
      mlbr::theme_cyber(base_size = 11) +
      theme(legend.position = "top")
  }

  p_lab02 <- make_lab02_plot(quad_summary)
  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")
  mlbr::save_plot_assets(p_lab02, "lab02_platoon_break_whiff_matrix", data = quad_summary,
                         width = 11.5, height = 6.8, dir = models_dir)
  stopifnot(file.copy(file.path(models_dir, "lab02_platoon_break_whiff_matrix.png"),
                      file.path(assets_dir, "lab02_platoon_break_whiff_matrix.png"), overwrite = TRUE))
  cat("\nLab 02 completed cleanly! Inspect models/lab02_platoon_break_whiff_matrix.png\n")
}

run_lab02()

# ==============================================================================
# MENTOR CHECKPOINTS (Try these in RStudio Server, then ask me to review!)
# ==============================================================================
# CHECKPOINT 1: Filter `v_statcast_features` to split-finger fastballs ('FS')
#   and cutters ('FC'). Why do splitters have smaller platoon splits than
#   sweepers ('ST')? Verify using `avg_hb_away_in` and `xwoba`.
# CHECKPOINT 2: Load `data/processed/starter_tto_platoon_2020_2026.parquet` and
#   plot `xwoba` vs `times_through_order` (1st, 2nd, 3rd TTO) faceted by
#   `platoon_advantage`. Which decays faster: same-handed or opposite-handed?
# CHECKPOINT 3: Change the shrinkage constant `M` in `calc_pvi()` from 800 to
#   200 and 2000. How does a lower vs. higher M change extreme small-sample
#   relievers' PVI rankings?
