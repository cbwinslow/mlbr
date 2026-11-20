# ==============================================================================
# mlbr Student Work-Along Lab 05: Markov Decomposed Pitching+ & Swing Decisions
# ==============================================================================
# Companion to: site/workbooks/lab05_decomposed_pitching_decisions_workbook.qmd
# Reference Study: site/analyses/05_decomposed_pitching_decisions.qmd (EXP-005)
# Instructions:
#   Open this file in RStudio Server (http://192.168.10.1:8787).
#   Run line-by-line with Ctrl + Enter and complete the 3 MENTOR CHECKPOINTS!
# ==============================================================================

# ------------------------------------------------------------------------------
# Part 1: Hand-Calculated 5-Head Markov Pitch Pricing & Zero-Mean Decision VoE
# ------------------------------------------------------------------------------
# Why decompose a pitch instead of regressing directly on raw `delta_run_exp`?
# Because raw `delta_run_exp` mixes pitch quality with base-out state (a bases-
# loaded walk costs ~0.85 runs vs. ~0.29 runs bases empty) and BABIP noise!
#
# Instead, we predict 5 clean probabilities and price them with context-neutral
# 12-count linear weights (fitted strictly on 2015-2023 Regular-Season pitches):
#   EV_take    = p_cs * rv_strike + (1 - p_cs) * rv_ball
#   EV_contact = p_fl * rv_foul   + (1 - p_fl) * (bip_alpha + beta_xw * xwoba_hat)
#   EV_swing   = p_wh * rv_strike + (1 - p_wh) * EV_contact
#   xRV_bat    = (1 - p_sw) * EV_take + p_sw * EV_swing
#
# And for the BATTER on that exact same pitch, Zero-Mean Decision Value Over Expected:
#   decision_voe = (is_swing - p_sw) * (EV_swing - EV_take)
# Notice: if an average batter swings at rate p_sw, E[decision_voe] == 0.000!

price_pitch_markov <- function(p_sw, p_cs, p_wh, p_fl, xw_hat,
                               rv_ball, rv_strike, rv_foul, bip_alpha, beta_xw = 0.8593) {
  ev_take <- p_cs * rv_strike + (1 - p_cs) * rv_ball
  ev_cont <- p_fl * rv_foul   + (1 - p_fl) * (bip_alpha + beta_xw * xw_hat)
  ev_sw   <- p_wh * rv_strike + (1 - p_wh) * ev_cont
  xrv_bat <- (1 - p_sw) * ev_take + p_sw * ev_sw
  list(
    ev_take = ev_take, ev_cont = ev_cont, ev_sw = ev_sw,
    xrv_bat = xrv_bat, xrv_pitcher = -xrv_bat,
    voe_if_swing = (1 - p_sw) * (ev_sw - ev_take),
    voe_if_take  = (0 - p_sw) * (ev_sw - ev_take)
  )
}

# Example A: 3-2 Full-Count Back-Foot Slider (chase pitch: p_sw=0.45, p_cs=0.05, p_wh=0.52, xw=0.220)
# Using 2015-2023 3-2 count weights: rv_ball=+0.2804, rv_strike=-0.3214, rv_foul=0.0, bip_alpha=-0.275
ex_chase <- price_pitch_markov(p_sw = 0.45, p_cs = 0.05, p_wh = 0.52, p_fl = 0.48, xw_hat = 0.220,
                               rv_ball = 0.2804, rv_strike = -0.3214, rv_foul = 0.0, bip_alpha = -0.275)

# Example B: 1-0 Heart-Zone Fastball ("meatball": p_sw=0.78, p_cs=0.96, p_wh=0.14, xw=0.460)
# Using 2015-2023 1-0 count weights: rv_ball=+0.0669, rv_strike=-0.0476, rv_foul=-0.0474, bip_alpha=-0.245
ex_meat <- price_pitch_markov(p_sw = 0.78, p_cs = 0.96, p_wh = 0.14, p_fl = 0.42, xw_hat = 0.460,
                              rv_ball = 0.0669, rv_strike = -0.0476, rv_foul = -0.0474, bip_alpha = -0.245)

cat("--- Part 1A: 3-2 Chase Slider (Spitting on It vs. Chasing) ---\n")
cat(sprintf("EV_take = %+.4f | EV_swing = %+.4f | Pitcher xRV = %+.4f\n", ex_chase$ev_take, ex_chase$ev_sw, ex_chase$xrv_pitcher))
cat(sprintf("Batter Decision VoE -> TAKE: %+.4f runs (reward!) | SWING: %+.4f runs (huge chase penalty!)\n\n",
            ex_chase$voe_if_take, ex_chase$voe_if_swing))

cat("--- Part 1B: 1-0 Heart-Zone Fastball (Attacking vs. Watching) ---\n")
cat(sprintf("EV_take = %+.4f | EV_swing = %+.4f | Pitcher xRV = %+.4f\n", ex_meat$ev_take, ex_meat$ev_sw, ex_meat$xrv_pitcher))
cat(sprintf("Batter Decision VoE -> SWING: %+.4f runs (attack!) | TAKE: %+.4f runs (passive miss!)\n\n",
            ex_meat$voe_if_swing, ex_meat$voe_if_take))


# ------------------------------------------------------------------------------
# Part 2: Querying Gold EXP-005 Tables & 12-Count Leverage Matrix
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

make_lab05_plot <- function(df) {
  force(df)
  ggplot(df, aes(x = reorder(batter_name, process_rv100), y = value, fill = component)) +
    geom_col(width = 0.72, alpha = 0.92) +
    coord_flip() +
    scale_fill_manual(values = c(
      "Decision VoE (Eye)"    = "#38bdf8",
      "BatControl (Contact%)" = "#10b981",
      "Contact (Damage)"      = "#f59e0b"
    )) +
    labs(
      title = "Lab 05 Diagnostic: Additive Pitch-Level Process Decomposition (Top 15 2025 test_oos Batters)",
      subtitle = "Runs above average per 100 pitches seen = Decision VoE + BatControl (whiff avoidance) + Contact (xwOBA over expected)",
      x = NULL, y = "Runs Above Expected per 100 Pitches Seen", fill = "Skill Component",
      caption = "Source: mlbr Gold batter_process_metrics.parquet (EXP-005, 2025 test_oos) | Student Lab 05"
    ) +
    mlbr::theme_cyber(base_size = 10.5) +
    theme(legend.position = "top")
}

run_lab05 <- function() {
  lake <- mlbr::default_lake()
  con  <- mlbr::lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")

  proc <- file.path(lake, "data/processed")
  crv  <- as.data.frame(arrow::read_parquet(file.path(proc, "decomp_count_rv_table.parquet")))
  pit  <- as.data.frame(arrow::read_parquet(file.path(proc, "pitcher_decomp_pitching_plus.parquet")))
  bat  <- as.data.frame(arrow::read_parquet(file.path(proc, "batter_process_metrics.parquet")))
  bm   <- as.data.frame(arrow::read_parquet(file.path(proc, "decomp_benchmark_ablation.parquet")))

  cat("--- Part 2A: Context-Neutral 12-Count Markov RV Table (2015-2023 Train) ---\n")
  print(crv[, c("balls", "strikes", "n_pitches", "rv_ball", "rv_strike", "rv_foul", "bip_alpha")], row.names = FALSE)

  cat("\n--- Part 2B: Top 10 Locked 2025 test_oos Pitchers by Decomp Pitching+Tunnel (>= 1,000 Pitches) ---\n")
  p25 <- pit[pit$game_year == 2025L & pit$eval_split == "test_oos" & pit$n_pitches >= 1000L, ]
  p25 <- p25[order(-p25$decomp_pitching_tunnel_plus), ]
  print(head(p25[, c("pitcher_name", "team", "n_pitches", "decomp_stuff_plus", "decomp_location_plus",
                     "decomp_pitching_tunnel_plus", "fg_siera")], 10), row.names = FALSE)

  cat("\n--- Part 2C: Top 10 Locked 2025 test_oos Batters by BatterProcess+ (>= 1,500 Pitches) ---\n")
  b25 <- bat[bat$game_year == 2025L & bat$eval_split == "test_oos" & bat$n_pitches >= 1500L, ]
  b25 <- b25[order(-b25$batter_process_plus), ]
  print(head(b25[, c("batter_name", "team", "n_pitches", "decision_plus", "bat_control_plus",
                     "contact_plus", "batter_process_plus", "fg_woba")], 10), row.names = FALSE)

  cat("\n--- Part 2D: Study 03 vs. EXP-005 Decomposed vs. FanGraphs (2024->2025 SIERA w_r) ---\n")
  siera_bm <- bm[bm$domain == "Pitcher" & bm$target == "siera_25", c("model_or_metric", "n_eval", "pearson_r", "w_r")]
  print(siera_bm[order(-abs(siera_bm$w_r)), ], row.names = FALSE)

  # ----------------------------------------------------------------------------
  # Part 3: Dark Cyber Stacked Process Decomposition Chart (4-Asset Preservation)
  # ----------------------------------------------------------------------------
  top15 <- head(b25, 15)
  plot_df <- rbind(
    data.frame(batter_name = top15$batter_name, process_rv100 = top15$process_rv100,
               component = "Decision VoE (Eye)",    value = top15$decision_rv100),
    data.frame(batter_name = top15$batter_name, process_rv100 = top15$process_rv100,
               component = "BatControl (Contact%)", value = top15$bat_ctrl_rv100),
    data.frame(batter_name = top15$batter_name, process_rv100 = top15$process_rv100,
               component = "Contact (Damage)",      value = top15$contact_rv100)
  )
  p_lab05 <- make_lab05_plot(plot_df)
  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")
  mlbr::save_plot_assets(p_lab05, "lab05_batter_process_decomposition", data = plot_df,
                         width = 11.8, height = 7.4, dir = models_dir)
  stopifnot(file.copy(file.path(models_dir, "lab05_batter_process_decomposition.png"),
                      file.path(assets_dir, "lab05_batter_process_decomposition.png"), overwrite = TRUE))
  cat("\nLab 05 completed cleanly! Inspect models/lab05_batter_process_decomposition.png\n")
}

run_lab05()

# ==============================================================================
# MENTOR CHECKPOINTS (Try these in RStudio Server, then ask me to review!)
# ==============================================================================
# CHECKPOINT 1: In Part 1, change the 3-2 chase slider's `p_sw` from 0.45 to 0.05
#   (an obvious pitch in the dirt). Notice how `voe_if_take` shrinks while
#   `voe_if_swing` explodes negatively. Explain why subtracting `p_sw` eliminates
#   pitch-mix bias across hitters.
# CHECKPOINT 2: Compare `decomp_stuff_plus` (2015-2023) vs. `decomp_stuff_he_plus`
#   (Hawk-Eye 2020-2023) in `pitcher_decomp_pitching_plus.parquet` for 2025
#   pitchers with high-spin sweepers or high release extension. Where do they differ?
# CHECKPOINT 3: Find the top 10 "Free-Swinging Sluggers" (`decision_plus < 95` &
#   `contact_plus > 115`) and "Patient Table-Setters" (`decision_plus > 115` &
#   `contact_plus < 95`) in `2025`/`2026` `test_oos`. Who has the higher 2026 wOBA?
