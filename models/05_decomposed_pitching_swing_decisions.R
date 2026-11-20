#!/usr/bin/env Rscript
# ==============================================================================
# mlbr EXP-005: Context-Neutral Decomposed Pitching+ & Pitch-Level Swing Decisions
# ==============================================================================
# Authoritative Spec: specs/05_decomposed_pitching_swing_decisions.md
# Architecture Tier:  Level 1.5 (Pitch Physics) + Level 2 (Batter Process Talent)
# Zero-Leakage Protocol:
#   - 12-count Markov RV table & BIP xwOBA->RV map fit STRICTLY on 2015-2023 Regular Season
#   - All 5 XGBoost heads trained on 2015-2023 (plus Hawk-Eye 2020-2023 Stuff+ ablation),
#     early-stopped & 100-scaled strictly on 2024 (val_calibration), locked OOS on 2025-2026
#   - Both Gold tables carry explicit eval_split tags (train_insample / val_calibration / test_oos)
# ==============================================================================

suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(arrow)
  library(data.table)
  library(xgboost)
  library(ggplot2)
  library(patchwork)
  library(ggrepel)
  library(scales)
  library(plotly)
  library(htmlwidgets)
})

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) stop(paste0("[FAIL-FAST] ", msg), call. = FALSE)
}

strip_env <- function(obj) {
  clean_quos <- function(x) {
    if (is.null(x) || is.environment(x) || is.data.frame(x) || is.atomic(x)) return(x)
    if (inherits(x, "formula") || inherits(x, "quosure")) {
      environment(x) <- globalenv()
      return(x)
    }
    if (is.list(x)) {
      attrs <- attributes(x)
      for (i in seq_along(x)) {
        if (!is.null(x[[i]])) x[[i]] <- clean_quos(x[[i]])
      }
      if (!is.null(attrs)) attributes(x) <- attrs
    }
    x
  }
  if (inherits(obj, "ggplot")) {
    if (inherits(obj, "S7_object")) {
      attr(obj, "plot_env") <- globalenv()
      if (!is.null(attr(obj, "mapping"))) attr(obj, "mapping") <- clean_quos(attr(obj, "mapping"))
      if (!is.null(attr(obj, "patches")) && !is.null(attr(obj, "patches")$plots)) {
        ps <- attr(obj, "patches")
        ps$plots <- lapply(ps$plots, strip_env)
        attr(obj, "patches") <- ps
      }
    } else {
      obj$plot_env <- globalenv()
      if (!is.null(obj$mapping)) obj$mapping <- clean_quos(obj$mapping)
      if (!is.null(obj$patches$plots)) obj$patches$plots <- lapply(obj$patches$plots, strip_env)
    }
  }
  obj
}

calc_logloss <- function(y, p) {
  assert_true(length(y) > 0L && length(y) == length(p) && all(y %in% c(0L, 1L)) && all(is.finite(p)),
              "Invalid inputs to calc_logloss")
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

calc_rmse <- function(y, p) {
  assert_true(length(y) > 0L && length(y) == length(p) && all(is.finite(y)) && all(is.finite(p)),
              "Invalid inputs to calc_rmse")
  sqrt(mean((y - p)^2))
}

w_cor <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(ok) < 5L) return(NA_real_)
  ww <- w[ok] / sum(w[ok])
  mx <- sum(ww * x[ok]); my <- sum(ww * y[ok])
  vx <- sum(ww * (x[ok] - mx)^2); vy <- sum(ww * (y[ok] - my)^2)
  if (vx <= 0 || vy <= 0) return(NA_real_)
  sum(ww * (x[ok] - mx) * (y[ok] - my)) / sqrt(vx * vy)
}

w_rmse <- function(pred, actual, w) {
  ok <- is.finite(pred) & is.finite(actual) & is.finite(w) & w > 0
  assert_true(sum(ok) > 0L, "w_rmse received 0 valid rows")
  sqrt(sum(w[ok] * (pred[ok] - actual[ok])^2) / sum(w[ok]))
}

# Top-level lean Plotly & ggplot builders (force() prevents R lazy-promise stack capture)
make_decomp03_plotly <- function(df) {
  force(df)
  col_cyan <- "#38bdf8"; col_emerald <- "#10b981"; col_amber <- "#f59e0b"; col_rose <- "#f43f5e"
  plotly::plot_ly(
    data = df, x = ~decomp_stuff_plus, y = ~decomp_location_plus, type = "scatter", mode = "markers",
    text = ~hover_txt, hoverinfo = "text",
    marker = list(
      size = ~scales::rescale(n_pitches, to = c(6, 17)), color = ~decomp_pitching_tunnel_plus,
      colorscale = list(c(0, col_rose), c(0.35, col_amber), c(0.55, "#e2e8f0"), c(0.78, col_cyan), c(1, col_emerald)),
      cmin = 82, cmax = 122, opacity = 0.86, line = list(width = 0.6, color = "#090d16"),
      colorbar = list(title = list(text = "Decomp Pitch+Tun", font = list(color = "#e2e8f0")), tickfont = list(color = "#94a3b8"))
    )
  ) %>%
    plotly::layout(
      title = list(text = "Decomposed Pitcher Frontier (2025 Locked test_oos): Decomp Stuff+ vs. Decomp Location+",
                   font = list(color = "#f8fafc", size = 15)),
      paper_bgcolor = "#090d16", plot_bgcolor = "#111827",
      xaxis = list(title = "Decomposed Pure Stuff+ (Zero Location/Count)", gridcolor = "#1e293b", color = "#e2e8f0"),
      yaxis = list(title = "Decomposed Pure Location+ (Zero Velo/Break)", gridcolor = "#1e293b", color = "#e2e8f0"),
      shapes = list(
        list(type = "line", x0 = 100, x1 = 100, y0 = 68, y1 = 132, line = list(color = "#475569", dash = "dash", width = 1)),
        list(type = "line", x0 = 68, x1 = 132, y0 = 100, y1 = 100, line = list(color = "#475569", dash = "dash", width = 1))
      ),
      hoverlabel = list(bgcolor = "#0f172a", bordercolor = col_cyan, font = list(color = "#f8fafc", size = 12))
    )
}

make_decomp05_plotly <- function(df) {
  force(df)
  col_cyan <- "#38bdf8"; col_emerald <- "#10b981"; col_amber <- "#f59e0b"; col_rose <- "#f43f5e"
  plotly::plot_ly(
    data = df, x = ~decision_plus, y = ~contact_plus, type = "scatter", mode = "markers",
    text = ~hover_txt, hoverinfo = "text",
    marker = list(
      size = ~scales::rescale(n_pitches, to = c(6, 17)), color = ~batter_process_plus,
      colorscale = list(c(0, col_rose), c(0.35, col_amber), c(0.55, "#e2e8f0"), c(0.78, col_cyan), c(1, col_emerald)),
      cmin = 80, cmax = 125, opacity = 0.86, line = list(width = 0.6, color = "#090d16"),
      colorbar = list(title = list(text = "BatterProcess+", font = list(color = "#e2e8f0")), tickfont = list(color = "#94a3b8"))
    )
  ) %>%
    plotly::layout(
      title = list(text = "Batter Process Archetype Frontier (2025 Locked test_oos): Decision+ (True SEAGER) vs. Contact+",
                   font = list(color = "#f8fafc", size = 15)),
      paper_bgcolor = "#090d16", plot_bgcolor = "#111827",
      xaxis = list(title = "Pitch-Level Swing Decision+ (Zero-Mean Value Over Expected Choice)", gridcolor = "#1e293b", color = "#e2e8f0"),
      yaxis = list(title = "Contact Quality Over Expected (Contact+)", gridcolor = "#1e293b", color = "#e2e8f0"),
      shapes = list(
        list(type = "line", x0 = 100, x1 = 100, y0 = 65, y1 = 140, line = list(color = "#475569", dash = "dash", width = 1)),
        list(type = "line", x0 = 65, x1 = 140, y0 = 100, y1 = 100, line = list(color = "#475569", dash = "dash", width = 1))
      ),
      hoverlabel = list(bgcolor = "#0f172a", bordercolor = col_cyan, font = list(color = "#f8fafc", size = 12))
    )
}

build_decomp_visuals <- function(count_rv_dt, pitch_bench_dt, bat_bench_dt,
                                 p25_viz, b25_viz, head_diag_dt, models_dir, assets_dir) {
  force(count_rv_dt); force(pitch_bench_dt); force(bat_bench_dt)
  force(p25_viz); force(b25_viz); force(head_diag_dt); force(models_dir); force(assets_dir)

  col_cyan <- "#38bdf8"; col_emerald <- "#10b981"; col_amber <- "#f59e0b"
  col_rose <- "#f43f5e"; col_violet  <- "#a78bfa"

  # --- FIGURE 1: 12-Count Markov Context-Neutral Run-Value Matrix ---
  c_dt <- copy(count_rv_dt)
  c_dt[, count_lbl := sprintf("%d-%d", balls, strikes)]
  c_dt[, spread_ball_strike := rv_ball - rv_strike]

  p1_left <- ggplot(c_dt, aes(x = factor(strikes), y = factor(balls, levels = 3:0), fill = spread_ball_strike)) +
    geom_tile(color = "#090d16", linewidth = 1.1) +
    geom_text(aes(label = sprintf("%s\nSpread: +%.3f\n(B: %+.3f / S: %+.3f)",
                                  count_lbl, spread_ball_strike, rv_ball, rv_strike)),
              color = "#f8fafc", size = 3.1, fontface = "bold") +
    scale_fill_gradientn(colors = c("#1e293b", "#0ea5e9", "#10b981", "#f59e0b"),
                         name = "Ball - Strike\nRV Spread") +
    labs(title = "12-Count Context-Neutral Ball vs. Strike Leverage",
         subtitle = "Exact 2015-2023 Regular-Season empirical count transitions (runs)",
         x = "Strikes", y = "Balls") +
    mlbr::theme_cyber(base_size = 10.5)

  p1_right <- ggplot(c_dt, aes(x = reorder(count_lbl, spread_ball_strike), y = p_swing_emp)) +
    geom_col(aes(fill = p_whiff_emp), width = 0.68, alpha = 0.92) +
    geom_text(aes(label = sprintf("Sw: %.1f%% | Wh: %.1f%%", 100 * p_swing_emp, 100 * p_whiff_emp)),
              hjust = -0.06, color = "#e2e8f0", size = 2.9, fontface = "bold") +
    coord_flip(ylim = c(0, 1.05)) +
    scale_fill_gradientn(colors = c(col_cyan, col_amber, col_rose), labels = scales::percent_format(accuracy = 1),
                         name = "Whiff% |\nSwing") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Empirical Swing Rate & Whiff Rate by Count Leverage",
         subtitle = "Ordered from highest Ball-Strike spread (3-2: 0.602 runs) to 0-0",
         x = "Count (B-S)", y = "Empirical Swing Probability") +
    mlbr::theme_cyber(base_size = 10.5)

  fig1 <- (p1_left | p1_right) +
    plot_annotation(
      title = "Markov Pitch Decomposition Engine: Context-Neutral 12-Count Run-Value Matrix (2015-2023 Train)",
      subtitle = "Decoupling pitch probabilities from base-out state eliminates bases-loaded leverage noise while pricing every swing/take decision.",
      caption = "Source: mlbr Silver v_statcast_features (2015-2023 Regular Season) | Zero 2024-2026 leakage",
      theme = mlbr::theme_cyber(base_size = 11)
    )
  mlbr::save_plot_assets(strip_env(fig1), "decomp01_markov_count_rv_matrix", data = c_dt,
                         width = 14.5, height = 7.4, dir = models_dir)

  # --- FIGURE 2: Pitcher Head-to-Head Ablation (Study 03 vs Decomp v2 vs FanGraphs) ---
  pb_plot <- copy(pitch_bench_dt[target %in% c("siera_25", "fip_25", "k_pct_25", "bb_pct_25")])
  pb_plot[, target_lbl := factor(fcase(
    target == "siera_25", "Forecasting 2025 SIERA (|r|)",
    target == "fip_25",   "Forecasting 2025 FIP (|r|)",
    target == "k_pct_25", "Forecasting 2025 K% (|r|)",
    target == "bb_pct_25","Forecasting 2025 BB% (|r|)"
  ), levels = c("Forecasting 2025 SIERA (|r|)", "Forecasting 2025 FIP (|r|)",
                "Forecasting 2025 K% (|r|)", "Forecasting 2025 BB% (|r|)"))]
  pb_plot[, abs_r := abs(w_r)]
  pb_plot[, is_best := (abs_r == max(abs_r)), by = target_lbl]
  pb_plot[, bar_grp := fcase(
    is_best, "Best OOS Predictor",
    grepl("^Decomp", model), "EXP-005 Decomposed",
    grepl("^FanGraphs", model), "Commercial Benchmark",
    default = "Study 03 Baseline"
  )]

  fig2 <- ggplot(pb_plot, aes(x = reorder(model, abs_r), y = abs_r, fill = bar_grp)) +
    geom_col(width = 0.68, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.3f", abs_r)), hjust = -0.12, color = "#f8fafc", size = 3.1, fontface = "bold") +
    coord_flip() + facet_wrap(~ target_lbl, scales = "free", ncol = 2) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
    scale_fill_manual(values = c(
      "Best OOS Predictor" = col_emerald, "EXP-005 Decomposed" = col_cyan,
      "Commercial Benchmark" = col_amber, "Study 03 Baseline" = col_violet
    )) +
    labs(title = "Locked 2024 -> 2025 Pitcher Out-of-Sample Forecasting Validity (Matched Qualified Cohort)",
         subtitle = "Exposure-weighted |r| comparing Study 03 (raw delta_run_exp) vs. EXP-005 (5-head Markov Decomposed) vs. FanGraphs Pitching+.",
         x = NULL, y = "Exposure-Weighted Out-of-Sample |r| (Higher Is Better)", fill = NULL,
         caption = "Source: mlbr EXP-005 Chronological Backtest | 2024 val_calibration features -> locked 2025 FanGraphs outcomes") +
    mlbr::theme_cyber(base_size = 10.5) +
    theme(legend.position = "top", strip.text = element_text(face = "bold", color = "#e2e8f0"))
  mlbr::save_plot_assets(strip_env(fig2), "decomp02_pitcher_ablation_vs_study03_fg", data = pb_plot,
                         width = 14.0, height = 8.0, dir = models_dir)

  # --- FIGURE 3: Pitcher Decomposed Stuff+ vs Location+ Frontier (Static + Plotly) ---
  fig3_static <- ggplot(p25_viz, aes(x = decomp_stuff_plus, y = decomp_location_plus,
                                     color = decomp_pitching_tunnel_plus, size = n_pitches)) +
    geom_vline(xintercept = 100, linetype = "dashed", color = "#475569", linewidth = 0.5) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "#475569", linewidth = 0.5) +
    geom_point(alpha = 0.84) +
    ggrepel::geom_text_repel(
      data = p25_viz[label_flag == TRUE], aes(label = pitcher_name), color = "#f8fafc",
      size = 3.1, fontface = "bold", box.padding = 0.35, max.overlaps = 20, segment.color = "#475569", show.legend = FALSE
    ) +
    scale_color_gradientn(colors = c(col_rose, col_amber, "#e2e8f0", col_cyan, col_emerald),
                          limits = c(84, 122), oob = scales::squish) +
    scale_size_continuous(range = c(1.8, 5.5), guide = "none") +
    annotate("text", x = 123, y = 120, label = "COMPLETE ACES\n(Elite Stuff + Elite Command)", color = col_emerald, size = 3.1, fontface = "bold") +
    annotate("text", x = 123, y = 80,  label = "POWER / WILD\n(Elite Stuff, Volatile Loc)", color = col_amber, size = 3.1, fontface = "bold") +
    annotate("text", x = 80,  y = 120, label = "CRAFTY SURGEONS\n(Command Specialists)", color = col_cyan, size = 3.1, fontface = "bold") +
    labs(title = "Decomposed Pitcher Frontier (2025 Locked test_oos): Decomp Stuff+ vs. Decomp Location+",
         subtitle = sprintf("n = %d qualified 2025 out-of-sample pitchers (>= 750 pitches); colored by Decomp Pitching+Tunnel.", nrow(p25_viz)),
         x = "Decomposed Pure Stuff+ (Zero Location/Count)", y = "Decomposed Pure Location+ (Zero Velo/Break)",
         color = "Decomp\nPitch+Tun",
         caption = "Source: mlbr EXP-005 Gold Pitcher Table (test_oos 2025) | Zero 2025 training leakage") +
    mlbr::theme_cyber(base_size = 11)
  mlbr::save_plot_assets(strip_env(fig3_static), "decomp03_pitcher_decomp_explorer_static", data = p25_viz,
                         width = 12.6, height = 7.4, dir = models_dir)

  fig3_plotly <- make_decomp03_plotly(p25_viz)
  mlbr::save_plot_assets(fig3_plotly, "decomp03_pitcher_decomp_explorer", data = p25_viz, dir = models_dir)
  dep3 <- file.path(models_dir, "decomp03_pitcher_decomp_explorer_files")
  if (dir.exists(dep3)) unlink(dep3, recursive = TRUE)

  # --- FIGURE 4: Batter Process Validity (YoY Stability & OOS Forecasting) ---
  bb_plot <- copy(bat_bench_dt)
  p4_left <- ggplot(bb_plot[eval_type == "YoY_Stability_24_25"],
                    aes(x = reorder(metric, value), y = value, fill = tier)) +
    geom_col(width = 0.68, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.3f", value)), hjust = -0.12, color = "#f8fafc", size = 3.2, fontface = "bold") +
    coord_flip(ylim = c(0, 1.02)) +
    scale_fill_manual(values = c("EXP-005 Process" = col_cyan, "Study 04 Proxy" = col_amber, "Outcome Baseline" = col_rose)) +
    labs(title = "2024 -> 2025 YoY Repeatability (Weighted r)",
         subtitle = "Matched active batters (>= 250 PA in both years)",
         x = NULL, y = "Exposure-Weighted YoY r", fill = NULL) +
    mlbr::theme_cyber(base_size = 10) + theme(legend.position = "top")

  p4_right <- ggplot(bb_plot[eval_type == "OOS_24_to_25_Validity"],
                     aes(x = reorder(metric, abs(value)), y = abs(value), fill = tier)) +
    geom_col(width = 0.68, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.3f", abs(value))), hjust = -0.12, color = "#f8fafc", size = 3.0, fontface = "bold") +
    coord_flip() + facet_wrap(~ target, scales = "free", ncol = 1) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    scale_fill_manual(values = c("EXP-005 Process" = col_emerald, "Study 04 Proxy" = col_amber, "Outcome Baseline" = col_rose)) +
    labs(title = "2024 -> 2025 Out-of-Sample Forecasting |r|",
         subtitle = "Predicting next-year wOBA, BB%, and K% strictly OOS",
         x = NULL, y = "Out-of-Sample |r|", fill = NULL) +
    mlbr::theme_cyber(base_size = 10) + theme(legend.position = "top", strip.text = element_text(face = "bold", color = "#e2e8f0"))

  fig4 <- (p4_left | p4_right) +
    plot_annotation(
      title = "Pitch-Level Batter Process Engine: Stability & Next-Year Forecasting vs. Z-O% Proxy & wOBA",
      subtitle = "Decomposing every pitch into zero-mean Decision+, BatControl+, and Contact+ exposes true hitting skill faster than box-score outcomes.",
      caption = "Source: mlbr EXP-005 Gold Batter Process Table | 2024 val_calibration -> locked 2025 FanGraphs OOS",
      theme = mlbr::theme_cyber(base_size = 11)
    )
  mlbr::save_plot_assets(strip_env(fig4), "decomp04_batter_process_validity", data = bb_plot,
                         width = 14.5, height = 8.0, dir = models_dir)

  # --- FIGURE 5: Batter Decision+ vs Contact+ Archetype Frontier (Static + Plotly) ---
  fig5_static <- ggplot(b25_viz, aes(x = decision_plus, y = contact_plus,
                                     color = batter_process_plus, size = n_pitches)) +
    geom_vline(xintercept = 100, linetype = "dashed", color = "#475569", linewidth = 0.5) +
    geom_hline(yintercept = 100, linetype = "dashed", color = "#475569", linewidth = 0.5) +
    geom_point(alpha = 0.84) +
    ggrepel::geom_text_repel(
      data = b25_viz[label_flag == TRUE], aes(label = batter_name), color = "#f8fafc",
      size = 3.1, fontface = "bold", box.padding = 0.35, max.overlaps = 20, segment.color = "#475569", show.legend = FALSE
    ) +
    scale_color_gradientn(colors = c(col_rose, col_amber, "#e2e8f0", col_cyan, col_emerald),
                          limits = c(82, 125), oob = scales::squish) +
    scale_size_continuous(range = c(1.8, 5.5), guide = "none") +
    annotate("text", x = 123, y = 128, label = "UNICORN PROCESS\n(Elite Eye + Elite Damage)", color = col_emerald, size = 3.1, fontface = "bold") +
    annotate("text", x = 80,  y = 126, label = "FREE-SWINGING SLUGGERS\n(High Chase, Massive Damage)", color = col_amber, size = 3.1, fontface = "bold") +
    annotate("text", x = 123, y = 76,  label = "PATIENT TABLE-SETTERS\n(Elite Eye, Modest Exit Velo)", color = col_cyan, size = 3.1, fontface = "bold") +
    labs(title = "Batter Process Archetype Frontier (2025 Locked test_oos): Decision+ vs. Contact+",
         subtitle = sprintf("n = %d qualified 2025 batters (>= 1,200 pitches seen); colored by total BatterProcess+.", nrow(b25_viz)),
         x = "Pitch-Level Swing Decision+ (Zero-Mean Value Over Expected Choice)",
         y = "Contact Quality Over Expected (Contact+)",
         color = "Batter\nProcess+",
         caption = "Source: mlbr EXP-005 Gold Batter Process Table (test_oos 2025) | Zero 2025 training leakage") +
    mlbr::theme_cyber(base_size = 11)
  mlbr::save_plot_assets(strip_env(fig5_static), "decomp05_batter_decision_vs_contact_explorer_static", data = b25_viz,
                         width = 12.6, height = 7.4, dir = models_dir)

  fig5_plotly <- make_decomp05_plotly(b25_viz)
  mlbr::save_plot_assets(fig5_plotly, "decomp05_batter_decision_vs_contact_explorer", data = b25_viz, dir = models_dir)
  dep5 <- file.path(models_dir, "decomp05_batter_decision_vs_contact_explorer_files")
  if (dir.exists(dep5)) unlink(dep5, recursive = TRUE)

  expected_assets <- c(
    "decomp01_markov_count_rv_matrix.png",
    "decomp02_pitcher_ablation_vs_study03_fg.png",
    "decomp03_pitcher_decomp_explorer_static.png",
    "decomp03_pitcher_decomp_explorer.html",
    "decomp04_batter_process_validity.png",
    "decomp05_batter_decision_vs_contact_explorer_static.png",
    "decomp05_batter_decision_vs_contact_explorer.html"
  )
  asset_files <- file.path(models_dir, expected_assets)
  assert_true(all(file.exists(asset_files)), paste("Missing decomp visual assets:", paste(expected_assets[!file.exists(asset_files)], collapse = ", ")))
  assert_true(all(file.copy(asset_files, assets_dir, overwrite = TRUE)), "Failed copying decomp0* assets to site/assets/")
  rds_files <- Sys.glob(file.path(models_dir, "decomp0*.rds"))
  assert_true(length(rds_files) == 7L, sprintf("Expected 7 decomp .rds files, found %d", length(rds_files)))
  rds_mb <- file.info(rds_files)$size / 1e6
  assert_true(all(rds_mb < 5.0), paste("Oversized decomp .rds:", paste(sprintf("%s (%.2f MB)", basename(rds_files[rds_mb >= 5.0]), rds_mb[rds_mb >= 5.0]), collapse = ", ")))
  cat(sprintf("   Verified & copied exact 7 PNG/HTML assets to site/assets/ | 7 .rds files max: %.2f MB\n", max(rds_mb)))
  invisible(TRUE)
}

run_exp005 <- function() {
  t0_total <- proc.time()[["elapsed"]]
  set.seed(20260505L)
  data.table::setDTthreads(min(8L, data.table::getDTthreads()))

  lake <- mlbr::default_lake()
  models_dir    <- file.path(lake, "models")
  processed_dir <- file.path(lake, "data/processed")
  assets_dir    <- file.path(lake, "site/assets")
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(assets_dir,    recursive = TRUE, showWarnings = FALSE)

  cat("==============================================================================\n")
  cat("mlbr EXP-005: Decomposed Pitching+ & Pitch-Level Batter Process Engine\n")
  cat("Lake root:", lake, "\n")
  cat("==============================================================================\n\n")

  con <- mlbr::lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")
  DBI::dbExecute(con, "SET threads = 8;")

  cat("-> [0/7] Pre-flight schema check: loading Chadwick, FanGraphs (2020-2025) & Study 03 Gold...\n")
  cw_names <- as.data.table(DBI::dbGetQuery(con, "
    SELECT CAST(key_mlbam AS INTEGER) AS mlbam,
           MAX(CONCAT(TRIM(name_first), ' ', TRIM(name_last))) AS player_name,
           MAX(CAST(key_fangraphs AS INTEGER)) AS key_fg
    FROM v_chadwick_register WHERE key_mlbam IS NOT NULL GROUP BY 1
  "))
  fg_p <- as.data.table(DBI::dbGetQuery(con, "
    SELECT CAST(c.key_mlbam AS INTEGER) AS pitcher, CAST(p.Season AS INTEGER) AS game_year, MAX(p.team_name) AS team,
           SUM(CAST(p.TBF AS DOUBLE)) AS fg_tbf,
           SUM(FLOOR(CAST(p.IP AS DOUBLE)) + ROUND((CAST(p.IP AS DOUBLE) - FLOOR(CAST(p.IP AS DOUBLE))) * 10.0) / 3.0) AS fg_ip,
           SUM(CAST(p.SO AS DOUBLE)) / NULLIF(SUM(CAST(p.TBF AS DOUBLE)), 0) AS fg_k_pct,
           SUM(CAST(p.BB AS DOUBLE)) / NULLIF(SUM(CAST(p.TBF AS DOUBLE)), 0) AS fg_bb_pct,
           SUM(CAST(p.FIP AS DOUBLE) * (FLOOR(CAST(p.IP AS DOUBLE)) + ROUND((CAST(p.IP AS DOUBLE) - FLOOR(CAST(p.IP AS DOUBLE))) * 10.0) / 3.0)) /
             NULLIF(SUM(FLOOR(CAST(p.IP AS DOUBLE)) + ROUND((CAST(p.IP AS DOUBLE) - FLOOR(CAST(p.IP AS DOUBLE))) * 10.0) / 3.0), 0) AS fg_fip,
           SUM(CAST(p.SIERA AS DOUBLE) * CAST(p.TBF AS DOUBLE)) / NULLIF(SUM(CAST(p.TBF AS DOUBLE)), 0) AS fg_siera,
           SUM(CAST(p.sp_stuff AS DOUBLE) * CAST(p.TBF AS DOUBLE)) / NULLIF(SUM(CAST(p.TBF AS DOUBLE)), 0) AS fg_stuff_plus,
           SUM(CAST(p.sp_location AS DOUBLE) * CAST(p.TBF AS DOUBLE)) / NULLIF(SUM(CAST(p.TBF AS DOUBLE)), 0) AS fg_location_plus,
           SUM(CAST(p.sp_pitching AS DOUBLE) * CAST(p.TBF AS DOUBLE)) / NULLIF(SUM(CAST(p.TBF AS DOUBLE)), 0) AS fg_pitching_plus
    FROM v_fangraphs_pitch p
    JOIN (SELECT CAST(key_fangraphs AS INTEGER) AS fg_id, MIN(CAST(key_mlbam AS INTEGER)) AS key_mlbam
          FROM v_chadwick_register WHERE key_fangraphs IS NOT NULL AND key_mlbam IS NOT NULL GROUP BY 1) c
      ON CAST(p.playerid AS INTEGER) = c.fg_id
    WHERE CAST(p.Season AS INTEGER) BETWEEN 2020 AND 2025 AND CAST(p.TBF AS DOUBLE) > 0
    GROUP BY 1, 2
  "))

  fg_b <- as.data.table(DBI::dbGetQuery(con, "
    SELECT CAST(c.key_mlbam AS INTEGER) AS batter, CAST(b.Season AS INTEGER) AS game_year, MAX(b.team_name) AS team,
           SUM(CAST(b.PA AS DOUBLE)) AS fg_pa,
           SUM(CAST(b.wOBA AS DOUBLE) * CAST(b.PA AS DOUBLE)) / NULLIF(SUM(CAST(b.PA AS DOUBLE)), 0) AS fg_woba,
           SUM(CAST(b.xwOBA AS DOUBLE) * CAST(b.PA AS DOUBLE)) / NULLIF(SUM(CAST(b.PA AS DOUBLE)), 0) AS fg_xwoba,
           SUM(CAST(b.SO AS DOUBLE)) / NULLIF(SUM(CAST(b.PA AS DOUBLE)), 0) AS fg_k_pct,
           SUM(CAST(b.BB AS DOUBLE)) / NULLIF(SUM(CAST(b.PA AS DOUBLE)), 0) AS fg_bb_pct,
           SUM(CAST(b.ISO AS DOUBLE) * CAST(b.AB AS DOUBLE)) / NULLIF(SUM(CAST(b.AB AS DOUBLE)), 0) AS fg_iso,
           SUM(CAST(b.wRC_plus AS DOUBLE) * CAST(b.PA AS DOUBLE)) / NULLIF(SUM(CAST(b.PA AS DOUBLE)), 0) AS fg_wrc_plus
    FROM v_fangraphs_bat b
    JOIN (SELECT CAST(key_fangraphs AS INTEGER) AS fg_id, MIN(CAST(key_mlbam AS INTEGER)) AS key_mlbam
          FROM v_chadwick_register WHERE key_fangraphs IS NOT NULL AND key_mlbam IS NOT NULL GROUP BY 1) c
      ON CAST(b.playerid AS INTEGER) = c.fg_id
    WHERE CAST(b.Season AS INTEGER) BETWEEN 2020 AND 2025 AND CAST(b.PA AS DOUBLE) > 0 AND COALESCE(b.position, '') != 'P'
    GROUP BY 1, 2
  "))

  s3_path <- file.path(processed_dir, "pitcher_stuff_plus.parquet")
  assert_true(file.exists(s3_path), "Study 03 pitcher_stuff_plus.parquet not found")
  s3_dt <- as.data.table(arrow::read_parquet(s3_path))[, .(
    pitcher = as.integer(pitcher), game_year = as.integer(game_year),
    s3_stuff_plus = stuff_plus, s3_location_plus = location_plus,
    s3_pitching_plus = pitching_plus, s3_pitching_tunnel_plus = pitching_tunnel_plus
  )]

  assert_true(nrow(fg_p) > 4000L && nrow(fg_b) > 3500L && nrow(s3_dt) > 4000L, "Pre-flight FanGraphs/Study03 row check failed")
  cat(sprintf("   Pre-flight OK: fg_p=%s, fg_b=%s, s3_dt=%s rows.\n", format(nrow(fg_p), big.mark=","), format(nrow(fg_b), big.mark=","), format(nrow(s3_dt), big.mark=",")))

  # ============================================================================
  # 1. FULL-POPULATION EXTRACTION (2015-2026 REGULAR SEASON, STRICTLY CAUSAL)
  # ============================================================================
  cat("-> [1/7] Extracting 2015-2026 Regular-Season Statcast + Tunneling Silver views...\n")
  t_ext0 <- proc.time()[["elapsed"]]
  extract_sql <- "
    WITH base_fb AS (
      SELECT
        game_pk, game_year, game_date, at_bat_number, pitch_number,
        pitcher, batter, pitch_type, p_throws, stand, matchup, is_same_hand,
        release_speed, effective_speed, release_spin_rate, spin_axis,
        ivb_in, hb_glove_in, hb_away_in,
        release_pos_x_arm, release_pos_z, release_extension, arm_angle_deg,
        plate_x, plate_x_norm, plate_z, sz_top, sz_bot, vaa, haa_away,
        balls, strikes, outs_when_up, n_thruorder_pitcher,
        is_called_strike, is_swing, is_whiff, is_bip, is_csw, is_chase, is_in_zone,
        xwoba, delta_run_exp, pitcher_run_exp,
        AVG(CASE WHEN pitch_type IN ('FF', 'SI', 'FC') THEN release_speed END)
          OVER (PARTITION BY pitcher, game_year
                ORDER BY game_date, game_pk, at_bat_number, pitch_number
                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pitcher_fb_velo_to_date,
        AVG(CASE WHEN pitch_type IN ('FF', 'SI', 'FC') THEN ivb_in END)
          OVER (PARTITION BY pitcher, game_year
                ORDER BY game_date, game_pk, at_bat_number, pitch_number
                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pitcher_fb_ivb_to_date,
        AVG(CASE WHEN pitch_type IN ('FF', 'SI', 'FC') THEN hb_glove_in END)
          OVER (PARTITION BY pitcher, game_year
                ORDER BY game_date, game_pk, at_bat_number, pitch_number
                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pitcher_fb_hb_to_date
      FROM v_statcast_features
      WHERE game_year BETWEEN 2015 AND 2026
        AND game_type = 'R'
        AND balls BETWEEN 0 AND 3
        AND strikes BETWEEN 0 AND 2
        AND stand IN ('R', 'L')
        AND p_throws IN ('R', 'L')
    )
    SELECT
      f.game_pk,
      CAST(f.game_year AS INTEGER) AS game_year,
      f.game_date,
      CAST(f.at_bat_number AS INTEGER) AS at_bat_number,
      CAST(f.pitch_number AS INTEGER) AS pitch_number,
      CAST(f.pitcher AS INTEGER) AS pitcher,
      CAST(f.batter AS INTEGER) AS batter,
      f.pitch_type,
      f.p_throws,
      f.stand,
      CAST(f.is_same_hand AS INTEGER) AS is_same_hand,
      f.release_speed, f.effective_speed, f.release_spin_rate, f.spin_axis,
      f.ivb_in, f.hb_glove_in, f.hb_away_in,
      f.release_pos_x_arm, f.release_pos_z, f.release_extension, f.arm_angle_deg,
      (f.release_speed - f.pitcher_fb_velo_to_date) AS delta_fb_velo,
      (f.ivb_in - f.pitcher_fb_ivb_to_date) AS delta_fb_ivb,
      (f.hb_glove_in - f.pitcher_fb_hb_to_date) AS delta_fb_hb,
      f.plate_x_norm, f.plate_z,
      CASE WHEN f.sz_top IS NOT NULL AND f.sz_bot IS NOT NULL AND (f.sz_top - f.sz_bot) > 0.2
           THEN (f.plate_z - f.sz_bot) / (f.sz_top - f.sz_bot)
           ELSE NULL END AS sz_norm_z,
      f.vaa, f.haa_away,
      CAST(f.balls AS INTEGER) AS balls,
      CAST(f.strikes AS INTEGER) AS strikes,
      CAST(f.outs_when_up AS DOUBLE) AS outs_when_up,
      CAST(f.n_thruorder_pitcher AS DOUBLE) AS n_thruorder_pitcher,
      t.tunnel_dist_in, t.plate_sep_in, t.post_tunnel_break_in,
      t.break_to_tunnel_ratio, t.delta_velo_mph, t.delta_flight_ms,
      CAST(t.is_repeat_pitch AS DOUBLE) AS is_repeat_pitch,
      CAST(f.is_called_strike AS INTEGER) AS is_called_strike,
      CAST(f.is_swing AS INTEGER) AS is_swing,
      CAST(f.is_whiff AS INTEGER) AS is_whiff,
      CAST(f.is_bip AS INTEGER) AS is_bip,
      CAST(f.is_in_zone AS INTEGER) AS is_in_zone,
      CAST(f.is_chase AS INTEGER) AS is_chase,
      f.xwoba,
      f.delta_run_exp
    FROM base_fb f
    LEFT JOIN v_statcast_tunneling t
      USING (game_pk, at_bat_number, pitch_number)
    WHERE f.release_speed IS NOT NULL
      AND f.ivb_in IS NOT NULL
      AND f.hb_glove_in IS NOT NULL
      AND f.plate_x IS NOT NULL
      AND f.plate_z IS NOT NULL
      AND f.delta_run_exp IS NOT NULL
      AND f.pitch_type IS NOT NULL
  "
  dt <- as.data.table(DBI::dbGetQuery(con, extract_sql))
  cat(sprintf("   Extracted %s valid regular-season pitches in %.1fs.\n",
              format(nrow(dt), big.mark = ","), proc.time()[["elapsed"]] - t_ext0))
  assert_true(nrow(dt) >= 7500000L, paste("Unexpected row count:", nrow(dt)))
  assert_true(anyDuplicated(dt, by = c("game_pk", "at_bat_number", "pitch_number")) == 0L,
              "Duplicate pitch keys after tunneling LEFT JOIN")
  assert_true(all(dt$is_swing %in% c(0L, 1L)) && all(dt$is_whiff %in% c(0L, 1L)) &&
              all(dt$is_called_strike %in% c(0L, 1L)) && all(dt$is_bip %in% c(0L, 1L)),
              "Invalid binary outcome flags")

  # Derived encodings & exact count_id (1..12)
  dt[, count_id := as.integer(balls * 3L + strikes + 1L)]
  dt[, is_foul := as.integer(is_swing == 1L & is_whiff == 0L & is_bip == 0L)]
  dt[, `:=`(
    balls_num   = as.numeric(balls),
    strikes_num = as.numeric(strikes),
    is_rhp      = as.numeric(p_throws == "R"),
    is_rhb      = as.numeric(stand == "R"),
    is_same_num = as.numeric(is_same_hand),
    has_tunnel  = as.numeric(is.finite(tunnel_dist_in))
  )]
  canonical_pts <- c("FF", "SI", "FC", "SL", "ST", "CU", "KC", "CH", "FS")
  for (pt in canonical_pts) dt[, (paste0("pt_", pt)) := as.numeric(pitch_type == pt)]
  dt[, pt_OTHER := as.numeric(!(pitch_type %in% canonical_pts))]
  pt_cols <- c(paste0("pt_", canonical_pts), "pt_OTHER")

  feats_stuff <- c(
    "release_speed", "effective_speed", "release_spin_rate", "spin_axis",
    "ivb_in", "hb_glove_in", "hb_away_in",
    "release_pos_x_arm", "release_pos_z", "release_extension", "arm_angle_deg",
    "delta_fb_velo", "delta_fb_ivb", "delta_fb_hb",
    "is_same_num", "is_rhp", "is_rhb", pt_cols
  )
  feats_loc <- c(
    "plate_x_norm", "plate_z", "sz_norm_z",
    "balls_num", "strikes_num", "outs_when_up", "n_thruorder_pitcher",
    "is_same_num", "is_rhp", "is_rhb", pt_cols
  )
  feats_pitching <- unique(c(
    feats_stuff,
    "plate_x_norm", "plate_z", "sz_norm_z", "vaa", "haa_away",
    "balls_num", "strikes_num", "outs_when_up", "n_thruorder_pitcher"
  ))
  feats_tunnel <- c(
    "has_tunnel", "tunnel_dist_in", "plate_sep_in", "post_tunnel_break_in",
    "break_to_tunnel_ratio", "delta_velo_mph", "delta_flight_ms", "is_repeat_pitch"
  )
  feats_pitching_tun <- c(feats_pitching, feats_tunnel)

  # Strict chronological split indices
  idx_tr_full <- which(dt$game_year >= 2015L & dt$game_year <= 2023L)
  idx_tr_he   <- which(dt$game_year >= 2020L & dt$game_year <= 2023L)
  idx_val     <- which(dt$game_year == 2024L)
  idx_test    <- which(dt$game_year >= 2025L & dt$game_year <= 2026L)
  idx_score   <- which(dt$game_year >= 2020L & dt$game_year <= 2026L)

  assert_true(length(idx_tr_full) > 5000000L && length(idx_tr_he) > 2000000L &&
              length(idx_val) > 600000L && length(idx_test) > 1000000L,
              "Chronological split sizes out of bounds")
  assert_true(max(dt$game_date[idx_tr_full]) < min(dt$game_date[idx_val]) &&
              max(dt$game_date[idx_val]) < min(dt$game_date[idx_test]),
              "Chronological date boundary violation")

  # ============================================================================
  # 2. CONTEXT-NEUTRAL 12-COUNT MARKOV RV TABLE & BIP xwOBA->RV MAP (2015-2023)
  # ============================================================================
  cat("-> [2/7] Fitting 12-count Markov RV table & BIP xwOBA->RV map (2015-2023 train only)...\n")
  tr_sub <- dt[idx_tr_full]
  count_rv_dt <- tr_sub[, .(
    n_pitches   = .N,
    p_swing_emp = mean(is_swing),
    p_cs_emp    = sum(is_swing == 0L & is_called_strike == 1L) / pmax(1, sum(is_swing == 0L)),
    p_whiff_emp = sum(is_whiff == 1L) / pmax(1, sum(is_swing == 1L)),
    rv_ball     = mean(delta_run_exp[is_swing == 0L & is_called_strike == 0L]),
    rv_strike   = mean(delta_run_exp[is_called_strike == 1L | is_whiff == 1L]),
    rv_foul_raw = mean(delta_run_exp[is_foul == 1L]),
    rv_bip_emp  = mean(delta_run_exp[is_bip == 1L])
  ), by = .(count_id, balls, strikes)][order(count_id)]
  count_rv_dt[, rv_foul := fifelse(strikes == 2L, 0.0, rv_foul_raw)]
  count_rv_dt[, rv_foul_raw := NULL]
  assert_true(nrow(count_rv_dt) == 12L && all(is.finite(count_rv_dt$rv_ball)) &&
              all(is.finite(count_rv_dt$rv_strike)) && all(is.finite(count_rv_dt$rv_foul)),
              "12-count Markov RV table incomplete or non-finite")

  # Exact context-neutral linear map: delta_run_exp ~ factor(count_id) + xwoba on 2015-2023 BIPs
  bip_tr <- tr_sub[is_bip == 1L & is.finite(xwoba)]
  assert_true(nrow(bip_tr) > 900000L, "Too few training BIPs for xwOBA->RV map")
  bip_fit <- stats::lm(delta_run_exp ~ factor(count_id) - 1 + xwoba, data = bip_tr)
  bip_co  <- stats::coef(bip_fit)
  beta_xw <- unname(bip_co[["xwoba"]])
  alpha_vec <- unname(bip_co[paste0("factor(count_id)", 1:12)])
  assert_true(is.finite(beta_xw) && beta_xw > 0.5 && beta_xw < 1.2 && all(is.finite(alpha_vec)),
              sprintf("Unexpected BIP xwOBA->RV slope: %.4f", beta_xw))
  count_rv_dt[, `:=`(bip_alpha = alpha_vec[count_id], bip_beta_xwoba = beta_xw)]

  # Marginal count-weighted weights for count-free Decomp Stuff+
  w_cnt <- count_rv_dt$n_pitches / sum(count_rv_dt$n_pitches)
  marg_rv <- list(
    rv_ball   = sum(w_cnt * count_rv_dt$rv_ball),
    rv_strike = sum(w_cnt * count_rv_dt$rv_strike),
    rv_foul   = sum(w_cnt * count_rv_dt$rv_foul),
    bip_alpha = sum(w_cnt * count_rv_dt$bip_alpha),
    bip_beta  = beta_xw
  )
  cat(sprintf("   BIP xwOBA->RV slope beta = %.4f (R^2 = %.4f) | Marginal B/S = %+.4f / %+.4f\n",
              beta_xw, summary(bip_fit)$r.squared, marg_rv$rv_ball, marg_rv$rv_strike))
  rm(tr_sub, bip_tr, bip_fit); gc(verbose = FALSE)

  # ============================================================================
  # 3. FIVE-HEAD MARKOV XGBOOST ENGINE (MEMORY-SAFE, STRICTLY CHRONOLOGICAL)
  # ============================================================================
  fit_single_head <- function(cols, tr_idx, va_idx, sc_idx, y_vec, objective, eval_metric,
                              max_depth = 6L, lr = 0.05, nrounds = 450L, min_cw = 150, seed_val = 42L) {
    dtr <- xgb.DMatrix(as.matrix(dt[tr_idx, ..cols]), label = y_vec[tr_idx])
    dva <- xgb.DMatrix(as.matrix(dt[va_idx, ..cols]), label = y_vec[va_idx])
    bst <- xgb.train(
      params = xgb.params(
        objective = objective, eval_metric = eval_metric, tree_method = "hist",
        max_depth = max_depth, learning_rate = lr, subsample = 0.8, colsample_bytree = 0.8,
        min_child_weight = min_cw, nthread = 28L, seed = seed_val
      ),
      data = dtr, nrounds = nrounds, evals = list(train = dtr, val = dva),
      early_stopping_rounds = 35L, verbose = 0
    )
    bi <- as.integer(xgb.attr(bst, "best_iteration"))
    assert_true(length(bi) == 1L && !is.na(bi) && bi >= 1L, "Invalid XGBoost best_iteration")
    p_va <- predict(bst, dva)
    val_loss <- if (objective == "binary:logistic") calc_logloss(y_vec[va_idx], p_va) else calc_rmse(y_vec[va_idx], p_va)
    rm(dtr, dva); gc(verbose = FALSE)

    # Chunked prediction on sc_idx (2020-2026) + train sample for martingale count centering
    pred_chunked <- function(indices) {
      n <- length(indices); out <- numeric(n); ch <- 600000L
      for (s0 in seq(1L, n, by = ch)) {
        s1 <- min(s0 + ch - 1L, n)
        dm <- xgb.DMatrix(as.matrix(dt[indices[s0:s1], ..cols]))
        out[s0:s1] <- predict(bst, dm)
        rm(dm)
      }
      out
    }
    p_sc <- pred_chunked(sc_idx)
    rm(bst); gc(verbose = FALSE)
    list(pred_sc = p_sc, val_loss = val_loss, best_iter = bi)
  }

  # Precompute outcome subset masks on train (full & HE) and val (2024)
  make_subsets <- function(base_idx) {
    list(
      all   = base_idx,
      take  = base_idx[dt$is_swing[base_idx] == 0L],
      swing = base_idx[dt$is_swing[base_idx] == 1L],
      cont  = base_idx[dt$is_swing[base_idx] == 1L & dt$is_whiff[base_idx] == 0L],
      bip   = base_idx[dt$is_bip[base_idx] == 1L & is.finite(dt$xwoba[base_idx])]
    )
  }
  sub_tr_full <- make_subsets(idx_tr_full)
  sub_tr_he   <- make_subsets(idx_tr_he)
  sub_val     <- make_subsets(idx_val)

  head_diag_list <- list()

  run_5head_tier <- function(tier_name, cols, sub_tr, use_count_weights = TRUE, seed_base = 100L) {
    t_s <- proc.time()[["elapsed"]]
    cat(sprintf("   -> Training 5-head tier: %-22s (%d features, n_tr = %s)...\n",
                tier_name, length(cols), format(length(sub_tr$all), big.mark = ",")))
    h1 <- fit_single_head(cols, sub_tr$all,   sub_val$all,   idx_score, dt$is_swing,        "binary:logistic", "logloss", 6L, 0.05, 450L, 200, seed_base + 1L)
    h2 <- fit_single_head(cols, sub_tr$take,  sub_val$take,  idx_score, dt$is_called_strike,"binary:logistic", "logloss", 6L, 0.05, 450L, 150, seed_base + 2L)
    h3 <- fit_single_head(cols, sub_tr$swing, sub_val$swing, idx_score, dt$is_whiff,        "binary:logistic", "logloss", 6L, 0.05, 450L, 150, seed_base + 3L)
    h4 <- fit_single_head(cols, sub_tr$cont,  sub_val$cont,  idx_score, dt$is_foul,         "binary:logistic", "logloss", 5L, 0.05, 350L, 150, seed_base + 4L)
    h5 <- fit_single_head(cols, sub_tr$bip,   sub_val$bip,   idx_score, dt$xwoba,           "reg:squarederror","rmse",    5L, 0.04, 350L, 200, seed_base + 5L)

    head_diag_list[[tier_name]] <<- data.table(
      tier = tier_name,
      val_ll_swing = h1$val_loss, val_ll_cstr = h2$val_loss,
      val_ll_whiff = h3$val_loss, val_ll_foul = h4$val_loss,
      val_rmse_xwoba = h5$val_loss,
      it_swing = h1$best_iter, it_cstr = h2$best_iter, it_whiff = h3$best_iter,
      it_foul = h4$best_iter, it_xwoba = h5$best_iter
    )

    p_sw <- pmin(pmax(h1$pred_sc, 1e-6), 1 - 1e-6)
    p_cs <- pmin(pmax(h2$pred_sc, 1e-6), 1 - 1e-6)
    p_wh <- pmin(pmax(h3$pred_sc, 1e-6), 1 - 1e-6)
    p_fl <- pmin(pmax(h4$pred_sc, 1e-6), 1 - 1e-6)
    xw_h <- pmin(pmax(h5$pred_sc, 0.0),  2.0)

    if (use_count_weights) {
      cid <- dt$count_id[idx_score]
      rv_b <- count_rv_dt$rv_ball[cid]
      rv_s <- count_rv_dt$rv_strike[cid]
      rv_f <- count_rv_dt$rv_foul[cid]
      al_c <- count_rv_dt$bip_alpha[cid]
      ev_take <- p_cs * rv_s + (1.0 - p_cs) * rv_b
      ev_cont <- p_fl * rv_f + (1.0 - p_fl) * (al_c + beta_xw * xw_h)
      ev_sw   <- p_wh * rv_s + (1.0 - p_wh) * ev_cont
      xrv_bat <- (1.0 - p_sw) * ev_take + p_sw * ev_sw

      # Exact martingale count centering computed strictly on 2020-2023 train rows within idx_score
      tr_sc_mask <- dt$game_year[idx_score] <= 2023L
      mu_by_cnt <- tapply(xrv_bat[tr_sc_mask], cid[tr_sc_mask], mean)
      xrv_bat_c <- xrv_bat - as.numeric(mu_by_cnt[as.character(cid)])
    } else {
      ev_take <- p_cs * marg_rv$rv_strike + (1.0 - p_cs) * marg_rv$rv_ball
      ev_cont <- p_fl * marg_rv$rv_foul   + (1.0 - p_fl) * (marg_rv$bip_alpha + marg_rv$bip_beta * xw_h)
      ev_sw   <- p_wh * marg_rv$rv_strike + (1.0 - p_wh) * ev_cont
      xrv_bat <- (1.0 - p_sw) * ev_take + p_sw * ev_sw
      tr_sc_mask <- dt$game_year[idx_score] <= 2023L
      xrv_bat_c <- xrv_bat - mean(xrv_bat[tr_sc_mask])
    }

    cat(sprintf("      Done in %.1fs | Val LL: sw=%.4f cs=%.4f wh=%.4f fl=%.4f | xwOBA RMSE=%.4f\n",
                proc.time()[["elapsed"]] - t_s, h1$val_loss, h2$val_loss, h3$val_loss, h4$val_loss, h5$val_loss))
    list(xrv_pit = -xrv_bat_c, p_sw = p_sw, p_cs = p_cs, p_wh = p_wh, p_fl = p_fl, xw_h = xw_h,
         ev_take = ev_take, ev_cont = ev_cont, ev_sw = ev_sw)
  }

  cat("-> [3/7] Running 5-head Markov XGBoost models across all feature & era tiers...\n")
  res_stuff    <- run_5head_tier("Decomp_Stuff_Full",   feats_stuff,        sub_tr_full, use_count_weights = FALSE, seed_base = 100L)
  res_stuff_he <- run_5head_tier("Decomp_Stuff_HawkEye",feats_stuff,        sub_tr_he,   use_count_weights = FALSE, seed_base = 200L)
  res_loc      <- run_5head_tier("Decomp_Location",     feats_loc,          sub_tr_full, use_count_weights = TRUE,  seed_base = 300L)
  res_pitch    <- run_5head_tier("Decomp_Pitching",     feats_pitching,     sub_tr_full, use_count_weights = TRUE,  seed_base = 400L)
  res_ptun     <- run_5head_tier("Decomp_Pitching_Tun", feats_pitching_tun, sub_tr_full, use_count_weights = TRUE,  seed_base = 500L)

  head_diag_dt <- rbindlist(head_diag_list)
  print(head_diag_dt)

  # ============================================================================
  # 4. PITCHER & BATTER PROCESS AGGREGATION (2020-2026, 2024-ONLY SCALING)
  # ============================================================================
  cat("\n-> [4/7] Aggregating Pitcher Decomposed Grades & Batter Process Metrics (2020-2026)...\n")
  sc_dt <- dt[idx_score, .(
    game_year, pitcher, batter, p_throws, stand, count_id,
    is_swing, is_whiff, is_bip, is_in_zone, is_chase, xwoba
  )]
  rm(dt); gc(verbose = FALSE)

  sc_dt[, `:=`(
    xrv_stuff    = res_stuff$xrv_pit,
    xrv_stuff_he = res_stuff_he$xrv_pit,
    xrv_loc      = res_loc$xrv_pit,
    xrv_pitch    = res_pitch$xrv_pit,
    xrv_ptun     = res_ptun$xrv_pit,
    p_sw_hat     = res_ptun$p_sw,
    p_wh_hat     = res_ptun$p_wh,
    xw_hat       = res_ptun$xw_h,
    ev_take      = res_ptun$ev_take,
    ev_cont      = res_ptun$ev_cont,
    ev_sw        = res_ptun$ev_sw
  )]
  rv_s_vec <- count_rv_dt$rv_strike[sc_dt$count_id]
  sc_dt[, `:=`(
    decision_voe = (is_swing - p_sw_hat) * (ev_sw - ev_take),
    bat_ctrl_rv  = fifelse(is_swing == 1L, (p_wh_hat - is_whiff) * (ev_cont - rv_s_vec), 0.0),
    contact_rv   = fifelse(is_bip == 1L & is.finite(xwoba), beta_xw * (xwoba - xw_hat), 0.0)
  )]
  sc_dt[, process_rv := decision_voe + bat_ctrl_rv + contact_rv]
  rm(res_stuff, res_stuff_he, res_loc, res_pitch, res_ptun, rv_s_vec); gc(verbose = FALSE)

  pit_gold <- sc_dt[, .(
    n_pitches         = .N,
    p_throws          = p_throws[1],
    rv100_stuff       = 100.0 * sum(xrv_stuff)    / .N,
    rv100_stuff_he    = 100.0 * sum(xrv_stuff_he) / .N,
    rv100_loc         = 100.0 * sum(xrv_loc)      / .N,
    rv100_pitch       = 100.0 * sum(xrv_pitch)    / .N,
    rv100_ptun        = 100.0 * sum(xrv_ptun)     / .N,
    whiff_pct         = 100.0 * sum(is_whiff) / pmax(1, sum(is_swing)),
    chase_pct         = 100.0 * sum(is_chase) / pmax(1, sum(is_in_zone == 0L))
  ), by = .(pitcher, game_year)]
  pit_gold[, eval_split := fcase(
    game_year <= 2023L, "train_insample",
    game_year == 2024L, "val_calibration",
    default = "test_oos"
  )]

  scale_on_2024 <- function(dt_in, col_in, min_n) {
    ref <- dt_in[game_year == 2024L & n_pitches >= min_n][[col_in]]
    mu <- mean(ref, na.rm = TRUE); sd_v <- stats::sd(ref, na.rm = TRUE)
    assert_true(is.finite(mu) && is.finite(sd_v) && sd_v > 1e-9, paste("Scaling failed for", col_in))
    100.0 + 10.0 * (dt_in[[col_in]] - mu) / sd_v
  }
  pit_gold[, `:=`(
    decomp_stuff_plus           = scale_on_2024(pit_gold, "rv100_stuff",    500L),
    decomp_stuff_he_plus        = scale_on_2024(pit_gold, "rv100_stuff_he", 500L),
    decomp_location_plus        = scale_on_2024(pit_gold, "rv100_loc",      500L),
    decomp_pitching_plus        = scale_on_2024(pit_gold, "rv100_pitch",    500L),
    decomp_pitching_tunnel_plus = scale_on_2024(pit_gold, "rv100_ptun",     500L)
  )]
  pit_gold[, tunnel_lift_plus := decomp_pitching_tunnel_plus - decomp_pitching_plus]

  bat_gold <- sc_dt[, .(
    n_pitches         = .N,
    n_swings          = sum(is_swing),
    n_bip             = sum(is_bip == 1L & is.finite(xwoba)),
    decision_rv100    = 100.0 * sum(decision_voe) / .N,
    bat_ctrl_rv100    = 100.0 * sum(bat_ctrl_rv)  / .N,
    contact_rv100     = 100.0 * sum(contact_rv)   / .N,
    process_rv100     = 100.0 * sum(process_rv)   / .N,
    z_swing_pct       = sum(is_swing == 1L & is_in_zone == 1L) / pmax(1, sum(is_in_zone == 1L)),
    o_swing_pct       = sum(is_swing == 1L & is_in_zone == 0L) / pmax(1, sum(is_in_zone == 0L)),
    sc_whiff_pct      = sum(is_whiff) / pmax(1, sum(is_swing)),
    sc_bip_xwoba      = sum(fifelse(is_bip == 1L & is.finite(xwoba), xwoba, 0.0)) / pmax(1, sum(is_bip == 1L & is.finite(xwoba)))
  ), by = .(batter, game_year)]
  bat_gold[, zone_chase_proxy := z_swing_pct - o_swing_pct]
  bat_gold[, eval_split := fcase(
    game_year <= 2023L, "train_insample",
    game_year == 2024L, "val_calibration",
    default = "test_oos"
  )]
  bat_gold[, `:=`(
    decision_plus       = scale_on_2024(bat_gold, "decision_rv100", 1000L),
    bat_control_plus    = scale_on_2024(bat_gold, "bat_ctrl_rv100", 1000L),
    contact_plus        = scale_on_2024(bat_gold, "contact_rv100",  1000L),
    batter_process_plus = scale_on_2024(bat_gold, "process_rv100",  1000L)
  )]
  rm(sc_dt); gc(verbose = FALSE)

  pit_gold <- merge(pit_gold, cw_names[, .(pitcher = mlbam, pitcher_name = player_name, key_fangraphs = key_fg)], by = "pitcher", all.x = TRUE)
  bat_gold <- merge(bat_gold, cw_names[, .(batter = mlbam, batter_name = player_name, key_fangraphs = key_fg)],  by = "batter",  all.x = TRUE)
  assert_true(mean(!is.na(pit_gold$pitcher_name)) > 0.99 && mean(!is.na(bat_gold$batter_name)) > 0.99, "Chadwick name join rate below 99%")

  pit_gold <- merge(pit_gold, fg_p, by = c("pitcher", "game_year"), all.x = TRUE)
  pit_gold <- merge(pit_gold, s3_dt, by = c("pitcher", "game_year"), all.x = TRUE)
  bat_gold <- merge(bat_gold, fg_b, by = c("batter", "game_year"), all.x = TRUE)

  # ============================================================================
  # 5. LOCKED OUT-OF-SAMPLE BENCHMARKS (2024 -> 2025 & 2025 -> 2026)
  # ============================================================================
  cat("-> [5/7] Running locked 2024->2025 & 2025->2026 Pitcher & Batter OOS benchmarks...\n")

  p24 <- pit_gold[game_year == 2024L & n_pitches >= 500L &
                  is.finite(s3_pitching_plus) & is.finite(fg_pitching_plus)]
  p25 <- pit_gold[game_year == 2025L & is.finite(fg_tbf) & fg_tbf >= 100,
                  .(pitcher, tbf_25 = fg_tbf, ip_25 = fg_ip, siera_25 = fg_siera,
                    fip_25 = fg_fip, k_pct_25 = fg_k_pct, bb_pct_25 = fg_bb_pct)]
  p_eval <- merge(p24, p25, by = "pitcher")
  assert_true(nrow(p_eval) >= 250L, sprintf("Pitcher 24->25 matched cohort too small: %d", nrow(p_eval)))

  pit_models <- list(
    "Study03 Stuff+"        = "s3_stuff_plus",
    "Study03 Pitching+"     = "s3_pitching_plus",
    "Study03 Pitch+Tunnel"  = "s3_pitching_tunnel_plus",
    "Decomp Stuff+ (15-23)" = "decomp_stuff_plus",
    "Decomp Stuff+ (20-23)" = "decomp_stuff_he_plus",
    "Decomp Location+"      = "decomp_location_plus",
    "Decomp Pitching+"      = "decomp_pitching_plus",
    "Decomp Pitch+Tunnel"   = "decomp_pitching_tunnel_plus",
    "FanGraphs Stuff+"      = "fg_stuff_plus",
    "FanGraphs Pitching+"   = "fg_pitching_plus"
  )
  pit_targets <- list(siera_25 = "tbf_25", fip_25 = "ip_25", k_pct_25 = "tbf_25", bb_pct_25 = "tbf_25")
  pb_rows <- list()
  for (mn in names(pit_models)) {
    col_x <- pit_models[[mn]]
    for (tn in names(pit_targets)) {
      w_c <- pit_targets[[tn]]
      pb_rows[[length(pb_rows) + 1L]] <- data.table(
        domain = "Pitcher", eval_type = "OOS_24_to_25_Validity",
        model = mn, target = tn, n_eval = nrow(p_eval),
        pearson_r = stats::cor(p_eval[[col_x]], p_eval[[tn]], use = "complete.obs"),
        w_r       = w_cor(p_eval[[col_x]], p_eval[[tn]], p_eval[[w_c]])
      )
    }
  }
  pitch_bench_dt <- rbindlist(pb_rows)
  cat("\n=== PITCHER 2024 -> 2025 LOCKED OOS BENCHMARK (n =", nrow(p_eval), ") ===\n")
  print(pitch_bench_dt[target %in% c("siera_25", "fip_25"),
                       .(target, model, pearson_r = round(pearson_r, 4), w_r = round(w_r, 4))][order(target, -abs(w_r))])

  p_yoy_24_25 <- merge(
    pit_gold[game_year == 2024L & n_pitches >= 500L, .(pitcher, st24 = decomp_stuff_plus, lo24 = decomp_location_plus, pt24 = decomp_pitching_tunnel_plus)],
    pit_gold[game_year == 2025L & n_pitches >= 500L, .(pitcher, n25 = n_pitches, st25 = decomp_stuff_plus, lo25 = decomp_location_plus, pt25 = decomp_pitching_tunnel_plus)],
    by = "pitcher"
  )
  pit_yoy_stuff <- w_cor(p_yoy_24_25$st24, p_yoy_24_25$st25, p_yoy_24_25$n25)
  pit_yoy_loc   <- w_cor(p_yoy_24_25$lo24, p_yoy_24_25$lo25, p_yoy_24_25$n25)
  pit_yoy_ptun  <- w_cor(p_yoy_24_25$pt24, p_yoy_24_25$pt25, p_yoy_24_25$n25)
  cat(sprintf("   Pitcher 2024->2025 YoY Stability (n=%d): Decomp Stuff+ = %.4f | Location+ = %.4f | Pitch+Tun = %.4f\n",
              nrow(p_yoy_24_25), pit_yoy_stuff, pit_yoy_loc, pit_yoy_ptun))

  b24 <- bat_gold[game_year == 2024L & is.finite(fg_pa) & fg_pa >= 200]
  b25 <- bat_gold[game_year == 2025L & is.finite(fg_pa) & fg_pa >= 200,
                  .(batter, pa_25 = fg_pa, woba_25 = fg_woba, xwoba_25 = fg_xwoba,
                    k_pct_25 = fg_k_pct, bb_pct_25 = fg_bb_pct, iso_25 = fg_iso,
                    dec_25 = decision_plus, bctrl_25 = bat_control_plus, cont_25 = contact_plus,
                    proc_25 = batter_process_plus, zc_25 = zone_chase_proxy)]
  b_eval <- merge(b24, b25, by = "batter")
  assert_true(nrow(b_eval) >= 220L, sprintf("Batter 24->25 matched cohort too small: %d", nrow(b_eval)))

  bat_bench_dt <- rbindlist(list(
    data.table(eval_type = "YoY_Stability_24_25", target = "YoY", metric = "BatControl+ (Whiff VoE)", tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$bat_control_plus,    b_eval$bctrl_25, b_eval$pa_25)),
    data.table(eval_type = "YoY_Stability_24_25", target = "YoY", metric = "Zone-Chase (Z-O% Proxy)", tier = "Study 04 Proxy",  n_eval = nrow(b_eval), value = w_cor(b_eval$zone_chase_proxy,    b_eval$zc_25,    b_eval$pa_25)),
    data.table(eval_type = "YoY_Stability_24_25", target = "YoY", metric = "Contact+ (xwOBA VoE)",    tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$contact_plus,        b_eval$cont_25,  b_eval$pa_25)),
    data.table(eval_type = "YoY_Stability_24_25", target = "YoY", metric = "BatterProcess+ (Total)",  tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$batter_process_plus, b_eval$proc_25,  b_eval$pa_25)),
    data.table(eval_type = "YoY_Stability_24_25", target = "YoY", metric = "Decision+ (True SEAGER)", tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$decision_plus,       b_eval$dec_25,   b_eval$pa_25)),
    data.table(eval_type = "YoY_Stability_24_25", target = "YoY", metric = "FG xwOBA",                tier = "Study 04 Proxy",  n_eval = nrow(b_eval), value = w_cor(b_eval$fg_xwoba,            b_eval$xwoba_25, b_eval$pa_25)),
    data.table(eval_type = "YoY_Stability_24_25", target = "YoY", metric = "Raw wOBA",                tier = "Outcome Baseline",n_eval = nrow(b_eval), value = w_cor(b_eval$fg_woba,             b_eval$woba_25,  b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 wOBA", metric = "BatterProcess+ (Total)", tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$batter_process_plus, b_eval$woba_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 wOBA", metric = "FG xwOBA",               tier = "Study 04 Proxy",  n_eval = nrow(b_eval), value = w_cor(b_eval$fg_xwoba,            b_eval$woba_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 wOBA", metric = "Contact+ (xwOBA VoE)",   tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$contact_plus,        b_eval$woba_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 wOBA", metric = "Raw wOBA",               tier = "Outcome Baseline",n_eval = nrow(b_eval), value = w_cor(b_eval$fg_woba,             b_eval$woba_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 wOBA", metric = "Decision+ (True SEAGER)",tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$decision_plus,       b_eval$woba_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 wOBA", metric = "Zone-Chase (Z-O% Proxy)",tier = "Study 04 Proxy",  n_eval = nrow(b_eval), value = w_cor(b_eval$zone_chase_proxy,    b_eval$woba_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 BB%", metric = "Decision+ (True SEAGER)", tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$decision_plus,    b_eval$bb_pct_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 BB%", metric = "Zone-Chase (Z-O% Proxy)", tier = "Study 04 Proxy",  n_eval = nrow(b_eval), value = w_cor(b_eval$zone_chase_proxy, b_eval$bb_pct_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 BB%", metric = "Prior BB%",               tier = "Outcome Baseline",n_eval = nrow(b_eval), value = w_cor(b_eval$fg_bb_pct,        b_eval$bb_pct_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 K%", metric = "BatControl+ (Whiff VoE)",  tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$bat_control_plus, b_eval$k_pct_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 K%", metric = "Decision+ (True SEAGER)",  tier = "EXP-005 Process", n_eval = nrow(b_eval), value = w_cor(b_eval$decision_plus,    b_eval$k_pct_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 K%", metric = "Zone-Chase (Z-O% Proxy)",  tier = "Study 04 Proxy",  n_eval = nrow(b_eval), value = w_cor(b_eval$zone_chase_proxy, b_eval$k_pct_25, b_eval$pa_25)),
    data.table(eval_type = "OOS_24_to_25_Validity", target = "2025 K%", metric = "Prior K%",                 tier = "Outcome Baseline",n_eval = nrow(b_eval), value = w_cor(b_eval$fg_k_pct,         b_eval$k_pct_25, b_eval$pa_25))
  ))

  lm_base <- stats::lm(woba_25 ~ fg_woba + fg_xwoba + zone_chase_proxy, data = b_eval, weights = pa_25)
  lm_proc <- stats::lm(woba_25 ~ fg_woba + fg_xwoba + zone_chase_proxy + decision_plus + bat_control_plus + contact_plus, data = b_eval, weights = pa_25)
  inc_p <- stats::anova(lm_base, lm_proc)$`Pr(>F)`[2]
  cat("\n=== BATTER 2024 -> 2025 PROCESS BENCHMARK (n =", nrow(b_eval), ") ===\n")
  print(bat_bench_dt[, .(eval_type, target, metric, value = round(value, 4))])
  cat(sprintf("   Nested F-test adding (Decision+, BatControl+, Contact+) over (wOBA+xwOBA+Z-O%%): R^2 %.4f -> %.4f (p = %.4g)\n",
              summary(lm_base)$r.squared, summary(lm_proc)$r.squared, inc_p))

  b25_26 <- merge(
    bat_gold[game_year == 2025L & n_pitches >= 1000L, .(batter, proc25 = batter_process_plus, dec25 = decision_plus, bctrl25 = bat_control_plus, cont25 = contact_plus)],
    bat_gold[game_year == 2026L & n_pitches >= 1000L, .(batter, n26 = n_pitches, proc26 = batter_process_plus, dec26 = decision_plus, bctrl26 = bat_control_plus, cont26 = contact_plus)],
    by = "batter"
  )
  cat(sprintf("   2025->2026 Untouched test_oos Batter Stability (n=%d): Process+=%.4f | Decision+=%.4f | BatControl+=%.4f | Contact+=%.4f\n",
              nrow(b25_26), w_cor(b25_26$proc25, b25_26$proc26, b25_26$n26),
              w_cor(b25_26$dec25, b25_26$dec26, b25_26$n26),
              w_cor(b25_26$bctrl25, b25_26$bctrl26, b25_26$n26),
              w_cor(b25_26$cont25, b25_26$cont26, b25_26$n26)))

  # ============================================================================
  # 6. WRITE GOLD ZSTD PARQUETS (STRICT FINITENESS ASSERTIONS)
  # ============================================================================
  cat("\n-> [6/7] Writing Gold ZSTD Parquets...\n")
  setorder(pit_gold, game_year, pitcher)
  setorder(bat_gold, game_year, batter)
  for (cc in c("decomp_stuff_plus", "decomp_stuff_he_plus", "decomp_location_plus",
               "decomp_pitching_plus", "decomp_pitching_tunnel_plus")) {
    assert_true(all(is.finite(pit_gold[[cc]])), paste("Non-finite pitcher Gold column:", cc))
  }
  for (cc in c("decision_plus", "bat_control_plus", "contact_plus", "batter_process_plus")) {
    assert_true(all(is.finite(bat_gold[[cc]])), paste("Non-finite batter Gold column:", cc))
  }

  bench_export <- rbindlist(list(
    pitch_bench_dt[, .(domain, eval_type, target, model_or_metric = model, n_eval, pearson_r, w_r)],
    bat_bench_dt[,   .(domain = "Batter", eval_type, target, model_or_metric = metric, n_eval, pearson_r = NA_real_, w_r = value)]
  ))

  arrow::write_parquet(pit_gold,     file.path(processed_dir, "pitcher_decomp_pitching_plus.parquet"), compression = "zstd")
  arrow::write_parquet(bat_gold,     file.path(processed_dir, "batter_process_metrics.parquet"),       compression = "zstd")
  arrow::write_parquet(count_rv_dt,  file.path(processed_dir, "decomp_count_rv_table.parquet"),        compression = "zstd")
  arrow::write_parquet(bench_export, file.path(processed_dir, "decomp_benchmark_ablation.parquet"),    compression = "zstd")
  cat(sprintf("   Wrote Gold Parquets: pitcher_decomp (%s rows), batter_process (%s rows)\n",
              format(nrow(pit_gold), big.mark = ","), format(nrow(bat_gold), big.mark = ",")))

  # ============================================================================
  # 7. DARK CYBER 4-ASSET VISUAL SUITE (DELEGATED TO LEAN TOP-LEVEL BUILDER)
  # ============================================================================
  cat("-> [7/7] Generating Dark Cyber 4-Asset Visual Suite & Interactive Plotly Explorers...\n")
  p25_viz <- pit_gold[game_year == 2025L & eval_split == "test_oos" & n_pitches >= 750L & !is.na(pitcher_name)]
  p25_viz <- merge(
    p25_viz,
    pit_gold[game_year == 2026L & eval_split == "test_oos", .(pitcher, ptun_2026 = decomp_pitching_tunnel_plus, stuff_2026 = decomp_stuff_plus)],
    by = "pitcher", all.x = TRUE
  )
  p25_viz[, label_flag := FALSE]
  p25_viz[pitcher %in% unique(c(
    p25_viz[order(-decomp_pitching_tunnel_plus)][1:6]$pitcher,
    p25_viz[order(-decomp_stuff_plus)][1:4]$pitcher,
    p25_viz[order(-decomp_location_plus)][1:4]$pitcher
  )), label_flag := TRUE]
  p25_viz[, hover_txt := sprintf(
    "<b>%s</b> (%s | %d pitches, 2025 test_oos)<br><b>Decomp Pitch+Tunnel: %.1f</b> | Pitching+: %.1f<br>Decomp Stuff+: %.1f | Location+: %.1f | Tunnel Lift: %+.1f<br>2025 FIP: %s | SIERA: %s | Whiff%%: %.1f%%%s",
    pitcher_name, fifelse(!is.na(team), team, p_throws), n_pitches,
    decomp_pitching_tunnel_plus, decomp_pitching_plus,
    decomp_stuff_plus, decomp_location_plus, tunnel_lift_plus,
    fifelse(is.finite(fg_fip), sprintf("%.2f", fg_fip), "NA"),
    fifelse(is.finite(fg_siera), sprintf("%.2f", fg_siera), "NA"), whiff_pct,
    fifelse(is.finite(ptun_2026), sprintf("<br><b>2026 test_oos Pitch+Tun: %.1f</b> (Stuff+: %.1f)", ptun_2026, stuff_2026), "")
  )]

  b25_viz <- bat_gold[game_year == 2025L & eval_split == "test_oos" & n_pitches >= 1200L & !is.na(batter_name)]
  b25_viz <- merge(
    b25_viz,
    bat_gold[game_year == 2026L & eval_split == "test_oos", .(batter, proc_2026 = batter_process_plus, dec_2026 = decision_plus)],
    by = "batter", all.x = TRUE
  )
  b25_viz[, label_flag := FALSE]
  b25_viz[batter %in% unique(c(
    b25_viz[order(-batter_process_plus)][1:6]$batter,
    b25_viz[order(-decision_plus)][1:4]$batter,
    b25_viz[order(-contact_plus)][1:4]$batter,
    b25_viz[order(decision_plus)][1:2]$batter
  )), label_flag := TRUE]
  b25_viz[, hover_txt := sprintf(
    "<b>%s</b> (%s | %d pitches seen, 2025 test_oos)<br><b>BatterProcess+: %.1f</b> (RV/100: %+.2f)<br>Decision+: %.1f | BatControl+: %.1f | Contact+: %.1f<br>2025 wOBA: %s | xwOBA: %s | K%%: %s | BB%%: %s%s",
    batter_name, fifelse(!is.na(team), team, "MLB"), n_pitches,
    batter_process_plus, process_rv100,
    decision_plus, bat_control_plus, contact_plus,
    fifelse(is.finite(fg_woba),  sprintf(".%03d", round(fg_woba * 1000)), "NA"),
    fifelse(is.finite(fg_xwoba), sprintf(".%03d", round(fg_xwoba * 1000)), "NA"),
    fifelse(is.finite(fg_k_pct), sprintf("%.1f%%", 100 * fg_k_pct), "NA"),
    fifelse(is.finite(fg_bb_pct),sprintf("%.1f%%", 100 * fg_bb_pct), "NA"),
    fifelse(is.finite(proc_2026), sprintf("<br><b>2026 test_oos BatterProcess+: %.1f</b> (Dec+: %.1f)", proc_2026, dec_2026), "")
  )]

  build_decomp_visuals(count_rv_dt, pitch_bench_dt, bat_bench_dt,
                       p25_viz, b25_viz, head_diag_dt, models_dir, assets_dir)

  # ============================================================================
  # 8. LOG EXPERIMENT REGISTRY (6-DIGIT PRECISION) & PRINT LEADERBOARDS
  # ============================================================================
  get_pr <- function(mn, tn) {
    v <- pitch_bench_dt[model == mn & target == tn]$w_r
    assert_true(length(v) == 1L && is.finite(v), paste("get_pr failed:", mn, tn))
    round(v, 6)
  }
  get_br <- function(et, tg, mt) {
    v <- bat_bench_dt[eval_type == et & target == tg & metric == mt]$value
    assert_true(length(v) == 1L && is.finite(v), paste("get_br failed:", et, tg, mt))
    round(v, 6)
  }

  mlbr::lake_log_experiment(
    id = "EXP-005-DECOMPOSED-PITCHING-DECISIONS",
    title = "Context-Neutral Markov Decomposed Pitching+ & Pitch-Level Batter Process Engine (Decision+, BatControl+, Contact+)",
    tier = 1,
    status = "VALIDATED",
    summary = "Replaced single-head raw delta_run_exp regression with a strictly chronological 5-head Markov probability decomposition (p_swing, p_cstr|take, p_whiff|swing, p_foul|contact, E[xwOBA|BIP]) priced via 2015-2023 context-neutral 12-count linear weights. Simultaneously derived zero-mean pitch-level batter Decision+ (true SEAGER-class Value Over Expected Choice), BatControl+ (whiff avoidance over expected), Contact+ (xwOBA over expected), and additive BatterProcess+.",
    hypotheses = c(
      "Decomposing pitch value into 5 probability heads priced by context-neutral 12-count linear weights eliminates base-out delta_run_exp variance and improves locked 2024->2025 SIERA/FIP forecasting over Study 03.",
      "Feeding v_statcast_tunneling into p_swing and p_whiff|swing improves validation logloss and Pitching+Tunnel validity.",
      "Hawk-Eye 2020-2023 Decomp Stuff+ ablation tests tracking-era homogeneity vs. 2015-2023 full volume.",
      "Pitch-level Decision+, BatControl+, and Contact+ add statistically significant next-year wOBA/BB%/K% forecasting signal beyond aggregate Z-O% and raw xwOBA."
    ),
    formulas = list(
      bip_rv_map = "delta_run_exp ~ factor(count_id) + beta * xwoba (fit strictly on 2015-2023 BIPs)",
      ev_take = "p_cs * rv_strike(b,s) + (1 - p_cs) * rv_ball(b,s)",
      ev_swing = "p_wh * rv_strike(b,s) + (1 - p_wh) * (p_fl * rv_foul(b,s) + (1 - p_fl) * rv_bip(b,s,xwoba_hat))",
      decision_voe = "(is_swing - p_sw_hat) * (ev_swing - ev_take)",
      bat_ctrl_rv = "is_swing * (p_wh_hat - is_whiff) * (ev_contact - rv_strike(b,s))",
      contact_rv = "is_bip * beta_xw * (xwoba - xwoba_hat)",
      batter_process_rv = "decision_voe + bat_ctrl_rv + contact_rv"
    ),
    metrics = list(
      bip_xwoba_rv_beta = round(beta_xw, 6),
      val_ll_ptun_swing = round(head_diag_dt[tier == "Decomp_Pitching_Tun"]$val_ll_swing, 6),
      val_ll_ptun_whiff = round(head_diag_dt[tier == "Decomp_Pitching_Tun"]$val_ll_whiff, 6),
      val_ll_pitch_whiff = round(head_diag_dt[tier == "Decomp_Pitching"]$val_ll_whiff, 6),
      pitcher_24_25_s3_pitching_siera_wr = get_pr("Study03 Pitching+", "siera_25"),
      pitcher_24_25_decomp_pitching_siera_wr = get_pr("Decomp Pitching+", "siera_25"),
      pitcher_24_25_decomp_ptun_siera_wr = get_pr("Decomp Pitch+Tunnel", "siera_25"),
      pitcher_24_25_fg_pitching_siera_wr = get_pr("FanGraphs Pitching+", "siera_25"),
      pitcher_24_25_decomp_stuff_siera_wr = get_pr("Decomp Stuff+ (15-23)", "siera_25"),
      pitcher_24_25_decomp_stuff_he_siera_wr = get_pr("Decomp Stuff+ (20-23)", "siera_25"),
      pitcher_24_25_s3_pitching_fip_wr = get_pr("Study03 Pitching+", "fip_25"),
      pitcher_24_25_decomp_ptun_fip_wr = get_pr("Decomp Pitch+Tunnel", "fip_25"),
      pitcher_24_25_fg_pitching_fip_wr = get_pr("FanGraphs Pitching+", "fip_25"),
      pitcher_24_25_yoy_decomp_stuff = round(pit_yoy_stuff, 6),
      pitcher_24_25_yoy_decomp_ptun = round(pit_yoy_ptun, 6),
      batter_24_25_yoy_decision_plus = get_br("YoY_Stability_24_25", "YoY", "Decision+ (True SEAGER)"),
      batter_24_25_yoy_bat_control_plus = get_br("YoY_Stability_24_25", "YoY", "BatControl+ (Whiff VoE)"),
      batter_24_25_yoy_contact_plus = get_br("YoY_Stability_24_25", "YoY", "Contact+ (xwOBA VoE)"),
      batter_24_25_yoy_process_plus = get_br("YoY_Stability_24_25", "YoY", "BatterProcess+ (Total)"),
      batter_24_25_oos_process_woba_wr = get_br("OOS_24_to_25_Validity", "2025 wOBA", "BatterProcess+ (Total)"),
      batter_24_25_oos_xwoba_woba_wr = get_br("OOS_24_to_25_Validity", "2025 wOBA", "FG xwOBA"),
      batter_24_25_oos_decision_bb_wr = get_br("OOS_24_to_25_Validity", "2025 BB%", "Decision+ (True SEAGER)"),
      batter_24_25_oos_zoproxy_bb_wr = get_br("OOS_24_to_25_Validity", "2025 BB%", "Zone-Chase (Z-O% Proxy)"),
      batter_24_25_oos_batctrl_k_wr = get_br("OOS_24_to_25_Validity", "2025 K%", "BatControl+ (Whiff VoE)"),
      batter_24_25_inc_r2_base = round(summary(lm_base)$r.squared, 6),
      batter_24_25_inc_r2_proc = round(summary(lm_proc)$r.squared, 6),
      batter_24_25_inc_ftest_p = signif(inc_p, 6),
      gold_pitcher_decomp_rows = nrow(pit_gold),
      gold_batter_process_rows = nrow(bat_gold)
    ),
    features = c("decomp_stuff_plus", "decomp_stuff_he_plus", "decomp_location_plus",
                 "decomp_pitching_plus", "decomp_pitching_tunnel_plus", "tunnel_lift_plus",
                 "decision_plus", "bat_control_plus", "contact_plus", "batter_process_plus"),
    artifacts = list(
      spec = "specs/05_decomposed_pitching_swing_decisions.md",
      script = "models/05_decomposed_pitching_swing_decisions.R",
      study_qmd = "site/analyses/05_decomposed_pitching_decisions.qmd",
      workbook_r = "workbooks/lab05_decomposed_pitching_decisions_workbook.R",
      workbook_qmd = "site/workbooks/lab05_decomposed_pitching_decisions_workbook.qmd",
      gold_pitcher_decomp = "data/processed/pitcher_decomp_pitching_plus.parquet",
      gold_batter_process = "data/processed/batter_process_metrics.parquet",
      gold_count_rv = "data/processed/decomp_count_rv_table.parquet",
      gold_benchmarks = "data/processed/decomp_benchmark_ablation.parquet"
    ),
    sync_manifest = TRUE, lake = lake
  )

  cat("\n=== TOP 10 2025 LOCKED test_oos PITCHERS BY DECOMP PITCHING+TUNNEL (>= 1,000 Pitches) ===\n")
  print(head(pit_gold[game_year == 2025L & eval_split == "test_oos" & n_pitches >= 1000L][order(-decomp_pitching_tunnel_plus),
             .(pitcher_name, team, n_pitches, stuff = round(decomp_stuff_plus, 1), loc = round(decomp_location_plus, 1),
               pitch = round(decomp_pitching_plus, 1), ptun = round(decomp_pitching_tunnel_plus, 1),
               fg_fip = round(fg_fip, 2), fg_siera = round(fg_siera, 2))], 10))

  cat("\n=== TOP 10 2025 LOCKED test_oos BATTERS BY BATTERPROCESS+ (>= 1,500 Pitches Seen) ===\n")
  print(head(bat_gold[game_year == 2025L & eval_split == "test_oos" & n_pitches >= 1500L][order(-batter_process_plus),
             .(batter_name, team, n_pitches, proc = round(batter_process_plus, 1), dec = round(decision_plus, 1),
               bctrl = round(bat_control_plus, 1), cont = round(contact_plus, 1),
               woba = round(fg_woba, 3), xwoba = round(fg_xwoba, 3))], 10))

  cat("\n=== TOP 10 2025 LOCKED test_oos BATTERS BY DECISION+ (TRUE SEAGER, >= 1,500 Pitches) ===\n")
  print(head(bat_gold[game_year == 2025L & eval_split == "test_oos" & n_pitches >= 1500L][order(-decision_plus),
             .(batter_name, team, n_pitches, dec = round(decision_plus, 1), dec_rv100 = round(decision_rv100, 2),
               bb_pct = round(100 * fg_bb_pct, 1), k_pct = round(100 * fg_k_pct, 1), woba = round(fg_woba, 3))], 10))

  cat(sprintf("\n=== EXP-005 COMPLETED CLEANLY IN %.1f SECONDS ===\n", proc.time()[["elapsed"]] - t0_total))
}

run_exp005()
