#!/usr/bin/env Rscript
# ==============================================================================
# Study 04 (EXP-004-PLAYER-PROJECTIONS):
# Chronological True-Talent Player Projections — Tango Marcel, Empirical
# Delta-Method Aging Curves & Tier 1 Statcast Physics Priors
# ==============================================================================
# Spec: specs/04_player_projections_marcel_aging.md
# Audits Resolved: sabermetrics-method-auditor + silent-failure-hunter (0 BLOCKERs)
# ==============================================================================

suppressPackageStartupMessages({
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(arrow)
  library(data.table)
  library(glmnet)
  library(splines)
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

w_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
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

w_rmse <- function(pred, actual, w) {
  ok <- is.finite(pred) & is.finite(actual) & is.finite(w) & w > 0
  assert_true(sum(ok) > 0L, "w_rmse received 0 valid finite rows")
  sqrt(sum(w[ok] * (pred[ok] - actual[ok])^2) / sum(w[ok]))
}

w_mae <- function(pred, actual, w) {
  ok <- is.finite(pred) & is.finite(actual) & is.finite(w) & w > 0
  assert_true(sum(ok) > 0L, "w_mae received 0 valid finite rows")
  sum(w[ok] * abs(pred[ok] - actual[ok])) / sum(w[ok])
}

w_cor <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(ok) < 3L) return(NA_real_)
  ww <- w[ok] / sum(w[ok])
  mx <- sum(ww * x[ok]); my <- sum(ww * y[ok])
  vx <- sum(ww * (x[ok] - mx)^2); vy <- sum(ww * (y[ok] - my)^2)
  if (vx <= 0 || vy <= 0) return(NA_real_)
  sum(ww * (x[ok] - mx) * (y[ok] - my)) / sqrt(vx * vy)
}

w_cal_slope <- function(pred, actual, w) {
  ok <- is.finite(pred) & is.finite(actual) & is.finite(w) & w > 0
  if (sum(ok) < 5L || stats::sd(pred[ok]) < 1e-12) return(list(slope = NA_real_, intercept = NA_real_))
  fit <- stats::lm(actual[ok] ~ pred[ok], weights = w[ok])
  co <- stats::coef(fit)
  list(slope = unname(co[2]), intercept = unname(co[1]))
}

paired_boot_rmse_diff <- function(pred_a, pred_b, actual, w, n_boot = 400L) {
  ok <- is.finite(pred_a) & is.finite(pred_b) & is.finite(actual) & is.finite(w) & w > 0
  n <- sum(ok)
  if (n < 10L) return(list(diff = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_, p_better = NA_real_))
  ea2 <- (pred_a[ok] - actual[ok])^2
  eb2 <- (pred_b[ok] - actual[ok])^2
  ww <- w[ok]
  point_diff <- sqrt(sum(ww * ea2) / sum(ww)) - sqrt(sum(ww * eb2) / sum(ww))
  diffs <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample.int(n, n, replace = TRUE)
    wb <- ww[idx]; sw <- sum(wb)
    diffs[b] <- sqrt(sum(wb * ea2[idx]) / sw) - sqrt(sum(wb * eb2[idx]) / sw)
  }
  qs <- stats::quantile(diffs, probs = c(0.025, 0.975), names = FALSE)
  list(diff = point_diff, ci_lo = qs[1], ci_hi = qs[2], p_better = mean(diffs < 0))
}

# Tom Tango (2004) Marcel published direct age multiplier (Step 5)
tango_age_multiplier <- function(age) {
  a <- pmin(pmax(age, 20.0), 42.0)
  fifelse(a <= 29.0, 1.0 + 0.006 * (29.0 - a), 1.0 / (1.0 + 0.003 * (a - 29.0)))
}

make_proj4_plotly <- function(df) {
  force(df)
  col_cyan <- "#38bdf8"; col_emerald <- "#10b981"; col_amber <- "#f59e0b"; col_rose <- "#f43f5e"
  plotly::plot_ly(
    data = df, x = ~obs_t1_era, y = ~proj_fip, type = "scatter", mode = "markers",
    text = ~hover_txt, hoverinfo = "text",
    marker = list(
      size = ~scales::rescale(proj_tbf, to = c(6, 17)), color = ~stuff_plus_t1,
      colorscale = list(c(0, col_rose), c(0.35, col_amber), c(0.55, "#e2e8f0"), c(0.78, col_cyan), c(1, col_emerald)),
      cmin = 85, cmax = 118, opacity = 0.85, line = list(width = 0.6, color = "#090d16"),
      colorbar = list(title = list(text = "mlbr Stuff+", font = list(color = "#e2e8f0")), tickfont = list(color = "#94a3b8"))
    )
  ) %>%
    plotly::layout(
      title = list(text = "Pitcher True-Talent Projections Explorer: 2025 ERA vs. 2026 Proj FIP (Hover for 2027 Live)",
                   font = list(color = "#f8fafc", size = 15)),
      paper_bgcolor = "#090d16", plot_bgcolor = "#111827",
      xaxis = list(title = "Observed 2025 ERA", range = c(1.8, 6.4), gridcolor = "#1e293b", color = "#e2e8f0"),
      yaxis = list(title = "mlbr Projected 2026 FIP", range = c(2.4, 5.2), gridcolor = "#1e293b", color = "#e2e8f0"),
      shapes = list(list(type = "line", x0 = 2.0, y0 = 2.0, x1 = 5.5, y1 = 5.5,
                         line = list(color = "#64748b", dash = "dash", width = 1.2))),
      hoverlabel = list(bgcolor = "#0f172a", bordercolor = col_cyan, font = list(color = "#f8fafc", size = 12))
    )
}

make_proj5_plotly <- function(df) {
  force(df)
  col_cyan <- "#38bdf8"; col_emerald <- "#10b981"; col_amber <- "#f59e0b"; col_rose <- "#f43f5e"
  plotly::plot_ly(
    data = df, x = ~obs_t1_woba, y = ~proj_woba, type = "scatter", mode = "markers",
    text = ~hover_txt, hoverinfo = "text",
    marker = list(
      size = ~scales::rescale(proj_pa, to = c(6, 17)), color = ~xwoba_skill_gap,
      colorscale = list(c(0, col_rose), c(0.35, col_amber), c(0.5, "#e2e8f0"), c(0.7, col_cyan), c(1, col_emerald)),
      cmin = -0.045, cmax = 0.045, opacity = 0.85, line = list(width = 0.6, color = "#090d16"),
      colorbar = list(title = list(text = "xwOBA - wOBA", font = list(color = "#e2e8f0")), tickfont = list(color = "#94a3b8"))
    )
  ) %>%
    plotly::layout(
      title = list(text = "Batter True-Talent Projections Explorer: 2025 wOBA vs. 2026 Proj wOBA (Hover for 2027 Live)",
                   font = list(color = "#f8fafc", size = 15)),
      paper_bgcolor = "#090d16", plot_bgcolor = "#111827",
      xaxis = list(title = "Observed 2025 wOBA", range = c(0.235, 0.445), gridcolor = "#1e293b", color = "#e2e8f0"),
      yaxis = list(title = "mlbr Projected 2026 wOBA", range = c(0.260, 0.430), gridcolor = "#1e293b", color = "#e2e8f0"),
      shapes = list(list(type = "line", x0 = 0.25, y0 = 0.25, x1 = 0.43, y1 = 0.43,
                         line = list(color = "#64748b", dash = "dash", width = 1.2))),
      hoverlabel = list(bgcolor = "#0f172a", bordercolor = col_cyan, font = list(color = "#f8fafc", size = 12))
    )
}

build_proj_visuals <- function(aging_export, stab_params, yoy_diag, backtest_metrics,
                               p26_viz, b26_viz, models_dir, assets_dir) {
  # Force all arguments immediately so no unevaluated R promises retain run_exp004()'s stack frame
  force(aging_export); force(stab_params); force(yoy_diag); force(backtest_metrics)
  force(p26_viz); force(b26_viz); force(models_dir); force(assets_dir)
  col_cyan <- "#38bdf8"; col_emerald <- "#10b981"; col_amber <- "#f59e0b"
  col_rose <- "#f43f5e"; col_violet  <- "#a78bfa"

  # --- FIGURE 1: Empirical Delta-Method Aging Curves ---
  ag_plot_dt <- aging_export[
    (group == "Batter" & metric %in% c("wOBA", "ISO")) |
    (group %in% c("SP", "RP") & metric %in% c("FIP", "FBv (mph)"))
  ]
  ag_plot_dt[, panel_label := factor(fcase(
    group == "Batter" & metric == "wOBA",      "Batters: wOBA (Pts vs Age 27)",
    group == "Batter" & metric == "ISO",       "Batters: ISO (Power vs Age 27)",
    group == "SP"     & metric == "FIP",       "Starting Pitchers: FIP (+ = Worse)",
    group == "RP"     & metric == "FIP",       "Relief Pitchers: FIP (+ = Worse)",
    group == "SP"     & metric == "FBv (mph)", "Starting Pitchers: Fastball Velo (mph)",
    group == "RP"     & metric == "FBv (mph)", "Relief Pitchers: Fastball Velo (mph)"
  ), levels = c(
    "Batters: wOBA (Pts vs Age 27)", "Batters: ISO (Power vs Age 27)",
    "Starting Pitchers: FIP (+ = Worse)", "Relief Pitchers: FIP (+ = Worse)",
    "Starting Pitchers: Fastball Velo (mph)", "Relief Pitchers: Fastball Velo (mph)"
  ))]
  ag_long <- rbindlist(list(
    ag_plot_dt[, .(age, panel_label, val = cum_delta_smooth, method = "Delta-Method (Survivor-Bias Corrected)")],
    ag_plot_dt[, .(age, panel_label, val = naive_centered,   method = "Naive Cross-Sectional (Survivor Biased)")]
  ))

  fig1 <- ggplot(ag_long, aes(x = age, y = val, color = method, linetype = method)) +
    geom_hline(yintercept = 0, color = "#334155", linewidth = 0.5, linetype = "dotted") +
    geom_vline(xintercept = 27, color = "#475569", linewidth = 0.4, linetype = "dashed") +
    geom_line(linewidth = 1.15) + facet_wrap(~ panel_label, scales = "free_y", ncol = 3) +
    scale_color_manual(values = c("Delta-Method (Survivor-Bias Corrected)" = col_cyan,
                                  "Naive Cross-Sectional (Survivor Biased)" = col_rose)) +
    scale_linetype_manual(values = c("Delta-Method (Survivor-Bias Corrected)" = "solid",
                                     "Naive Cross-Sectional (Survivor Biased)" = "dashed")) +
    scale_x_continuous(breaks = seq(21, 39, by = 3)) +
    labs(title = "Empirical Delta-Method Aging Curves vs. Cross-Sectional Survivorship Bias (2002-2025)",
         subtitle = "Midpoint harmonic-weighted within-player transitions vs. matched-cohort cross-sectional curves (anchored at Age 27 = 0).",
         x = "Baseball Age (July 1 Convention)", y = "Cumulative Change vs. Age-27 Peak Baseline",
         color = NULL, linetype = NULL,
         caption = "Source: mlbr Data Lake (v_fangraphs_pitch, v_fangraphs_bat, 2002-2025) | Matched min_exp: 15 IP / 50 TBF / 50 PA") +
    mlbr::theme_cyber(base_size = 11) +
    theme(legend.position = "top", strip.text = element_text(face = "bold", color = "#e2e8f0"))
  mlbr::save_plot_assets(strip_env(fig1), "proj01_empirical_aging_curves", data = aging_export,
                         width = 14.0, height = 7.6, dir = models_dir)

  # --- FIGURE 2: Stabilization & YoY Repeatability ---
  exp_grid <- seq(25, 700, by = 25)
  stab_curve <- stab_params[, .(exposure = exp_grid, rel_weight = exp_grid / (exp_grid + n50)), by = .(metric, domain, n50)]
  p2_left <- ggplot(stab_curve, aes(x = exposure, y = rel_weight, color = metric)) +
    geom_hline(yintercept = 0.50, linetype = "dashed", color = "#475569", linewidth = 0.5) +
    geom_line(linewidth = 1.05) + facet_wrap(~ domain, ncol = 1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_color_manual(values = c(
      "Pitcher Stuff+ (Study 03)" = col_cyan, "Pitcher K%" = col_emerald, "Pitcher BB%" = col_violet,
      "Pitcher FIP" = col_amber, "Pitcher HR/9" = "#fb923c", "Pitcher ERA" = col_rose,
      "Batter xwOBA" = col_cyan, "Batter Pulled Air%" = "#22d3ee", "Batter K%" = col_emerald,
      "Batter BB%" = col_violet, "Batter wOBA" = col_amber, "Batter BABIP" = col_rose
    )) +
    labs(title = "Empirical Bayes True-Talent Weight by Exposure",
         subtitle = "Weight w(N) = N / (N + N50) calibrated on 2006-2015",
         x = "Single-Season Exposure (TBF or PA)", y = "Weight on Observed Rate", color = NULL) +
    mlbr::theme_cyber(base_size = 10) + theme(legend.position = "right", legend.text = element_text(size = 8.5))

  p2_right <- ggplot(yoy_diag, aes(x = reorder(metric, yoy_r), y = yoy_r, fill = tier)) +
    geom_col(width = 0.72, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.3f", yoy_r)), hjust = -0.15, color = "#e2e8f0", size = 3.1, fontface = "bold") +
    coord_flip(ylim = c(0, 1.06)) + facet_wrap(~ domain, scales = "free_y", ncol = 1) +
    scale_fill_manual(values = c("Tier 1 Physics" = col_cyan, "Component Skill" = col_emerald, "Outcome Metric" = col_rose)) +
    labs(title = "2024 -> 2025 Year-over-Year Repeatability (Weighted r)",
         subtitle = "Matched cohorts (>= 200 TBF / >= 200 PA): Physics beats lagging results",
         x = NULL, y = "Exposure-Weighted YoY Pearson r", fill = NULL) +
    mlbr::theme_cyber(base_size = 10) + theme(legend.position = "top")

  fig2 <- (p2_left | p2_right) +
    plot_annotation(
      title = "Why Physics-Enhanced Marcel Works: Stabilization Speed & Year-over-Year Signal Hierarchy",
      subtitle = "Physical measurements (Bat Speed, Stuff+, EV90, Pulled Air%, Zone-Chase Z-O%) repeat far more stably than ERA or BABIP.",
      caption = "Source: mlbr Data Lake (v_fangraphs_*, v_statcast, pitcher_stuff_plus.parquet) | Zero 2025 calibration leakage",
      theme = mlbr::theme_cyber(base_size = 11)
    )
  mlbr::save_plot_assets(strip_env(fig2), "proj02_stabilization_reliability_shrinkage", data = yoy_diag,
                         width = 14.5, height = 7.8, dir = models_dir)

  # --- FIGURE 3: OOS Backtest Ablation Ladder ---
  ab_plot_dt <- backtest_metrics[
    (track == "Track3_LockedOOS_2025" & domain == "Pitcher" & metric %in% c("k_pct", "fip", "siera") &
       model %in% c("B0_LeagueAvg", "B1_PriorYear", "B2_Marcel_LinAge", "B3_Marcel_DeltaAge",
                    "M4_Statcast_Physics", "M4_mlbr_StuffPlus", "M4_Component_FIP")) |
    (track == "Track3_LockedOOS_2025" & domain == "Batter" & metric %in% c("woba", "k_pct", "iso") &
       model %in% c("B0_LeagueAvg", "B1_PriorYear", "B2_Marcel_LinAge", "B3_Marcel_DeltaAge",
                    "M4_Statcast_Physics", "M4_BatTracking"))
  ]
  ab_plot_dt[, panel := fcase(
    domain == "Pitcher" & metric == "k_pct", "Pitcher K% (2025 Locked OOS)",
    domain == "Pitcher" & metric == "fip",   "Pitcher FIP (2025 Locked OOS)",
    domain == "Pitcher" & metric == "siera", "Pitcher SIERA (2025 Locked OOS)",
    domain == "Batter"  & metric == "woba",  "Batter wOBA (2025 Locked OOS)",
    domain == "Batter"  & metric == "k_pct", "Batter K% (2025 Locked OOS)",
    domain == "Batter"  & metric == "iso",   "Batter ISO (2025 Locked OOS)"
  )]
  ab_plot_dt[, model_short := factor(fcase(
    model == "B0_LeagueAvg",        "B0: League Avg",
    model == "B1_PriorYear",        "B1: Prior Year",
    model == "B2_Marcel_LinAge",    "B2: Pure Marcel",
    model == "B3_Marcel_DeltaAge",  "B3: Marcel+DeltaAge",
    model == "M4_Statcast_Physics", "M4: +Statcast Physics",
    model == "M4_mlbr_StuffPlus",   "M4+: +mlbr Stuff+",
    model == "M4_Component_FIP",    "M4+: Component FIP",
    model == "M4_BatTracking",      "M4+: +Bat Tracking"
  ), levels = rev(c("B0: League Avg", "B1: Prior Year", "B2: Pure Marcel", "B3: Marcel+DeltaAge",
                    "M4: +Statcast Physics", "M4+: +mlbr Stuff+", "M4+: Component FIP", "M4+: +Bat Tracking")))]
  ab_plot_dt[, is_best := (w_rmse == min(w_rmse)), by = panel]
  ab_plot_dt[, bar_col := fcase(is_best, "Best OOS Model", grepl("^M4", model), "Tier 1 Physics",
                                grepl("^B3|^B2", model), "Marcel Baseline", default = "Naive Baseline")]

  fig3 <- ggplot(ab_plot_dt, aes(x = model_short, y = w_rmse, fill = bar_col)) +
    geom_col(width = 0.68, alpha = 0.93) +
    geom_text(aes(label = sprintf("%.4f", w_rmse)), hjust = -0.1, color = "#e2e8f0", size = 3.0, fontface = "bold") +
    coord_flip() + facet_wrap(~ panel, scales = "free", ncol = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.24))) +
    scale_fill_manual(values = c("Best OOS Model" = col_emerald, "Tier 1 Physics" = col_cyan,
                                 "Marcel Baseline" = col_amber, "Naive Baseline" = col_rose)) +
    labs(title = "Locked 2024 -> 2025 Out-of-Sample Weighted RMSE Ladder (Lower Is Better)",
         subtitle = "Strict chronological validation across B0 -> B1 -> true Tango B2 -> B3 -> M4 (Pitchers TBF >= 100, Batters PA >= 100).",
         x = NULL, y = "Exposure-Weighted Out-of-Sample RMSE", fill = NULL,
         caption = "Source: mlbr EXP-004 Chronological Backtest | Zero 2025 target leakage; 2024 Stuff+ = val_calibration") +
    mlbr::theme_cyber(base_size = 10.5) +
    theme(legend.position = "top", strip.text = element_text(face = "bold", color = "#e2e8f0"))
  mlbr::save_plot_assets(strip_env(fig3), "proj03_oos_backtest_ablation", data = backtest_metrics,
                         width = 14.2, height = 7.8, dir = models_dir)

  # --- FIGURE 4: Pitcher Frontier (Static + Interactive Plotly) ---
  fig4_static <- ggplot(p26_viz, aes(x = obs_t1_era, y = proj_fip, color = stuff_plus_t1, size = proj_tbf)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#64748b", linewidth = 0.6) +
    geom_point(alpha = 0.82) +
    ggrepel::geom_text_repel(
      data = p26_viz[label_flag == TRUE], aes(label = player_name), color = "#f8fafc",
      size = 3.1, fontface = "bold", box.padding = 0.35, max.overlaps = 20, segment.color = "#475569", show.legend = FALSE
    ) +
    scale_color_gradientn(colors = c(col_rose, col_amber, "#e2e8f0", col_cyan, col_emerald),
                          limits = c(85, 118), oob = scales::squish) +
    scale_size_continuous(range = c(1.8, 5.5), guide = "none") +
    coord_cartesian(xlim = c(2.0, 6.2), ylim = c(2.5, 5.1)) +
    annotate("text", x = 5.4, y = 2.95, label = "BUY LOW / UNLUCKY\n(Proj FIP << 2025 ERA)",
             color = col_emerald, size = 3.3, fontface = "bold") +
    annotate("text", x = 2.6, y = 4.75, label = "REGRESSION RISK\n(Proj FIP >> 2025 ERA)",
             color = col_rose, size = 3.3, fontface = "bold") +
    labs(title = "Pitcher True-Talent Projection Frontier: Observed 2025 ERA vs. mlbr Projected 2026 FIP",
         subtitle = sprintf("n = %d active 2025 pitchers (TBF >= 50, test_oos Stuff+, Proj TBF >= 220); sized by projected TBF.", nrow(p26_viz)),
         x = "Observed 2025 Regular-Season ERA", y = "mlbr M4 Projected 2026 True-Talent FIP",
         color = "2025 mlbr\nStuff+",
         caption = "Source: mlbr EXP-004 Gold Pitcher Projections | Zero non-2025/retired pitchers included") +
    mlbr::theme_cyber(base_size = 11)
  mlbr::save_plot_assets(strip_env(fig4_static), "proj04_pitcher_projections_explorer_static", data = p26_viz,
                         width = 12.5, height = 7.4, dir = models_dir)

  fig4_plotly <- make_proj4_plotly(p26_viz)
  mlbr::save_plot_assets(fig4_plotly, "proj04_pitcher_projections_explorer", data = p26_viz, dir = models_dir)
  dep4 <- file.path(models_dir, "proj04_pitcher_projections_explorer_files")
  if (dir.exists(dep4)) unlink(dep4, recursive = TRUE)

  # --- FIGURE 5: Batter Frontier (Static + Interactive Plotly) ---
  fig5_static <- ggplot(b26_viz, aes(x = obs_t1_woba, y = proj_woba, color = xwoba_skill_gap, size = proj_pa)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#64748b", linewidth = 0.6) +
    geom_point(alpha = 0.82) +
    ggrepel::geom_text_repel(
      data = b26_viz[label_flag == TRUE], aes(label = player_name), color = "#f8fafc",
      size = 3.1, fontface = "bold", box.padding = 0.35, max.overlaps = 20, segment.color = "#475569", show.legend = FALSE
    ) +
    scale_color_gradientn(colors = c(col_rose, col_amber, "#e2e8f0", col_cyan, col_emerald),
                          limits = c(-0.045, 0.045), oob = scales::squish) +
    scale_size_continuous(range = c(1.8, 5.5), guide = "none") +
    coord_cartesian(xlim = c(0.240, 0.440), ylim = c(0.265, 0.425)) +
    annotate("text", x = 0.275, y = 0.395, label = "UNDERVALUED CRUSHERS\n(Proj wOBA >> 2025 wOBA)",
             color = col_emerald, size = 3.3, fontface = "bold") +
    annotate("text", x = 0.395, y = 0.285, label = "REGRESSION CANDIDATES\n(2025 Outran Physics)",
             color = col_rose, size = 3.3, fontface = "bold") +
    labs(title = "Batter True-Talent Projection Frontier: Observed 2025 wOBA vs. mlbr Projected 2026 wOBA",
         subtitle = sprintf("n = %d active 2025 batters (PA >= 50, Proj PA >= 320); M4 incorporates Pulled Air%%, EV90, Barrel%%, and Zone-Chase Z-O%%.", nrow(b26_viz)),
         x = "Observed 2025 Regular-Season wOBA", y = "mlbr M4 Projected 2026 True-Talent wOBA",
         color = "xwOBA -\n2025 wOBA",
         caption = "Source: mlbr EXP-004 Gold Batter Projections | Zero non-2025 batters included") +
    mlbr::theme_cyber(base_size = 11)
  mlbr::save_plot_assets(strip_env(fig5_static), "proj05_batter_projections_explorer_static", data = b26_viz,
                         width = 12.5, height = 7.4, dir = models_dir)

  fig5_plotly <- make_proj5_plotly(b26_viz)
  mlbr::save_plot_assets(fig5_plotly, "proj05_batter_projections_explorer", data = b26_viz, dir = models_dir)
  dep5 <- file.path(models_dir, "proj05_batter_projections_explorer_files")
  if (dir.exists(dep5)) unlink(dep5, recursive = TRUE)

  expected_assets <- c(
    "proj01_empirical_aging_curves.png",
    "proj02_stabilization_reliability_shrinkage.png",
    "proj03_oos_backtest_ablation.png",
    "proj04_pitcher_projections_explorer_static.png",
    "proj04_pitcher_projections_explorer.html",
    "proj05_batter_projections_explorer_static.png",
    "proj05_batter_projections_explorer.html"
  )
  asset_files <- file.path(models_dir, expected_assets)
  assert_true(all(file.exists(asset_files)), paste("Missing expected visual assets:", paste(expected_assets[!file.exists(asset_files)], collapse = ", ")))
  assert_true(all(file.copy(asset_files, assets_dir, overwrite = TRUE)), "Failed copying proj0* assets to site/assets/")
  rds_files <- Sys.glob(file.path(models_dir, "proj0*.rds"))
  assert_true(length(rds_files) == 7L, sprintf("Expected 7 .rds files, found %d", length(rds_files)))
  rds_mb <- file.info(rds_files)$size / 1e6
  assert_true(all(rds_mb < 5.0), paste("Oversized .rds detected:", paste(sprintf("%s (%.2f MB)", basename(rds_files[rds_mb >= 5.0]), rds_mb[rds_mb >= 5.0]), collapse = ", ")))
  cat(sprintf("   Verified & copied exact 7 PNG/HTML assets to site/assets/ | 7 .rds files max: %.2f MB\n", max(rds_mb)))
  invisible(TRUE)
}

run_exp004 <- function() {
  t0_total <- proc.time()[["elapsed"]]
  set.seed(20260404L)
  data.table::setDTthreads(min(8L, data.table::getDTthreads()))

  lake <- mlbr::default_lake()
  models_dir <- file.path(lake, "models")
  processed_dir <- file.path(lake, "data", "processed")
  assets_dir <- file.path(lake, "site", "assets")
  for (d in c(models_dir, processed_dir, assets_dir)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  cat("==============================================================================\n")
  cat("mlbr EXP-004: Chronological True-Talent Player Projections (Audit-Hardened)\n")
  cat("Lake root:", lake, "\n")
  cat("==============================================================================\n")

  con <- mlbr::lake_duckdb(lake = lake)
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB'")

  # ============================================================================
  # 1. FULL-POPULATION LAKE EXTRACTION + FAIL-FAST JOIN GUARDS
  # ============================================================================
  cat("\n-> [1/7] Extracting FanGraphs (2002-2025), Statcast (2015-2026), and Tier 1 Stuff+...\n")

  fg_pitch <- as.data.table(DBI::dbGetQuery(con, "
    WITH cw AS (
      SELECT key_mlbam, MIN(birth_year) AS birth_year, MIN(birth_month) AS birth_month
      FROM v_chadwick_register WHERE key_mlbam IS NOT NULL GROUP BY key_mlbam
    )
    SELECT
      p.Season AS game_year, CAST(p.xMLBAMID AS INTEGER) AS pitcher_mlbam,
      CAST(p.playerid AS INTEGER) AS fg_id, p.PlayerName AS player_name,
      p.team_name_abb AS team, p.Throws AS p_throws,
      CAST(p.Age AS DOUBLE) AS age,
      CAST(cw.birth_year AS INTEGER) AS birth_year, CAST(cw.birth_month AS INTEGER) AS birth_month,
      CAST(p.G AS DOUBLE) AS g, CAST(p.GS AS DOUBLE) AS gs, CAST(p.TBF AS DOUBLE) AS tbf,
      CAST(p.SO AS DOUBLE) AS so, CAST(p.BB AS DOUBLE) AS bb,
      CAST(COALESCE(p.HBP, 0) AS DOUBLE) AS hbp, CAST(p.HR AS DOUBLE) AS hr,
      CAST(p.ER AS DOUBLE) AS er, CAST(p.IP AS DOUBLE) AS ip_raw,
      (FLOOR(p.IP) + ROUND((p.IP - FLOOR(p.IP)) * 10.0) / 3.0) AS ip_true,
      CAST(p.FIP AS DOUBLE) AS fip, CAST(p.xFIP AS DOUBLE) AS xfip,
      CAST(p.SIERA AS DOUBLE) AS siera, CAST(p.ERA AS DOUBLE) AS era,
      CAST(p.GB_pct AS DOUBLE) AS gb_pct, CAST(p.FB_pct AS DOUBLE) AS fb_pct,
      CAST(p.FBv AS DOUBLE) AS fbv, CAST(p.\"SwStr_pct\" AS DOUBLE) AS swstr_pct,
      CAST(p.\"C+SwStr_pct\" AS DOUBLE) AS csw_pct,
      CAST(p.HardHit_pct AS DOUBLE) AS hard_hit_pct, CAST(p.Barrel_pct AS DOUBLE) AS barrel_pct,
      CAST(p.xERA AS DOUBLE) AS xera,
      CAST(p.sp_stuff AS DOUBLE) AS fg_stuff_plus,
      CAST(p.sp_location AS DOUBLE) AS fg_location_plus,
      CAST(p.sp_pitching AS DOUBLE) AS fg_pitching_plus
    FROM v_fangraphs_pitch p
    LEFT JOIN cw ON CAST(p.xMLBAMID AS INTEGER) = CAST(cw.key_mlbam AS INTEGER)
    WHERE p.Season BETWEEN 2002 AND 2025 AND p.TBF > 0 AND p.xMLBAMID IS NOT NULL
  "))

  fg_bat <- as.data.table(DBI::dbGetQuery(con, "
    WITH cw AS (
      SELECT key_mlbam, MIN(birth_year) AS birth_year, MIN(birth_month) AS birth_month
      FROM v_chadwick_register WHERE key_mlbam IS NOT NULL GROUP BY key_mlbam
    )
    SELECT
      b.Season AS game_year, CAST(b.xMLBAMID AS INTEGER) AS batter_mlbam,
      CAST(b.playerid AS INTEGER) AS fg_id, b.PlayerName AS player_name,
      b.team_name_abb AS team, b.Bats AS bats, b.position,
      CAST(b.Age AS DOUBLE) AS age,
      CAST(cw.birth_year AS INTEGER) AS birth_year, CAST(cw.birth_month AS INTEGER) AS birth_month,
      CAST(b.G AS DOUBLE) AS g, CAST(b.PA AS DOUBLE) AS pa, CAST(b.AB AS DOUBLE) AS ab,
      CAST(b.H AS DOUBLE) AS h, CAST(b.HR AS DOUBLE) AS hr, CAST(b.BB AS DOUBLE) AS bb,
      CAST(b.SO AS DOUBLE) AS so, CAST(COALESCE(b.HBP, 0) AS DOUBLE) AS hbp,
      CAST(b.wOBA AS DOUBLE) AS woba, CAST(b.OBP AS DOUBLE) AS obp, CAST(b.SLG AS DOUBLE) AS slg,
      CAST(b.ISO AS DOUBLE) AS iso, CAST(b.BABIP AS DOUBLE) AS babip,
      CAST(b.xwOBA AS DOUBLE) AS xwoba, CAST(b.EV AS DOUBLE) AS ev, CAST(b.EV90 AS DOUBLE) AS ev90,
      CAST(b.Barrel_pct AS DOUBLE) AS barrel_pct, CAST(b.HardHit_pct AS DOUBLE) AS hard_hit_pct,
      CAST(b.\"O-Swing_pct\" AS DOUBLE) AS o_swing_pct, CAST(b.\"Z-Swing_pct\" AS DOUBLE) AS z_swing_pct,
      CAST(b.\"Z-Contact_pct\" AS DOUBLE) AS z_contact_pct, CAST(b.\"SwStr_pct\" AS DOUBLE) AS swstr_pct,
      CAST(b.Pull_pct AS DOUBLE) AS pull_pct, CAST(b.wRC_plus AS DOUBLE) AS wrc_plus,
      CAST(b.AvgBatSpeed AS DOUBLE) AS avg_bat_speed, CAST(b.FastSwing_pct AS DOUBLE) AS fast_swing_pct,
      CAST(b.SwingLength AS DOUBLE) AS swing_length,
      CAST(b.SquaredUpSwing_pct AS DOUBLE) AS squared_up_swing_pct,
      CAST(b.BlastSwing_pct AS DOUBLE) AS blast_swing_pct,
      CAST(b.Fielding AS DOUBLE) AS fld_runs,
      CAST(b.Defense AS DOUBLE) AS def_runs,
      CAST(b.wBsR AS DOUBLE) AS bsr_runs,
      CAST(b.\"Positional\" AS DOUBLE) AS pos_runs,
      CAST(b.WAR AS DOUBLE) AS fg_war
    FROM v_fangraphs_bat b
    LEFT JOIN cw ON CAST(b.xMLBAMID AS INTEGER) = CAST(cw.key_mlbam AS INTEGER)
    WHERE b.Season BETWEEN 2002 AND 2025 AND b.PA > 0
      AND COALESCE(b.position, '') != 'P' AND b.xMLBAMID IS NOT NULL
  "))

  assert_true(nrow(fg_pitch) > 15000L && nrow(fg_bat) > 14000L, "FanGraphs row count unexpectedly low")
  assert_true(nrow(unique(fg_pitch, by = c("game_year", "pitcher_mlbam"))) == nrow(fg_pitch), "Duplicate fg_pitch key")
  assert_true(nrow(unique(fg_bat, by = c("game_year", "batter_mlbam"))) == nrow(fg_bat), "Duplicate fg_bat key")
  assert_true(all(abs(fg_pitch$ip_true * 3 - round(fg_pitch$ip_true * 3)) < 1e-6), "IP outs conversion failed")
  assert_true(mean(!is.na(fg_pitch$birth_year) & !is.na(fg_pitch$birth_month)) > 0.995, "Chadwick join rate dropped on fg_pitch")
  assert_true(mean(!is.na(fg_bat$birth_year) & !is.na(fg_bat$birth_month)) > 0.995, "Chadwick join rate dropped on fg_bat")

  fg_pitch[, `:=`(
    role = fifelse(gs / pmax(g, 1) >= 0.5, "SP", "RP"),
    k_pct = so / tbf, bb_pct = bb / tbf, hbp_pct = hbp / tbf,
    hr9 = fifelse(ip_true > 0, 9.0 * hr / ip_true, NA_real_),
    era_calc = fifelse(ip_true > 0, 9.0 * er / ip_true, NA_real_),
    tbf_per_ip = fifelse(ip_true > 0, tbf / ip_true, NA_real_),
    fip = fifelse(is.finite(fip) & ip_true > 0, fip, NA_real_),
    siera = fifelse(is.finite(siera), siera, NA_real_)
  )]
  setorder(fg_pitch, pitcher_mlbam, game_year)
  fg_pitch[, `:=`(prev_year = shift(game_year, 1L), prev_fbv = shift(fbv, 1L)), by = pitcher_mlbam]
  fg_pitch[, delta_fbv := fifelse(game_year - prev_year == 1L & is.finite(fbv) & is.finite(prev_fbv), fbv - prev_fbv, NA_real_)]
  fg_pitch[, c("prev_year", "prev_fbv") := NULL]

  fg_bat[, `:=`(
    k_pct = so / pa, bb_pct = bb / pa, bip_denom = pmax(ab - so - hr, 0),
    zone_chase_selectivity = fifelse(is.finite(z_swing_pct) & is.finite(o_swing_pct), z_swing_pct - o_swing_pct, NA_real_),
    xwoba_minus_woba = fifelse(is.finite(xwoba) & is.finite(woba), xwoba - woba, NA_real_)
  )]
  age_check_p <- fg_pitch[game_year >= 2015L, mean(age == (game_year - birth_year - as.integer(birth_month >= 7L)))]
  assert_true(isTRUE(age_check_p > 0.999), sprintf("Baseball Age parity check failed: %.4f", age_check_p))

  sc_pitch_yr <- as.data.table(DBI::dbGetQuery(con, "
    WITH by_hand AS (
      SELECT game_year, CAST(pitcher AS INTEGER) AS pitcher_mlbam, is_same_hand,
             COUNT(*) AS n_pitches,
             SUM(is_csw)::DOUBLE / NULLIF(COUNT(*), 0) AS csw_rate,
             SUM(CASE WHEN xwoba IS NOT NULL THEN xwoba ELSE 0 END) /
               NULLIF(SUM(CASE WHEN xwoba IS NOT NULL THEN 1 ELSE 0 END), 0) AS xwoba_rate
      FROM v_statcast_features
      WHERE game_type = 'R' AND game_year BETWEEN 2015 AND 2026 AND is_same_hand IS NOT NULL
      GROUP BY 1, 2, 3
    ),
    pvi AS (
      SELECT game_year, pitcher_mlbam,
             SUM(CASE WHEN is_same_hand = 1 THEN n_pitches ELSE 0 END) AS n_same_pitches,
             SUM(CASE WHEN is_same_hand = 0 THEN n_pitches ELSE 0 END) AS n_opp_pitches,
             MAX(CASE WHEN is_same_hand = 0 THEN xwoba_rate END) -
               MAX(CASE WHEN is_same_hand = 1 THEN xwoba_rate END) AS pvi_xwoba_raw,
             MAX(CASE WHEN is_same_hand = 1 THEN csw_rate END) -
               MAX(CASE WHEN is_same_hand = 0 THEN csw_rate END) AS pvi_csw_raw
      FROM by_hand GROUP BY 1, 2
    ),
    outcomes AS (
      SELECT game_year, CAST(pitcher AS INTEGER) AS pitcher_mlbam,
             COUNT(*) AS sc_n_pitches,
             COUNT(DISTINCT CASE WHEN events IS NOT NULL
                   THEN CAST(game_pk AS VARCHAR) || '_' || CAST(at_bat_number AS VARCHAR) END) AS sc_tbf,
             SUM(CASE WHEN events IN ('strikeout', 'strikeout_double_play') THEN 1 ELSE 0 END) AS sc_so,
             SUM(CASE WHEN events IN ('walk', 'intent_walk') THEN 1 ELSE 0 END) AS sc_bb,
             SUM(CASE WHEN events = 'hit_by_pitch' THEN 1 ELSE 0 END) AS sc_hbp,
             SUM(CASE WHEN events = 'home_run' THEN 1 ELSE 0 END) AS sc_hr,
             SUM(CASE WHEN xwoba IS NOT NULL THEN xwoba ELSE 0 END) /
               NULLIF(SUM(CASE WHEN xwoba IS NOT NULL THEN 1 ELSE 0 END), 0) AS sc_xwoba_allowed,
             SUM(is_csw)::DOUBLE / NULLIF(COUNT(*), 0) AS sc_csw_pct,
             SUM(is_whiff)::DOUBLE / NULLIF(COUNT(*), 0) AS sc_swstr_pct,
             SUM(is_gb)::DOUBLE / NULLIF(SUM(is_bip), 0) AS sc_gb_pct,
             SUM(is_hard_hit)::DOUBLE / NULLIF(SUM(is_bip), 0) AS sc_hard_hit_pct,
             AVG(CASE WHEN pitch_type IN ('FF','SI','FC') THEN release_speed END) AS sc_fbv
      FROM v_statcast_features
      WHERE game_type = 'R' AND game_year BETWEEN 2015 AND 2026
      GROUP BY 1, 2
    )
    SELECT o.*, COALESCE(p.n_same_pitches, 0) AS n_same_pitches,
           COALESCE(p.n_opp_pitches, 0) AS n_opp_pitches,
           p.pvi_xwoba_raw, p.pvi_csw_raw
    FROM outcomes o
    LEFT JOIN pvi p ON o.game_year = p.game_year AND o.pitcher_mlbam = p.pitcher_mlbam
  "))
  assert_true(nrow(sc_pitch_yr) > 7000L, "sc_pitch_yr row count unexpectedly low")
  sc_pitch_yr[, `:=`(
    pvi_n_eff = fifelse(n_same_pitches > 0 & n_opp_pitches > 0,
                        2.0 * n_same_pitches * n_opp_pitches / (n_same_pitches + n_opp_pitches), 0.0),
    sc_k_pct = fifelse(sc_tbf > 0, sc_so / sc_tbf, NA_real_),
    sc_bb_pct = fifelse(sc_tbf > 0, sc_bb / sc_tbf, NA_real_),
    sc_hbp_pct = fifelse(sc_tbf > 0, sc_hbp / sc_tbf, NA_real_),
    sc_hr_per_bf = fifelse(sc_tbf > 0, sc_hr / sc_tbf, NA_real_)
  )]

  sc_spray_bat <- as.data.table(DBI::dbGetQuery(con, "
    WITH bip AS (
      SELECT game_year, CAST(batter AS INTEGER) AS batter_mlbam, stand,
             CAST(hc_x AS DOUBLE) AS hcx, CAST(hc_y AS DOUBLE) AS hcy,
             CAST(launch_angle AS DOUBLE) AS la
      FROM v_statcast
      WHERE game_type = 'R' AND game_year BETWEEN 2015 AND 2026
        AND stand IN ('R', 'L')
        AND hc_x IS NOT NULL AND hc_y IS NOT NULL AND launch_angle IS NOT NULL
    )
    SELECT game_year, batter_mlbam, COUNT(*) AS n_bip_spray,
           SUM(CASE WHEN la >= 10.0 AND (
                     (stand = 'R' AND DEGREES(ATAN2(hcx - 125.42, 198.27 - hcy)) < -15.0)
                  OR (stand = 'L' AND DEGREES(ATAN2(hcx - 125.42, 198.27 - hcy)) > 15.0)
               ) THEN 1 ELSE 0 END)::DOUBLE / NULLIF(COUNT(*), 0) AS pulled_air_pct,
           SUM(CASE WHEN la BETWEEN 8.0 AND 32.0 THEN 1 ELSE 0 END)::DOUBLE / NULLIF(COUNT(*), 0) AS sweet_spot_pct
    FROM bip GROUP BY 1, 2
  "))

  sc_spray_pitch <- as.data.table(DBI::dbGetQuery(con, "
    WITH bip AS (
      SELECT game_year, CAST(pitcher AS INTEGER) AS pitcher_mlbam, stand,
             CAST(hc_x AS DOUBLE) AS hcx, CAST(hc_y AS DOUBLE) AS hcy,
             CAST(launch_angle AS DOUBLE) AS la
      FROM v_statcast
      WHERE game_type = 'R' AND game_year BETWEEN 2015 AND 2026
        AND stand IN ('R', 'L')
        AND hc_x IS NOT NULL AND hc_y IS NOT NULL AND launch_angle IS NOT NULL
    )
    SELECT game_year, pitcher_mlbam, COUNT(*) AS n_bip_allowed_spray,
           SUM(CASE WHEN la >= 10.0 AND (
                     (stand = 'R' AND DEGREES(ATAN2(hcx - 125.42, 198.27 - hcy)) < -15.0)
                  OR (stand = 'L' AND DEGREES(ATAN2(hcx - 125.42, 198.27 - hcy)) > 15.0)
               ) THEN 1 ELSE 0 END)::DOUBLE / NULLIF(COUNT(*), 0) AS pulled_air_allowed_pct
    FROM bip GROUP BY 1, 2
  "))
  assert_true(nrow(sc_spray_bat) > 7000L && nrow(sc_spray_pitch) > 7000L, "Spray table row count unexpectedly low")

  # Regular-Season 2026 Statcast Batter outcomes with proper woba_denom + exact OBP/SLG/ISO
  sc_bat_2026 <- as.data.table(DBI::dbGetQuery(con, "
    WITH ev26 AS (
      SELECT
        f.game_year, CAST(f.batter AS INTEGER) AS batter_mlbam,
        f.events, f.woba_value, f.xwoba, f.is_bip, f.is_hard_hit,
        f.launch_speed, f.bat_speed_mph, f.is_in_zone, f.is_swing, f.is_whiff,
        f.game_pk, f.at_bat_number,
        TRY_CAST(r.woba_denom AS DOUBLE) AS woba_denom
      FROM v_statcast_features f
      JOIN v_statcast r USING (game_pk, at_bat_number, pitch_number)
      WHERE f.game_type = 'R' AND f.game_year = 2026 AND r.game_year = 2026
    )
    SELECT
      game_year, batter_mlbam,
      COUNT(DISTINCT CASE WHEN events IS NOT NULL
            THEN CAST(game_pk AS VARCHAR) || '_' || CAST(at_bat_number AS VARCHAR) END) AS sc_pa,
      SUM(CASE WHEN events IN ('single','double','triple','home_run','field_out','force_out',
                               'grounded_into_double_play','double_play','triple_play','field_error',
                               'fielders_choice','fielders_choice_out','strikeout','strikeout_double_play')
               THEN 1 ELSE 0 END) AS sc_ab,
      SUM(CASE WHEN events = 'single' THEN 1 ELSE 0 END) AS sc_1b,
      SUM(CASE WHEN events = 'double' THEN 1 ELSE 0 END) AS sc_2b,
      SUM(CASE WHEN events = 'triple' THEN 1 ELSE 0 END) AS sc_3b,
      SUM(CASE WHEN events = 'home_run' THEN 1 ELSE 0 END) AS sc_hr,
      SUM(CASE WHEN events IN ('strikeout','strikeout_double_play') THEN 1 ELSE 0 END) AS sc_so,
      SUM(CASE WHEN events IN ('walk','intent_walk') THEN 1 ELSE 0 END) AS sc_bb,
      SUM(CASE WHEN events = 'hit_by_pitch' THEN 1 ELSE 0 END) AS sc_hbp,
      SUM(CASE WHEN events IN ('sac_fly','sac_fly_double_play') THEN 1 ELSE 0 END) AS sc_sf,
      SUM(CASE WHEN woba_denom > 0 AND woba_value IS NOT NULL THEN woba_value ELSE 0 END) /
        NULLIF(SUM(CASE WHEN woba_denom > 0 THEN woba_denom ELSE 0 END), 0) AS sc_woba,
      SUM(CASE WHEN xwoba IS NOT NULL THEN xwoba
               WHEN events IN ('walk','hit_by_pitch','strikeout','strikeout_double_play') THEN woba_value END) /
        NULLIF(SUM(CASE WHEN xwoba IS NOT NULL
               OR events IN ('walk','hit_by_pitch','strikeout','strikeout_double_play') THEN 1 END), 0) AS sc_xwoba,
      SUM(is_hard_hit)::DOUBLE / NULLIF(SUM(is_bip), 0) AS sc_hard_hit_pct,
      QUANTILE_CONT(launch_speed, 0.90) FILTER (WHERE is_bip = 1 AND launch_speed IS NOT NULL) AS sc_ev90,
      AVG(bat_speed_mph) FILTER (WHERE bat_speed_mph IS NOT NULL) AS sc_bat_speed,
      SUM(CASE WHEN is_in_zone = 0 AND is_swing = 1 THEN 1 ELSE 0 END)::DOUBLE /
        NULLIF(SUM(CASE WHEN is_in_zone = 0 THEN 1 ELSE 0 END), 0) AS sc_o_swing_pct,
      SUM(CASE WHEN is_in_zone = 1 AND is_swing = 1 THEN 1 ELSE 0 END)::DOUBLE /
        NULLIF(SUM(CASE WHEN is_in_zone = 1 THEN 1 ELSE 0 END), 0) AS sc_z_swing_pct
    FROM ev26 GROUP BY 1, 2
  "))
  assert_true(nrow(sc_bat_2026) > 500L, "sc_bat_2026 row count unexpectedly low")
  sc_bat_2026[, `:=`(
    sc_h = sc_1b + sc_2b + sc_3b + sc_hr,
    sc_tb = sc_1b + 2 * sc_2b + 3 * sc_3b + 4 * sc_hr,
    sc_k_pct = fifelse(sc_pa > 0, sc_so / sc_pa, NA_real_),
    sc_bb_pct = fifelse(sc_pa > 0, sc_bb / sc_pa, NA_real_),
    sc_zone_chase = fifelse(is.finite(sc_z_swing_pct) & is.finite(sc_o_swing_pct), sc_z_swing_pct - sc_o_swing_pct, NA_real_)
  )]
  sc_bat_2026[, `:=`(
    sc_obp = fifelse((sc_ab + sc_bb + sc_hbp + sc_sf) > 0, (sc_h + sc_bb + sc_hbp) / (sc_ab + sc_bb + sc_hbp + sc_sf), NA_real_),
    sc_slg = fifelse(sc_ab > 0, sc_tb / sc_ab, NA_real_),
    sc_iso = fifelse(sc_ab > 0, (sc_tb - sc_h) / sc_ab, NA_real_)
  )]

  sp_path <- file.path(processed_dir, "pitcher_stuff_plus.parquet")
  assert_true(file.exists(sp_path), "Missing Tier 1 Gold pitcher_stuff_plus.parquet")
  gold_sp <- as.data.table(arrow::read_parquet(sp_path))
  setnames(gold_sp, "pitcher", "pitcher_mlbam")
  gold_sp[, `:=`(pitcher_mlbam = as.integer(pitcher_mlbam), game_year = as.integer(game_year))]
  assert_true(all(gold_sp[game_year <= 2023L]$eval_split == "train_insample"), "eval_split violation 2020-2023")
  assert_true(all(gold_sp[game_year == 2024L]$eval_split == "val_calibration"), "eval_split violation 2024")
  assert_true(all(gold_sp[game_year >= 2025L]$eval_split == "test_oos"), "eval_split violation 2025-2026")

  fg_pitch <- merge(fg_pitch,
                    sc_pitch_yr[, .(game_year, pitcher_mlbam, pvi_n_eff, pvi_xwoba_raw, pvi_csw_raw,
                                    sc_xwoba_allowed, sc_n_pitches)],
                    by = c("game_year", "pitcher_mlbam"), all.x = TRUE)
  fg_pitch <- merge(fg_pitch,
                    sc_spray_pitch[, .(game_year, pitcher_mlbam, n_bip_allowed_spray, pulled_air_allowed_pct)],
                    by = c("game_year", "pitcher_mlbam"), all.x = TRUE)
  decomp_p_path <- file.path(processed_dir, "pitcher_decomp_pitching_plus.parquet")
  assert_true(file.exists(decomp_p_path), "Missing Gold pitcher_decomp_pitching_plus.parquet")
  gold_decomp_p <- as.data.table(arrow::read_parquet(decomp_p_path))
  setnames(gold_decomp_p, "pitcher", "pitcher_mlbam")
  gold_decomp_p[, `:=`(pitcher_mlbam = as.integer(pitcher_mlbam), game_year = as.integer(game_year))]
  assert_true(all(gold_decomp_p[game_year <= 2023L]$eval_split == "train_insample"), "decomp eval_split violation 2020-2023")
  assert_true(all(gold_decomp_p[game_year == 2024L]$eval_split == "val_calibration"), "decomp eval_split violation 2024")
  assert_true(all(gold_decomp_p[game_year >= 2025L]$eval_split == "test_oos"), "decomp eval_split violation 2025-2026")

  bat_proc_path <- file.path(processed_dir, "batter_process_metrics.parquet")
  assert_true(file.exists(bat_proc_path), "Missing Gold batter_process_metrics.parquet")
  gold_bat_proc <- as.data.table(arrow::read_parquet(bat_proc_path))
  setnames(gold_bat_proc, "batter", "batter_mlbam")
  gold_bat_proc[, `:=`(batter_mlbam = as.integer(batter_mlbam), game_year = as.integer(game_year))]
  assert_true(all(gold_bat_proc[game_year <= 2023L]$eval_split == "train_insample"), "process eval_split violation 2020-2023")
  assert_true(all(gold_bat_proc[game_year == 2024L]$eval_split == "val_calibration"), "process eval_split violation 2024")
  assert_true(all(gold_bat_proc[game_year >= 2025L]$eval_split == "test_oos"), "process eval_split violation 2025-2026")

  fg_pitch <- merge(fg_pitch,
                    gold_decomp_p[, .(game_year, pitcher_mlbam,
                                      decomp_stuff_plus, decomp_location_plus,
                                      decomp_pitching_plus, decomp_pitching_tunnel_plus,
                                      decomp_n_pitches = n_pitches)],
                    by = c("game_year", "pitcher_mlbam"), all.x = TRUE)
  fg_pitch <- merge(fg_pitch,
                    gold_sp[, .(game_year, pitcher_mlbam, stuff_plus, location_plus, pitching_plus,
                                pitching_tunnel_plus, mean_velo_mph, agg_break_tunnel_ratio,
                                sp_n_pitches = n_pitches, sp_eval_split = eval_split)],
                    by = c("game_year", "pitcher_mlbam"), all.x = TRUE)
  fg_bat <- merge(fg_bat,
                  gold_bat_proc[, .(game_year, batter_mlbam,
                                    decision_plus, bat_control_plus,
                                    contact_plus, batter_process_plus,
                                    bp_n_pitches = n_pitches)],
                  by = c("game_year", "batter_mlbam"), all.x = TRUE)
  fg_bat <- merge(fg_bat,
                  sc_spray_bat[, .(game_year, batter_mlbam, n_bip_spray, pulled_air_pct, sweet_spot_pct)],
                  by = c("game_year", "batter_mlbam"), all.x = TRUE)

  # Fail-fast join-rate assertions across all Statcast/Gold joins
  decomp_join_rate <- fg_pitch[game_year >= 2020L & tbf >= 50, mean(!is.na(decomp_location_plus))]
  proc_join_rate   <- fg_bat[game_year >= 2020L & pa >= 50, mean(!is.na(decision_plus))]
  assert_true(decomp_join_rate > 0.995 && proc_join_rate > 0.995,
              sprintf("Decomp/Process join-rate gate failed: decomp=%.4f proc=%.4f", decomp_join_rate, proc_join_rate))
  sp_join_rate   <- fg_pitch[game_year >= 2020L & tbf >= 50, mean(!is.na(stuff_plus))]
  pvi_join_rate  <- fg_pitch[game_year >= 2015L & tbf >= 50, mean(!is.na(sc_n_pitches))]
  pspr_join_rate <- fg_pitch[game_year >= 2015L & tbf >= 50, mean(!is.na(pulled_air_allowed_pct))]
  bspr_join_rate <- fg_bat[game_year >= 2015L & pa >= 50, mean(!is.na(pulled_air_pct))]
  assert_true(sp_join_rate > 0.995 && pvi_join_rate > 0.99 && pspr_join_rate > 0.99 && bspr_join_rate > 0.99,
              sprintf("Join-rate gate failed: sp=%.4f pvi=%.4f pspr=%.4f bspr=%.4f",
                      sp_join_rate, pvi_join_rate, pspr_join_rate, bspr_join_rate))
  cat(sprintf("   Extracted %s pitcher-seasons, %s batter-seasons | Stuff+: %.2f%% | Spray: %.2f%% / %.2f%%\n",
              format(nrow(fg_pitch), big.mark = ","), format(nrow(fg_bat), big.mark = ","),
              100 * sp_join_rate, 100 * pspr_join_rate, 100 * bspr_join_rate))

  # ============================================================================
  # 2. ROLE-SPECIFIC SEASONAL LEAGUE BASELINES (SUM-BEFORE-DIVIDE, ZERO NaN)
  # ============================================================================
  cat("\n-> [2/7] Computing role-specific seasonal league averages & pre-2016 reliability M...\n")

  lg_pitch <- fg_pitch[, .(
    lg_k_pct    = sum(so, na.rm = TRUE) / sum(tbf, na.rm = TRUE),
    lg_bb_pct   = sum(bb, na.rm = TRUE) / sum(tbf, na.rm = TRUE),
    lg_hbp_pct  = sum(hbp, na.rm = TRUE) / sum(tbf, na.rm = TRUE),
    lg_hr9      = 9.0 * sum(hr, na.rm = TRUE) / sum(ip_true, na.rm = TRUE),
    lg_era      = 9.0 * sum(er, na.rm = TRUE) / sum(ip_true, na.rm = TRUE),
    lg_fip      = w_mean_safe(fip, ip_true),
    lg_siera    = w_mean_safe(siera, tbf),
    lg_tbf_ip   = sum(tbf, na.rm = TRUE) / sum(ip_true, na.rm = TRUE),
    lg_swstr    = w_mean_safe(swstr_pct, tbf),
    lg_csw      = w_mean_safe(csw_pct, tbf),
    lg_fbv      = w_mean_safe(fbv, tbf),
    lg_gb       = w_mean_safe(gb_pct, tbf),
    lg_hh       = w_mean_safe(hard_hit_pct, tbf),
    lg_barrel   = w_mean_safe(barrel_pct, tbf),
    lg_xera     = w_mean_safe(xera, ip_true),
    lg_pull_air = w_mean_safe(pulled_air_allowed_pct, n_bip_allowed_spray)
  ), by = .(game_year, role)]

  lg_cfip <- fg_pitch[, .(
    c_fip = (9.0 * sum(er, na.rm = TRUE) / sum(ip_true, na.rm = TRUE)) -
            (13.0 * sum(hr, na.rm = TRUE) + 3.0 * (sum(bb, na.rm = TRUE) + sum(hbp, na.rm = TRUE)) -
             2.0 * sum(so, na.rm = TRUE)) / sum(ip_true, na.rm = TRUE)
  ), by = game_year]
  lg_pitch <- merge(lg_pitch, lg_cfip, by = "game_year", all.x = TRUE)

  lg_bat <- fg_bat[, .(
    lg_woba       = w_mean_safe(woba, pa),
    lg_k_pct      = sum(so, na.rm = TRUE) / sum(pa, na.rm = TRUE),
    lg_bb_pct     = sum(bb, na.rm = TRUE) / sum(pa, na.rm = TRUE),
    lg_iso        = w_mean_safe(iso, ab),
    lg_obp        = w_mean_safe(obp, pa),
    lg_slg        = w_mean_safe(slg, ab),
    lg_babip      = w_mean_safe(babip, bip_denom),
    lg_xwoba      = w_mean_safe(xwoba, pa),
    lg_barrel     = w_mean_safe(barrel_pct, pa),
    lg_hh         = w_mean_safe(hard_hit_pct, pa),
    lg_ev         = w_mean_safe(ev, pa),
    lg_ev90       = w_mean_safe(ev90, pa),
    lg_oswing     = w_mean_safe(o_swing_pct, pa),
    lg_zswing     = w_mean_safe(z_swing_pct, pa),
    lg_zcon       = w_mean_safe(z_contact_pct, pa),
    lg_swstr      = w_mean_safe(swstr_pct, pa),
    lg_zone_chase = w_mean_safe(zone_chase_selectivity, pa),
    lg_pull_air   = w_mean_safe(pulled_air_pct, n_bip_spray),
    lg_sweet      = w_mean_safe(sweet_spot_pct, n_bip_spray),
    lg_batspeed   = w_mean_safe(avg_bat_speed, pa),
    lg_blast      = w_mean_safe(blast_swing_pct, pa),
    lg_sq_swing   = w_mean_safe(squared_up_swing_pct, pa),
    lg_swing_len  = w_mean_safe(swing_length, pa),
    lg_fld_runs   = w_mean_safe(fld_runs / pmax(pa, 1), pa),
    lg_def_runs   = w_mean_safe(def_runs / pmax(pa, 1), pa),
    lg_bsr_runs   = w_mean_safe(bsr_runs / pmax(pa, 1), pa),
    lg_pos_runs   = w_mean_safe(pos_runs / pmax(pa, 1), pa)
  ), by = game_year]

  fg_pitch <- merge(fg_pitch, lg_pitch, by = c("game_year", "role"), all.x = TRUE)
  fg_bat   <- merge(fg_bat, lg_bat, by = "game_year", all.x = TRUE)

  # ============================================================================
  # 3. CHRONOLOGICAL MIDPOINT DELTA-METHOD AGING (LINEAR BOUNDARY EXTRAPOLATION)
  # ============================================================================
  fit_delta_aging_curve <- function(dt, id_col, metric_col, lg_col, exp_col,
                                    max_year, role_filter = NULL, min_exp = 50,
                                    age_min = 21, age_max = 39) {
    sub <- dt[game_year <= max_year & is.finite(get(metric_col)) &
              is.finite(get(lg_col)) & get(exp_col) >= min_exp]
    if (!is.null(role_filter)) sub <- sub[role == role_filter]
    sub <- sub[, .(id = get(id_col), game_year, age,
                   rel = get(metric_col) - get(lg_col), exp = get(exp_col))]
    setorder(sub, id, game_year)
    sub[, `:=`(prev_yr = shift(game_year, 1L), prev_age = shift(age, 1L),
               prev_rel = shift(rel, 1L), prev_exp = shift(exp, 1L)), by = id]

    pairs <- sub[game_year - prev_yr == 1L & age - prev_age == 1L &
                 age >= age_min & age <= age_max]
    assert_true(nrow(pairs) >= 100L,
                sprintf("Insufficient pairs for delta aging (%s, max_year=%d)", metric_col, max_year))
    pairs[, `:=`(
      delta = rel - prev_rel,
      age_mid = age - 0.5,  # Exact midpoint of (age-1 -> age) transition (eliminates 0.5-yr phase shift)
      w_harm = 2.0 * prev_exp * exp / (prev_exp + exp)
    )]

    by_age <- pairs[, .(n_pairs = .N, sum_w_harm = sum(w_harm),
                        delta_raw = sum(w_harm * delta) / sum(w_harm)), by = .(age_to = age)]
    setorder(by_age, age_to)

    fit <- stats::lm(delta ~ splines::ns(age_mid, knots = c(24.5, 30.5)), data = pairs, weights = w_harm)
    step <- 0.25
    grid_age <- seq(age_min, age_max, by = step)
    # Midpoint Riemann / trapezoid integral over [age_min, age_max]
    grid_mid <- grid_age[-length(grid_age)] + 0.5 * step
    mid_slopes <- as.numeric(stats::predict(fit, newdata = data.frame(age_mid = grid_mid)))
    cum_raw <- c(0.0, cumsum(mid_slopes * step))
    idx27 <- which.min(abs(grid_age - 27.0))
    cum_val <- cum_raw - cum_raw[idx27]

    # Boundary marginal slopes for linear extrapolation outside [age_min, age_max] (fixes Age 40+ inversion!)
    s_lo <- as.numeric(stats::predict(fit, newdata = data.frame(age_mid = age_min - 0.5)))
    s_hi <- as.numeric(stats::predict(fit, newdata = data.frame(age_mid = age_max - 0.5)))
    c_lo <- cum_val[1L]
    c_hi <- cum_val[length(cum_val)]
    interp_inner <- stats::approxfun(grid_age, cum_val, rule = 2)

    cum_fn <- function(a) {
      a_num <- as.numeric(a)
      res <- interp_inner(a_num)
      below <- is.finite(a_num) & a_num < age_min
      above <- is.finite(a_num) & a_num > age_max
      if (any(below)) res[below] <- c_lo + (a_num[below] - age_min) * s_lo
      if (any(above)) res[above] <- c_hi + (a_num[above] - age_max) * s_hi
      res
    }

    int_ages <- seq(age_min, age_max, by = 1L)
    curve_dt <- data.table(
      age = int_ages,
      cum_delta_smooth = cum_fn(int_ages),
      annual_delta_smooth = as.numeric(stats::predict(fit, newdata = data.frame(age_mid = int_ages - 0.5)))
    )
    curve_dt <- merge(curve_dt, by_age, by.x = "age", by.y = "age_to", all.x = TRUE)
    list(cum_fn = cum_fn, curve_dt = curve_dt, n_pairs = nrow(pairs))
  }

  # ============================================================================
  # 4. CHRONOLOGICAL MARCEL + ZERO-SUM DELTA-AGING BUILDERS
  # ============================================================================
  build_pitcher_year <- function(target_yr, weights = c(3, 2, 1), reg_M,
                                 aging_curves = NULL, include_actuals = TRUE) {
    lags <- target_yr - 1:3
    w_map <- data.table(game_year = lags, w_yr = weights, lag_k = 1:3)
    hist <- merge(fg_pitch[game_year %in% lags], w_map, by = "game_year")
    assert_true(nrow(hist) > 100L, sprintf("No pitcher history for target_yr=%d", target_yr))

    hist[, `:=`(
      tbf_pt = fifelse(game_year == 2020L, pmin(tbf * (162 / 60), fifelse(role == "SP", 900, 320)), tbf),
      ip_pt  = fifelse(game_year == 2020L, pmin(ip_true * (162 / 60), fifelse(role == "SP", 215, 78)), ip_true)
    )]

    lg_anchor <- lg_pitch[game_year == (target_yr - 1L)]
    assert_true(nrow(lg_anchor) == 2L, sprintf("Expected SP+RP league anchors for %d", target_yr - 1L))

    agg <- hist[, .(
      player_name = player_name[which.max(game_year)],
      fg_id       = fg_id[which.max(game_year)],
      team_prev   = team[which.max(game_year)],
      p_throws    = p_throws[which.max(game_year)],
      birth_year  = birth_year[which.max(game_year)],
      birth_month = birth_month[which.max(game_year)],
      last_year   = max(game_year),
      age_last    = age[which.max(game_year)],
      sp_share_w  = sum(w_yr * gs) / pmax(sum(w_yr * g), 1),
      tbf_t1 = sum(tbf[lag_k == 1L]),
      tbf_t1_pt = sum(tbf_pt[lag_k == 1L]),
      tbf_t2_pt = sum(tbf_pt[lag_k == 2L]),
      ip_t1_pt  = sum(ip_pt[lag_k == 1L]),
      ip_t2_pt  = sum(ip_pt[lag_k == 2L]),
      w_tbf   = sum(w_yr * tbf),
      w_ip    = sum(w_yr * ip_true),
      w_age_bar = sum(w_yr * tbf * age) / sum(w_yr * tbf),
      ex_k_pct   = sum(w_yr * tbf * (k_pct - lg_k_pct), na.rm = TRUE),
      ex_bb_pct  = sum(w_yr * tbf * (bb_pct - lg_bb_pct), na.rm = TRUE),
      ex_hbp_pct = sum(w_yr * tbf * (hbp_pct - lg_hbp_pct), na.rm = TRUE),
      ex_hr9     = sum(w_yr * ip_true * (hr9 - lg_hr9), na.rm = TRUE),
      w_ip_hr    = sum(w_yr * ip_true * as.numeric(is.finite(hr9) & is.finite(lg_hr9))),
      ex_fip     = sum(w_yr * ip_true * (fip - lg_fip), na.rm = TRUE),
      w_ip_fip   = sum(w_yr * ip_true * as.numeric(is.finite(fip) & is.finite(lg_fip))),
      ex_siera   = sum(w_yr * tbf * (siera - lg_siera), na.rm = TRUE),
      w_tbf_siera = sum(w_yr * tbf * as.numeric(is.finite(siera) & is.finite(lg_siera))),
      ex_era     = sum(w_yr * ip_true * (era_calc - lg_era), na.rm = TRUE),
      w_ip_era   = sum(w_yr * ip_true * as.numeric(is.finite(era_calc) & is.finite(lg_era))),
      # Genuine observed T-1 values (NA when pitcher did not pitch in T-1 — zero silent fabrication!)
      obs_t1_k_pct = fifelse(any(lag_k == 1L), k_pct[lag_k == 1L][1], NA_real_),
      obs_t1_bb_pct = fifelse(any(lag_k == 1L), bb_pct[lag_k == 1L][1], NA_real_),
      obs_t1_hr9   = fifelse(any(lag_k == 1L), hr9[lag_k == 1L][1], NA_real_),
      obs_t1_fip   = fifelse(any(lag_k == 1L), fip[lag_k == 1L][1], NA_real_),
      obs_t1_siera = fifelse(any(lag_k == 1L), siera[lag_k == 1L][1], NA_real_),
      obs_t1_era   = fifelse(any(lag_k == 1L), era_calc[lag_k == 1L][1], NA_real_),
      # Symmetric finite masks for Statcast/FG physical priors
      ex_swstr   = sum(w_yr * tbf * (swstr_pct - lg_swstr), na.rm = TRUE),
      w_swstr    = sum(w_yr * tbf * as.numeric(is.finite(swstr_pct) & is.finite(lg_swstr))),
      ex_csw     = sum(w_yr * tbf * (csw_pct - lg_csw), na.rm = TRUE),
      w_csw      = sum(w_yr * tbf * as.numeric(is.finite(csw_pct) & is.finite(lg_csw))),
      ex_fbv     = sum(w_yr * tbf * (fbv - lg_fbv), na.rm = TRUE),
      w_fbv      = sum(w_yr * tbf * as.numeric(is.finite(fbv) & is.finite(lg_fbv))),
      ex_gb      = sum(w_yr * tbf * (gb_pct - lg_gb), na.rm = TRUE),
      w_gb       = sum(w_yr * tbf * as.numeric(is.finite(gb_pct) & is.finite(lg_gb))),
      ex_hh      = sum(w_yr * tbf * (hard_hit_pct - lg_hh), na.rm = TRUE),
      w_hh       = sum(w_yr * tbf * as.numeric(is.finite(hard_hit_pct) & is.finite(lg_hh))),
      ex_barrel  = sum(w_yr * tbf * (barrel_pct - lg_barrel), na.rm = TRUE),
      w_barrel   = sum(w_yr * tbf * as.numeric(is.finite(barrel_pct) & is.finite(lg_barrel))),
      ex_xera    = sum(w_yr * ip_true * (xera - lg_xera), na.rm = TRUE),
      w_xera     = sum(w_yr * ip_true * as.numeric(is.finite(xera) & is.finite(lg_xera))),
      ex_pull_air = sum(w_yr * n_bip_allowed_spray * (pulled_air_allowed_pct - lg_pull_air), na.rm = TRUE),
      w_pull_air  = sum(w_yr * fifelse(is.finite(pulled_air_allowed_pct) & is.finite(lg_pull_air), n_bip_allowed_spray, 0)),
      delta_fbv_raw = fifelse(any(lag_k == 1L & is.finite(delta_fbv)), delta_fbv[lag_k == 1L][1], 0.0),
      # Sum-before-shrink multi-year PVI (strictly raw seasonal PVI pooled by w_yr * pvi_n_eff, shrunk once!)
      pvi_xwoba_prior = sum(w_yr * fifelse(is.finite(pvi_xwoba_raw) & pvi_n_eff > 0, pvi_n_eff * pvi_xwoba_raw, 0.0), na.rm = TRUE) /
                        (sum(w_yr * fifelse(is.finite(pvi_xwoba_raw) & pvi_n_eff > 0, pvi_n_eff, 0.0), na.rm = TRUE) + 800.0),
      pvi_csw_prior   = sum(w_yr * fifelse(is.finite(pvi_csw_raw) & pvi_n_eff > 0, pvi_n_eff * pvi_csw_raw, 0.0), na.rm = TRUE) /
                        (sum(w_yr * fifelse(is.finite(pvi_csw_raw) & pvi_n_eff > 0, pvi_n_eff, 0.0), na.rm = TRUE) + 800.0),
      # Dedicated availability masks & pitch denominators for mlbr Stuff+ vs FanGraphs Stuff+
      t1_sp_pitches = sum(fifelse(lag_k == 1L & is.finite(stuff_plus), sp_n_pitches, 0), na.rm = TRUE),
      t1_fg_pitches = sum(fifelse(lag_k == 1L & is.finite(fg_stuff_plus), fifelse(is.finite(sc_n_pitches), sc_n_pitches, tbf * 3.9), 0), na.rm = TRUE),
      t1_stuff_raw  = fifelse(any(lag_k == 1L & is.finite(stuff_plus)), stuff_plus[lag_k == 1L][1], NA_real_),
      t1_loc_raw    = fifelse(any(lag_k == 1L & is.finite(location_plus)), location_plus[lag_k == 1L][1], NA_real_),
      t1_pitch_raw  = fifelse(any(lag_k == 1L & is.finite(pitching_plus)), pitching_plus[lag_k == 1L][1], NA_real_),
      t1_tunnel_raw = fifelse(any(lag_k == 1L & is.finite(pitching_tunnel_plus)), pitching_tunnel_plus[lag_k == 1L][1], NA_real_),
      t1_fg_stuff   = fifelse(any(lag_k == 1L & is.finite(fg_stuff_plus)), fg_stuff_plus[lag_k == 1L][1], NA_real_),
      t1_fg_loc     = fifelse(any(lag_k == 1L & is.finite(fg_location_plus)), fg_location_plus[lag_k == 1L][1], NA_real_),
      t1_fg_pitch   = fifelse(any(lag_k == 1L & is.finite(fg_pitching_plus)), fg_pitching_plus[lag_k == 1L][1], NA_real_),
      t1_decomp_pitches   = sum(fifelse(lag_k == 1L & is.finite(decomp_location_plus), decomp_n_pitches, 0), na.rm = TRUE),
      t1_decomp_stuff_raw = fifelse(any(lag_k == 1L & is.finite(decomp_stuff_plus)), decomp_stuff_plus[lag_k == 1L][1], NA_real_),
      t1_decomp_loc_raw   = fifelse(any(lag_k == 1L & is.finite(decomp_location_plus)), decomp_location_plus[lag_k == 1L][1], NA_real_),
      t1_decomp_pitch_raw = fifelse(any(lag_k == 1L & is.finite(decomp_pitching_plus)), decomp_pitching_plus[lag_k == 1L][1], NA_real_),
      t1_decomp_ptun_raw  = fifelse(any(lag_k == 1L & is.finite(decomp_pitching_tunnel_plus)), decomp_pitching_tunnel_plus[lag_k == 1L][1], NA_real_)
    ), by = pitcher_mlbam]

    agg[, `:=`(
      projection_year = target_yr,
      proj_role = fifelse(sp_share_w >= 0.5, "SP", "RP"),
      age_proj = fifelse(!is.na(birth_year) & !is.na(birth_month),
                         target_yr - birth_year - as.integer(birth_month >= 7L),
                         age_last + (target_yr - last_year))
    )]
    agg[, `:=`(
      proj_tbf = fifelse(proj_role == "SP", 0.50 * tbf_t1_pt + 0.10 * tbf_t2_pt + 220, 0.50 * tbf_t1_pt + 0.10 * tbf_t2_pt + 100),
      proj_ip  = fifelse(proj_role == "SP", 0.50 * ip_t1_pt  + 0.10 * ip_t2_pt  + 52,  0.50 * ip_t1_pt  + 0.10 * ip_t2_pt  + 24)
    )]

    agg <- merge(agg, lg_anchor[, .(
      proj_role = role,
      b0_k_pct = lg_k_pct, b0_bb_pct = lg_bb_pct, b0_hbp_pct = lg_hbp_pct,
      b0_hr9 = lg_hr9, b0_fip = lg_fip, b0_siera = lg_siera, b0_era = lg_era,
      anchor_tbf_ip = lg_tbf_ip, anchor_cfip = c_fip,
      a_swstr = lg_swstr, a_csw = lg_csw, a_fbv = lg_fbv, a_gb = lg_gb,
      a_hh = lg_hh, a_barrel = lg_barrel, a_xera = lg_xera, a_pull_air = lg_pull_air
    )], by = "proj_role", all.x = TRUE)

    # B1 baseline (prior-year persistence with B0 fallback strictly for B1 benchmark)
    agg[, `:=`(
      b1_k_pct  = fifelse(is.finite(obs_t1_k_pct),  obs_t1_k_pct,  b0_k_pct),
      b1_bb_pct = fifelse(is.finite(obs_t1_bb_pct), obs_t1_bb_pct, b0_bb_pct),
      b1_hr9    = fifelse(is.finite(obs_t1_hr9),    obs_t1_hr9,    b0_hr9),
      b1_fip    = fifelse(is.finite(obs_t1_fip),    obs_t1_fip,    b0_fip),
      b1_siera  = fifelse(is.finite(obs_t1_siera),  obs_t1_siera,  b0_siera),
      b1_era    = fifelse(is.finite(obs_t1_era),    obs_t1_era,    b0_era)
    )]

    agg[, `:=`(
      m_k_pct   = b0_k_pct   + ex_k_pct   / (w_tbf + reg_M[["k_pct"]]),
      m_bb_pct  = b0_bb_pct  + ex_bb_pct  / (w_tbf + reg_M[["bb_pct"]]),
      m_hbp_pct = b0_hbp_pct + ex_hbp_pct / (w_tbf + 1500),
      m_hr9     = b0_hr9     + ex_hr9     / (w_ip_hr + reg_M[["hr9"]]),
      m_fip     = b0_fip     + ex_fip     / (w_ip_fip + reg_M[["fip"]]),
      m_siera   = b0_siera   + ex_siera   / (w_tbf_siera + reg_M[["siera"]]),
      m_era     = b0_era     + ex_era     / (w_ip_era + reg_M[["era"]]),
      swstr_prior    = a_swstr    + ex_swstr    / (w_swstr + 300),
      csw_prior      = a_csw      + ex_csw      / (w_csw + 350),
      fbv_prior      = a_fbv      + ex_fbv      / (w_fbv + 150),
      gb_prior       = a_gb       + ex_gb       / (w_gb + 350),
      hh_prior       = a_hh       + ex_hh       / (w_hh + 700),
      barrel_prior   = a_barrel   + ex_barrel   / (w_barrel + 700),
      xera_prior     = a_xera     + ex_xera     / (w_xera + 300),
      pull_air_prior = a_pull_air + ex_pull_air / (w_pull_air + 450),
      delta_fbv_prior = pmin(pmax(delta_fbv_raw, -3.5), 3.5),
      stuff_plus_t1   = 100.0 + fifelse(is.finite(t1_stuff_raw),  (t1_stuff_raw  - 100.0) * (t1_sp_pitches / (t1_sp_pitches + 250.0)), 0.0),
      loc_plus_t1     = 100.0 + fifelse(is.finite(t1_loc_raw),    (t1_loc_raw    - 100.0) * (t1_sp_pitches / (t1_sp_pitches + 450.0)), 0.0),
      pitch_plus_t1   = 100.0 + fifelse(is.finite(t1_pitch_raw),  (t1_pitch_raw  - 100.0) * (t1_sp_pitches / (t1_sp_pitches + 300.0)), 0.0),
      tunnel_plus_t1  = 100.0 + fifelse(is.finite(t1_tunnel_raw), (t1_tunnel_raw - 100.0) * (t1_sp_pitches / (t1_sp_pitches + 300.0)), 0.0),
      fg_stuff_t1     = 100.0 + fifelse(is.finite(t1_fg_stuff),   (t1_fg_stuff   - 100.0) * (t1_fg_pitches / (t1_fg_pitches + 250.0)), 0.0),
      fg_loc_t1       = 100.0 + fifelse(is.finite(t1_fg_loc),     (t1_fg_loc     - 100.0) * (t1_fg_pitches / (t1_fg_pitches + 450.0)), 0.0),
      fg_pitch_t1     = 100.0 + fifelse(is.finite(t1_fg_pitch),   (t1_fg_pitch   - 100.0) * (t1_fg_pitches / (t1_fg_pitches + 300.0)), 0.0),
      decomp_stuff_plus_t1 = 100.0 + fifelse(is.finite(t1_decomp_stuff_raw), (t1_decomp_stuff_raw - 100.0) * (t1_decomp_pitches / (t1_decomp_pitches + 250.0)), 0.0),
      decomp_loc_plus_t1   = 100.0 + fifelse(is.finite(t1_decomp_loc_raw),   (t1_decomp_loc_raw   - 100.0) * (t1_decomp_pitches / (t1_decomp_pitches + 350.0)), 0.0),
      decomp_pitch_plus_t1 = 100.0 + fifelse(is.finite(t1_decomp_pitch_raw), (t1_decomp_pitch_raw - 100.0) * (t1_decomp_pitches / (t1_decomp_pitches + 250.0)), 0.0),
      decomp_ptun_plus_t1  = 100.0 + fifelse(is.finite(t1_decomp_ptun_raw),  (t1_decomp_ptun_raw  - 100.0) * (t1_decomp_pitches / (t1_decomp_pitches + 250.0)), 0.0),
      has_t1_stuff    = as.integer(is.finite(t1_stuff_raw)),
      has_t1_decomp   = as.integer(is.finite(t1_decomp_loc_raw)),
      has_t1_fg_stuff = as.integer(is.finite(t1_fg_stuff))
    )]

    # B2: True Tom Tango (2004) direct multiplier (Step 5) + Step 6 projected-exposure league rebaselining!
    agg[, tango_m := tango_age_multiplier(age_proj)]
    p_b2_raw <- list(
      k_pct = agg$m_k_pct * agg$tango_m, bb_pct = agg$m_bb_pct / agg$tango_m,
      hr9   = agg$m_hr9   / agg$tango_m, fip    = agg$m_fip    / agg$tango_m,
      siera = agg$m_siera / agg$tango_m, era    = agg$m_era    / agg$tango_m
    )
    for (met in c("k_pct", "bb_pct", "hr9", "fip", "siera", "era")) {
      wt_c <- if (met %in% c("hr9", "fip", "era")) agg$proj_ip else agg$proj_tbf
      agg[, `_r` := p_b2_raw[[met]]]
      agg[, `_m` := get(paste0("m_", met))]
      agg[, `_w` := wt_c]
      agg[, `_rebased` := `_r` + (sum(`_w` * `_m`) - sum(`_w` * `_r`)) / sum(`_w`), by = proj_role]
      agg[, paste0("b2_", met) := `_rebased`]
      agg[, c("_r", "_m", "_w", "_rebased") := NULL]
    }

    # B3: Marcel + Zero-Sum Midpoint Delta-Method Aging (linear boundary extrapolation > 39 / < 21)
    if (!is.null(aging_curves)) {
      for (met in c("k_pct", "bb_pct", "hr9", "fip", "siera", "era")) {
        d_sp <- aging_curves[["SP"]][[met]]$cum_fn(agg$age_proj) - aging_curves[["SP"]][[met]]$cum_fn(agg$w_age_bar)
        d_rp <- aging_curves[["RP"]][[met]]$cum_fn(agg$age_proj) - aging_curves[["RP"]][[met]]$cum_fn(agg$w_age_bar)
        delta_raw <- fifelse(agg$proj_role == "SP", d_sp, d_rp)
        wt_norm <- if (met %in% c("hr9", "fip", "era")) agg$proj_ip else agg$proj_tbf
        agg[, `_d` := delta_raw]; agg[, `_w` := wt_norm]
        agg[, `_mu` := sum(`_w` * `_d`, na.rm = TRUE) / sum(`_w`, na.rm = TRUE), by = proj_role]
        delta_norm <- agg$`_d` - agg$`_mu`
        agg[, c("_d", "_w", "_mu") := NULL]
        agg[, paste0("age_delta_", met) := delta_norm]
        agg[, paste0("b3_", met) := get(paste0("m_", met)) + delta_norm]
      }
    } else {
      for (met in c("k_pct", "bb_pct", "hr9", "fip", "siera", "era")) {
        agg[, paste0("age_delta_", met) := 0.0]
        agg[, paste0("b3_", met) := get(paste0("m_", met))]
      }
    }

    agg[, `:=`(
      b2_k_pct = pmax(b2_k_pct, 0.02), b3_k_pct = pmax(b3_k_pct, 0.02),
      b2_bb_pct = pmax(b2_bb_pct, 0.01), b3_bb_pct = pmax(b3_bb_pct, 0.01),
      b2_hr9 = pmax(b2_hr9, 0.15), b3_hr9 = pmax(b3_hr9, 0.15),
      b2_fip = pmax(b2_fip, 1.50), b3_fip = pmax(b3_fip, 1.50),
      b2_siera = pmax(b2_siera, 1.50), b3_siera = pmax(b3_siera, 1.50),
      b2_era = pmax(b2_era, 1.50), b3_era = pmax(b3_era, 1.50)
    )]

    if (include_actuals && target_yr <= 2025L) {
      act <- fg_pitch[game_year == target_yr, .(
        pitcher_mlbam, actual_tbf = tbf, actual_ip = ip_true, actual_role = role,
        actual_k_pct = k_pct, actual_bb_pct = bb_pct, actual_hr9 = hr9,
        actual_fip = fip, actual_siera = siera, actual_era = era_calc
      )]
      agg <- merge(agg, act, by = "pitcher_mlbam", all.x = TRUE)
    }
    agg
  }

  build_batter_year <- function(target_yr, weights = c(5, 4, 3), reg_M,
                                aging_curves = NULL, include_actuals = TRUE) {
    lags <- target_yr - 1:3
    w_map <- data.table(game_year = lags, w_yr = weights, lag_k = 1:3)
    hist <- merge(fg_bat[game_year %in% lags], w_map, by = "game_year")
    assert_true(nrow(hist) > 100L, sprintf("No batter history for target_yr=%d", target_yr))
    hist[, pa_pt := fifelse(game_year == 2020L, pmin(pa * (162 / 60), 700), pa)]

    lg_anchor <- lg_bat[game_year == (target_yr - 1L)]
    assert_true(nrow(lg_anchor) == 1L, sprintf("Expected batter league anchor for %d", target_yr - 1L))

    agg <- hist[, .(
      player_name = player_name[which.max(game_year)],
      fg_id       = fg_id[which.max(game_year)],
      team_prev   = team[which.max(game_year)],
      bats        = bats[which.max(game_year)],
      position    = position[which.max(game_year)],
      birth_year  = birth_year[which.max(game_year)],
      birth_month = birth_month[which.max(game_year)],
      last_year   = max(game_year),
      age_last    = age[which.max(game_year)],
      pa_t1    = sum(pa[lag_k == 1L]),
      pa_t1_pt = sum(pa_pt[lag_k == 1L]),
      pa_t2_pt = sum(pa_pt[lag_k == 2L]),
      w_pa  = sum(w_yr * pa), w_ab = sum(w_yr * ab), w_bip = sum(w_yr * bip_denom),
      w_age_bar = sum(w_yr * pa * age) / sum(w_yr * pa),
      ex_woba   = sum(w_yr * pa * (woba - lg_woba), na.rm = TRUE),
      ex_k_pct  = sum(w_yr * pa * (k_pct - lg_k_pct), na.rm = TRUE),
      ex_bb_pct = sum(w_yr * pa * (bb_pct - lg_bb_pct), na.rm = TRUE),
      ex_iso    = sum(w_yr * ab * (iso - lg_iso), na.rm = TRUE),
      ex_obp    = sum(w_yr * pa * (obp - lg_obp), na.rm = TRUE),
      ex_slg    = sum(w_yr * ab * (slg - lg_slg), na.rm = TRUE),
      ex_babip  = sum(w_yr * bip_denom * (babip - lg_babip), na.rm = TRUE),
      obs_t1_woba   = fifelse(any(lag_k == 1L), woba[lag_k == 1L][1], NA_real_),
      obs_t1_k_pct  = fifelse(any(lag_k == 1L), k_pct[lag_k == 1L][1], NA_real_),
      obs_t1_bb_pct = fifelse(any(lag_k == 1L), bb_pct[lag_k == 1L][1], NA_real_),
      obs_t1_iso    = fifelse(any(lag_k == 1L), iso[lag_k == 1L][1], NA_real_),
      obs_t1_obp    = fifelse(any(lag_k == 1L), obp[lag_k == 1L][1], NA_real_),
      obs_t1_slg    = fifelse(any(lag_k == 1L), slg[lag_k == 1L][1], NA_real_),
      ex_xwoba      = sum(w_yr * pa * (xwoba - lg_xwoba), na.rm = TRUE),
      w_xwoba       = sum(w_yr * pa * as.numeric(is.finite(xwoba) & is.finite(lg_xwoba))),
      ex_barrel     = sum(w_yr * pa * (barrel_pct - lg_barrel), na.rm = TRUE),
      w_barrel      = sum(w_yr * pa * as.numeric(is.finite(barrel_pct) & is.finite(lg_barrel))),
      ex_hh         = sum(w_yr * pa * (hard_hit_pct - lg_hh), na.rm = TRUE),
      w_hh          = sum(w_yr * pa * as.numeric(is.finite(hard_hit_pct) & is.finite(lg_hh))),
      ex_ev         = sum(w_yr * pa * (ev - lg_ev), na.rm = TRUE),
      w_ev          = sum(w_yr * pa * as.numeric(is.finite(ev) & is.finite(lg_ev))),
      ex_ev90       = sum(w_yr * pa * (ev90 - lg_ev90), na.rm = TRUE),
      w_ev90        = sum(w_yr * pa * as.numeric(is.finite(ev90) & is.finite(lg_ev90))),
      ex_oswing     = sum(w_yr * pa * (o_swing_pct - lg_oswing), na.rm = TRUE),
      w_oswing      = sum(w_yr * pa * as.numeric(is.finite(o_swing_pct) & is.finite(lg_oswing))),
      ex_zcon       = sum(w_yr * pa * (z_contact_pct - lg_zcon), na.rm = TRUE),
      w_zcon        = sum(w_yr * pa * as.numeric(is.finite(z_contact_pct) & is.finite(lg_zcon))),
      ex_swstr      = sum(w_yr * pa * (swstr_pct - lg_swstr), na.rm = TRUE),
      w_swstr       = sum(w_yr * pa * as.numeric(is.finite(swstr_pct) & is.finite(lg_swstr))),
      ex_zone_chase = sum(w_yr * pa * (zone_chase_selectivity - lg_zone_chase), na.rm = TRUE),
      w_zone_chase  = sum(w_yr * pa * as.numeric(is.finite(zone_chase_selectivity) & is.finite(lg_zone_chase))),
      ex_pull_air   = sum(w_yr * n_bip_spray * (pulled_air_pct - lg_pull_air), na.rm = TRUE),
      w_pull_air    = sum(w_yr * fifelse(is.finite(pulled_air_pct) & is.finite(lg_pull_air), n_bip_spray, 0)),
      ex_sweet      = sum(w_yr * n_bip_spray * (sweet_spot_pct - lg_sweet), na.rm = TRUE),
      w_sweet       = sum(w_yr * fifelse(is.finite(sweet_spot_pct) & is.finite(lg_sweet), n_bip_spray, 0)),
      ex_fld_runs = sum(w_yr * pa * (fld_runs / pa - lg_fld_runs), na.rm = TRUE),
      w_fld_runs  = sum(w_yr * pa * as.numeric(is.finite(fld_runs))),
      ex_def_runs = sum(w_yr * pa * (def_runs / pa - lg_def_runs), na.rm = TRUE),
      w_def_runs  = sum(w_yr * pa * as.numeric(is.finite(def_runs))),
      ex_bsr_runs = sum(w_yr * pa * (bsr_runs / pa - lg_bsr_runs), na.rm = TRUE),
      w_bsr_runs  = sum(w_yr * pa * as.numeric(is.finite(bsr_runs))),
      ex_pos_runs = sum(w_yr * pa * (pos_runs / pa - lg_pos_runs), na.rm = TRUE),
      w_pos_runs  = sum(w_yr * pa * as.numeric(is.finite(pos_runs))),
      t1_bp_pitches       = sum(fifelse(lag_k == 1L & is.finite(decision_plus), bp_n_pitches, 0), na.rm = TRUE),
      t1_decision_raw     = fifelse(any(lag_k == 1L & is.finite(decision_plus)), decision_plus[lag_k == 1L][1], NA_real_),
      t1_bat_ctrl_raw     = fifelse(any(lag_k == 1L & is.finite(bat_control_plus)), bat_control_plus[lag_k == 1L][1], NA_real_),
      t1_contact_raw      = fifelse(any(lag_k == 1L & is.finite(contact_plus)), contact_plus[lag_k == 1L][1], NA_real_),
      t1_process_raw      = fifelse(any(lag_k == 1L & is.finite(batter_process_plus)), batter_process_plus[lag_k == 1L][1], NA_real_),
      t1_batspeed   = fifelse(any(lag_k == 1L & is.finite(avg_bat_speed)), avg_bat_speed[lag_k == 1L][1], NA_real_),
      t1_blast      = fifelse(any(lag_k == 1L & is.finite(blast_swing_pct)), blast_swing_pct[lag_k == 1L][1], NA_real_),
      t1_sq_swing   = fifelse(any(lag_k == 1L & is.finite(squared_up_swing_pct)), squared_up_swing_pct[lag_k == 1L][1], NA_real_),
      t1_swing_len  = fifelse(any(lag_k == 1L & is.finite(swing_length)), swing_length[lag_k == 1L][1], NA_real_)
    ), by = batter_mlbam]

    agg[, `:=`(
      projection_year = target_yr,
      age_proj = fifelse(!is.na(birth_year) & !is.na(birth_month),
                         target_yr - birth_year - as.integer(birth_month >= 7L),
                         age_last + (target_yr - last_year)),
      proj_pa = 0.50 * pa_t1_pt + 0.10 * pa_t2_pt + 200.0
    )]

    agg[, `:=`(
      b0_woba = lg_anchor$lg_woba, b0_k_pct = lg_anchor$lg_k_pct, b0_bb_pct = lg_anchor$lg_bb_pct,
      b0_iso  = lg_anchor$lg_iso,  b0_obp   = lg_anchor$lg_obp,   b0_slg    = lg_anchor$lg_slg,
      b0_babip = lg_anchor$lg_babip,
      a_xwoba = lg_anchor$lg_xwoba, a_barrel = lg_anchor$lg_barrel, a_hh = lg_anchor$lg_hh,
      a_ev = lg_anchor$lg_ev, a_ev90 = lg_anchor$lg_ev90, a_oswing = lg_anchor$lg_oswing,
      a_zcon = lg_anchor$lg_zcon, a_swstr = lg_anchor$lg_swstr, a_zone_chase = lg_anchor$lg_zone_chase,
      a_pull_air = lg_anchor$lg_pull_air, a_sweet = lg_anchor$lg_sweet,
      a_batspeed = lg_anchor$lg_batspeed, a_blast = lg_anchor$lg_blast,
      a_sq_swing = lg_anchor$lg_sq_swing, a_swing_len = lg_anchor$lg_swing_len
    )]

    agg[, `:=`(
      b1_woba   = fifelse(is.finite(obs_t1_woba),   obs_t1_woba,   b0_woba),
      b1_k_pct  = fifelse(is.finite(obs_t1_k_pct),  obs_t1_k_pct,  b0_k_pct),
      b1_bb_pct = fifelse(is.finite(obs_t1_bb_pct), obs_t1_bb_pct, b0_bb_pct),
      b1_iso    = fifelse(is.finite(obs_t1_iso),    obs_t1_iso,    b0_iso),
      b1_obp    = fifelse(is.finite(obs_t1_obp),    obs_t1_obp,    b0_obp),
      b1_slg    = fifelse(is.finite(obs_t1_slg),    obs_t1_slg,    b0_slg)
    )]

    agg[, `:=`(
      m_woba   = b0_woba   + ex_woba   / (w_pa  + reg_M[["woba"]]),
      m_k_pct  = b0_k_pct  + ex_k_pct  / (w_pa  + reg_M[["k_pct"]]),
      m_bb_pct = b0_bb_pct + ex_bb_pct / (w_pa  + reg_M[["bb_pct"]]),
      m_iso    = b0_iso    + ex_iso    / (w_ab  + reg_M[["iso"]]),
      m_obp    = b0_obp    + ex_obp    / (w_pa  + reg_M[["obp"]]),
      m_slg    = b0_slg    + ex_slg    / (w_ab  + reg_M[["slg"]]),
      m_babip  = b0_babip  + ex_babip  / (w_bip + 1800),
      xwoba_prior      = a_xwoba      + ex_xwoba      / (w_xwoba + 600),
      barrel_prior     = a_barrel     + ex_barrel     / (w_barrel + 500),
      hh_prior         = a_hh         + ex_hh         / (w_hh + 450),
      ev_prior         = a_ev         + ex_ev         / (w_ev + 350),
      ev90_prior       = a_ev90       + ex_ev90       / (w_ev90 + 350),
      oswing_prior     = a_oswing     + ex_oswing     / (w_oswing + 350),
      zcon_prior       = a_zcon       + ex_zcon       / (w_zcon + 350),
      swstr_prior      = a_swstr      + ex_swstr      / (w_swstr + 300),
      zone_chase_prior = a_zone_chase + ex_zone_chase / (w_zone_chase + 350),
      pull_air_prior   = a_pull_air   + ex_pull_air   / (w_pull_air + 350),
      sweet_prior      = a_sweet      + ex_sweet      / (w_sweet + 450),
      decision_plus_t1    = 100.0 + fifelse(is.finite(t1_decision_raw), (t1_decision_raw - 100.0) * (t1_bp_pitches / (t1_bp_pitches + 250.0)), 0.0),
      bat_control_plus_t1 = 100.0 + fifelse(is.finite(t1_bat_ctrl_raw), (t1_bat_ctrl_raw - 100.0) * (t1_bp_pitches / (t1_bp_pitches + 250.0)), 0.0),
      contact_plus_t1     = 100.0 + fifelse(is.finite(t1_contact_raw),  (t1_contact_raw  - 100.0) * (t1_bp_pitches / (t1_bp_pitches + 300.0)), 0.0),
      process_plus_t1     = 100.0 + fifelse(is.finite(t1_process_raw),  (t1_process_raw  - 100.0) * (t1_bp_pitches / (t1_bp_pitches + 250.0)), 0.0),
      has_t1_process      = as.integer(is.finite(t1_decision_raw)),
      proj_fld_runs       = (lg_anchor$lg_fld_runs + ex_fld_runs / (w_fld_runs + 1200.0)) * proj_pa,
      proj_bsr_runs       = (lg_anchor$lg_bsr_runs + ex_bsr_runs / (w_bsr_runs + 600.0)) * proj_pa,
      proj_pos_runs       = (lg_anchor$lg_pos_runs + ex_pos_runs / (w_pos_runs + 300.0)) * proj_pa,
      proj_def_runs       = (lg_anchor$lg_fld_runs + ex_fld_runs / (w_fld_runs + 1200.0)) * proj_pa + (lg_anchor$lg_pos_runs + ex_pos_runs / (w_pos_runs + 300.0)) * proj_pa,
      batspeed_t1      = a_batspeed   + fifelse(is.finite(t1_batspeed),  (t1_batspeed  - a_batspeed)  * (pa_t1 / (pa_t1 + 50)),  0.0),
      blast_t1         = a_blast      + fifelse(is.finite(t1_blast),     (t1_blast     - a_blast)     * (pa_t1 / (pa_t1 + 150)), 0.0),
      sq_swing_t1      = a_sq_swing   + fifelse(is.finite(t1_sq_swing),  (t1_sq_swing  - a_sq_swing)  * (pa_t1 / (pa_t1 + 150)), 0.0),
      swing_len_t1     = a_swing_len  + fifelse(is.finite(t1_swing_len), (t1_swing_len - a_swing_len) * (pa_t1 / (pa_t1 + 50)),  0.0),
      has_t1_battrack  = as.integer(is.finite(t1_batspeed))
    )]
    agg[, xwoba_gap_prior := xwoba_prior - m_woba]

    # B2: True Tom Tango (2004) direct multiplier (Step 5) + Step 6 projected-PA league rebaselining!
    agg[, tango_m := tango_age_multiplier(age_proj)]
    b_b2_raw <- list(
      woba = agg$m_woba * agg$tango_m, k_pct = agg$m_k_pct / agg$tango_m,
      bb_pct = agg$m_bb_pct * agg$tango_m, iso = agg$m_iso * agg$tango_m,
      obp = agg$m_obp * agg$tango_m, slg = agg$m_slg * agg$tango_m
    )
    for (met in c("woba", "k_pct", "bb_pct", "iso", "obp", "slg")) {
      r_v <- b_b2_raw[[met]]; m_v <- agg[[paste0("m_", met)]]; w_v <- agg$proj_pa
      agg[, paste0("b2_", met) := r_v + (sum(w_v * m_v) - sum(w_v * r_v)) / sum(w_v)]
    }

    if (!is.null(aging_curves)) {
      for (met in c("woba", "k_pct", "bb_pct", "iso", "obp", "slg")) {
        delta_raw <- aging_curves[[met]]$cum_fn(agg$age_proj) - aging_curves[[met]]$cum_fn(agg$w_age_bar)
        delta_mu <- sum(agg$proj_pa * delta_raw, na.rm = TRUE) / sum(agg$proj_pa, na.rm = TRUE)
        delta_norm <- delta_raw - delta_mu
        agg[, paste0("age_delta_", met) := delta_norm]
        agg[, paste0("b3_", met) := get(paste0("m_", met)) + delta_norm]
      }
    } else {
      for (met in c("woba", "k_pct", "bb_pct", "iso", "obp", "slg")) {
        agg[, paste0("age_delta_", met) := 0.0]
        agg[, paste0("b3_", met) := get(paste0("m_", met))]
      }
    }

    agg[, `:=`(
      b2_woba = pmax(b2_woba, 0.150), b3_woba = pmax(b3_woba, 0.150),
      b2_k_pct = pmax(b2_k_pct, 0.03), b3_k_pct = pmax(b3_k_pct, 0.03),
      b2_bb_pct = pmax(b2_bb_pct, 0.01), b3_bb_pct = pmax(b3_bb_pct, 0.01),
      b2_iso = pmax(b2_iso, 0.02), b3_iso = pmax(b3_iso, 0.02),
      b2_obp = pmax(b2_obp, 0.18), b3_obp = pmax(b3_obp, 0.18),
      b2_slg = pmax(b2_slg, 0.22), b3_slg = pmax(b3_slg, 0.22)
    )]

    if (include_actuals && target_yr <= 2025L) {
      act <- fg_bat[game_year == target_yr, .(
        batter_mlbam, actual_pa = pa, actual_ab = ab,
        actual_woba = woba, actual_k_pct = k_pct, actual_bb_pct = bb_pct,
        actual_iso = iso, actual_obp = obp, actual_slg = slg
      )]
      agg <- merge(agg, act, by = "batter_mlbam", all.x = TRUE)
    }
    agg
  }

  # ============================================================================
  # 4A. PRE-BACKTEST (2006-2015) CALIBRATION OF M_r & ROLLING 2016-2026 ENGINE
  # ============================================================================
  cal_years  <- 2006L:2015L
  pitch_mets <- c("k_pct", "bb_pct", "hr9", "fip", "siera", "era")
  bat_mets   <- c("woba", "k_pct", "bb_pct", "iso", "obp", "slg")
  pitch_wcol <- c(k_pct = "actual_tbf", bb_pct = "actual_tbf", hr9 = "actual_ip",
                  fip = "actual_ip", siera = "actual_tbf", era = "actual_ip")
  bat_wcol   <- c(woba = "actual_pa", k_pct = "actual_pa", bb_pct = "actual_pa",
                  iso = "actual_ab", obp = "actual_pa", slg = "actual_ab")

  calibrate_M_pitch <- function(weights_vec) {
    grid_tbf <- c(150, 250, 400, 600, 900, 1200, 1600, 2200, 3000)
    grid_ip  <- c(80, 140, 220, 320, 450, 650, 900, 1250, 1700)
    base_M <- list(k_pct = 500, bb_pct = 900, hr9 = 500, fip = 400, siera = 900, era = 700)
    cal_dt <- rbindlist(lapply(cal_years, function(yr) build_pitcher_year(yr, weights_vec, base_M, NULL, TRUE)), fill = TRUE)
    cal_dt <- cal_dt[!is.na(actual_tbf) & actual_tbf >= 100]
    best_M <- list()
    for (m in pitch_mets) {
      cand <- if (m %in% c("hr9", "fip", "era")) grid_ip else grid_tbf
      w_den <- if (m == "hr9") "w_ip_hr" else if (m == "fip") "w_ip_fip" else if (m == "era") "w_ip_era" else if (m == "siera") "w_tbf_siera" else "w_tbf"
      errs <- sapply(cand, function(M_val) {
        pred <- cal_dt[[paste0("b0_", m)]] + cal_dt[[paste0("ex_", m)]] / (cal_dt[[w_den]] + M_val)
        w_rmse(pred, cal_dt[[paste0("actual_", m)]], cal_dt[[pitch_wcol[[m]]]])
      })
      best_M[[m]] <- cand[which.min(errs)]
    }
    best_M
  }

  calibrate_M_bat <- function(weights_vec = c(5, 4, 3)) {
    grid_pa <- c(250, 400, 600, 900, 1200, 1600, 2200, 3000)
    base_M <- list(woba = 1200, k_pct = 500, bb_pct = 800, iso = 1000, obp = 1200, slg = 1200)
    cal_dt <- rbindlist(lapply(cal_years, function(yr) build_batter_year(yr, weights_vec, base_M, NULL, TRUE)), fill = TRUE)
    cal_dt <- cal_dt[!is.na(actual_pa) & actual_pa >= 100]
    best_M <- list()
    for (m in bat_mets) {
      w_den <- if (m %in% c("iso", "slg")) "w_ab" else "w_pa"
      errs <- sapply(grid_pa, function(M_val) {
        pred <- cal_dt[[paste0("b0_", m)]] + cal_dt[[paste0("ex_", m)]] / (cal_dt[[w_den]] + M_val)
        w_rmse(pred, cal_dt[[paste0("actual_", m)]], cal_dt[[bat_wcol[[m]]]])
      })
      best_M[[m]] <- grid_pa[which.min(errs)]
    }
    best_M
  }

  reg_M_pitch_321 <- calibrate_M_pitch(c(3, 2, 1))
  reg_M_pitch_543 <- calibrate_M_pitch(c(5, 4, 3))
  reg_M_bat_543   <- calibrate_M_bat(c(5, 4, 3))
  cat("   Pitcher M (3/2/1):", paste(names(reg_M_pitch_321), unlist(reg_M_pitch_321), sep = "=", collapse = ", "), "\n")
  cat("   Batter  M (5/4/3):", paste(names(reg_M_bat_543), unlist(reg_M_bat_543), sep = "=", collapse = ", "), "\n")

  cat("\n-> [3/7] Running strictly chronological rolling Marcel + Delta-Aging (2016-2026)...\n")
  fit_all_pitch_aging <- function(max_yr) {
    out <- list(SP = list(), RP = list())
    for (rl in c("SP", "RP")) {
      for (m in pitch_mets) {
        exp_c <- if (m %in% c("hr9", "fip", "era")) "ip_true" else "tbf"
        min_e <- if (m %in% c("hr9", "fip", "era")) 15 else 50
        out[[rl]][[m]] <- fit_delta_aging_curve(
          fg_pitch, "pitcher_mlbam", if (m == "era") "era_calc" else m,
          paste0("lg_", m), exp_c, max_year = max_yr, role_filter = rl, min_exp = min_e
        )
      }
    }
    out
  }
  fit_all_bat_aging <- function(max_yr) {
    out <- list()
    for (m in bat_mets) {
      exp_c <- if (m %in% c("iso", "slg")) "ab" else "pa"
      out[[m]] <- fit_delta_aging_curve(
        fg_bat, "batter_mlbam", m, paste0("lg_", m), exp_c, max_year = max_yr, role_filter = NULL, min_exp = 50
      )
    }
    out
  }

  all_years <- 2016L:2026L
  pitch_panel_list <- vector("list", length(all_years))
  pitch_543_list   <- vector("list", length(all_years))
  bat_panel_list   <- vector("list", length(all_years))
  for (i in seq_along(all_years)) {
    yr <- all_years[i]
    ag_p <- fit_all_pitch_aging(yr - 1L)
    ag_b <- fit_all_bat_aging(yr - 1L)
    pitch_panel_list[[i]] <- build_pitcher_year(yr, c(3, 2, 1), reg_M_pitch_321, ag_p, include_actuals = (yr <= 2025L))
    pitch_543_list[[i]]   <- build_pitcher_year(yr, c(5, 4, 3), reg_M_pitch_543, ag_p, include_actuals = (yr <= 2025L))
    bat_panel_list[[i]]   <- build_batter_year(yr,  c(5, 4, 3), reg_M_bat_543,   ag_b, include_actuals = (yr <= 2025L))
  }
  pitch_panel <- rbindlist(pitch_panel_list, fill = TRUE)
  pitch_543   <- rbindlist(pitch_543_list, fill = TRUE)
  bat_panel   <- rbindlist(bat_panel_list, fill = TRUE)

  # Export 2002-2025 Delta-Method vs Naive Cross-Sectional aging curves (aligned min_exp: 15 IP vs 50 TBF/PA!)
  ag_p_full <- fit_all_pitch_aging(2025L)
  ag_b_full <- fit_all_bat_aging(2025L)
  ag_fbv_sp <- fit_delta_aging_curve(fg_pitch, "pitcher_mlbam", "fbv", "lg_fbv", "tbf", 2025L, "SP", 50)
  ag_fbv_rp <- fit_delta_aging_curve(fg_pitch, "pitcher_mlbam", "fbv", "lg_fbv", "tbf", 2025L, "RP", 50)

  build_aging_export <- function(curve_obj, dt, metric_col, lg_col, exp_col, group_label, metric_label, role_filter = NULL) {
    min_e <- if (exp_col == "ip_true") 15 else 50
    sub <- dt[game_year <= 2025L & is.finite(get(metric_col)) & is.finite(get(lg_col)) &
              get(exp_col) >= min_e & age >= 21 & age <= 39]
    if (!is.null(role_filter)) sub <- sub[role == role_filter]
    naive <- sub[, .(n_obs_cross = .N,
                     naive_rel = sum(get(exp_col) * (get(metric_col) - get(lg_col))) / sum(get(exp_col))),
                 by = .(age = as.integer(age))]
    base27 <- naive[age == 27L]$naive_rel[1]
    assert_true(is.finite(base27), sprintf("Missing Age 27 naive anchor for %s %s", group_label, metric_label))
    naive[, naive_centered := naive_rel - base27]
    res <- merge(curve_obj$curve_dt, naive, by = "age", all.x = TRUE)
    res[, `:=`(group = group_label, metric = metric_label)]
    res
  }

  aging_export <- rbindlist(list(
    build_aging_export(ag_b_full$woba,     fg_bat,   "woba",  "lg_woba",  "pa",      "Batter", "wOBA"),
    build_aging_export(ag_b_full$iso,      fg_bat,   "iso",   "lg_iso",   "ab",      "Batter", "ISO"),
    build_aging_export(ag_b_full$k_pct,    fg_bat,   "k_pct", "lg_k_pct", "pa",      "Batter", "K%"),
    build_aging_export(ag_b_full$bb_pct,   fg_bat,   "bb_pct","lg_bb_pct","pa",      "Batter", "BB%"),
    build_aging_export(ag_p_full$SP$k_pct, fg_pitch, "k_pct", "lg_k_pct", "tbf",     "SP", "K%", "SP"),
    build_aging_export(ag_p_full$SP$fip,   fg_pitch, "fip",   "lg_fip",   "ip_true", "SP", "FIP", "SP"),
    build_aging_export(ag_fbv_sp,          fg_pitch, "fbv",   "lg_fbv",   "tbf",     "SP", "FBv (mph)", "SP"),
    build_aging_export(ag_p_full$RP$k_pct, fg_pitch, "k_pct", "lg_k_pct", "tbf",     "RP", "K%", "RP"),
    build_aging_export(ag_p_full$RP$fip,   fg_pitch, "fip",   "lg_fip",   "ip_true", "RP", "FIP", "RP"),
    build_aging_export(ag_fbv_rp,          fg_pitch, "fbv",   "lg_fbv",   "tbf",     "RP", "FBv (mph)", "RP")
  ), fill = TRUE)

  # ============================================================================
  # 5. HARDENED CHRONOLOGICAL RIDGE & STAGE-2 RESIDUAL STACKING (M4)
  # ============================================================================
  cat("\n-> [4/7] Fitting strictly chronological Stage-1 Statcast Physics & Stage-2 Sensor Residual Ridge...\n")

  fit_predict_ridge <- function(train_dt, test_dt, target_col, weight_col, feat_cols,
                                season_col = "projection_year", seed_val = 2026L, max_imp_frac = 0.15) {
    tr <- copy(train_dt[is.finite(get(target_col)) & is.finite(get(weight_col)) & get(weight_col) > 0])
    te <- copy(test_dt)
    assert_true(nrow(tr) >= 50L && nrow(te) > 0L, sprintf("Insufficient train/test rows for %s", target_col))
    assert_true(all(feat_cols %in% names(tr)) && all(feat_cols %in% names(te)), sprintf("Missing features for %s", target_col))
    w_tr <- tr[[weight_col]]; y_tr <- tr[[target_col]]
    X_tr <- matrix(NA_real_, nrow = nrow(tr), ncol = length(feat_cols), dimnames = list(NULL, feat_cols))
    X_te <- matrix(NA_real_, nrow = nrow(te), ncol = length(feat_cols), dimnames = list(NULL, feat_cols))
    n_imp_tr <- 0L; n_imp_te <- 0L
    for (j in seq_along(feat_cols)) {
      fc <- feat_cols[j]; v_tr <- as.numeric(tr[[fc]]); v_te <- as.numeric(te[[fc]])
      ok_j <- is.finite(v_tr)
      assert_true(sum(ok_j) > 10L && mean(!ok_j) <= max_imp_frac,
                  sprintf("Feature %s train missingness %.1f%% exceeds %.0f%%", fc, 100 * mean(!ok_j), 100 * max_imp_frac))
      mu_j <- sum(w_tr[ok_j] * v_tr[ok_j]) / sum(w_tr[ok_j])
      if (any(!ok_j)) { n_imp_tr <- n_imp_tr + sum(!ok_j); v_tr[!ok_j] <- mu_j }
      bad_te <- !is.finite(v_te)
      assert_true(mean(bad_te) <= max_imp_frac,
                  sprintf("Feature %s test missingness %.1f%% exceeds %.0f%%", fc, 100 * mean(bad_te), 100 * max_imp_frac))
      if (any(bad_te)) { n_imp_te <- n_imp_te + sum(bad_te); v_te[bad_te] <- mu_j }
      X_tr[, j] <- v_tr; X_te[, j] <- v_te
    }
    set.seed(seed_val)
    yrs_tr <- sort(unique(tr[[season_col]]))
    foldid <- if (length(yrs_tr) >= 4L) match(tr[[season_col]], yrs_tr) else sample(rep(seq_len(10L), length.out = nrow(tr)))
    cv_fit <- glmnet::cv.glmnet(X_tr, y_tr, weights = w_tr, alpha = 0, standardize = TRUE, foldid = foldid)
    preds <- as.numeric(stats::predict(cv_fit, newx = X_te, s = "lambda.min"))
    assert_true(all(is.finite(preds)), sprintf("Non-finite Ridge predictions for %s", target_col))
    list(pred = preds, n_imp_tr = n_imp_tr, n_imp_te = n_imp_te)
  }

  fit_predict_residual_ridge <- function(train_dt, test_dt, base_col, target_col, weight_col,
                                         sensor_cols, seed_val = 5050L, max_imp_frac = 0.15) {
    assert_true(all(c(base_col, target_col, weight_col, sensor_cols) %in% names(train_dt)),
                sprintf("Missing columns in residual train_dt for %s", target_col))
    assert_true(all(c(base_col, sensor_cols) %in% names(test_dt)),
                sprintf("Missing columns in residual test_dt for %s", target_col))
    tr <- copy(train_dt[is.finite(get(base_col)) & is.finite(get(target_col)) &
                        is.finite(get(weight_col)) & get(weight_col) > 0])
    te <- copy(test_dt)
    assert_true(nrow(tr) >= 50L && nrow(te) > 0L, sprintf("Insufficient residual train/test rows for %s", target_col))
    w_tr <- tr[[weight_col]]
    resid_tr <- tr[[target_col]] - tr[[base_col]]
    X_tr <- matrix(NA_real_, nrow = nrow(tr), ncol = length(sensor_cols), dimnames = list(NULL, sensor_cols))
    X_te <- matrix(NA_real_, nrow = nrow(te), ncol = length(sensor_cols), dimnames = list(NULL, sensor_cols))
    n_imp_tr <- 0L; n_imp_te <- 0L
    for (j in seq_along(sensor_cols)) {
      fc <- sensor_cols[j]; v_tr <- as.numeric(tr[[fc]]); v_te <- as.numeric(te[[fc]])
      ok_j <- is.finite(v_tr)
      assert_true(sum(ok_j) > 10L && mean(!ok_j) <= max_imp_frac,
                  sprintf("Sensor %s train missingness %.1f%% exceeds limit", fc, 100 * mean(!ok_j)))
      mu_j <- sum(w_tr[ok_j] * v_tr[ok_j]) / sum(w_tr[ok_j])
      if (any(!ok_j)) { n_imp_tr <- n_imp_tr + sum(!ok_j); v_tr[!ok_j] <- mu_j }
      bad_te <- !is.finite(v_te)
      assert_true(mean(bad_te) <= max_imp_frac,
                  sprintf("Sensor %s test missingness %.1f%% exceeds limit", fc, 100 * mean(bad_te)))
      if (any(bad_te)) { n_imp_te <- n_imp_te + sum(bad_te); v_te[bad_te] <- mu_j }
      X_tr[, j] <- v_tr; X_te[, j] <- v_te
    }
    set.seed(seed_val)
    n_f <- min(10L, max(5L, floor(nrow(tr) / 40L)))
    foldid <- sample(rep(seq_len(n_f), length.out = nrow(tr)))
    cv_fit <- glmnet::cv.glmnet(X_tr, resid_tr, weights = w_tr, alpha = 0, standardize = TRUE, foldid = foldid)
    b_vec <- as.numeric(stats::coef(cv_fit, s = "lambda.min"))[-1]
    mu_vec <- apply(X_tr, 2, function(col) sum(w_tr * col) / sum(w_tr))
    delta_te <- as.numeric((X_te - rep(mu_vec, each = nrow(X_te))) %*% b_vec)
    pred_te <- te[[base_col]] + delta_te
    assert_true(all(is.finite(pred_te)), sprintf("Non-finite residual Ridge predictions for %s", target_col))
    list(pred = pred_te, delta = delta_te, coef = setNames(b_vec, sensor_cols),
         mu_vec = setNames(mu_vec, sensor_cols), n_imp_tr = n_imp_tr, n_imp_te = n_imp_te)
  }

  pitch_panel[, `:=`(is_rp = as.numeric(proj_role == "RP"), age_over28 = pmax(age_proj - 28.0, 0.0))]
  bat_panel[, `:=`(age_over28 = pmax(age_proj - 28.0, 0.0))]

  bat_statcast_feats <- c(
    "b3_woba", "b3_k_pct", "b3_bb_pct", "b3_iso", "m_babip",
    "xwoba_prior", "xwoba_gap_prior", "barrel_prior", "hh_prior", "ev90_prior",
    "pull_air_prior", "sweet_prior", "oswing_prior", "zcon_prior", "swstr_prior",
    "zone_chase_prior", "age_proj", "age_over28"
  )
  bat_sensor_cols <- c("batspeed_t1", "blast_t1", "sq_swing_t1", "swing_len_t1")
  pitch_statcast_feats <- c(
    "b3_k_pct", "b3_bb_pct", "b3_hr9", "b3_fip", "b3_siera", "is_rp",
    "swstr_prior", "csw_prior", "fbv_prior", "delta_fbv_prior",
    "gb_prior", "hh_prior", "barrel_prior", "xera_prior",
    "pull_air_prior", "pvi_xwoba_prior", "pvi_csw_prior", "age_proj", "age_over28"
  )
  pitch_stuff_sensors    <- c("stuff_plus_t1", "decomp_loc_plus_t1", "decomp_pitch_plus_t1", "decomp_ptun_plus_t1", "tunnel_plus_t1")
  pitch_fg_stuff_sensors <- c("fg_stuff_t1", "fg_loc_t1", "fg_pitch_t1")
  bat_process_sensors    <- c("decision_plus_t1", "bat_control_plus_t1", "contact_plus_t1", "process_plus_t1")

  for (m in bat_mets)   bat_panel[, c(paste0("m4_", m), paste0("m4_process_", m), paste0("m4_battrack_", m)) := NA_real_]
  for (m in pitch_mets) pitch_panel[, c(paste0("m4_sc_", m), paste0("m4_stuff_", m), paste0("m4_fgstuff_", m)) := NA_real_]

  tot_imp_tr <- 0L; tot_imp_te <- 0L
  for (yr in 2019L:2026L) {
    tr_b <- bat_panel[projection_year >= 2016L & projection_year <= (yr - 1L) & !is.na(actual_pa) & actual_pa >= 50]
    te_b_idx <- which(bat_panel$projection_year == yr)
    for (m in bat_mets) {
      res <- fit_predict_ridge(tr_b, bat_panel[te_b_idx], paste0("actual_", m), bat_wcol[[m]], bat_statcast_feats, seed_val = 2026L + yr)
      bat_panel[te_b_idx, (paste0("m4_", m)) := res$pred]
      tot_imp_tr <- tot_imp_tr + res$n_imp_tr; tot_imp_te <- tot_imp_te + res$n_imp_te
    }
    tr_p <- pitch_panel[projection_year >= 2016L & projection_year <= (yr - 1L) & !is.na(actual_tbf) & actual_tbf >= 50]
    te_p_idx <- which(pitch_panel$projection_year == yr)
    for (m in pitch_mets) {
      res <- fit_predict_ridge(tr_p, pitch_panel[te_p_idx], paste0("actual_", m), pitch_wcol[[m]], pitch_statcast_feats, seed_val = 3026L + yr)
      pitch_panel[te_p_idx, (paste0("m4_sc_", m)) := res$pred]
      tot_imp_tr <- tot_imp_tr + res$n_imp_tr; tot_imp_te <- tot_imp_te + res$n_imp_te
    }
  }
  cat(sprintf("   Stage-1 Ridge total imputed cells across all folds/metrics: train=%d, test=%d\n", tot_imp_tr, tot_imp_te))

  tot_s2_imp_tr <- 0L; tot_s2_imp_te <- 0L

  # Batter Stage-2 Process Ridge across 2021-2026
  tr_bp_all <- bat_panel[projection_year >= 2021L & !is.na(actual_pa) & actual_pa >= 50 & has_t1_process == 1L]
  for (yr in 2022L:2026L) {
    tr_bp <- tr_bp_all[projection_year <= (yr - 1L)]
    te_idx <- which(bat_panel$projection_year == yr); te_bp <- bat_panel[te_idx]
    act_bp <- which(te_bp$has_t1_process == 1L)
    if (nrow(tr_bp) >= 50L && length(act_bp) > 0L) {
      for (m in c("woba", "iso", "k_pct", "bb_pct")) {
        base_v <- te_bp[[paste0("m4_", m)]]
        res_bp <- fit_predict_residual_ridge(tr_bp, te_bp[act_bp], paste0("m4_", m), paste0("actual_", m),
                                             bat_wcol[[m]], bat_process_sensors, seed_val = 3030L + yr)
        tot_s2_imp_tr <- tot_s2_imp_tr + res_bp$n_imp_tr; tot_s2_imp_te <- tot_s2_imp_te + res_bp$n_imp_te
        out_v <- base_v; out_v[act_bp] <- res_bp$pred
        bat_panel[te_idx, (paste0("m4_process_", m)) := out_v]
      }
    }
  }

  tr_bt <- bat_panel[projection_year == 2024L & !is.na(actual_pa) & actual_pa >= 50 & has_t1_battrack == 1L]
  for (yr in 2025L:2026L) {
    te_idx <- which(bat_panel$projection_year == yr); te_bt <- bat_panel[te_idx]
    act_bt <- which(te_bt$has_t1_battrack == 1L)
    for (m in c("woba", "iso", "k_pct", "bb_pct")) {
      base_v <- fifelse(is.finite(te_bt[[paste0("m4_process_", m)]]), te_bt[[paste0("m4_process_", m)]], te_bt[[paste0("m4_", m)]])
      res_bt <- fit_predict_residual_ridge(tr_bt, te_bt[act_bt], paste0("m4_", m), paste0("actual_", m),
                                           bat_wcol[[m]], bat_sensor_cols, seed_val = 4040L + yr)
      tot_s2_imp_tr <- tot_s2_imp_tr + res_bt$n_imp_tr; tot_s2_imp_te <- tot_s2_imp_te + res_bt$n_imp_te
      out_v <- base_v; out_v[act_bt] <- res_bt$pred
      bat_panel[te_idx, (paste0("m4_battrack_", m)) := out_v]
    }
  }

  tr_sp_all <- pitch_panel[projection_year >= 2021L & !is.na(actual_tbf) & actual_tbf >= 50 & (has_t1_stuff == 1L | has_t1_decomp == 1L)]
  tr_fg_24  <- pitch_panel[projection_year == 2024L & !is.na(actual_tbf) & actual_tbf >= 50 & has_t1_fg_stuff == 1L]
  pitch_stuff_fits <- list()
  for (yr in 2025L:2026L) {
    tr_sp <- tr_sp_all[projection_year <= (yr - 1L)]
    te_idx <- which(pitch_panel$projection_year == yr); te_sp <- pitch_panel[te_idx]
    act_sp <- which(te_sp$has_t1_stuff == 1L | te_sp$has_t1_decomp == 1L)
    act_fg <- which(te_sp$has_t1_fg_stuff == 1L)
    for (m in pitch_mets) {
      base_v <- te_sp[[paste0("m4_sc_", m)]]
      res_sp <- fit_predict_residual_ridge(tr_sp, te_sp[act_sp], paste0("m4_sc_", m), paste0("actual_", m),
                                           pitch_wcol[[m]], pitch_stuff_sensors, seed_val = 5050L + yr)
      res_fg <- fit_predict_residual_ridge(tr_fg_24, te_sp[act_fg], paste0("m4_sc_", m), paste0("actual_", m),
                                           pitch_wcol[[m]], pitch_fg_stuff_sensors, seed_val = 6060L + yr)
      tot_s2_imp_tr <- tot_s2_imp_tr + res_sp$n_imp_tr + res_fg$n_imp_tr
      tot_s2_imp_te <- tot_s2_imp_te + res_sp$n_imp_te + res_fg$n_imp_te
      out_sp <- base_v; out_sp[act_sp] <- res_sp$pred
      out_fg <- base_v; out_fg[act_fg] <- res_fg$pred
      pitch_panel[te_idx, (paste0("m4_stuff_", m))   := out_sp]
      pitch_panel[te_idx, (paste0("m4_fgstuff_", m)) := out_fg]
      if (yr == 2025L) pitch_stuff_fits[[m]] <- list(coef = res_sp$coef, mu_vec = res_sp$mu_vec)
    }
  }
  cat(sprintf("   Stage-2 Residual Ridge total imputed cells (active sensor slices): train=%d, test=%d\n", tot_s2_imp_tr, tot_s2_imp_te))

  for (m in pitch_mets) {
    pitch_panel[, paste0("proj_", m) := fifelse(
      projection_year >= 2025L & is.finite(get(paste0("m4_stuff_", m))), get(paste0("m4_stuff_", m)),
      fifelse(is.finite(get(paste0("m4_sc_", m))), get(paste0("m4_sc_", m)), get(paste0("b3_", m)))
    )]
  }
  pitch_panel[, proj_fip_comp := (13.0 * proj_hr9 + 3.0 * (proj_bb_pct + m_hbp_pct) * anchor_tbf_ip * 9.0 -
                                  2.0 * proj_k_pct * anchor_tbf_ip * 9.0) / 9.0 + anchor_cfip]
  pitch_panel[, `:=`(
    proj_rar = (b0_era - proj_fip + fifelse(proj_role == "SP", 0.38, 0.47)) * (proj_ip / 9.0)
  )]
  pitch_panel[, proj_war := proj_rar / 9.77]

  for (m in c("woba", "iso", "k_pct", "bb_pct")) {
    bat_panel[, paste0("proj_", m) := fifelse(
      is.finite(get(paste0("m4_battrack_", m))), get(paste0("m4_battrack_", m)),
      fifelse(is.finite(get(paste0("m4_process_", m))), get(paste0("m4_process_", m)),
      fifelse(is.finite(get(paste0("m4_", m))), get(paste0("m4_", m)), get(paste0("b3_", m))))
    )]
  }
  for (m in setdiff(bat_mets, c("woba", "iso", "k_pct", "bb_pct"))) {
    bat_panel[, paste0("proj_", m) := fifelse(is.finite(get(paste0("m4_", m))), get(paste0("m4_", m)), get(paste0("b3_", m)))]
  }
  bat_panel[, `:=`(
    proj_wraa = ((proj_woba - b0_woba) / 1.15) * proj_pa,
    proj_repl_runs = (proj_pa / 600.0) * 20.0
  )]
  bat_panel[, proj_war := (proj_wraa + proj_bsr_runs + proj_fld_runs + proj_pos_runs + proj_repl_runs) / 9.77]
  bat_panel[, proj_def_runs := proj_fld_runs + proj_pos_runs]

  # ============================================================================
  # 5C. EXPOSURE-CALIBRATED 80% UNCERTAINTY BANDS (STRICTLY <= 2024)
  # ============================================================================
  calibrate_uncertainty <- function(dt, pred_col, actual_col, exp_col, max_yr = 2024L, min_exp = 100) {
    sub <- dt[projection_year >= 2019L & projection_year <= max_yr &
              is.finite(get(pred_col)) & is.finite(get(actual_col)) & get(exp_col) >= min_exp]
    assert_true(nrow(sub) >= 50L, sprintf("calibrate_uncertainty insufficient rows (%d) for %s", nrow(sub), pred_col))
    e2 <- (sub[[pred_col]] - sub[[actual_col]])^2
    inv_n <- 1.0 / sub[[exp_col]]
    fit <- stats::lm(e2 ~ inv_n, weights = sub[[exp_col]])
    co <- stats::coef(fit)
    assert_true(all(is.finite(co)), sprintf("Non-finite variance regression for %s", pred_col))
    if (co[1] <= 0) warning(sprintf("calibrate_uncertainty(%s): skill variance floored at 1e-8 (raw=%.6g)", pred_col, co[1]))
    if (co[2] < 0)  warning(sprintf("calibrate_uncertainty(%s): noise variance floored at 0 (raw=%.6g)", pred_col, co[2]))
    list(sig_skill = sqrt(max(unname(co[1]), 1e-8)), sig_noise = sqrt(max(unname(co[2]), 0.0)))
  }

  u_p_fip   <- calibrate_uncertainty(pitch_panel, "m4_sc_fip",   "actual_fip",   "actual_ip",  2024L, 30)
  u_p_siera <- calibrate_uncertainty(pitch_panel, "m4_sc_siera", "actual_siera", "actual_tbf", 2024L, 100)
  u_b_woba  <- calibrate_uncertainty(bat_panel,   "m4_woba",     "actual_woba",  "actual_pa",  2024L, 100)

  z80 <- stats::qnorm(0.90)
  pitch_panel[, `:=`(
    se_fip_proj   = sqrt(u_p_fip$sig_skill^2   + u_p_fip$sig_noise^2   / pmax(proj_ip, 15)),
    se_siera_proj = sqrt(u_p_siera$sig_skill^2 + u_p_siera$sig_noise^2 / pmax(proj_tbf, 50))
  )]
  pitch_panel[, `:=`(
    proj_fip_lo80   = pmax(proj_fip - z80 * se_fip_proj, 1.50),
    proj_fip_hi80   = proj_fip + z80 * se_fip_proj,
    proj_siera_lo80 = pmax(proj_siera - z80 * se_siera_proj, 1.50),
    proj_siera_hi80 = proj_siera + z80 * se_siera_proj
  )]
  bat_panel[, se_woba_proj := sqrt(u_b_woba$sig_skill^2 + u_b_woba$sig_noise^2 / pmax(proj_pa, 50))]
  bat_panel[, `:=`(
    proj_woba_lo80 = pmax(proj_woba - z80 * se_woba_proj, 0.150),
    proj_woba_hi80 = pmin(proj_woba + z80 * se_woba_proj, 0.550)
  )]

  p25_cov <- pitch_panel[projection_year == 2025L & !is.na(actual_fip) & actual_tbf >= 100, {
    se_act <- sqrt(u_p_fip$sig_skill^2 + u_p_fip$sig_noise^2 / pmax(actual_ip, 15))
    mean(actual_fip >= (proj_fip - z80 * se_act) & actual_fip <= (proj_fip + z80 * se_act))
  }]
  b25_cov <- bat_panel[projection_year == 2025L & !is.na(actual_woba) & actual_pa >= 100, {
    se_act <- sqrt(u_b_woba$sig_skill^2 + u_b_woba$sig_noise^2 / pmax(actual_pa, 50))
    mean(actual_woba >= (proj_woba - z80 * se_act) & actual_woba <= (proj_woba + z80 * se_act))
  }]
  assert_true(is.finite(p25_cov) && is.finite(b25_cov), "Non-finite 80% CI coverage")
  cat(sprintf("   Locked 2025 80%% CI Empirical Coverage: Pitcher FIP = %.1f%%, Batter wOBA = %.1f%%\n",
              100 * p25_cov, 100 * b25_cov))

  # ============================================================================
  # 5D. TRACK 4 (2025->2026 STATCAST FULL-OOS) & CLEAN SINGLE-PASS LIVE 2027
  # ============================================================================
  cat("\n-> [5/7] Executing Track 4 (2025->2026 Statcast) & single-pass Live 2027 Bayesian updates...\n")

  p26_oos <- merge(
    pitch_panel[projection_year == 2026L],
    sc_pitch_yr[game_year == 2026L, .(pitcher_mlbam, sc26_tbf = sc_tbf, sc26_k_pct = sc_k_pct,
                                      sc26_bb_pct = sc_bb_pct, sc26_hr_per_bf = sc_hr_per_bf,
                                      sc26_xwoba = sc_xwoba_allowed)],
    by = "pitcher_mlbam", all.x = TRUE
  )
  tr_sp_25_pure <- pitch_panel[projection_year == 2025L & !is.na(actual_tbf) & actual_tbf >= 50 & (has_t1_stuff == 1L | has_t1_decomp == 1L)]
  act_p26 <- which(p26_oos$has_t1_stuff == 1L | p26_oos$has_t1_decomp == 1L)
  res_p26_k_pure  <- fit_predict_residual_ridge(tr_sp_25_pure, p26_oos[act_p26], "m4_sc_k_pct",  "actual_k_pct",  "actual_tbf", pitch_stuff_sensors, seed_val = 7071L)
  res_p26_bb_pure <- fit_predict_residual_ridge(tr_sp_25_pure, p26_oos[act_p26], "m4_sc_bb_pct", "actual_bb_pct", "actual_tbf", pitch_stuff_sensors, seed_val = 7072L)
  cat(sprintf("   Track 4 PureOOSCal Stage-2 imputed cells: train=%d, test=%d\n",
              res_p26_k_pure$n_imp_tr + res_p26_bb_pure$n_imp_tr, res_p26_k_pure$n_imp_te + res_p26_bb_pure$n_imp_te))
  out_k_p <- p26_oos$m4_sc_k_pct;  out_k_p[act_p26]  <- res_p26_k_pure$pred
  out_b_p <- p26_oos$m4_sc_bb_pct; out_b_p[act_p26]  <- res_p26_bb_pure$pred
  p26_oos[, `:=`(m4_stuff_pureoos_k_pct = out_k_p, m4_stuff_pureoos_bb_pct = out_b_p)]

  b26_oos <- merge(
    bat_panel[projection_year == 2026L],
    sc_bat_2026[, .(batter_mlbam, sc26_pa = sc_pa, sc26_k_pct = sc_k_pct,
                    sc26_bb_pct = sc_bb_pct, sc26_xwoba = sc_xwoba, sc26_woba = sc_woba)],
    by = "batter_mlbam", all.x = TRUE
  )

  cw_meta <- as.data.table(DBI::dbGetQuery(con, "
    SELECT CAST(key_mlbam AS INTEGER) AS mlbam,
           MAX(name_first || ' ' || name_last) AS cw_name,
           MIN(birth_year) AS cw_by, MIN(birth_month) AS cw_bm,
           MAX(CAST(key_fangraphs AS INTEGER)) AS cw_fg
    FROM v_chadwick_register WHERE key_mlbam IS NOT NULL GROUP BY 1
  "))
  # Pitcher throws fallback from 2026 Statcast for rookies
  sc26_throws <- as.data.table(DBI::dbGetQuery(con, "
    SELECT CAST(pitcher AS INTEGER) AS pitcher_mlbam, MAX(p_throws) AS sc26_throws
    FROM v_statcast_features WHERE game_year = 2026 AND game_type = 'R' AND p_throws IS NOT NULL GROUP BY 1
  "))

  sp_2026 <- gold_sp[game_year == 2026L]
  sc_p26  <- merge(sc_pitch_yr[game_year == 2026L],
                   sc_spray_pitch[game_year == 2026L, .(pitcher_mlbam, sc26_pull_air = pulled_air_allowed_pct)],
                   by = "pitcher_mlbam", all.x = TRUE)
  live_p27_base <- merge(
    pitch_panel[projection_year == 2026L, .(
      pitcher_mlbam, player_name, fg_id, team_prev, p_throws, proj_role, birth_year, birth_month,
      age_2026 = age_proj, proj_tbf_26 = proj_tbf, proj_ip_26 = proj_ip, tbf_2025 = tbf_t1,
      anchor_tbf_ip, anchor_cfip,
      prior_k_pct = proj_k_pct, prior_bb_pct = proj_bb_pct, prior_hbp_pct = m_hbp_pct,
      prior_hr9 = proj_hr9, prior_fip = proj_fip, prior_siera = proj_siera, prior_era = proj_era,
      prior_fbv = fbv_prior, prior_pull_air = pull_air_prior,
      prior_pvi_xwoba = pvi_xwoba_prior, prior_pvi_csw = pvi_csw_prior
    )],
    sc_p26[, .(pitcher_mlbam, sc26_tbf = sc_tbf, sc26_n_pitches = sc_n_pitches,
               sc26_k_pct = sc_k_pct, sc26_bb_pct = sc_bb_pct, sc26_hbp_pct = sc_hbp_pct,
               sc26_hr_per_bf = sc_hr_per_bf, sc26_fbv = sc_fbv, sc26_pull_air,
               sc26_pvi_n = pvi_n_eff, sc26_pvi_xwoba = pvi_xwoba_raw, sc26_pvi_csw = pvi_csw_raw)],
    by = "pitcher_mlbam", all = TRUE
  )
  decomp_2026 <- gold_decomp_p[game_year == 2026L]
  live_p27_base <- merge(
    live_p27_base,
    decomp_2026[, .(pitcher_mlbam, decomp26_stuff = decomp_stuff_plus, decomp26_loc = decomp_location_plus,
                    decomp26_pitch = decomp_pitching_plus, decomp26_ptun = decomp_pitching_tunnel_plus,
                    decomp26_n = n_pitches)],
    by = "pitcher_mlbam", all.x = TRUE
  )
  live_p27_base <- merge(
    live_p27_base,
    sp_2026[, .(pitcher_mlbam, sp26_name = pitcher_name, sp26_throws = p_throws, sp26_n = n_pitches,
                sp26_stuff = stuff_plus, sp26_loc = location_plus, sp26_pitch = pitching_plus,
                sp26_tunnel = pitching_tunnel_plus)],
    by = "pitcher_mlbam", all = TRUE
  )
  live_p27_base <- merge(live_p27_base, sc26_throws, by = "pitcher_mlbam", all.x = TRUE)
  live_p27_base <- merge(live_p27_base, cw_meta, by.x = "pitcher_mlbam", by.y = "mlbam", all.x = TRUE)

  # Retain active 2026 pitchers (>= 25 TBF) PLUS injured/missed-2026 pitchers with meaningful 2026 projection!
  live_p27_base[, played_2026 := as.integer(fifelse(!is.na(sc26_tbf) & sc26_tbf > 0, 1L, 0L))]
  live_p27_base <- live_p27_base[(played_2026 == 1L & sc26_tbf >= 25) | (played_2026 == 0L & !is.na(prior_fip) & tbf_2025 >= 50)]
  assert_true(nrow(live_p27_base) > 500L, "live_p27_base unexpectedly small")

  lg25_sp <- lg_pitch[game_year == 2025L & role == "SP"]
  lg25_rp <- lg_pitch[game_year == 2025L & role == "RP"]

  live_p27_base[, `:=`(
    projection_year = 2027L,
    player_name = fifelse(!is.na(player_name), player_name, fifelse(!is.na(sp26_name), sp26_name, cw_name)),
    fg_id = fifelse(!is.na(fg_id), fg_id, cw_fg),
    p_throws = fifelse(!is.na(p_throws), p_throws, fifelse(!is.na(sp26_throws), sp26_throws, sc26_throws)),
    birth_year = fifelse(!is.na(birth_year), birth_year, cw_by),
    birth_month = fifelse(!is.na(birth_month), birth_month, cw_bm),
    last_year = fifelse(played_2026 == 1L, 2026L, 2025L),
    tbf_t1 = fifelse(played_2026 == 1L, sc26_tbf, 0.0),
    proj_role = fifelse(!is.na(proj_role), proj_role, fifelse(fifelse(is.na(sc26_tbf), 0, sc26_tbf) >= 350, "SP", "RP"))
  )]
  n_p27_age_fb <- live_p27_base[, sum(is.na(birth_year) | is.na(birth_month))]
  if (n_p27_age_fb > 0L) cat(sprintf("   [INFO] live_p27_base age fallback rows: %d\n", n_p27_age_fb))
  live_p27_base[, `:=`(
    age_proj = fifelse(!is.na(birth_year) & !is.na(birth_month),
                       2027L - birth_year - as.integer(birth_month >= 7L),
                       fifelse(is.finite(age_2026), age_2026 + 1.0, 26.0)),
    anchor_tbf_ip = fifelse(proj_role == "SP", lg25_sp$lg_tbf_ip, lg25_rp$lg_tbf_ip),
    anchor_cfip   = lg25_sp$c_fip,
    b0_k_pct      = fifelse(proj_role == "SP", lg25_sp$lg_k_pct,   lg25_rp$lg_k_pct),
    b0_bb_pct     = fifelse(proj_role == "SP", lg25_sp$lg_bb_pct,  lg25_rp$lg_bb_pct),
    b0_hbp_pct    = fifelse(proj_role == "SP", lg25_sp$lg_hbp_pct, lg25_rp$lg_hbp_pct),
    b0_hr9        = fifelse(proj_role == "SP", lg25_sp$lg_hr9,     lg25_rp$lg_hr9),
    b0_fip        = fifelse(proj_role == "SP", lg25_sp$lg_fip,     lg25_rp$lg_fip),
    b0_siera      = fifelse(proj_role == "SP", lg25_sp$lg_siera,   lg25_rp$lg_siera),
    b0_era        = fifelse(proj_role == "SP", lg25_sp$lg_era,     lg25_rp$lg_era),
    b0_fbv        = fifelse(proj_role == "SP", lg25_sp$lg_fbv,     lg25_rp$lg_fbv),
    b0_pull_air   = fifelse(proj_role == "SP", lg25_sp$lg_pull_air, lg25_rp$lg_pull_air)
  )]
  live_p27_base[, `:=`(
    prior_k_use     = fifelse(is.finite(prior_k_pct),   prior_k_pct,   b0_k_pct),
    prior_bb_use    = fifelse(is.finite(prior_bb_pct),  prior_bb_pct,  b0_bb_pct),
    prior_hbp_use   = fifelse(is.finite(prior_hbp_pct), prior_hbp_pct, b0_hbp_pct),
    prior_hr9_use   = fifelse(is.finite(prior_hr9),     prior_hr9,     b0_hr9),
    prior_fip_use   = fifelse(is.finite(prior_fip),     prior_fip,     b0_fip),
    prior_siera_use = fifelse(is.finite(prior_siera),   prior_siera,   b0_siera),
    prior_era_use   = fifelse(is.finite(prior_era),     prior_era,     b0_era),
    sc26_tbf_k      = fifelse(played_2026 == 1L & is.finite(sc26_k_pct),   sc26_tbf, 0.0),
    sc26_tbf_bb     = fifelse(played_2026 == 1L & is.finite(sc26_bb_pct),  sc26_tbf, 0.0),
    sc26_tbf_hbp    = fifelse(played_2026 == 1L & is.finite(sc26_hbp_pct), sc26_tbf, 0.0),
    sc26_ip_hr      = fifelse(played_2026 == 1L & is.finite(sc26_hr_per_bf), sc26_tbf / anchor_tbf_ip, 0.0),
    sp26_n_clean    = fifelse(is.finite(sp26_n), sp26_n, 0.0),
    decomp26_n_clean = fifelse(is.finite(decomp26_n), decomp26_n, fifelse(is.finite(sp26_n), sp26_n, 0.0)),
    has_t1_stuff    = as.integer(played_2026 == 1L & is.finite(sp26_stuff)),
    has_t1_decomp   = as.integer(played_2026 == 1L & is.finite(decomp26_loc)),
    has_t1_fg_stuff = 0L,
    proj_tbf = fifelse(proj_role == "SP",
                       0.50 * fifelse(played_2026 == 1L, sc26_tbf, 0.0) + 0.10 * fifelse(is.finite(tbf_2025), tbf_2025, 0.0) + 220,
                       0.50 * fifelse(played_2026 == 1L, sc26_tbf, 0.0) + 0.10 * fifelse(is.finite(tbf_2025), tbf_2025, 0.0) + 100)
  )]
  live_p27_base[, `:=`(
    proj_ip = proj_tbf / anchor_tbf_ip,
    sc26_hr9_est = fifelse(sc26_ip_hr > 0, sc26_hr_per_bf * anchor_tbf_ip * 9.0, prior_hr9_use),
    stuff_plus_t1  = 100.0 + fifelse(has_t1_stuff == 1L, (sp26_stuff  - 100.0) * (sp26_n_clean / (sp26_n_clean + 250.0)), 0.0),
    loc_plus_t1    = 100.0 + fifelse(has_t1_stuff == 1L, (sp26_loc    - 100.0) * (sp26_n_clean / (sp26_n_clean + 450.0)), 0.0),
    pitch_plus_t1  = 100.0 + fifelse(has_t1_stuff == 1L, (sp26_pitch  - 100.0) * (sp26_n_clean / (sp26_n_clean + 300.0)), 0.0),
    tunnel_plus_t1 = 100.0 + fifelse(has_t1_stuff == 1L, (sp26_tunnel - 100.0) * (sp26_n_clean / (sp26_n_clean + 300.0)), 0.0),
    decomp_stuff_plus_t1 = 100.0 + fifelse((has_t1_stuff == 1L | has_t1_decomp == 1L) & is.finite(decomp26_stuff), (decomp26_stuff - 100.0) * (decomp26_n_clean / (decomp26_n_clean + 250.0)), 0.0),
    decomp_loc_plus_t1   = 100.0 + fifelse((has_t1_stuff == 1L | has_t1_decomp == 1L) & is.finite(decomp26_loc),   (decomp26_loc   - 100.0) * (decomp26_n_clean / (decomp26_n_clean + 350.0)), 0.0),
    decomp_pitch_plus_t1 = 100.0 + fifelse((has_t1_stuff == 1L | has_t1_decomp == 1L) & is.finite(decomp26_pitch), (decomp26_pitch - 100.0) * (decomp26_n_clean / (decomp26_n_clean + 250.0)), 0.0),
    decomp_ptun_plus_t1  = 100.0 + fifelse((has_t1_stuff == 1L | has_t1_decomp == 1L) & is.finite(decomp26_ptun),  (decomp26_ptun  - 100.0) * (decomp26_n_clean / (decomp26_n_clean + 250.0)), 0.0),
    fbv_prior = fifelse(played_2026 == 1L & is.finite(sc26_fbv), sc26_fbv, fifelse(is.finite(prior_fbv), prior_fbv, b0_fbv)),
    pull_air_prior = fifelse(played_2026 == 1L & is.finite(sc26_pull_air), sc26_pull_air, fifelse(is.finite(prior_pull_air), prior_pull_air, b0_pull_air)),
    pvi_xwoba_prior = fifelse(played_2026 == 1L & is.finite(sc26_pvi_xwoba) & sc26_pvi_n > 0,
                              sc26_pvi_xwoba * (sc26_pvi_n / (sc26_pvi_n + 400.0)),
                              fifelse(is.finite(prior_pvi_xwoba), prior_pvi_xwoba, 0.0)),
    pvi_csw_prior = fifelse(played_2026 == 1L & is.finite(sc26_pvi_csw) & sc26_pvi_n > 0,
                            sc26_pvi_csw * (sc26_pvi_n / (sc26_pvi_n + 400.0)),
                            fifelse(is.finite(prior_pvi_csw), prior_pvi_csw, 0.0)),
    # 1-year midpoint Delta-Method aging with linear extrapolation above 39 / below 21!
    ad_k   = fifelse(proj_role == "SP", ag_p_full$SP$k_pct$cum_fn(age_proj)  - ag_p_full$SP$k_pct$cum_fn(age_proj - 1),
                     ag_p_full$RP$k_pct$cum_fn(age_proj)  - ag_p_full$RP$k_pct$cum_fn(age_proj - 1)),
    ad_bb  = fifelse(proj_role == "SP", ag_p_full$SP$bb_pct$cum_fn(age_proj) - ag_p_full$SP$bb_pct$cum_fn(age_proj - 1),
                     ag_p_full$RP$bb_pct$cum_fn(age_proj) - ag_p_full$RP$bb_pct$cum_fn(age_proj - 1)),
    ad_hr9 = fifelse(proj_role == "SP", ag_p_full$SP$hr9$cum_fn(age_proj)    - ag_p_full$SP$hr9$cum_fn(age_proj - 1),
                     ag_p_full$RP$hr9$cum_fn(age_proj)    - ag_p_full$RP$hr9$cum_fn(age_proj - 1)),
    ad_fip = fifelse(proj_role == "SP", ag_p_full$SP$fip$cum_fn(age_proj)    - ag_p_full$SP$fip$cum_fn(age_proj - 1),
                     ag_p_full$RP$fip$cum_fn(age_proj)    - ag_p_full$RP$fip$cum_fn(age_proj - 1)),
    ad_sie = fifelse(proj_role == "SP", ag_p_full$SP$siera$cum_fn(age_proj)  - ag_p_full$SP$siera$cum_fn(age_proj - 1),
                     ag_p_full$RP$siera$cum_fn(age_proj)  - ag_p_full$RP$siera$cum_fn(age_proj - 1))
  )]
  # Zero-sum aging normalization by role
  live_p27_base[, `:=`(
    age_delta_k_pct  = ad_k   - sum(proj_tbf * ad_k,   na.rm = TRUE) / sum(proj_tbf, na.rm = TRUE),
    age_delta_bb_pct = ad_bb  - sum(proj_tbf * ad_bb,  na.rm = TRUE) / sum(proj_tbf, na.rm = TRUE),
    age_delta_hr9    = ad_hr9 - sum(proj_ip  * ad_hr9, na.rm = TRUE) / sum(proj_ip,  na.rm = TRUE),
    age_delta_fip    = ad_fip - sum(proj_ip  * ad_fip, na.rm = TRUE) / sum(proj_ip,  na.rm = TRUE),
    age_delta_siera  = ad_sie - sum(proj_tbf * ad_sie, na.rm = TRUE) / sum(proj_tbf, na.rm = TRUE)
  ), by = proj_role]

  # Single-pass Stage-2 residual helper centered on exact training mu_vec (ZERO double-counting!)
  sp26_delta <- function(fit_obj) {
    co <- fit_obj$coef; mu <- fit_obj$mu_vec
    d <- rep(0.0, nrow(live_p27_base))
    for (nm in names(co)) {
      if (nm %in% names(live_p27_base) && is.finite(mu[[nm]])) {
        d <- d + co[[nm]] * (live_p27_base[[nm]] - mu[[nm]])
      }
    }
    fifelse(live_p27_base$has_t1_stuff == 1L | live_p27_base$has_t1_decomp == 1L, d, 0.0)
  }

  # Step 1: Unadjusted Bayesian base component updates (consistent TBF / IP scales!)
  live_p27_base[, `:=`(
    k_base   = (prior_k_use   * 400 + fifelse(sc26_tbf_k > 0,   sc26_k_pct   * sc26_tbf_k,   0.0)) / (400 + sc26_tbf_k),
    bb_base  = (prior_bb_use  * 600 + fifelse(sc26_tbf_bb > 0,  sc26_bb_pct  * sc26_tbf_bb,  0.0)) / (600 + sc26_tbf_bb),
    hbp_base = (prior_hbp_use * 900 + fifelse(sc26_tbf_hbp > 0, sc26_hbp_pct * sc26_tbf_hbp, 0.0)) / (900 + sc26_tbf_hbp),
    hr9_base = (prior_hr9_use * 220 + fifelse(sc26_ip_hr > 0,   sc26_hr9_est * sc26_ip_hr,   0.0)) / (220 + sc26_ip_hr)
  )]
  live_p27_base[, fip_comp_base := (13.0 * hr9_base + 3.0 * (bb_base + hbp_base) * anchor_tbf_ip * 9.0 -
                                    2.0 * k_base * anchor_tbf_ip * 9.0) / 9.0 + anchor_cfip]
  # Step 2: Apply Stage-2 Stuff+ residual and 1-year zero-sum aging EXACTLY ONCE!
  live_p27_base[, `:=`(
    proj_k_pct  = pmax(0.05, k_base   + sp26_delta(pitch_stuff_fits[["k_pct"]])  + age_delta_k_pct),
    proj_bb_pct = pmax(0.02, bb_base  + sp26_delta(pitch_stuff_fits[["bb_pct"]]) + age_delta_bb_pct),
    proj_hr9    = pmax(0.25, hr9_base + sp26_delta(pitch_stuff_fits[["hr9"]])    + age_delta_hr9),
    proj_fip    = pmax(2.00, (0.50 * prior_fip_use + 0.50 * fip_comp_base) + sp26_delta(pitch_stuff_fits[["fip"]]) + age_delta_fip),
    proj_siera  = pmax(2.00, (0.60 * prior_siera_use + 0.40 * (b0_siera + (fip_comp_base - b0_fip) * 0.72)) +
                             sp26_delta(pitch_stuff_fits[["siera"]]) + age_delta_siera)
  )]
  live_p27_base[, `:=`(
    proj_fip_comp = pmax(2.00, (13.0 * proj_hr9 + 3.0 * (proj_bb_pct + hbp_base) * anchor_tbf_ip * 9.0 -
                                2.0 * proj_k_pct * anchor_tbf_ip * 9.0) / 9.0 + anchor_cfip),
    proj_era      = pmax(2.00, proj_fip + (b0_era - b0_fip)),
    se_fip_proj   = sqrt(u_p_fip$sig_skill^2   + u_p_fip$sig_noise^2   / pmax(proj_ip, 15)),
    se_siera_proj = sqrt(u_p_siera$sig_skill^2 + u_p_siera$sig_noise^2 / pmax(proj_tbf, 50)),
    eval_split    = "live_2027_forecast",
    model_version = "EXP-004-v2-statcast-bayes-singlepass"
  )]
  live_p27_base[, `:=`(
    proj_fip_lo80   = pmax(proj_fip - z80 * se_fip_proj, 1.50),
    proj_fip_hi80   = proj_fip + z80 * se_fip_proj,
    proj_siera_lo80 = pmax(proj_siera - z80 * se_siera_proj, 1.50),
    proj_siera_hi80 = proj_siera + z80 * se_siera_proj,
    proj_rar        = (b0_era - proj_fip + fifelse(proj_role == "SP", 0.38, 0.47)) * (proj_ip / 9.0)
  )]
  live_p27_base[, proj_war := proj_rar / 9.77]

  # --- Live 2027 Batter Projections (re-centered to FG scale, retains injured players, full OBP/SLG/ISO!) ---
  sc_b26_all <- merge(sc_bat_2026, sc_spray_bat[game_year == 2026L], by = c("game_year", "batter_mlbam"), all.x = TRUE)
  lg25_b <- lg_bat[game_year == 2025L]
  # Re-center 2026 Statcast batter rates to 2025 FanGraphs league anchor (eliminates +0.0091 Savant wOBA scale drift!)
  sc26_lg_woba  <- w_mean_safe(sc_b26_all$sc_woba,  sc_b26_all$sc_pa)
  sc26_lg_xwoba <- w_mean_safe(sc_b26_all$sc_xwoba, sc_b26_all$sc_pa)
  sc26_lg_obp   <- w_mean_safe(sc_b26_all$sc_obp,   sc_b26_all$sc_pa)
  sc26_lg_slg   <- w_mean_safe(sc_b26_all$sc_slg,   sc_b26_all$sc_ab)
  sc26_lg_iso   <- w_mean_safe(sc_b26_all$sc_iso,   sc_b26_all$sc_ab)
  sc_b26_all[, `:=`(
    sc26_woba_fg  = sc_woba  + (lg25_b$lg_woba  - sc26_lg_woba),
    sc26_xwoba_fg = sc_xwoba + (lg25_b$lg_xwoba - sc26_lg_xwoba),
    sc26_obp_fg   = sc_obp   + (lg25_b$lg_obp   - sc26_lg_obp),
    sc26_slg_fg   = sc_slg   + (lg25_b$lg_slg   - sc26_lg_slg),
    sc26_iso_fg   = sc_iso   + (lg25_b$lg_iso   - sc26_lg_iso)
  )]

  live_b27_base <- merge(
    bat_panel[projection_year == 2026L, .(
      batter_mlbam, player_name, fg_id, team_prev, bats, position, birth_year, birth_month,
      age_2026 = age_proj, proj_pa_26 = proj_pa, pa_2025 = pa_t1,
      prior_woba = proj_woba, prior_k_pct = proj_k_pct, prior_bb_pct = proj_bb_pct,
      prior_iso = proj_iso, prior_obp = proj_obp, prior_slg = proj_slg,
      prior_fld = proj_fld_runs, prior_def = proj_def_runs, prior_bsr = proj_bsr_runs, prior_pos = proj_pos_runs,
      prior_xwoba = xwoba_prior, prior_hh = hh_prior, prior_ev90 = ev90_prior,
      prior_pull_air = pull_air_prior, prior_sweet = sweet_prior,
      prior_zone_chase = zone_chase_prior, prior_batspeed = batspeed_t1
    )],
    sc_b26_all[, .(batter_mlbam, sc26_pa = sc_pa, sc26_ab = sc_ab, sc26_k_pct = sc_k_pct, sc26_bb_pct = sc_bb_pct,
                   sc26_woba = sc26_woba_fg, sc26_xwoba = sc26_xwoba_fg, sc26_obp = sc26_obp_fg,
                   sc26_slg = sc26_slg_fg, sc26_iso = sc26_iso_fg,
                   sc26_hh = sc_hard_hit_pct, sc26_ev90 = sc_ev90, sc26_pull_air = pulled_air_pct,
                   sc26_sweet = sweet_spot_pct, sc26_zone_chase = sc_zone_chase, sc26_batspeed = sc_bat_speed)],
    by = "batter_mlbam", all = TRUE
  )
  proc_2026 <- gold_bat_proc[game_year == 2026L]
  live_b27_base <- merge(
    live_b27_base,
    proc_2026[, .(batter_mlbam, proc26_dec = decision_plus, proc26_ctrl = bat_control_plus,
                  proc26_contact = contact_plus, proc26_total = batter_process_plus,
                  proc26_n = n_pitches)],
    by = "batter_mlbam", all.x = TRUE
  )
  live_b27_base <- merge(live_b27_base, cw_meta, by.x = "batter_mlbam", by.y = "mlbam", all.x = TRUE)
  live_b27_base[, played_2026 := as.integer(fifelse(!is.na(sc26_pa) & sc26_pa > 0, 1L, 0L))]
  live_b27_base <- live_b27_base[(played_2026 == 1L & sc26_pa >= 50) | (played_2026 == 0L & !is.na(prior_woba) & pa_2025 >= 50)]
  assert_true(nrow(live_b27_base) > 450L, "live_b27_base unexpectedly small")

  live_b27_base[, `:=`(
    projection_year = 2027L,
    player_name = fifelse(!is.na(player_name), player_name, cw_name),
    fg_id = fifelse(!is.na(fg_id), fg_id, cw_fg),
    birth_year = fifelse(!is.na(birth_year), birth_year, cw_by),
    birth_month = fifelse(!is.na(birth_month), birth_month, cw_bm),
    last_year = fifelse(played_2026 == 1L, 2026L, 2025L),
    pa_t1 = fifelse(played_2026 == 1L, sc26_pa, 0.0),
    has_t1_battrack = as.integer(played_2026 == 1L & is.finite(sc26_batspeed)),
    proj_pa = 0.50 * fifelse(played_2026 == 1L, sc26_pa, 0.0) + 0.10 * fifelse(is.finite(pa_2025), pa_2025, 0.0) + 200.0
  )]
  live_b27_base[, `:=`(
    age_proj = fifelse(!is.na(birth_year) & !is.na(birth_month),
                       2027L - birth_year - as.integer(birth_month >= 7L),
                       fifelse(is.finite(age_2026), age_2026 + 1.0, 26.0)),
    decision_plus_t1    = 100.0 + fifelse(played_2026 == 1L & is.finite(proc26_dec), (proc26_dec - 100.0) * (proc26_n / (proc26_n + 250.0)), 0.0),
    bat_control_plus_t1 = 100.0 + fifelse(played_2026 == 1L & is.finite(proc26_ctrl), (proc26_ctrl - 100.0) * (proc26_n / (proc26_n + 250.0)), 0.0),
    contact_plus_t1     = 100.0 + fifelse(played_2026 == 1L & is.finite(proc26_contact), (proc26_contact - 100.0) * (proc26_n / (proc26_n + 300.0)), 0.0),
    process_plus_t1     = 100.0 + fifelse(played_2026 == 1L & is.finite(proc26_total), (proc26_total - 100.0) * (proc26_n / (proc26_n + 250.0)), 0.0),
    has_t1_process      = as.integer(played_2026 == 1L & is.finite(proc26_dec)),
    proj_fld_runs       = fifelse(!is.na(prior_fld), (prior_fld / pmax(proj_pa_26, 50.0)) * 0.90 * proj_pa, 0.0),
    proj_bsr_runs       = fifelse(!is.na(prior_bsr), (prior_bsr / pmax(proj_pa_26, 50.0)) * 0.90 * proj_pa, 0.0),
    proj_pos_runs       = fifelse(!is.na(prior_pos), (prior_pos / pmax(proj_pa_26, 50.0)) * proj_pa, 0.0),
    proj_def_runs       = fifelse(!is.na(prior_fld), (prior_fld / pmax(proj_pa_26, 50.0)) * 0.90 * proj_pa, 0.0) + fifelse(!is.na(prior_pos), (prior_pos / pmax(proj_pa_26, 50.0)) * proj_pa, 0.0),
    prior_woba_use = fifelse(is.finite(prior_woba), prior_woba, lg25_b$lg_woba),
    prior_k_use    = fifelse(is.finite(prior_k_pct), prior_k_pct, lg25_b$lg_k_pct),
    prior_bb_use   = fifelse(is.finite(prior_bb_pct), prior_bb_pct, lg25_b$lg_bb_pct),
    prior_iso_use  = fifelse(is.finite(prior_iso), prior_iso, lg25_b$lg_iso),
    prior_obp_use  = fifelse(is.finite(prior_obp), prior_obp, lg25_b$lg_obp),
    prior_slg_use  = fifelse(is.finite(prior_slg), prior_slg, lg25_b$lg_slg),
    pa_eff = fifelse(played_2026 == 1L & is.finite(sc26_woba), sc26_pa, 0.0),
    ab_eff = fifelse(played_2026 == 1L & is.finite(sc26_slg) & sc26_ab > 0, sc26_ab, 0.0)
  )]
  live_b27_base[, `:=`(
    ad_w   = ag_b_full$woba$cum_fn(age_proj)   - ag_b_full$woba$cum_fn(age_proj - 1),
    ad_k   = ag_b_full$k_pct$cum_fn(age_proj)  - ag_b_full$k_pct$cum_fn(age_proj - 1),
    ad_b   = ag_b_full$bb_pct$cum_fn(age_proj) - ag_b_full$bb_pct$cum_fn(age_proj - 1),
    ad_i   = ag_b_full$iso$cum_fn(age_proj)    - ag_b_full$iso$cum_fn(age_proj - 1),
    ad_obp = ag_b_full$obp$cum_fn(age_proj)    - ag_b_full$obp$cum_fn(age_proj - 1),
    ad_slg = ag_b_full$slg$cum_fn(age_proj)    - ag_b_full$slg$cum_fn(age_proj - 1),
    obs26_talent = 0.70 * fifelse(is.finite(sc26_xwoba), sc26_xwoba, lg25_b$lg_woba) +
                   0.30 * fifelse(is.finite(sc26_woba),  sc26_woba,  lg25_b$lg_woba) +
                   0.05 * (fifelse(is.finite(sc26_pull_air), sc26_pull_air, lg25_b$lg_pull_air) - lg25_b$lg_pull_air)
  )]
  live_b27_base[, `:=`(
    age_delta_woba   = ad_w   - sum(proj_pa * ad_w,   na.rm = TRUE) / sum(proj_pa, na.rm = TRUE),
    age_delta_k_pct  = ad_k   - sum(proj_pa * ad_k,   na.rm = TRUE) / sum(proj_pa, na.rm = TRUE),
    age_delta_bb_pct = ad_b   - sum(proj_pa * ad_b,   na.rm = TRUE) / sum(proj_pa, na.rm = TRUE),
    age_delta_iso    = ad_i   - sum(proj_pa * ad_i,   na.rm = TRUE) / sum(proj_pa, na.rm = TRUE),
    age_delta_obp    = ad_obp - sum(proj_pa * ad_obp, na.rm = TRUE) / sum(proj_pa, na.rm = TRUE),
    age_delta_slg    = ad_slg - sum(proj_pa * ad_slg, na.rm = TRUE) / sum(proj_pa, na.rm = TRUE)
  )]
  live_b27_base[, `:=`(
    proj_woba   = pmin(0.460, pmax(0.220, (prior_woba_use * 400 + fifelse(pa_eff > 0, obs26_talent * pa_eff, 0.0)) / (400 + pa_eff) + age_delta_woba)),
    proj_k_pct  = pmax(0.05,  (prior_k_use   * 250 + fifelse(pa_eff > 0 & is.finite(sc26_k_pct),  sc26_k_pct  * pa_eff, 0.0)) / (250 + pa_eff) + age_delta_k_pct),
    proj_bb_pct = pmax(0.02,  (prior_bb_use  * 350 + fifelse(pa_eff > 0 & is.finite(sc26_bb_pct), sc26_bb_pct * pa_eff, 0.0)) / (350 + pa_eff) + age_delta_bb_pct),
    proj_iso    = pmax(0.04,  (prior_iso_use * 350 + fifelse(ab_eff > 0 & is.finite(sc26_iso),    sc26_iso    * ab_eff, 0.0)) / (350 + ab_eff) + age_delta_iso),
    proj_obp    = pmin(0.480, pmax(0.220, (prior_obp_use * 450 + fifelse(pa_eff > 0 & is.finite(sc26_obp), sc26_obp * pa_eff, 0.0)) / (450 + pa_eff) + age_delta_obp)),
    proj_slg    = pmin(0.680, pmax(0.260, (prior_slg_use * 400 + fifelse(ab_eff > 0 & is.finite(sc26_slg), sc26_slg * ab_eff, 0.0)) / (400 + ab_eff) + age_delta_slg)),
    xwoba_prior      = fifelse(played_2026 == 1L & is.finite(sc26_xwoba), sc26_xwoba, fifelse(is.finite(prior_xwoba), prior_xwoba, lg25_b$lg_xwoba)),
    hh_prior         = fifelse(played_2026 == 1L & is.finite(sc26_hh), sc26_hh, fifelse(is.finite(prior_hh), prior_hh, lg25_b$lg_hh)),
    ev90_prior       = fifelse(played_2026 == 1L & is.finite(sc26_ev90), sc26_ev90, fifelse(is.finite(prior_ev90), prior_ev90, lg25_b$lg_ev90)),
    pull_air_prior   = fifelse(played_2026 == 1L & is.finite(sc26_pull_air), sc26_pull_air, fifelse(is.finite(prior_pull_air), prior_pull_air, lg25_b$lg_pull_air)),
    sweet_prior      = fifelse(played_2026 == 1L & is.finite(sc26_sweet), sc26_sweet, fifelse(is.finite(prior_sweet), prior_sweet, lg25_b$lg_sweet)),
    zone_chase_prior = fifelse(played_2026 == 1L & is.finite(sc26_zone_chase), sc26_zone_chase, fifelse(is.finite(prior_zone_chase), prior_zone_chase, lg25_b$lg_zone_chase)),
    batspeed_t1      = fifelse(played_2026 == 1L & is.finite(sc26_batspeed), sc26_batspeed, fifelse(is.finite(prior_batspeed), prior_batspeed, lg25_b$lg_batspeed)),
    eval_split       = "live_2027_forecast",
    model_version    = "EXP-004-v2-statcast-bayes-singlepass"
  )]
  live_b27_base[, se_woba_proj := sqrt(u_b_woba$sig_skill^2 + u_b_woba$sig_noise^2 / pmax(proj_pa, 50))]
  live_b27_base[, `:=`(
    proj_woba_lo80 = pmax(proj_woba - z80 * se_woba_proj, 0.150),
    proj_woba_hi80 = pmin(proj_woba + z80 * se_woba_proj, 0.550),
    proj_wraa      = ((proj_woba - lg25_b$lg_woba) / 1.15) * proj_pa,
    proj_repl_runs = (proj_pa / 600.0) * 20.0
  )]
  live_b27_base[, proj_war := (proj_wraa + proj_bsr_runs + proj_fld_runs + proj_pos_runs + proj_repl_runs) / 9.77]
  live_b27_base[, proj_def_runs := proj_fld_runs + proj_pos_runs]

  # ============================================================================
  # 6. CHRONOLOGICAL EVALUATION TABLE (FAIL-FAST COHORT INTEGRITY)
  # ============================================================================
  cat("\n-> [6/7] Evaluating all 4 chronological tracks & ablations...\n")

  eval_block <- function(dt, track_label, domain_label, metric_name, actual_col, weight_col,
                         model_cols, ref_col = "B2_Marcel_LinAge", min_w = 100) {
    assert_true(ref_col %in% names(model_cols), sprintf("ref_col '%s' missing from model_cols", ref_col))
    sub <- dt[is.finite(get(actual_col)) & is.finite(get(weight_col)) & get(weight_col) >= min_w]
    n_cand <- nrow(sub)
    for (mc in model_cols) sub <- sub[is.finite(get(mc))]
    assert_true(nrow(sub) == n_cand && nrow(sub) >= 30L,
                sprintf("eval_block dropped %d/%d rows on %s/%s", n_cand - nrow(sub), n_cand, track_label, metric_name))
    ref_pred <- sub[[model_cols[[ref_col]]]]
    rbindlist(lapply(names(model_cols), function(mname) {
      p <- sub[[model_cols[[mname]]]]; a <- sub[[actual_col]]; w <- sub[[weight_col]]
      cal <- w_cal_slope(p, a, w)
      bt <- if (mname != ref_col) paired_boot_rmse_diff(p, ref_pred, a, w, n_boot = 400L) else
        list(diff = 0, ci_lo = 0, ci_hi = 0, p_better = NA_real_)
      data.table(
        track = track_label, domain = domain_label, metric = metric_name, model = mname,
        n_eval = nrow(sub), w_rmse = w_rmse(p, a, w), w_mae = w_mae(p, a, w),
        w_r = w_cor(p, a, w), cal_slope = cal$slope, cal_intercept = cal$intercept,
        delta_rmse_vs_b2 = bt$diff, boot_ci_lo = bt$ci_lo, boot_ci_hi = bt$ci_hi, p_beats_b2 = bt$p_better
      )
    }))
  }

  eval_rows <- list()
  p_t1 <- merge(
    pitch_panel[projection_year %in% 2016L:2025L],
    pitch_543[projection_year %in% 2016L:2025L, .(
      projection_year, pitcher_mlbam,
      b3_543_k_pct = b3_k_pct, b3_543_bb_pct = b3_bb_pct, b3_543_hr9 = b3_hr9,
      b3_543_fip = b3_fip, b3_543_siera = b3_siera, b3_543_era = b3_era
    )], by = c("projection_year", "pitcher_mlbam"), all.x = TRUE
  )
  for (m in pitch_mets) {
    min_w <- if (m %in% c("hr9", "fip", "era")) 25 else 100
    eval_rows[[length(eval_rows) + 1L]] <- eval_block(
      p_t1[actual_tbf >= 100], "Track1_Rolling_2016_2025", "Pitcher", m, paste0("actual_", m), pitch_wcol[[m]],
      c(B0_LeagueAvg = paste0("b0_", m), B1_PriorYear = paste0("b1_", m), B2_Marcel_LinAge = paste0("b2_", m),
        B3_Marcel_DeltaAge_321 = paste0("b3_", m), B3_Marcel_DeltaAge_543 = paste0("b3_543_", m)),
      ref_col = "B2_Marcel_LinAge", min_w = min_w
    )
  }
  b_t1 <- bat_panel[projection_year %in% 2016L:2025L]
  for (m in bat_mets) {
    eval_rows[[length(eval_rows) + 1L]] <- eval_block(
      b_t1[actual_pa >= 100], "Track1_Rolling_2016_2025", "Batter", m, paste0("actual_", m), bat_wcol[[m]],
      c(B0_LeagueAvg = paste0("b0_", m), B1_PriorYear = paste0("b1_", m),
        B2_Marcel_LinAge = paste0("b2_", m), B3_Marcel_DeltaAge = paste0("b3_", m)),
      ref_col = "B2_Marcel_LinAge", min_w = 80
    )
  }
  p_t2 <- pitch_panel[projection_year %in% 2019L:2025L]
  for (m in pitch_mets) {
    min_w <- if (m %in% c("hr9", "fip", "era")) 25 else 100
    eval_rows[[length(eval_rows) + 1L]] <- eval_block(
      p_t2[actual_tbf >= 100], "Track2_Statcast_2019_2025", "Pitcher", m, paste0("actual_", m), pitch_wcol[[m]],
      c(B0_LeagueAvg = paste0("b0_", m), B1_PriorYear = paste0("b1_", m), B2_Marcel_LinAge = paste0("b2_", m),
        B3_Marcel_DeltaAge = paste0("b3_", m), M4_Statcast_Physics = paste0("m4_sc_", m)),
      ref_col = "B2_Marcel_LinAge", min_w = min_w
    )
  }
  b_t2 <- bat_panel[projection_year %in% 2019L:2025L]
  for (m in bat_mets) {
    eval_rows[[length(eval_rows) + 1L]] <- eval_block(
      b_t2[actual_pa >= 100], "Track2_Statcast_2019_2025", "Batter", m, paste0("actual_", m), bat_wcol[[m]],
      c(B0_LeagueAvg = paste0("b0_", m), B1_PriorYear = paste0("b1_", m), B2_Marcel_LinAge = paste0("b2_", m),
        B3_Marcel_DeltaAge = paste0("b3_", m), M4_Statcast_Physics = paste0("m4_", m)),
      ref_col = "B2_Marcel_LinAge", min_w = 80
    )
  }
  p_t3 <- pitch_panel[projection_year == 2025L & actual_tbf >= 100]
  for (m in pitch_mets) {
    min_w <- if (m %in% c("hr9", "fip", "era")) 25 else 100
    mcols <- c(B0_LeagueAvg = paste0("b0_", m), B1_PriorYear = paste0("b1_", m), B2_Marcel_LinAge = paste0("b2_", m),
               B3_Marcel_DeltaAge = paste0("b3_", m), M4_Statcast_Physics = paste0("m4_sc_", m),
               M4_mlbr_StuffPlus = paste0("m4_stuff_", m), Ref_FG_StuffPlus = paste0("m4_fgstuff_", m))
    if (m == "fip") mcols <- c(mcols, M4_Component_FIP = "proj_fip_comp")
    eval_rows[[length(eval_rows) + 1L]] <- eval_block(
      p_t3, "Track3_LockedOOS_2025", "Pitcher", m, paste0("actual_", m), pitch_wcol[[m]], mcols,
      ref_col = "B2_Marcel_LinAge", min_w = min_w
    )
  }
  b_t3 <- bat_panel[projection_year == 2025L & actual_pa >= 100]
  for (m in bat_mets) {
    mcols <- c(B0_LeagueAvg = paste0("b0_", m), B1_PriorYear = paste0("b1_", m), B2_Marcel_LinAge = paste0("b2_", m),
               B3_Marcel_DeltaAge = paste0("b3_", m), M4_Statcast_Physics = paste0("m4_", m))
    if (m %in% c("woba", "iso", "k_pct", "bb_pct")) mcols <- c(mcols, M4_BatTracking = paste0("m4_battrack_", m))
    eval_rows[[length(eval_rows) + 1L]] <- eval_block(
      b_t3, "Track3_LockedOOS_2025", "Batter", m, paste0("actual_", m), bat_wcol[[m]], mcols,
      ref_col = "B2_Marcel_LinAge", min_w = 80
    )
  }
  eval_rows[[length(eval_rows) + 1L]] <- eval_block(
    p26_oos[sc26_tbf >= 100], "Track4_LockedOOS_2026_Statcast", "Pitcher", "k_pct", "sc26_k_pct", "sc26_tbf",
    c(B0_LeagueAvg = "b0_k_pct", B1_PriorYear = "b1_k_pct", B2_Marcel_LinAge = "b2_k_pct",
      B3_Marcel_DeltaAge = "b3_k_pct", M4_Statcast_Physics = "m4_sc_k_pct",
      M4_mlbr_StuffPlus = "m4_stuff_k_pct", M4_mlbr_StuffPlus_PureOOSCal = "m4_stuff_pureoos_k_pct"),
    ref_col = "B2_Marcel_LinAge", min_w = 100
  )
  eval_rows[[length(eval_rows) + 1L]] <- eval_block(
    p26_oos[sc26_tbf >= 100], "Track4_LockedOOS_2026_Statcast", "Pitcher", "bb_pct", "sc26_bb_pct", "sc26_tbf",
    c(B0_LeagueAvg = "b0_bb_pct", B1_PriorYear = "b1_bb_pct", B2_Marcel_LinAge = "b2_bb_pct",
      B3_Marcel_DeltaAge = "b3_bb_pct", M4_Statcast_Physics = "m4_sc_bb_pct",
      M4_mlbr_StuffPlus = "m4_stuff_bb_pct", M4_mlbr_StuffPlus_PureOOSCal = "m4_stuff_pureoos_bb_pct"),
    ref_col = "B2_Marcel_LinAge", min_w = 100
  )
  eval_rows[[length(eval_rows) + 1L]] <- eval_block(
    b26_oos[sc26_pa >= 100], "Track4_LockedOOS_2026_Statcast", "Batter", "k_pct", "sc26_k_pct", "sc26_pa",
    c(B0_LeagueAvg = "b0_k_pct", B1_PriorYear = "b1_k_pct", B2_Marcel_LinAge = "b2_k_pct",
      B3_Marcel_DeltaAge = "b3_k_pct", M4_Statcast_Physics = "m4_k_pct"), ref_col = "B2_Marcel_LinAge", min_w = 100
  )
  eval_rows[[length(eval_rows) + 1L]] <- eval_block(
    b26_oos[sc26_pa >= 100], "Track4_LockedOOS_2026_Statcast", "Batter", "bb_pct", "sc26_bb_pct", "sc26_pa",
    c(B0_LeagueAvg = "b0_bb_pct", B1_PriorYear = "b1_bb_pct", B2_Marcel_LinAge = "b2_bb_pct",
      B3_Marcel_DeltaAge = "b3_bb_pct", M4_Statcast_Physics = "m4_bb_pct"), ref_col = "B2_Marcel_LinAge", min_w = 100
  )
  eval_rows[[length(eval_rows) + 1L]] <- eval_block(
    b26_oos[sc26_pa >= 100], "Track4_LockedOOS_2026_Statcast", "Batter", "xwoba", "sc26_xwoba", "sc26_pa",
    c(B0_LeagueAvg = "b0_woba", B1_PriorYear = "b1_woba", B2_Marcel_LinAge = "b2_woba",
      B3_Marcel_DeltaAge = "b3_woba", M4_Statcast_Physics = "m4_woba"), ref_col = "B2_Marcel_LinAge", min_w = 100
  )
  backtest_metrics <- rbindlist(eval_rows, fill = TRUE)

  cat("\n=== TRACK 1 10-YEAR ROLLING OOS (2016-2025): TRUE TANGO B2 vs DELTA-AGE B3 ===\n")
  print(backtest_metrics[track == "Track1_Rolling_2016_2025" &
                         ((domain == "Pitcher" & metric %in% c("k_pct", "bb_pct", "fip", "siera")) |
                          (domain == "Batter"  & metric %in% c("woba", "k_pct", "iso"))),
                         .(domain, metric, model, n_eval, w_rmse = round(w_rmse, 5), w_r = round(w_r, 4),
                           d_rmse = round(delta_rmse_vs_b2, 5), p_beat = round(p_beats_b2, 3))])

  cat("\n=== TRACK 3 LOCKED 2025 OOS BENCHMARK: PITCHERS ===\n")
  print(backtest_metrics[track == "Track3_LockedOOS_2025" & domain == "Pitcher" &
                         metric %in% c("k_pct", "bb_pct", "hr9", "fip", "siera"),
                         .(metric, model, n_eval, w_rmse = round(w_rmse, 5), w_mae = round(w_mae, 5),
                           w_r = round(w_r, 4), cal_slope = round(cal_slope, 3),
                           d_rmse = round(delta_rmse_vs_b2, 5), p_beat = round(p_beats_b2, 3))])

  cat("\n=== TRACK 3 LOCKED 2025 OOS BENCHMARK: BATTERS ===\n")
  print(backtest_metrics[track == "Track3_LockedOOS_2025" & domain == "Batter" &
                         metric %in% c("woba", "k_pct", "bb_pct", "iso"),
                         .(metric, model, n_eval, w_rmse = round(w_rmse, 5), w_mae = round(w_mae, 5),
                           w_r = round(w_r, 4), cal_slope = round(cal_slope, 3),
                           d_rmse = round(delta_rmse_vs_b2, 5), p_beat = round(p_beats_b2, 3))])

  cat("\n=== TRACK 4 LOCKED 2026 STATCAST OOS ===\n")
  print(backtest_metrics[track == "Track4_LockedOOS_2026_Statcast",
                         .(domain, metric, model, n_eval, w_rmse = round(w_rmse, 5), w_r = round(w_r, 4),
                           d_rmse = round(delta_rmse_vs_b2, 5), p_beat = round(p_beats_b2, 3))])

  # ============================================================================
  # 7. WRITE GOLD TIER ZSTD PARQUET TABLES + FULL SCHEMA & MONOTONICITY GUARDS
  # ============================================================================
  cat("\n-> [7/7] Writing Gold ZSTD Parquets & generating Dark Cyber 4-Asset figures...\n")
  pitch_panel[, `:=`(
    played_2026 = NA_integer_,
    eval_split = fcase(projection_year <= 2024L, "backtest_oos_2016_2024",
                       projection_year == 2025L, "locked_oos_2025",
                       projection_year == 2026L, "locked_oos_2026_sc"),
    model_version = fifelse(projection_year >= 2025L, "EXP-004-v2-m4-stuffplus",
                    fifelse(projection_year >= 2019L, "EXP-004-v2-m4-statcast", "EXP-004-v2-b3-marcel-aging"))
  )]
  bat_panel[, `:=`(
    played_2026 = NA_integer_,
    eval_split = fcase(projection_year <= 2024L, "backtest_oos_2016_2024",
                       projection_year == 2025L, "locked_oos_2025",
                       projection_year == 2026L, "locked_oos_2026_sc"),
    model_version = fifelse(projection_year >= 2019L, "EXP-004-v2-m4-statcast", "EXP-004-v2-b3-marcel-aging")
  )]

  pitch_gold_cols <- c(
    "projection_year", "pitcher_mlbam", "fg_id", "player_name", "team_prev", "p_throws", "proj_role",
    "age_proj", "last_year", "tbf_t1", "has_t1_stuff", "has_t1_fg_stuff", "played_2026",
    "proj_tbf", "proj_ip",
    "proj_k_pct", "proj_bb_pct", "proj_hr9", "proj_fip", "proj_fip_comp", "proj_siera", "proj_era",
    "proj_fip_lo80", "proj_fip_hi80", "proj_siera_lo80", "proj_siera_hi80",
    "b2_k_pct", "b2_bb_pct", "b2_hr9", "b2_fip", "b2_siera",
    "b3_k_pct", "b3_bb_pct", "b3_hr9", "b3_fip", "b3_siera",
    "m4_sc_fip", "m4_sc_siera", "m4_stuff_fip", "m4_stuff_siera",
    "age_delta_k_pct", "age_delta_bb_pct", "age_delta_fip", "age_delta_siera",
    "stuff_plus_t1", "loc_plus_t1", "pitch_plus_t1", "tunnel_plus_t1",
    "decomp_stuff_plus_t1", "decomp_loc_plus_t1", "decomp_pitch_plus_t1", "decomp_ptun_plus_t1", "has_t1_decomp", "proj_war",
    "pvi_xwoba_prior", "pvi_csw_prior", "fbv_prior", "delta_fbv_prior", "pull_air_prior",
    "obs_t1_era", "obs_t1_fip", "obs_t1_siera", "b1_era", "b1_fip", "b1_siera", "b1_k_pct", "b1_bb_pct",
    "actual_tbf", "actual_ip", "actual_k_pct", "actual_bb_pct", "actual_hr9",
    "actual_fip", "actual_siera", "actual_era", "eval_split", "model_version"
  )
  pitch_gold <- rbindlist(list(
    pitch_panel[, intersect(pitch_gold_cols, names(pitch_panel)), with = FALSE],
    live_p27_base[, intersect(pitch_gold_cols, names(live_p27_base)), with = FALSE]
  ), fill = TRUE)
  setorder(pitch_gold, projection_year, pitcher_mlbam)

  bat_gold_cols <- c(
    "projection_year", "batter_mlbam", "fg_id", "player_name", "team_prev", "bats", "position",
    "age_proj", "last_year", "pa_t1", "has_t1_battrack", "played_2026", "proj_pa",
    "proj_woba", "proj_woba_lo80", "proj_woba_hi80",
    "proj_k_pct", "proj_bb_pct", "proj_iso", "proj_obp", "proj_slg",
    "b2_woba", "b2_k_pct", "b2_bb_pct", "b2_iso",
    "b3_woba", "b3_k_pct", "b3_bb_pct", "b3_iso",
    "m4_woba", "m4_k_pct", "m4_bb_pct", "m4_iso",
    "age_delta_woba", "age_delta_k_pct", "age_delta_bb_pct", "age_delta_iso",
    "xwoba_prior", "xwoba_gap_prior", "barrel_prior", "hh_prior", "ev90_prior",
    "pull_air_prior", "sweet_prior", "oswing_prior", "zcon_prior", "swstr_prior", "zone_chase_prior",
    "decision_plus_t1", "bat_control_plus_t1", "contact_plus_t1", "process_plus_t1", "has_t1_process",
    "proj_fld_runs", "proj_pos_runs", "proj_def_runs", "proj_bsr_runs", "proj_wraa", "proj_repl_runs", "proj_war",
    "batspeed_t1", "blast_t1", "obs_t1_woba", "b1_woba", "b1_k_pct", "b1_bb_pct", "b1_iso",
    "actual_pa", "actual_ab", "actual_woba", "actual_k_pct", "actual_bb_pct",
    "actual_iso", "actual_obp", "actual_slg", "eval_split", "model_version"
  )
  bat_gold <- rbindlist(list(
    bat_panel[, intersect(bat_gold_cols, names(bat_panel)), with = FALSE],
    live_b27_base[, intersect(bat_gold_cols, names(live_b27_base)), with = FALSE]
  ), fill = TRUE)
  setorder(bat_gold, projection_year, batter_mlbam)

  # Full fail-fast Gold schema & Age 40+ aging monotonicity assertions!
  assert_true(nrow(unique(pitch_gold, by = c("projection_year", "pitcher_mlbam"))) == nrow(pitch_gold), "Dup pitch_gold key")
  assert_true(nrow(unique(bat_gold, by = c("projection_year", "batter_mlbam"))) == nrow(bat_gold), "Dup bat_gold key")
  for (col in c("age_proj", "proj_tbf", "proj_ip", "proj_k_pct", "proj_bb_pct", "proj_hr9",
                "proj_fip", "proj_fip_comp", "proj_siera", "proj_era",
                "proj_fip_lo80", "proj_fip_hi80", "proj_siera_lo80", "proj_siera_hi80", "proj_war")) {
    assert_true(all(is.finite(pitch_gold[[col]])), sprintf("Non-finite values in pitch_gold$%s", col))
  }
  for (col in c("age_proj", "proj_pa", "proj_woba", "proj_woba_lo80", "proj_woba_hi80",
                "proj_k_pct", "proj_bb_pct", "proj_iso", "proj_obp", "proj_slg", "proj_war")) {
    assert_true(all(is.finite(bat_gold[[col]])), sprintf("Non-finite values in bat_gold$%s", col))
  }
  # Verify Age 40+ monotonicity (no aging inversion!)
  assert_true(
    mean(pitch_gold[projection_year == 2026L & age_proj >= 40]$age_delta_fip) >=
      mean(pitch_gold[projection_year == 2026L & age_proj == 39]$age_delta_fip) - 0.05,
    "Age 40+ pitcher aging inversion detected!"
  )
  assert_true(
    mean(bat_gold[projection_year == 2026L & age_proj >= 40]$age_delta_woba) <=
      mean(bat_gold[projection_year == 2026L & age_proj == 39]$age_delta_woba) + 0.005,
    "Age 40+ batter aging inversion detected!"
  )
  assert_true(all(is.finite(pitch_gold[projection_year == 2027L]$fbv_prior)) &&
              all(is.finite(pitch_gold[projection_year == 2027L]$pull_air_prior)),
              "Missing 2027 pitcher fbv_prior or pull_air_prior")

  arrow::write_parquet(pitch_gold, file.path(processed_dir, "pitcher_projections_true_talent.parquet"), compression = "zstd")
  arrow::write_parquet(bat_gold,   file.path(processed_dir, "batter_projections_true_talent.parquet"),  compression = "zstd")
  arrow::write_parquet(aging_export, file.path(processed_dir, "projections_aging_curves.parquet"),       compression = "zstd")
  arrow::write_parquet(backtest_metrics, file.path(processed_dir, "projections_backtest_metrics.parquet"), compression = "zstd")
  cat(sprintf("   Wrote Gold Parquets: pitcher_projections (%s rows), batter_projections (%s rows)\n",
              format(nrow(pitch_gold), big.mark = ","), format(nrow(bat_gold), big.mark = ",")))

  # ============================================================================
  # 8. DARK CYBER 4-ASSET VISUAL SUITE (DELEGATED TO LEAN TOP-LEVEL BUILDER)
  # ============================================================================
  stab_params <- data.table(
    metric = c("Pitcher Stuff+ (Study 03)", "Pitcher K%", "Pitcher BB%", "Pitcher FIP", "Pitcher HR/9", "Pitcher ERA",
               "Batter xwOBA", "Batter Pulled Air%", "Batter K%", "Batter BB%", "Batter wOBA", "Batter BABIP"),
    domain = c(rep("Pitchers", 6), rep("Batters", 6)),
    n50 = c(45, reg_M_pitch_321[["k_pct"]] / 3, reg_M_pitch_321[["bb_pct"]] / 3,
            (reg_M_pitch_321[["fip"]] * 4.2) / 3, (reg_M_pitch_321[["hr9"]] * 4.2) / 3,
            (reg_M_pitch_321[["era"]] * 4.2) / 3,
            150, 140, reg_M_bat_543[["k_pct"]] / 5, reg_M_bat_543[["bb_pct"]] / 5,
            reg_M_bat_543[["woba"]] / 5, 600)
  )

  p24_25 <- merge(
    fg_pitch[game_year == 2024L & tbf >= 200, .(pitcher_mlbam, k24 = k_pct, bb24 = bb_pct, hr24 = hr9, fip24 = fip,
                                                siera24 = siera, era24 = era_calc, stuff24 = stuff_plus, swstr24 = swstr_pct)],
    fg_pitch[game_year == 2025L & tbf >= 200, .(pitcher_mlbam, tbf25 = tbf, k25 = k_pct, bb25 = bb_pct, hr25 = hr9, fip25 = fip,
                                                siera25 = siera, era25 = era_calc, stuff25 = stuff_plus, swstr25 = swstr_pct)],
    by = "pitcher_mlbam"
  )
  b24_25 <- merge(
    fg_bat[game_year == 2024L & pa >= 200, .(batter_mlbam, woba24 = woba, xwoba24 = xwoba, k24 = k_pct, bb24 = bb_pct,
                                             iso24 = iso, babip24 = babip, ev90_24 = ev90, pull24 = pulled_air_pct,
                                             zc24 = zone_chase_selectivity, bs24 = avg_bat_speed)],
    fg_bat[game_year == 2025L & pa >= 200, .(batter_mlbam, pa25 = pa, woba25 = woba, xwoba25 = xwoba, k25 = k_pct, bb25 = bb_pct,
                                             iso25 = iso, babip25 = babip, ev90_25 = ev90, pull25 = pulled_air_pct,
                                             zc25 = zone_chase_selectivity, bs25 = avg_bat_speed)],
    by = "batter_mlbam"
  )
  yoy_diag <- rbindlist(list(
    data.table(domain = "Pitchers", metric = "mlbr Stuff+",       yoy_r = w_cor(p24_25$stuff24, p24_25$stuff25, p24_25$tbf25), tier = "Tier 1 Physics"),
    data.table(domain = "Pitchers", metric = "SwStr%",            yoy_r = w_cor(p24_25$swstr24, p24_25$swstr25, p24_25$tbf25), tier = "Tier 1 Physics"),
    data.table(domain = "Pitchers", metric = "K%",                yoy_r = w_cor(p24_25$k24,     p24_25$k25,     p24_25$tbf25), tier = "Component Skill"),
    data.table(domain = "Pitchers", metric = "SIERA",             yoy_r = w_cor(p24_25$siera24, p24_25$siera25, p24_25$tbf25), tier = "Component Skill"),
    data.table(domain = "Pitchers", metric = "BB%",               yoy_r = w_cor(p24_25$bb24,    p24_25$bb25,    p24_25$tbf25), tier = "Component Skill"),
    data.table(domain = "Pitchers", metric = "FIP",               yoy_r = w_cor(p24_25$fip24,   p24_25$fip25,   p24_25$tbf25), tier = "Outcome Metric"),
    data.table(domain = "Pitchers", metric = "HR/9",              yoy_r = w_cor(p24_25$hr24,    p24_25$hr25,    p24_25$tbf25), tier = "Outcome Metric"),
    data.table(domain = "Pitchers", metric = "ERA",               yoy_r = w_cor(p24_25$era24,   p24_25$era25,   p24_25$tbf25), tier = "Outcome Metric"),
    data.table(domain = "Batters",  metric = "Avg Bat Speed",     yoy_r = w_cor(b24_25$bs24,    b24_25$bs25,    b24_25$pa25),  tier = "Tier 1 Physics"),
    data.table(domain = "Batters",  metric = "EV90 (Power)",      yoy_r = w_cor(b24_25$ev90_24, b24_25$ev90_25, b24_25$pa25),  tier = "Tier 1 Physics"),
    data.table(domain = "Batters",  metric = "Zone-Chase (Z-O%)", yoy_r = w_cor(b24_25$zc24,    b24_25$zc25,    b24_25$pa25),  tier = "Tier 1 Physics"),
    data.table(domain = "Batters",  metric = "K%",                yoy_r = w_cor(b24_25$k24,     b24_25$k25,     b24_25$pa25),  tier = "Component Skill"),
    data.table(domain = "Batters",  metric = "Pulled Air%",       yoy_r = w_cor(b24_25$pull24,  b24_25$pull25,  b24_25$pa25),  tier = "Tier 1 Physics"),
    data.table(domain = "Batters",  metric = "BB%",               yoy_r = w_cor(b24_25$bb24,    b24_25$bb25,    b24_25$pa25),  tier = "Component Skill"),
    data.table(domain = "Batters",  metric = "xwOBA",             yoy_r = w_cor(b24_25$xwoba24, b24_25$xwoba25, b24_25$pa25),  tier = "Tier 1 Physics"),
    data.table(domain = "Batters",  metric = "ISO",               yoy_r = w_cor(b24_25$iso24,   b24_25$iso25,   b24_25$pa25),  tier = "Component Skill"),
    data.table(domain = "Batters",  metric = "wOBA",              yoy_r = w_cor(b24_25$woba24,  b24_25$woba25,  b24_25$pa25),  tier = "Outcome Metric"),
    data.table(domain = "Batters",  metric = "BABIP",             yoy_r = w_cor(b24_25$babip24, b24_25$babip25, b24_25$pa25),  tier = "Outcome Metric")
  ))

  p26_viz <- pitch_gold[projection_year == 2026L & tbf_t1 >= 50 & has_t1_stuff == 1L &
                          proj_tbf >= 220 & is.finite(obs_t1_era) & is.finite(obs_t1_fip)]
  assert_true(nrow(p26_viz) >= 250L && all(p26_viz$tbf_t1 >= 50) && all(p26_viz$has_t1_stuff == 1L),
              "Figure 4 pitcher cohort integrity check failed")
  p26_viz <- merge(
    p26_viz,
    pitch_gold[projection_year == 2027L, .(pitcher_mlbam, proj_fip_2027 = proj_fip, proj_k_2027 = proj_k_pct)],
    by = "pitcher_mlbam", all.x = TRUE
  )
  p26_viz[, `:=`(era_gap = obs_t1_era - proj_fip, label_flag = FALSE)]
  top_buy_p  <- p26_viz[proj_role == "SP" & proj_tbf >= 450][order(-era_gap)][1:5]$pitcher_mlbam
  top_sell_p <- p26_viz[proj_role == "SP" & proj_tbf >= 450][order(era_gap)][1:5]$pitcher_mlbam
  top_ace_p  <- p26_viz[proj_role == "SP" & proj_tbf >= 450][order(proj_fip)][1:4]$pitcher_mlbam
  p26_viz[pitcher_mlbam %in% unique(c(top_buy_p, top_sell_p, top_ace_p)), label_flag := TRUE]
  p26_viz[, hover_txt := sprintf(
    "<b>%s</b> (%s | %s | Age %d)<br>2025 Obs ERA: %.2f | 2025 Obs FIP: %.2f (%.0f TBF)<br><b>2026 Proj FIP: %.2f</b> [80%% CI: %.2f-%.2f]<br>2026 Proj SIERA: %.2f | K%%: %.1f%% | BB%%: %.1f%%<br>2025 mlbr Stuff+: %.1f | Loc+: %.1f | Pitch+: %.1f<br>PVI xwOBA: %+.3f | Proj TBF: %.0f%s",
    player_name, team_prev, proj_role, as.integer(age_proj),
    obs_t1_era, obs_t1_fip, tbf_t1, proj_fip, proj_fip_lo80, proj_fip_hi80,
    proj_siera, 100 * proj_k_pct, 100 * proj_bb_pct,
    stuff_plus_t1, loc_plus_t1, pitch_plus_t1, pvi_xwoba_prior, proj_tbf,
    fifelse(is.finite(proj_fip_2027), sprintf("<br><b>Live 2027 Proj FIP: %.2f</b> (K%%: %.1f%%)", proj_fip_2027, 100 * proj_k_2027), "")
  )]

  b26_viz <- bat_gold[projection_year == 2026L & pa_t1 >= 50 & proj_pa >= 320 & is.finite(obs_t1_woba) & is.finite(xwoba_prior)]
  assert_true(nrow(b26_viz) >= 250L && all(b26_viz$pa_t1 >= 50), "Figure 5 batter cohort integrity check failed")
  b26_viz <- merge(
    b26_viz,
    bat_gold[projection_year == 2027L, .(batter_mlbam, proj_woba_2027 = proj_woba, proj_k_2027 = proj_k_pct)],
    by = "batter_mlbam", all.x = TRUE
  )
  b26_viz[, `:=`(woba_rebound = proj_woba - obs_t1_woba, xwoba_skill_gap = xwoba_prior - obs_t1_woba, label_flag = FALSE)]
  top_buy_b  <- b26_viz[proj_pa >= 400][order(-woba_rebound)][1:5]$batter_mlbam
  top_sell_b <- b26_viz[proj_pa >= 400][order(woba_rebound)][1:5]$batter_mlbam
  top_star_b <- b26_viz[proj_pa >= 450][order(-proj_woba)][1:5]$batter_mlbam
  b26_viz[batter_mlbam %in% unique(c(top_buy_b, top_sell_b, top_star_b)), label_flag := TRUE]
  b26_viz[, hover_txt := sprintf(
    "<b>%s</b> (%s | %s | Age %d)<br>2025 Obs wOBA: %.3f (%.0f PA) | Reg. xwOBA: %.3f<br><b>2026 Proj wOBA: %.3f</b> [80%% CI: %.3f-%.3f]<br>Proj K%%: %.1f%% | BB%%: %.1f%% | ISO: %.3f | PA: %.0f<br>Pulled Air%%: %.1f%% | EV90: %.1f mph | Zone-Chase: %+.1f%%<br>2025 Bat Speed: %.1f mph%s",
    player_name, team_prev, position, as.integer(age_proj),
    obs_t1_woba, pa_t1, xwoba_prior, proj_woba, proj_woba_lo80, proj_woba_hi80,
    100 * proj_k_pct, 100 * proj_bb_pct, proj_iso, proj_pa,
    100 * pull_air_prior, ev90_prior, 100 * zone_chase_prior, batspeed_t1,
    fifelse(is.finite(proj_woba_2027), sprintf("<br><b>Live 2027 Proj wOBA: %.3f</b> (K%%: %.1f%%)", proj_woba_2027, 100 * proj_k_2027), "")
  )]

  build_proj_visuals(aging_export, stab_params, yoy_diag, backtest_metrics,
                     p26_viz, b26_viz, models_dir, assets_dir)

  # ============================================================================
  # 9. FAIL-FAST EXPERIMENT REGISTRY LOGGING (6-DIGIT PRECISION)
  # ============================================================================
  get_m <- function(tr, dom, met, mod) {
    val <- backtest_metrics[track == tr & domain == dom & metric == met & model == mod]$w_rmse
    assert_true(length(val) == 1L && is.finite(val), sprintf("get_m failed for %s/%s/%s/%s (n=%d)", tr, dom, met, mod, length(val)))
    round(val, 6)
  }

  mlbr::lake_log_experiment(
    id = "EXP-004-PLAYER-PROJECTIONS",
    title = "Chronological True-Talent Player Projections: Marcel, Delta-Method Aging & Tier 1 Physics Priors",
    tier = 2L,
    status = "VALIDATED",
    summary = paste0(
      "Built audit-hardened Tier 2 chronological true-talent projection engine for MLB pitchers and batters across 2002-2027 ",
      "(10-year rolling Tango Marcel vs. Midpoint Delta-Method Aging 2016-2025, 7-year Statcast Physics 2019-2025, ",
      "locked 2024->2025 FanGraphs OOS with Stage-2 Residual mlbr Stuff+ and Hawk-Eye Bat Tracking, and secondary 2025->2026 ",
      "Regular-Season Statcast OOS). Introduced Pulled Air-Ball%, Zone-Chase Selectivity (Z-O Swing%, SEAGER-inspired proxy), ",
      "Sweet-Spot%, Fastball Velocity Momentum, sum-before-shrink seasonal PVI, and exposure-calibrated 80% uncertainty bands."
    ),
    hypotheses = c(
      "Era-adjusted sum-before-divide Tango Marcel with component-specific pre-2016 reliability M strictly beats naive prior-year persistence across every pitching and batting metric.",
      "Midpoint harmonic-mean Delta-Method aging curves eliminate cross-sectional veteran survivorship bias and expose true post-30 biological decline (though Tango's damped piecewise-linear age adjustment remains competitive standalone, while Delta-Method aging feeds Stage-1 M4 cleanly).",
      "Blending Marcel+Aging with strictly chronological Tier 1 Statcast Physics (mlbr Stuff+/Location+/Pitching+, seasonal PVI, Pulled Air%, EV90, Zone-Chase Selectivity) yields statistically significant out-of-sample weighted RMSE reduction on locked 2025 and 2026 outcomes.",
      "Exposure-calibrated residual variance models produce well-calibrated ~80% projection uncertainty bands for downstream Level 4 Monte Carlo game simulation."
    ),
    formulas = list(
      exact_ip_true = "FLOOR(IP) + ROUND((IP - FLOOR(IP)) * 10) / 3",
      baseball_age_july1 = "Season - birth_year - I(birth_month >= 7)",
      tango_marcel_age = "age <= 29: 1 + 0.006*(29 - age); age > 29: 1 / (1 + 0.003*(age - 29)) + Step 6 rebaselining",
      marcel_rate_shrinkage = "mu_lg_{T-1,g} + SUM(w_k * N_{T-k} * (r_{T-k} - mu_lg_{T-k,g})) / (SUM(w_k * N_{T-k}) + M_r)",
      delta_aging_midpoint = "lm(delta ~ ns(age - 0.5, knots = c(24.5, 30.5)), weights = 2*N_{s-1}*N_s/(N_{s-1}+N_s)) + linear boundary extrapolation",
      pulled_air_pct = "mean(LA >= 10 & ((stand=='R' & spray < -15 deg) | (stand=='L' & spray > +15 deg)))",
      zone_chase_selectivity = "Z-Swing% - O-Swing% (SEAGER-inspired box-zone proxy)",
      uncertainty_se = "sqrt(sigma_skill^2 + sigma_noise^2 / exposure)"
    ),
    metrics = list(
      pitcher_2025_b1_fip_rmse          = get_m("Track3_LockedOOS_2025", "Pitcher", "fip",   "B1_PriorYear"),
      pitcher_2025_b2_fip_rmse          = get_m("Track3_LockedOOS_2025", "Pitcher", "fip",   "B2_Marcel_LinAge"),
      pitcher_2025_b3_fip_rmse          = get_m("Track3_LockedOOS_2025", "Pitcher", "fip",   "B3_Marcel_DeltaAge"),
      pitcher_2025_m4_sc_fip_rmse       = get_m("Track3_LockedOOS_2025", "Pitcher", "fip",   "M4_Statcast_Physics"),
      pitcher_2025_m4_stuff_fip_rmse    = get_m("Track3_LockedOOS_2025", "Pitcher", "fip",   "M4_mlbr_StuffPlus"),
      pitcher_2025_m4_comp_fip_rmse     = get_m("Track3_LockedOOS_2025", "Pitcher", "fip",   "M4_Component_FIP"),
      pitcher_2025_fg_stuff_fip_rmse    = get_m("Track3_LockedOOS_2025", "Pitcher", "fip",   "Ref_FG_StuffPlus"),
      pitcher_2025_b2_siera_rmse        = get_m("Track3_LockedOOS_2025", "Pitcher", "siera", "B2_Marcel_LinAge"),
      pitcher_2025_m4_stuff_siera_rmse  = get_m("Track3_LockedOOS_2025", "Pitcher", "siera", "M4_mlbr_StuffPlus"),
      pitcher_2025_b2_kpct_rmse         = get_m("Track3_LockedOOS_2025", "Pitcher", "k_pct", "B2_Marcel_LinAge"),
      pitcher_2025_m4_sc_kpct_rmse      = get_m("Track3_LockedOOS_2025", "Pitcher", "k_pct", "M4_Statcast_Physics"),
      pitcher_2025_m4_stuff_kpct_rmse   = get_m("Track3_LockedOOS_2025", "Pitcher", "k_pct", "M4_mlbr_StuffPlus"),
      pitcher_2025_b2_bbpct_rmse        = get_m("Track3_LockedOOS_2025", "Pitcher", "bb_pct","B2_Marcel_LinAge"),
      pitcher_2025_m4_stuff_bbpct_rmse  = get_m("Track3_LockedOOS_2025", "Pitcher", "bb_pct","M4_mlbr_StuffPlus"),
      batter_2025_b1_woba_rmse          = get_m("Track3_LockedOOS_2025", "Batter",  "woba",  "B1_PriorYear"),
      batter_2025_b2_woba_rmse          = get_m("Track3_LockedOOS_2025", "Batter",  "woba",  "B2_Marcel_LinAge"),
      batter_2025_b3_woba_rmse          = get_m("Track3_LockedOOS_2025", "Batter",  "woba",  "B3_Marcel_DeltaAge"),
      batter_2025_m4_woba_rmse          = get_m("Track3_LockedOOS_2025", "Batter",  "woba",  "M4_Statcast_Physics"),
      batter_2025_m4_battrack_woba_rmse = get_m("Track3_LockedOOS_2025", "Batter",  "woba",  "M4_BatTracking"),
      batter_2025_b2_iso_rmse           = get_m("Track3_LockedOOS_2025", "Batter",  "iso",   "B2_Marcel_LinAge"),
      batter_2025_m4_iso_rmse           = get_m("Track3_LockedOOS_2025", "Batter",  "iso",   "M4_Statcast_Physics"),
      batter_2025_m4_battrack_iso_rmse  = get_m("Track3_LockedOOS_2025", "Batter",  "iso",   "M4_BatTracking"),
      batter_2025_b2_kpct_rmse          = get_m("Track3_LockedOOS_2025", "Batter",  "k_pct", "B2_Marcel_LinAge"),
      batter_2025_m4_kpct_rmse          = get_m("Track3_LockedOOS_2025", "Batter",  "k_pct", "M4_Statcast_Physics"),
      batter_2025_m4_battrack_kpct_rmse = get_m("Track3_LockedOOS_2025", "Batter",  "k_pct", "M4_BatTracking"),
      pitcher_2026_sc_b2_kpct_rmse      = get_m("Track4_LockedOOS_2026_Statcast", "Pitcher", "k_pct",  "B2_Marcel_LinAge"),
      pitcher_2026_sc_m4_kpct_rmse      = get_m("Track4_LockedOOS_2026_Statcast", "Pitcher", "k_pct",  "M4_mlbr_StuffPlus"),
      pitcher_2026_sc_pureoos_kpct_rmse = get_m("Track4_LockedOOS_2026_Statcast", "Pitcher", "k_pct",  "M4_mlbr_StuffPlus_PureOOSCal"),
      pitcher_2026_sc_b2_bbpct_rmse     = get_m("Track4_LockedOOS_2026_Statcast", "Pitcher", "bb_pct", "B2_Marcel_LinAge"),
      pitcher_2026_sc_m4_bbpct_rmse     = get_m("Track4_LockedOOS_2026_Statcast", "Pitcher", "bb_pct", "M4_mlbr_StuffPlus"),
      batter_2026_sc_b2_xwoba_rmse      = get_m("Track4_LockedOOS_2026_Statcast", "Batter",  "xwoba",  "B2_Marcel_LinAge"),
      batter_2026_sc_m4_xwoba_rmse      = get_m("Track4_LockedOOS_2026_Statcast", "Batter",  "xwoba",  "M4_Statcast_Physics"),
      cov80_pitcher_fip_2025            = round(p25_cov, 4),
      cov80_batter_woba_2025            = round(b25_cov, 4),
      gold_pitcher_rows                 = nrow(pitch_gold),
      gold_batter_rows                  = nrow(bat_gold)
    ),
    features = c(
      "ip_true", "age_proj", "proj_role", "age_delta_woba", "age_delta_fip",
      "pulled_air_pct", "sweet_spot_pct", "zone_chase_selectivity", "delta_fbv",
      "pvi_xwoba_prior", "pvi_csw_prior", "stuff_plus_t1", "loc_plus_t1", "pitch_plus_t1",
      "proj_fip", "proj_fip_comp", "proj_siera", "proj_woba", "proj_obp", "proj_slg",
      "proj_fip_lo80", "proj_fip_hi80", "proj_woba_lo80", "proj_woba_hi80"
    ),
    artifacts = list(
      spec = "specs/04_player_projections_marcel_aging.md",
      script = "models/04_player_projections_marcel_aging.R",
      study_qmd = "site/analyses/04_player_projections.qmd",
      study_html = "http://192.168.10.1:8088/analyses/04_player_projections.html",
      gold_pitcher_projections = "data/processed/pitcher_projections_true_talent.parquet",
      gold_batter_projections = "data/processed/batter_projections_true_talent.parquet",
      gold_aging_curves = "data/processed/projections_aging_curves.parquet",
      gold_backtest_metrics = "data/processed/projections_backtest_metrics.parquet"
    ),
    sync_manifest = TRUE,
    lake = lake
  )

  cat("\n=== TOP 10 PROJECTED 2026 STARTING PITCHERS (Active 2025, Proj TBF >= 550) ===\n")
  print(head(pitch_gold[projection_year == 2026L & proj_role == "SP" & tbf_t1 >= 50 & proj_tbf >= 550][order(proj_fip), .(
    player_name, team_prev, age_proj, proj_tbf = round(proj_tbf), obs25_era = round(obs_t1_era, 2),
    proj_fip = round(proj_fip, 2), proj_siera = round(proj_siera, 2),
    k_pct = round(100 * proj_k_pct, 1), bb_pct = round(100 * proj_bb_pct, 1),
    stuff = round(stuff_plus_t1, 1), loc = round(loc_plus_t1, 1)
  )], 10))

  cat("\n=== TOP 10 PROJECTED 2026 BATTERS (Active 2025, Proj PA >= 500) ===\n")
  print(head(bat_gold[projection_year == 2026L & pa_t1 >= 50 & proj_pa >= 500][order(-proj_woba), .(
    player_name, team_prev, position, age_proj, proj_pa = round(proj_pa),
    obs25_woba = round(obs_t1_woba, 3), xwoba = round(xwoba_prior, 3), proj_woba = round(proj_woba, 3),
    k_pct = round(100 * proj_k_pct, 1), bb_pct = round(100 * proj_bb_pct, 1),
    iso = round(proj_iso, 3), pull_air = round(100 * pull_air_prior, 1)
  )], 10))

  cat("\n=== TOP 8 LIVE 2027 STARTING PITCHERS (Proj TBF >= 550) ===\n")
  print(head(pitch_gold[projection_year == 2027L & proj_role == "SP" & proj_tbf >= 550][order(proj_fip), .(
    player_name, age_proj, proj_tbf = round(proj_tbf), proj_fip = round(proj_fip, 2),
    proj_siera = round(proj_siera, 2), k_pct = round(100 * proj_k_pct, 1),
    bb_pct = round(100 * proj_bb_pct, 1), stuff26 = round(stuff_plus_t1, 1)
  )], 8))

  cat("\n=== TOP 8 LIVE 2027 BATTERS (Proj PA >= 500) ===\n")
  print(head(bat_gold[projection_year == 2027L & proj_pa >= 500][order(-proj_woba), .(
    player_name, age_proj, proj_pa = round(proj_pa), proj_woba = round(proj_woba, 3),
    proj_obp = round(proj_obp, 3), proj_slg = round(proj_slg, 3),
    k_pct = round(100 * proj_k_pct, 1), bb_pct = round(100 * proj_bb_pct, 1)
  )], 8))

  cat(sprintf("\n=== EXP-004 (AUDIT-HARDENED v2) COMPLETED CLEANLY IN %.1f SECONDS ===\n",
              proc.time()[["elapsed"]] - t0_total))
}

run_exp004()
