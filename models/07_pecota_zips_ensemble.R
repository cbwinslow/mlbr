# ==============================================================================
# mlbr EXP-007: PECOTA k-NN Similarity Comps, ZiPS Archetype Multi-Year Aging,
#               Non-Pitch Defense/Baserunning EDA & 5-Paradigm Meta-Ensemble
# ==============================================================================
# Architecture Tier: Level 2B (Complete Player Projection Gamut)
# CLI Usage:
#   Rscript models/07_pecota_zips_ensemble.R --smoke   # Fast 3-yr validation (<10s)
#   Rscript models/07_pecota_zips_ensemble.R --full    # Full 1960-2026 lake (default)
# ==============================================================================

suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(data.table)
  library(arrow)
  library(splines)
  library(ggplot2)
  library(plotly)
  library(htmlwidgets)
})

# --- Fail-Fast & Weighted Math Utilities --------------------------------------
assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) stop(paste0("[EXP-007 ASSERTION FAILED] ", msg), call. = FALSE)
}

w_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

w_sd_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) < 5L) return(NA_real_)
  mu <- sum(x[ok] * w[ok]) / sum(w[ok])
  v  <- sum(w[ok] * (x[ok] - mu)^2) / sum(w[ok])
  sqrt(max(v, 1e-10))
}

w_cor_safe <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(ok) < 15L) return(NA_real_)
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  mx <- sum(w * x) / sum(w); my <- sum(w * y) / sum(w)
  cov_xy <- sum(w * (x - mx) * (y - my)) / sum(w)
  vx <- sum(w * (x - mx)^2) / sum(w)
  vy <- sum(w * (y - my)^2) / sum(w)
  if (vx <= 1e-12 || vy <= 1e-12) return(NA_real_)
  cov_xy / sqrt(vx * vy)
}

w_rmse_safe <- function(pred, actual, w) {
  ok <- is.finite(pred) & is.finite(actual) & is.finite(w) & w > 0
  if (sum(ok) < 10L) return(NA_real_)
  sqrt(sum(w[ok] * (pred[ok] - actual[ok])^2) / sum(w[ok]))
}

w_mae_safe <- function(pred, actual, w) {
  ok <- is.finite(pred) & is.finite(actual) & is.finite(w) & w > 0
  if (sum(ok) < 10L) return(NA_real_)
  sum(w[ok] * abs(pred[ok] - actual[ok])) / sum(w[ok])
}

w_quantile <- function(x, w, probs = c(0.10, 0.25, 0.50, 0.75, 0.90)) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  x <- x[ok]; w <- w[ok]
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) {
    idx <- which(cw >= p)[1]
    if (is.na(idx)) x[length(x)] else x[idx]
  })
}

# --- Midpoint Delta-Method Spline Aging with Linear Boundary Extrapolation ----
fit_archetype_aging <- function(dt, val_col, lg_col, exp_col, id_col, max_year, min_exp = 50) {
  sub <- dt[game_year <= max_year & is.finite(get(val_col)) & is.finite(get(lg_col)) &
              is.finite(get(exp_col)) & get(exp_col) >= min_exp & is.finite(age)]
  setorderv(sub, c(id_col, "game_year"))
  sub[, `:=`(
    prev_yr  = shift(game_year, 1L),
    prev_val = shift(get(val_col), 1L),
    prev_lg  = shift(get(lg_col), 1L),
    prev_exp = shift(get(exp_col), 1L),
    prev_age = shift(age, 1L)
  ), by = id_col]

  tr <- sub[game_year == prev_yr + 1L & age == prev_age + 1L & age >= 20 & age <= 41]
  if (nrow(tr) < 80L) {
    return(list(cum_fn = function(a) rep(0.0, length(a)), n_pairs = nrow(tr)))
  }
  tr[, `:=`(
    delta   = (get(val_col) - get(lg_col)) - (prev_val - prev_lg),
    age_mid = age - 0.5,
    h_wt    = (2.0 * prev_exp * get(exp_col)) / (prev_exp + get(exp_col))
  )]

  fit <- stats::lm(delta ~ splines::ns(age_mid, knots = c(24.5, 30.5)), data = tr, weights = h_wt)
  grid_mid <- seq(20.5, 39.5, by = 1.0)
  d_pred <- as.numeric(stats::predict(fit, newdata = data.frame(age_mid = grid_mid)))
  ages_grid <- 20L:40L
  cum_vals <- numeric(length(ages_grid))
  idx27 <- which(ages_grid == 27L)
  for (k in (idx27 + 1L):length(ages_grid)) cum_vals[k] <- cum_vals[k - 1L] + d_pred[k - 1L]
  for (k in (idx27 - 1L):1L) cum_vals[k] <- cum_vals[k + 1L] - d_pred[k]
  slope_lo <- d_pred[1]; slope_hi <- d_pred[length(d_pred)]

  cum_fn <- function(a) {
    out <- numeric(length(a))
    for (i in seq_along(a)) {
      ai <- a[i]
      if (!is.finite(ai)) out[i] <- 0.0
      else if (ai < 20)   out[i] <- cum_vals[1] - (20 - ai) * slope_lo
      else if (ai > 40)   out[i] <- cum_vals[length(cum_vals)] + (ai - 40) * slope_hi
      else                out[i] <- stats::approx(ages_grid, cum_vals, xout = ai)$y
    }
    out
  }
  list(cum_fn = cum_fn, grid = data.table(age = ages_grid, cum_delta = cum_vals), n_pairs = nrow(tr))
}

# --- Vectorized PECOTA k-NN Similarity Matcher --------------------------------
pecota_knn_predict <- function(Q, C, cand_delta_y, cand_surv, cand_next_exp,
                               cand_ids, cand_names, cand_years, cand_ages,
                               q_base_y, q_ids, k = 50L, tau = 1.85,
                               breakout_thresh = 0.030, collapse_thresh = -0.030,
                               lower_is_better = FALSE, return_comps = FALSE) {
  n_q <- nrow(Q); n_c <- nrow(C)
  assert_true(n_q > 0L && n_c >= k, sprintf("pecota_knn_predict insufficient rows (n_q=%d, n_c=%d)", n_q, n_c))

  q_norm <- rowSums(Q^2); c_norm <- rowSums(C^2)
  D2 <- outer(q_norm, c_norm, "+") - 2.0 * tcrossprod(Q, C)
  D2[D2 < 0] <- 0.0

  p10 <- p25 <- p50 <- p75 <- p90 <- mean_pred <- surv_prob <- break_prob <- coll_prob <- exp_pred <- numeric(n_q)
  top_comps_list <- if (return_comps) vector("list", n_q) else NULL

  for (i in seq_len(n_q)) {
    d2_i <- D2[i, ]
    self_mask <- (cand_ids == q_ids[i])
    if (any(self_mask)) d2_i[self_mask] <- Inf

    ord <- order(d2_i)[seq_len(k)]
    d_k <- sqrt(d2_i[ord])
    d_shift <- d_k - min(d_k)
    w_k <- exp(-0.5 * (d_shift / tau)^2)
    w_k <- w_k / sum(w_k)

    surv_k <- cand_surv[ord]
    surv_prob[i] <- sum(w_k * surv_k)
    exp_pred[i]  <- sum(w_k * cand_next_exp[ord])

    ok_y <- is.finite(cand_delta_y[ord]) & surv_k == 1L
    if (sum(ok_y) >= 5L) {
      dy_k <- cand_delta_y[ord][ok_y]
      wy_k <- w_k[ok_y] / sum(w_k[ok_y])
      proj_dist <- q_base_y[i] + dy_k
      qs <- w_quantile(proj_dist, wy_k, c(0.10, 0.25, 0.50, 0.75, 0.90))
      p10[i] <- qs[1]; p25[i] <- qs[2]; p50[i] <- qs[3]; p75[i] <- qs[4]; p90[i] <- qs[5]
      mean_pred[i] <- sum(wy_k * proj_dist)
      if (!lower_is_better) {
        break_prob[i] <- sum(wy_k * as.numeric(dy_k >= breakout_thresh))
        coll_prob[i]  <- sum(wy_k * as.numeric(dy_k <= collapse_thresh))
      } else {
        break_prob[i] <- sum(wy_k * as.numeric(dy_k <= breakout_thresh))
        coll_prob[i]  <- sum(wy_k * as.numeric(dy_k >= collapse_thresh))
      }
    } else {
      p10[i] <- p25[i] <- p50[i] <- p75[i] <- p90[i] <- mean_pred[i] <- q_base_y[i]
      break_prob[i] <- NA_real_; coll_prob[i] <- NA_real_
    }

    if (return_comps) {
      top5 <- ord[1:5]
      sim_score <- round(100.0 * exp(-0.22 * sqrt(d2_i[top5])), 1)
      top_comps_list[[i]] <- data.table(
        query_idx  = i, comp_rank = 1:5, comp_name = cand_names[top5],
        comp_year  = cand_years[top5], comp_age = cand_ages[top5],
        similarity = sim_score, comp_delta = round(cand_delta_y[top5], 4), comp_surv = cand_surv[top5]
      )
    }
  }

  list(
    pred_mean = mean_pred, p10 = p10, p25 = p25, p50 = p50, p75 = p75, p90 = p90,
    surv_prob = surv_prob, break_prob = break_prob, coll_prob = coll_prob,
    exp_pred = exp_pred, comps = if (return_comps) rbindlist(top_comps_list) else NULL
  )
}

project_simplex <- function(v) {
  u <- sort(v, decreasing = TRUE)
  cssv <- cumsum(u) - 1.0
  ind <- seq_along(u)
  cond <- u - cssv / ind > 0
  rho <- max(ind[cond])
  theta <- cssv[rho] / rho
  pmax(v - theta, 0.0)
}

fit_convex_stack <- function(P_mat, y, w) {
  ok <- apply(P_mat, 1, function(r) all(is.finite(r))) & is.finite(y) & is.finite(w) & w > 0
  P <- P_mat[ok, , drop = FALSE]; y <- y[ok]; w <- w[ok]
  m <- ncol(P)
  if (nrow(P) < 30L) return(rep(1.0 / m, m))
  wn <- w / sum(w)
  PW <- P * wn
  H <- 2.0 * crossprod(P, PW)
  g <- as.numeric(2.0 * crossprod(PW, y))

  # Exact active-set KKT solver over all (2^m - 1) non-empty faces of the probability simplex
  best_x <- NULL
  best_mse <- Inf
  eval_mse <- function(x) {
    r <- as.numeric(P %*% x) - y
    sum(wn * r^2)
  }
  for (mask in seq_len(bitwShiftL(1L, m) - 1L)) {
    idx <- which(as.logical(intToBits(mask)[seq_len(m)]))
    k <- length(idx)
    if (k == 1L) {
      x <- numeric(m); x[idx] <- 1.0
      mse <- eval_mse(x)
      if (mse < best_mse) { best_mse <- mse; best_x <- x }
    } else {
      Hs <- H[idx, idx, drop = FALSE]
      gs <- g[idx]
      # KKT system: [Hs, 1; 1^T, 0] [x_s; lambda] = [gs; 1]
      Kmat <- rbind(cbind(Hs, rep(1.0, k)), c(rep(1.0, k), 0.0))
      rhs  <- c(gs, 1.0)
      sol <- tryCatch(solve(Kmat, rhs), error = function(e) NULL)
      if (!is.null(sol)) {
        xs <- sol[seq_len(k)]
        if (all(xs >= -1e-11)) {
          xs <- pmax(xs, 0.0); xs <- xs / sum(xs)
          x <- numeric(m); x[idx] <- xs
          mse <- eval_mse(x)
          if (mse < best_mse) { best_mse <- mse; best_x <- x }
        }
      }
    }
  }
  assert_true(!is.null(best_x) && abs(sum(best_x) - 1.0) < 1e-12 && all(best_x >= 0.0),
              "fit_convex_stack failed exact KKT simplex solve")
  # Verify KKT optimality: for all j, grad_j >= min_i(grad_i) - 1e-8 and active indices share minimum grad
  grad <- as.numeric(H %*% best_x) - g
  min_g <- min(grad)
  act <- which(best_x > 1e-7)
  assert_true(all(grad[act] <= min_g + 1e-7), "fit_convex_stack KKT optimality check failed")
  best_x
}

# --- Top-Level Dark Cyber Plotting Functions (Zero Closure Bloat < 5 MB) ------
make_pz01_eda <- function(df_rel) {
  force(df_rel)
  ggplot(df_rel, aes(x = reorder(metric_label, yoy_r), y = yoy_r, fill = domain_group)) +
    geom_col(width = 0.68, alpha = 0.92) +
    geom_text(aes(label = sprintf("r = %.3f", yoy_r)), hjust = -0.15, color = "#f8fafc", size = 3.4, fontface = "bold") +
    coord_flip(ylim = c(0, 0.95)) +
    scale_fill_manual(values = c("Offense" = "#38bdf8", "Baserunning" = "#10b981", "Defense" = "#f59e0b"), name = "Skill Domain") +
    labs(
      title = "EXP-007 Non-Pitch EDA: Year-over-Year Skill Reliability (2002-2025 Full Lake)",
      subtitle = "Plate discipline (K%, BB%) stabilizes rapidly; Baserunning (wBsR) & Fielding (OAA/DRS/UZR) require heavier regression",
      x = NULL, y = "Exposure-Weighted Year-over-Year Correlation (r_{t, t+1})",
      caption = "Source: mlbr v_fangraphs_bat & v_fangraphs_fielder (2002-2025, Inn >= 300 / PA >= 200)"
    ) +
    mlbr::theme_cyber(base_size = 11)
}

make_pz02_pecota_static <- function(df) {
  force(df)
  ggplot(df, aes(y = player_label, x = pecota_p50)) +
    geom_segment(aes(x = pecota_p10, xend = pecota_p90, yend = player_label), color = "#38bdf8", linewidth = 2.0, alpha = 0.45) +
    geom_segment(aes(x = pecota_p25, xend = pecota_p75, yend = player_label), color = "#10b981", linewidth = 4.2, alpha = 0.80) +
    geom_point(aes(x = ensemble_woba), color = "#f59e0b", size = 3.2, shape = 18) +
    geom_point(color = "#f8fafc", size = 2.6) +
    scale_x_continuous(labels = function(x) sprintf(".%03d", round(x * 1000))) +
    labs(
      title = "EXP-007 PECOTA k-NN Percentile Fan: Top 20 Projected 2026 Batters (1960-2025 Comp Pool)",
      subtitle = "Cyan = 10th-90th Percentile | Emerald = 25th-75th IQR | White Dot = PECOTA p50 | Gold Diamond = M5 Ensemble",
      x = "Projected 2026 True-Talent wOBA Distribution", y = NULL,
      caption = "Source: mlbr EXP-007 PECOTA k-NN Similarity Engine (k = 50 era-neutralized historical comps)"
    ) +
    mlbr::theme_cyber(base_size = 10.5)
}

make_pz02_pecota_plotly <- function(df) {
  force(df)
  df$hover_txt <- sprintf(
    "<b>%s</b> (%s, Age %d)<br>Ensemble wOBA: <b>.%.0f</b> | PECOTA p50: <b>.%.0f</b><br>PECOTA 10th-90th: .%.0f - .%.0f<br>Breakout: <b>%.1f%%</b> | Collapse: %.1f%% | Survival: %.1f%%<br>Top Comp: <b>%s</b>",
    df$player_name, df$pos_group, as.integer(df$age_proj),
    df$ensemble_woba * 1000, df$pecota_p50 * 1000, df$pecota_p10 * 1000, df$pecota_p90 * 1000,
    100 * df$breakout_prob, 100 * df$collapse_prob, 100 * df$survival_prob, df$top_comp_label
  )
  plotly::plot_ly(
    data = df, x = ~pecota_p50, y = ~breakout_prob * 100, type = "scatter", mode = "markers",
    text = ~hover_txt, hoverinfo = "text",
    marker = list(
      size = 10, color = ~age_proj, colorscale = "Viridis", reversescale = TRUE,
      colorbar = list(title = list(text = "2026 Age", font = list(color = "#e2e8f0")), tickfont = list(color = "#94a3b8")),
      line = list(width = 0.7, color = "#090d16")
    )
  ) %>%
    plotly::layout(
      title = list(text = "EXP-007 Interactive PECOTA Explorer: Median wOBA vs. Breakout Probability (2026)", font = list(color = "#f8fafc", size = 14)),
      paper_bgcolor = "#090d16", plot_bgcolor = "#111827",
      xaxis = list(title = "PECOTA k-NN Median Projected 2026 wOBA", gridcolor = "#1e293b", color = "#e2e8f0"),
      yaxis = list(title = "Empirical Breakout Probability (% with >= +30 pts wOBA)", gridcolor = "#1e293b", color = "#e2e8f0")
    )
}

make_pz03_zips_5yr <- function(df) {
  force(df)
  ggplot(df, aes(x = horizon_year, y = proj_woba, color = player_label, group = player_label)) +
    geom_line(linewidth = 1.35, alpha = 0.92) +
    geom_point(size = 3.0) +
    scale_color_manual(values = c("#38bdf8", "#10b981", "#f59e0b", "#f43f5e", "#a78bfa", "#2dd4bf")) +
    scale_x_continuous(breaks = 2026:2030) +
    scale_y_continuous(labels = function(x) sprintf(".%03d", round(x * 1000))) +
    labs(
      title = "EXP-007 ZiPS-Style 5-Year Archetype Career Trajectories (2026-2030)",
      subtitle = "Young stars hold/peak through late 20s; 30+ veterans follow archetype-specific midpoint Delta-Method decay",
      x = "Projection Season", y = "Projected True-Talent wOBA (Archetype Delta-Aged)", color = "Player (2026 Age / Archetype)",
      caption = "Source: mlbr EXP-007 ZiPS 8/5/4/3 Multi-Year Archetype Aging Engine"
    ) +
    mlbr::theme_cyber(base_size = 11)
}

make_pz04_shootout <- function(df) {
  force(df)
  ggplot(df, aes(x = reorder(model_label, -w_rmse), y = w_rmse, fill = is_best)) +
    geom_col(width = 0.65, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.4f", w_rmse)), vjust = -0.35, color = "#f8fafc", size = 3.2, fontface = "bold") +
    facet_wrap(~panel_title, scales = "free_y") +
    scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#38bdf8"), guide = "none") +
    labs(
      title = "EXP-007 5-Paradigm Projection Shootout: Locked 2024 -> 2025 Out-of-Sample Weighted RMSE",
      subtitle = "Tango Marcel vs. ZiPS Archetype vs. PECOTA k-NN vs. Statcast Physics (M4) vs. M5 Consensus Ensemble (lower is better)",
      x = NULL, y = "Exposure-Weighted Out-of-Sample RMSE",
      caption = "Source: mlbr EXP-007 Strictly Chronological Out-of-Sample Benchmark (2024 -> 2025 Locked Test)"
    ) +
    mlbr::theme_cyber(base_size = 10.5) +
    theme(axis.text.x = element_text(angle = 22, hjust = 1))
}

# ==============================================================================
# MAIN PIPELINE
# ==============================================================================
run_exp007 <- function(smoke = FALSE, lake = NULL) {
  t0 <- Sys.time()
  if (is.null(lake)) lake <- mlbr::default_lake()
  mode_label <- if (smoke) "SMOKE (Fast 2024-2026 Verification)" else "FULL (1960-2026 Complete Lake)"
  cat("==============================================================================\n")
  cat(sprintf("mlbr EXP-007: PECOTA k-NN, ZiPS Archetype Aging & Meta-Ensemble [%s]\n", mode_label))
  cat("==============================================================================\n")

  processed_dir <- file.path(lake, "data/processed")
  models_dir    <- file.path(lake, "models")
  assets_dir    <- file.path(lake, "site/assets")
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(assets_dir, recursive = TRUE, showWarnings = FALSE)

  eval_years  <- if (smoke) 2024L:2026L else 2016L:2026L
  min_comp_yr <- if (smoke) 2005L else 1960L

  # ----------------------------------------------------------------------------
  # 1. EXTRACT HISTORICAL LAKE DATA (1960-2025) & NON-PITCH FIELDING DATA
  # ----------------------------------------------------------------------------
  cat("\n-> [1/6] Extracting historical FanGraphs (1960-2025), Fielding (2002-2025) & Lahman physicals...\n")
  con <- mlbr::lake_duckdb(lake = lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")

  bat_raw <- as.data.table(DBI::dbGetQuery(con, sprintf("
    WITH cw AS (
      SELECT DISTINCT CAST(key_mlbam AS INTEGER) AS mlbam, key_bbref, birth_year, birth_month
      FROM v_chadwick_register WHERE key_mlbam IS NOT NULL
    ),
    lp_dedup AS (
      SELECT bbrefID, height, weight, bats, throws
      FROM v_lahman_people
      WHERE bbrefID IS NOT NULL
      QUALIFY ROW_NUMBER() OVER (PARTITION BY bbrefID ORDER BY CASE WHEN playerID = bbrefID THEN 0 ELSE 1 END, birthYear) = 1
    )
    SELECT
      CAST(b.Season AS INTEGER) AS game_year,
      CAST(b.xMLBAMID AS INTEGER) AS mlbam,
      b.PlayerName AS player_name, b.team_name_abb AS team,
      COALESCE(b.position, 'OF') AS position,
      CAST(b.PA AS DOUBLE) AS pa, CAST(b.AB AS DOUBLE) AS ab,
      COALESCE(CAST(b.SO AS DOUBLE), 0.0) AS so,
      COALESCE(CAST(b.HR AS DOUBLE), 0.0) AS hr,
      COALESCE(CAST(b.SF AS DOUBLE), 0.0) AS sf,
      CAST(b.wOBA AS DOUBLE) AS woba, CAST(b.K_pct AS DOUBLE) AS k_pct,
      CAST(b.BB_pct AS DOUBLE) AS bb_pct, CAST(b.ISO AS DOUBLE) AS iso,
      CAST(b.BABIP AS DOUBLE) AS babip, CAST(b.OBP AS DOUBLE) AS obp, CAST(b.SLG AS DOUBLE) AS slg,
      COALESCE(CAST(b.SB AS DOUBLE), 0.0) AS sb, COALESCE(CAST(b.CS AS DOUBLE), 0.0) AS cs,
      CAST(b.wBsR AS DOUBLE) AS bsr_runs, CAST(b.Fielding AS DOUBLE) AS fld_runs,
      CAST(b.\"Positional\" AS DOUBLE) AS pos_runs, CAST(b.Defense AS DOUBLE) AS def_runs,
      CAST(b.WAR AS DOUBLE) AS war,
      cw.birth_year, cw.birth_month,
      CAST(lp.height AS DOUBLE) AS height_in, CAST(lp.weight AS DOUBLE) AS weight_lb,
      COALESCE(lp.bats, 'R') AS bats
    FROM v_fangraphs_bat b
    LEFT JOIN cw ON CAST(b.xMLBAMID AS INTEGER) = cw.mlbam
    LEFT JOIN lp_dedup lp ON cw.key_bbref = lp.bbrefID
    WHERE b.Season BETWEEN %d AND 2025 AND b.PA >= 1
      AND COALESCE(b.position, '') != 'P' AND b.xMLBAMID IS NOT NULL
  ", min_comp_yr)))

  pitch_raw <- as.data.table(DBI::dbGetQuery(con, sprintf("
    WITH cw AS (
      SELECT DISTINCT CAST(key_mlbam AS INTEGER) AS mlbam, key_bbref, birth_year, birth_month
      FROM v_chadwick_register WHERE key_mlbam IS NOT NULL
    ),
    lp_dedup AS (
      SELECT bbrefID, height, weight, bats, throws
      FROM v_lahman_people
      WHERE bbrefID IS NOT NULL
      QUALIFY ROW_NUMBER() OVER (PARTITION BY bbrefID ORDER BY CASE WHEN playerID = bbrefID THEN 0 ELSE 1 END, birthYear) = 1
    )
    SELECT
      CAST(p.Season AS INTEGER) AS game_year,
      CAST(p.xMLBAMID AS INTEGER) AS mlbam,
      p.PlayerName AS player_name, p.team_name_abb AS team,
      CAST(p.IP AS DOUBLE) AS ip_box, CAST(p.TBF AS DOUBLE) AS tbf,
      CAST(p.G AS DOUBLE) AS g, CAST(p.GS AS DOUBLE) AS gs,
      CAST(p.SO AS DOUBLE) AS so, CAST(p.BB AS DOUBLE) AS bb, CAST(p.HR AS DOUBLE) AS hr,
      CAST(p.FIP AS DOUBLE) AS fip, CAST(p.ERA AS DOUBLE) AS era,
      CAST(p.K_pct AS DOUBLE) AS k_pct, CAST(p.BB_pct AS DOUBLE) AS bb_pct,
      CAST(p.WAR AS DOUBLE) AS war,
      cw.birth_year, cw.birth_month,
      CAST(lp.height AS DOUBLE) AS height_in, CAST(lp.weight AS DOUBLE) AS weight_lb,
      COALESCE(lp.throws, 'R') AS throws
    FROM v_fangraphs_pitch p
    LEFT JOIN cw ON CAST(p.xMLBAMID AS INTEGER) = cw.mlbam
    LEFT JOIN lp_dedup lp ON cw.key_bbref = lp.bbrefID
    WHERE p.Season BETWEEN %d AND 2025 AND p.TBF >= 1 AND p.xMLBAMID IS NOT NULL
  ", min_comp_yr)))

  fielder_raw <- as.data.table(DBI::dbGetQuery(con, "
    SELECT
      CAST(Season AS INTEGER) AS game_year, CAST(xMLBAMID AS INTEGER) AS mlbam,
      PlayerName AS player_name, Pos AS pos, CAST(Inn AS DOUBLE) AS inn,
      CAST(DRS AS DOUBLE) AS drs, CAST(UZR AS DOUBLE) AS uzr,
      CAST(UZR_150 AS DOUBLE) AS uzr_150, CAST(OAA AS DOUBLE) AS oaa,
      CAST(Defense AS DOUBLE) AS defense
    FROM v_fangraphs_fielder
    WHERE Season BETWEEN 2002 AND 2025 AND xMLBAMID IS NOT NULL AND Pos != 'P' AND CAST(Inn AS DOUBLE) >= 100
  "))

  assert_true(nrow(unique(bat_raw, by = c("game_year", "mlbam"))) == nrow(bat_raw), "Duplicate (game_year, mlbam) in bat_raw")
  assert_true(nrow(unique(pitch_raw, by = c("game_year", "mlbam"))) == nrow(pitch_raw), "Duplicate (game_year, mlbam) in pitch_raw")

  pitch_raw[, `:=`(
    ip_true = floor(ip_box) + round((ip_box - floor(ip_box)) * 10) / 3.0,
    age     = game_year - birth_year - as.integer(birth_month >= 7L),
    role    = fifelse(gs / pmax(g, 1.0) >= 0.50, "SP", "RP")
  )]
  pitch_raw[, `:=`(
    hr9 = 9.0 * hr / pmax(ip_true, 1.0 / 3.0),
    bmi = fifelse(is.finite(height_in) & height_in > 50 & is.finite(weight_lb) & weight_lb > 100,
                  703.0 * weight_lb / (height_in^2), 26.5)
  )]

  bat_raw[, `:=`(
    age       = game_year - birth_year - as.integer(birth_month >= 7L),
    sb_rate   = sb / pmax(pa, 1.0),
    pos_group = fcase(grepl("^C", position), "C", grepl("SS|2B|CF", position), "Middle", default = "Corner_DH"),
    bmi       = fifelse(is.finite(height_in) & height_in > 50 & is.finite(weight_lb) & weight_lb > 100,
                        703.0 * weight_lb / (height_in^2), 26.5)
  )]

  cat(sprintf("   Extracted %d batter-seasons, %d pitcher-seasons (%d-2025), %d fielder-seasons (2002-2025).\n",
              nrow(bat_raw), nrow(pitch_raw), min_comp_yr, nrow(fielder_raw)))

  # ----------------------------------------------------------------------------
  # 2. NON-PITCH EDA: DEFENSE, BASERUNNING & SKILL STABILIZATION (2002-2025)
  # ----------------------------------------------------------------------------
  cat("\n-> [2/6] Running Non-Pitch EDA (Defense OAA/DRS/UZR, Baserunning wBsR, Archetype Aging)...\n")

  fld_yr <- fielder_raw[, .(
    inn_tot = sum(inn, na.rm = TRUE),
    oaa     = if (all(is.na(oaa))) NA_real_ else sum(oaa, na.rm = TRUE),
    drs     = if (all(is.na(drs))) NA_real_ else sum(drs, na.rm = TRUE),
    uzr_150 = w_mean_safe(uzr_150, inn),
    defense = if (all(is.na(defense))) NA_real_ else sum(defense, na.rm = TRUE)
  ), by = .(game_year, mlbam)]

  calc_yoy_rel <- function(dt, col, wt_col, min_wt, domain_group, label) {
    s <- dt[is.finite(get(col)) & get(wt_col) >= min_wt]
    setorder(s, mlbam, game_year)
    s[, `:=`(prev_yr = shift(game_year, 1L), prev_val = shift(get(col), 1L), prev_wt = shift(get(wt_col), 1L)), by = mlbam]
    p <- s[game_year == prev_yr + 1L]
    p[, hw := (2.0 * prev_wt * get(wt_col)) / (prev_wt + get(wt_col))]
    data.table(domain_group = domain_group, metric_col = col, metric_label = label,
               n_pairs = nrow(p), yoy_r = round(w_cor_safe(p$prev_val, p[[col]], p$hw), 4))
  }

  bat_mod <- bat_raw[game_year >= 2002L]
  eda_rel <- rbindlist(list(
    calc_yoy_rel(bat_mod, "k_pct",    "pa",      200, "Offense",     "Batter K% (Strikeout Rate)"),
    calc_yoy_rel(bat_mod, "bb_pct",   "pa",      200, "Offense",     "Batter BB% (Walk Rate)"),
    calc_yoy_rel(bat_mod, "iso",      "pa",      200, "Offense",     "Batter ISO (Isolated Power)"),
    calc_yoy_rel(bat_mod, "woba",     "pa",      200, "Offense",     "Batter wOBA (Overall Offense)"),
    calc_yoy_rel(bat_mod, "babip",    "pa",      200, "Offense",     "Batter BABIP"),
    calc_yoy_rel(bat_mod, "sb_rate",  "pa",      200, "Baserunning", "Stolen Base Rate (SB / PA)"),
    calc_yoy_rel(bat_mod, "bsr_runs", "pa",      200, "Baserunning", "Baserunning Runs (wBsR)"),
    calc_yoy_rel(fld_yr,  "oaa",      "inn_tot", 300, "Defense",     "Outs Above Average (OAA)"),
    calc_yoy_rel(fld_yr,  "drs",      "inn_tot", 300, "Defense",     "Defensive Runs Saved (DRS)"),
    calc_yoy_rel(fld_yr,  "uzr_150",  "inn_tot", 300, "Defense",     "UZR / 150 Games")
  ))
  print(eda_rel[, .(domain_group, metric_label, n_pairs, yoy_r)])

  # ----------------------------------------------------------------------------
  # 3. ERA-NEUTRALIZED SEASONAL Z-SCORES & ARCHETYPE TAGGING (1960-2025)
  # ----------------------------------------------------------------------------
  cat("\n-> [3/6] Computing era-neutralized seasonal z-scores & archetype classifications...\n")

  lg_bat <- bat_raw[, .(
    lg_woba  = w_mean_safe(woba, pa),    sd_woba  = w_sd_safe(woba, pa),
    lg_k     = w_mean_safe(k_pct, pa),   sd_k     = w_sd_safe(k_pct, pa),
    lg_bb    = w_mean_safe(bb_pct, pa),  sd_bb    = w_sd_safe(bb_pct, pa),
    lg_iso   = w_mean_safe(iso, ab),     sd_iso   = w_sd_safe(iso, ab),
    lg_babip = w_mean_safe(babip, pmax(ab - so - hr + sf, 0.0)), sd_babip = w_sd_safe(babip, pmax(ab - so - hr + sf, 0.0)),
    lg_sb    = w_mean_safe(sb_rate, pa), sd_sb    = w_sd_safe(sb_rate, pa)
  ), by = game_year]
  bat_raw <- merge(bat_raw, lg_bat, by = "game_year", all.x = TRUE)
  bat_raw[, `:=`(
    z_woba = (woba - lg_woba) / sd_woba, z_k = (k_pct - lg_k) / sd_k,
    z_bb = (bb_pct - lg_bb) / sd_bb, z_iso = (iso - lg_iso) / sd_iso,
    z_babip = (babip - lg_babip) / sd_babip, z_sb = (sb_rate - lg_sb) / sd_sb
  )]

  lg_pitch <- pitch_raw[, .(
    lg_fip = w_mean_safe(fip, ip_true), sd_fip = w_sd_safe(fip, ip_true),
    lg_k   = sum(so, na.rm = TRUE) / pmax(sum(tbf, na.rm = TRUE), 1.0), sd_k = w_sd_safe(k_pct, tbf),
    lg_bb  = sum(bb, na.rm = TRUE) / pmax(sum(tbf, na.rm = TRUE), 1.0), sd_bb = w_sd_safe(bb_pct, tbf),
    lg_hr9 = 9.0 * sum(hr, na.rm = TRUE) / pmax(sum(ip_true, na.rm = TRUE), 1.0), sd_hr9 = w_sd_safe(hr9, ip_true)
  ), by = .(game_year, role)]
  pitch_raw <- merge(pitch_raw, lg_pitch, by = c("game_year", "role"), all.x = TRUE)
  pitch_raw[, `:=`(
    z_fip = (fip - lg_fip) / sd_fip, z_k = (k_pct - lg_k) / sd_k,
    z_bb = (bb_pct - lg_bb) / sd_bb, z_hr9 = (hr9 - lg_hr9) / sd_hr9,
    archetype = fifelse(role == "RP", "RP", fifelse(k_pct >= lg_k, "SP_Power", "SP_Finesse"))
  )]

  setorder(bat_raw, mlbam, game_year)
  # Vectorized 4-year 8/5/4/3 regressed baseline for EVERY historical candidate season (symmetric with query!)
  bat_raw[, `:=`(
    y1 = shift(game_year, 1L), pa1 = shift(pa, 1L), ex1 = shift(pa * (woba - lg_woba), 1L),
    y2 = shift(game_year, 2L), pa2 = shift(pa, 2L), ex2 = shift(pa * (woba - lg_woba), 2L),
    y3 = shift(game_year, 3L), pa3 = shift(pa, 3L), ex3 = shift(pa * (woba - lg_woba), 3L),
    next_yr = shift(game_year, -1L), next_pa = shift(pa, -1L),
    next_woba = shift(woba, -1L), next_lg_w = shift(lg_woba, -1L)
  ), by = mlbam]
  bat_raw[, `:=`(
    w_ex_4yr = 8.0 * pa * (woba - lg_woba) +
               fifelse(!is.na(y1) & (game_year - y1) <= 3L, fifelse(game_year - y1 == 1L, 5.0, fifelse(game_year - y1 == 2L, 4.0, 3.0)) * ex1, 0.0) +
               fifelse(!is.na(y2) & (game_year - y2) <= 3L, fifelse(game_year - y2 == 2L, 4.0, 3.0) * ex2, 0.0) +
               fifelse(!is.na(y3) & (game_year - y3) == 3L, 3.0 * ex3, 0.0),
    w_pa_4yr = 8.0 * pa +
               fifelse(!is.na(y1) & (game_year - y1) <= 3L, fifelse(game_year - y1 == 1L, 5.0, fifelse(game_year - y1 == 2L, 4.0, 3.0)) * pa1, 0.0) +
               fifelse(!is.na(y2) & (game_year - y2) <= 3L, fifelse(game_year - y2 == 2L, 4.0, 3.0) * pa2, 0.0) +
               fifelse(!is.na(y3) & (game_year - y3) == 3L, 3.0 * pa3, 0.0)
  )]
  bat_raw[, `:=`(
    base_woba_hist  = lg_woba + w_ex_4yr / (w_pa_4yr + 2400.0),
    surv_next       = as.integer(!is.na(next_yr) & next_yr == game_year + 1L & next_pa >= 50),
    next_pa_clean   = fifelse(!is.na(next_yr) & next_yr == game_year + 1L, next_pa, 0.0)
  )]
  bat_raw[, delta_woba_next := fifelse(surv_next == 1L, (next_woba - next_lg_w) - (base_woba_hist - lg_woba), NA_real_)]

  setorder(pitch_raw, mlbam, game_year)
  pitch_raw[, `:=`(
    y1 = shift(game_year, 1L), ip1 = shift(ip_true, 1L), ex1 = shift(ip_true * (fip - lg_fip), 1L),
    y2 = shift(game_year, 2L), ip2 = shift(ip_true, 2L), ex2 = shift(ip_true * (fip - lg_fip), 2L),
    y3 = shift(game_year, 3L), ip3 = shift(ip_true, 3L), ex3 = shift(ip_true * (fip - lg_fip), 3L),
    next_yr = shift(game_year, -1L), next_tbf = shift(tbf, -1L),
    next_fip = shift(fip, -1L), next_lg_f = shift(lg_fip, -1L)
  ), by = mlbam]
  pitch_raw[, `:=`(
    w_ex_4yr = 8.0 * ip_true * (fip - lg_fip) +
               fifelse(!is.na(y1) & (game_year - y1) <= 3L, fifelse(game_year - y1 == 1L, 5.0, fifelse(game_year - y1 == 2L, 4.0, 3.0)) * ex1, 0.0) +
               fifelse(!is.na(y2) & (game_year - y2) <= 3L, fifelse(game_year - y2 == 2L, 4.0, 3.0) * ex2, 0.0) +
               fifelse(!is.na(y3) & (game_year - y3) == 3L, 3.0 * ex3, 0.0),
    w_ip_4yr = 8.0 * ip_true +
               fifelse(!is.na(y1) & (game_year - y1) <= 3L, fifelse(game_year - y1 == 1L, 5.0, fifelse(game_year - y1 == 2L, 4.0, 3.0)) * ip1, 0.0) +
               fifelse(!is.na(y2) & (game_year - y2) <= 3L, fifelse(game_year - y2 == 2L, 4.0, 3.0) * ip2, 0.0) +
               fifelse(!is.na(y3) & (game_year - y3) == 3L, 3.0 * ip3, 0.0)
  )]
  pitch_raw[, `:=`(
    base_fip_hist  = lg_fip + w_ex_4yr / (w_ip_4yr + 360.0),
    surv_next      = as.integer(!is.na(next_yr) & next_yr == game_year + 1L & next_tbf >= 50),
    next_tbf_clean = fifelse(!is.na(next_yr) & next_yr == game_year + 1L, next_tbf, 0.0)
  )]
  pitch_raw[, delta_fip_next := fifelse(surv_next == 1L, (next_fip - next_lg_f) - (base_fip_hist - lg_fip), NA_real_)]

  # ----------------------------------------------------------------------------
  # 4. CHRONOLOGICAL ZiPS ARCHETYPE (8/5/4/3) & PECOTA k-NN SIMILARITY ENGINE
  # ----------------------------------------------------------------------------
  cat("\n-> [4/6] Running strictly chronological ZiPS (8/5/4/3) & PECOTA k-NN across eval years...\n")

  exp04_bat   <- as.data.table(arrow::read_parquet(file.path(processed_dir, "batter_projections_true_talent.parquet")))
  exp04_pitch <- as.data.table(arrow::read_parquet(file.path(processed_dir, "pitcher_projections_true_talent.parquet")))

  bat_ens_list <- list(); pitch_ens_list <- list()
  bat_comps_2026 <- NULL; pit_comps_2026 <- NULL
  zips_5yr_bat   <- NULL; zips_5yr_pit   <- NULL

  build_b_mat <- function(dt, is_query = FALSE) {
    zw <- if (is_query) dt$z_woba_t1  else dt$z_woba
    zk <- if (is_query) dt$z_k_t1     else dt$z_k
    zb <- if (is_query) dt$z_bb_t1    else dt$z_bb
    zi <- if (is_query) dt$z_iso_t1   else dt$z_iso
    zp <- if (is_query) dt$z_babip_t1 else dt$z_babip
    zs <- if (is_query) dt$z_sb_t1    else dt$z_sb
    ag <- if (is_query) (dt$age_proj - 1.0) else dt$age
    cbind(
      1.40 * (ag - 27.5) / 3.5,
      1.20 * fifelse(is.finite(zw), zw, 0.0),
      1.00 * fifelse(is.finite(zk), zk, 0.0),
      1.00 * fifelse(is.finite(zb), zb, 0.0),
      1.10 * fifelse(is.finite(zi), zi, 0.0),
      0.60 * fifelse(is.finite(zp), zp, 0.0),
      0.70 * fifelse(is.finite(zs), zs, 0.0),
      0.35 * (fifelse(is.finite(dt$bmi), dt$bmi, 26.5) - 26.5) / 2.5,
      0.90 * as.numeric(dt$pos_group == "C"),
      0.70 * as.numeric(dt$pos_group == "Middle"),
      0.35 * as.numeric(dt$bats == "L")
    )
  }

  build_p_mat <- function(dt, is_query = FALSE) {
    zf <- if (is_query) dt$z_fip_t1 else dt$z_fip
    zk <- if (is_query) dt$z_k_t1   else dt$z_k
    zb <- if (is_query) dt$z_bb_t1  else dt$z_bb
    zh <- if (is_query) dt$z_hr9_t1 else dt$z_hr9
    ag <- if (is_query) (dt$age_proj - 1.0) else dt$age
    cbind(
      1.40 * (ag - 28.0) / 3.5,
      1.10 * fifelse(is.finite(zf), zf, 0.0),
      1.25 * fifelse(is.finite(zk), zk, 0.0),
      1.10 * fifelse(is.finite(zb), zb, 0.0),
      0.75 * fifelse(is.finite(zh), zh, 0.0),
      0.35 * (fifelse(is.finite(dt$bmi), dt$bmi, 26.5) - 26.5) / 2.5,
      1.20 * as.numeric(dt$role == "SP"),
      0.35 * as.numeric(dt$throws == "L")
    )
  }

  for (yr in eval_years) {
    # --- A. BATTER ZiPS (8/5/4/3 + Position-Group Aging) ---
    b_hist <- bat_raw[game_year %in% (yr - 4L):(yr - 1L) & pa >= 1]
    b_hist[, lag_k := yr - game_year]
    b_hist[, w_z := fcase(lag_k == 1L, 8.0, lag_k == 2L, 5.0, lag_k == 3L, 4.0, default = 3.0)]
    lg_t1_b <- lg_bat[game_year == (yr - 1L)]

    ag_b_grp <- list()
    for (pg in c("C", "Middle", "Corner_DH")) {
      ag_b_grp[[pg]] <- fit_archetype_aging(bat_raw[pos_group == pg], "woba", "lg_woba", "pa", "mlbam", max_year = yr - 1L, min_exp = 100)
    }

    b_zips <- b_hist[, .(
      player_name = player_name[which.max(game_year)],
      pos_group   = pos_group[which.max(game_year)],
      bats        = bats[which.max(game_year)],
      age_proj    = age[which.max(game_year)] + (yr - max(game_year)),
      bmi         = bmi[which.max(game_year)],
      pa_t1       = sum(fifelse(lag_k == 1L, pa, 0.0)),
      ex_woba     = sum(w_z * pa * (woba - lg_woba), na.rm = TRUE),
      w_pa        = sum(w_z * pa, na.rm = TRUE),
      z_woba_t1   = w_mean_safe(z_woba, w_z * pa),
      z_k_t1      = w_mean_safe(z_k, w_z * pa),
      z_bb_t1     = w_mean_safe(z_bb, w_z * pa),
      z_iso_t1    = w_mean_safe(z_iso, w_z * pa),
      z_babip_t1  = w_mean_safe(z_babip, w_z * pa),
      z_sb_t1     = w_mean_safe(z_sb, w_z * pa)
    ), by = mlbam][pa_t1 >= 50 & is.finite(age_proj)]

    b_zips[, base_woba := lg_t1_b$lg_woba + ex_woba / (w_pa + 2400.0)]
    b_zips[, zips_age_delta := mapply(function(pg, a) ag_b_grp[[pg]]$cum_fn(a) - ag_b_grp[[pg]]$cum_fn(a - 1.0), pos_group, age_proj)]
    b_zips[, zips_age_delta := zips_age_delta - w_mean_safe(zips_age_delta, pa_t1)]
    b_zips[, zips_woba := base_woba + zips_age_delta]

    # --- B. BATTER PECOTA k-NN (Comps strictly <= yr - 2) ---
    b_cand <- bat_raw[game_year <= (yr - 2L) & ((game_year == 2020L & pa >= 55) | (game_year != 2020L & pa >= 150)) & is.finite(age) &
                        is.finite(z_woba) & is.finite(z_k) & is.finite(z_bb) & is.finite(z_iso) & is.finite(z_babip)]
    ret_c <- (yr == 2026L)
    pk_b <- pecota_knn_predict(
      Q = build_b_mat(b_zips, TRUE), C = build_b_mat(b_cand, FALSE),
      cand_delta_y = b_cand$delta_woba_next, cand_surv = b_cand$surv_next, cand_next_exp = b_cand$next_pa_clean,
      cand_ids = b_cand$mlbam, cand_names = b_cand$player_name, cand_years = b_cand$game_year, cand_ages = b_cand$age,
      q_base_y = b_zips$base_woba, q_ids = b_zips$mlbam,
      k = 50L, tau = 1.85, breakout_thresh = 0.030, collapse_thresh = -0.030,
      lower_is_better = FALSE, return_comps = ret_c
    )

    b_zips[, `:=`(
      projection_year = yr, pecota_woba = pk_b$pred_mean,
      pecota_p10 = pk_b$p10, pecota_p25 = pk_b$p25, pecota_p50 = pk_b$p50, pecota_p75 = pk_b$p75, pecota_p90 = pk_b$p90,
      survival_prob = pk_b$surv_prob, breakout_prob = pk_b$break_prob, collapse_prob = pk_b$coll_prob
    )]
    bat_ens_list[[as.character(yr)]] <- b_zips

    if (yr == 2026L) {
      bc <- pk_b$comps
      bc[, `:=`(
        batter_mlbam = b_zips$mlbam[query_idx], player_name = b_zips$player_name[query_idx],
        age_2026 = b_zips$age_proj[query_idx], pos_group = b_zips$pos_group[query_idx],
        pecota_p10 = round(b_zips$pecota_p10[query_idx], 4), pecota_p50 = round(b_zips$pecota_p50[query_idx], 4),
        pecota_p90 = round(b_zips$pecota_p90[query_idx], 4), breakout_prob = round(b_zips$breakout_prob[query_idx], 4),
        collapse_prob = round(b_zips$collapse_prob[query_idx], 4), survival_prob = round(b_zips$survival_prob[query_idx], 4),
        domain = "Batter"
      )]
      bat_comps_2026 <- bc

      b_curr <- copy(b_zips[, .(mlbam, player_name, pos_group, age_proj, pa_t1, curr_woba = zips_woba)])
      z5b_list <- vector("list", 5L)
      z5b_list[[1L]] <- b_curr[, .(mlbam, player_name, pos_group, domain = "Batter", horizon_year = 2026L, age_at_yr = age_proj, proj_woba = curr_woba)]
      for (h in 1L:4L) {
        inc <- mapply(function(pg, a0) ag_b_grp[[pg]]$cum_fn(a0 + h) - ag_b_grp[[pg]]$cum_fn(a0 + h - 1.0), b_curr$pos_group, b_curr$age_proj)
        inc_c <- inc - w_mean_safe(inc, b_curr$pa_t1)
        b_curr[, curr_woba := curr_woba + inc_c]
        z5b_list[[h + 1L]] <- b_curr[, .(mlbam, player_name, pos_group, domain = "Batter", horizon_year = 2026L + h, age_at_yr = age_proj + h, proj_woba = curr_woba)]
      }
      zips_5yr_bat <- rbindlist(z5b_list)
    }

    # --- C. PITCHER ZiPS (8/5/4/3 + Archetype Aging) ---
    p_hist <- pitch_raw[game_year %in% (yr - 4L):(yr - 1L) & tbf >= 1]
    p_hist[, lag_k := yr - game_year]
    p_hist[, w_z := fcase(lag_k == 1L, 8.0, lag_k == 2L, 5.0, lag_k == 3L, 4.0, default = 3.0)]
    lg_t1_p <- lg_pitch[game_year == (yr - 1L)]

    ag_p_grp <- list()
    for (arch in c("SP_Power", "SP_Finesse", "RP")) {
      ag_p_grp[[arch]] <- fit_archetype_aging(pitch_raw[archetype == arch], "fip", "lg_fip", "ip_true", "mlbam", max_year = yr - 1L, min_exp = 30)
    }

    p_zips <- p_hist[, .(
      player_name = player_name[which.max(game_year)],
      role        = role[which.max(game_year)],
      archetype   = archetype[which.max(game_year)],
      throws      = throws[which.max(game_year)],
      age_proj    = age[which.max(game_year)] + (yr - max(game_year)),
      bmi         = bmi[which.max(game_year)],
      tbf_t1      = sum(fifelse(lag_k == 1L, tbf, 0.0)),
      ip_t1       = sum(fifelse(lag_k == 1L, ip_true, 0.0)),
      ex_fip      = sum(w_z * ip_true * (fip - lg_fip), na.rm = TRUE),
      w_ip        = sum(w_z * ip_true, na.rm = TRUE),
      z_fip_t1    = w_mean_safe(z_fip, w_z * ip_true),
      z_k_t1      = w_mean_safe(z_k, w_z * tbf),
      z_bb_t1     = w_mean_safe(z_bb, w_z * tbf),
      z_hr9_t1    = w_mean_safe(z_hr9, w_z * ip_true)
    ), by = mlbam][tbf_t1 >= 50 & is.finite(age_proj)]

    p_zips <- merge(p_zips, lg_t1_p[, .(role, lg_fip_anchor = lg_fip)], by = "role", all.x = TRUE)
    p_zips[, base_fip := lg_fip_anchor + ex_fip / (w_ip + 360.0)]
    p_zips[, zips_age_delta := mapply(function(ar, a) ag_p_grp[[ar]]$cum_fn(a) - ag_p_grp[[ar]]$cum_fn(a - 1.0), archetype, age_proj)]
    p_zips[, zips_age_delta := zips_age_delta - w_mean_safe(zips_age_delta, ip_t1), by = role]
    p_zips[, zips_fip := base_fip + zips_age_delta]

    # --- D. PITCHER PECOTA k-NN (Comps strictly <= yr - 2) ---
    p_cand <- pitch_raw[game_year <= (yr - 2L) & ((game_year == 2020L & tbf >= 55) | (game_year != 2020L & tbf >= 150)) & is.finite(age) &
                          is.finite(z_fip) & is.finite(z_k) & is.finite(z_bb) & is.finite(z_hr9)]
    pk_p <- pecota_knn_predict(
      Q = build_p_mat(p_zips, TRUE), C = build_p_mat(p_cand, FALSE),
      cand_delta_y = p_cand$delta_fip_next, cand_surv = p_cand$surv_next, cand_next_exp = p_cand$next_tbf_clean,
      cand_ids = p_cand$mlbam, cand_names = p_cand$player_name, cand_years = p_cand$game_year, cand_ages = p_cand$age,
      q_base_y = p_zips$base_fip, q_ids = p_zips$mlbam,
      k = 50L, tau = 1.85, breakout_thresh = -0.50, collapse_thresh = 0.50,
      lower_is_better = TRUE, return_comps = ret_c
    )

    p_zips[, `:=`(
      projection_year = yr, pecota_fip = pk_p$pred_mean,
      pecota_p10 = pk_p$p10, pecota_p25 = pk_p$p25, pecota_p50 = pk_p$p50, pecota_p75 = pk_p$p75, pecota_p90 = pk_p$p90,
      survival_prob = pk_p$surv_prob, breakout_prob = pk_p$break_prob, collapse_prob = pk_p$coll_prob
    )]
    pitch_ens_list[[as.character(yr)]] <- p_zips

    if (yr == 2026L) {
      pc <- pk_p$comps
      pc[, `:=`(
        pitcher_mlbam = p_zips$mlbam[query_idx], player_name = p_zips$player_name[query_idx],
        age_2026 = p_zips$age_proj[query_idx], archetype = p_zips$archetype[query_idx],
        pecota_p10 = round(p_zips$pecota_p10[query_idx], 3), pecota_p50 = round(p_zips$pecota_p50[query_idx], 3),
        pecota_p90 = round(p_zips$pecota_p90[query_idx], 3), breakout_prob = round(p_zips$breakout_prob[query_idx], 4),
        collapse_prob = round(p_zips$collapse_prob[query_idx], 4), survival_prob = round(p_zips$survival_prob[query_idx], 4),
        domain = "Pitcher"
      )]
      pit_comps_2026 <- pc

      p_curr <- copy(p_zips[, .(mlbam, player_name, role, archetype, age_proj, ip_t1, curr_fip = zips_fip)])
      z5p_list <- vector("list", 5L)
      z5p_list[[1L]] <- p_curr[, .(mlbam, player_name, archetype, domain = "Pitcher", horizon_year = 2026L, age_at_yr = age_proj, proj_fip = curr_fip)]
      for (h in 1L:4L) {
        p_curr[, inc := mapply(function(ar, a0) ag_p_grp[[ar]]$cum_fn(a0 + h) - ag_p_grp[[ar]]$cum_fn(a0 + h - 1.0), archetype, age_proj)]
        p_curr[, inc_c := inc - w_mean_safe(inc, ip_t1), by = role]
        p_curr[, curr_fip := curr_fip + inc_c]
        z5p_list[[h + 1L]] <- p_curr[, .(mlbam, player_name, archetype, domain = "Pitcher", horizon_year = 2026L + h, age_at_yr = age_proj + h, proj_fip = curr_fip)]
      }
      zips_5yr_pit <- rbindlist(z5p_list)
    }
  }

  bat_all   <- rbindlist(bat_ens_list)
  pitch_all <- rbindlist(pitch_ens_list)

  # ----------------------------------------------------------------------------
  # 5. MERGE WITH EXP-004 & FIT STRICTLY CHRONOLOGICAL M5 CONVEX META-ENSEMBLE
  # ----------------------------------------------------------------------------
  cat("\n-> [5/6] Merging with EXP-004 Marcel/Statcast & fitting strictly chronological M5 Convex Meta-Ensemble...\n")

  bat_panel <- merge(
    exp04_bat[projection_year %in% eval_years, .(
      projection_year, mlbam = batter_mlbam, team_prev, proj_pa,
      b2_woba, m4_woba, proj_woba_exp04 = proj_woba, proj_war_exp04 = proj_war, actual_pa, actual_woba
    )],
    bat_all, by = c("projection_year", "mlbam"), all = FALSE
  )

  pitch_panel <- merge(
    exp04_pitch[projection_year %in% eval_years, .(
      projection_year, mlbam = pitcher_mlbam, team_prev, proj_ip, proj_tbf,
      b2_fip, m4_sc_fip, proj_fip_exp04 = proj_fip, proj_war_exp04 = proj_war, actual_ip, actual_tbf, actual_fip
    )],
    pitch_all, by = c("projection_year", "mlbam"), all = FALSE
  )

  bat_panel[, ensemble_woba := NA_real_]
  pitch_panel[, ensemble_fip := NA_real_]
  b_wts_2025 <- c(0.25, 0.20, 0.15, 0.40); p_wts_2025 <- c(0.25, 0.20, 0.15, 0.40)

  for (yr in eval_years) {
    tr_b <- bat_panel[projection_year < yr & is.finite(actual_woba) & actual_pa >= 100 &
                        is.finite(b2_woba) & is.finite(zips_woba) & is.finite(pecota_woba) & is.finite(m4_woba)]
    wb <- if (nrow(tr_b) >= 100L) {
      fit_convex_stack(as.matrix(tr_b[, .(b2_woba, zips_woba, pecota_woba, m4_woba)]), tr_b$actual_woba, tr_b$actual_pa)
    } else c(0.25, 0.20, 0.15, 0.40)
    if (yr == 2025L) b_wts_2025 <- wb

    idx_b <- which(bat_panel$projection_year == yr); sub_b <- bat_panel[idx_b]
    m4_use <- fifelse(is.finite(sub_b$m4_woba), sub_b$m4_woba, sub_b$b2_woba)
    bat_panel[idx_b, ensemble_woba := wb[1] * b2_woba + wb[2] * zips_woba + wb[3] * pecota_woba + wb[4] * m4_use]

    tr_p <- pitch_panel[projection_year < yr & is.finite(actual_fip) & actual_ip >= 30 &
                          is.finite(b2_fip) & is.finite(zips_fip) & is.finite(pecota_fip) & is.finite(m4_sc_fip)]
    wp <- if (nrow(tr_p) >= 100L) {
      fit_convex_stack(as.matrix(tr_p[, .(b2_fip, zips_fip, pecota_fip, m4_sc_fip)]), tr_p$actual_fip, tr_p$actual_ip)
    } else c(0.25, 0.20, 0.15, 0.40)
    if (yr == 2025L) p_wts_2025 <- wp

    idx_p <- which(pitch_panel$projection_year == yr); sub_p <- pitch_panel[idx_p]
    m4p_use <- fifelse(is.finite(sub_p$m4_sc_fip), sub_p$m4_sc_fip, sub_p$b2_fip)
    pitch_panel[idx_p, ensemble_fip := wp[1] * b2_fip + wp[2] * zips_fip + wp[3] * pecota_fip + wp[4] * m4p_use]
  }

  cat(sprintf("   2025 Locked OOS Convex Weights [Marcel, ZiPS, PECOTA, Statcast]:\n"))
  cat(sprintf("     Batter  wOBA: [%.3f, %.3f, %.3f, %.3f]\n", b_wts_2025[1], b_wts_2025[2], b_wts_2025[3], b_wts_2025[4]))
  cat(sprintf("     Pitcher FIP : [%.3f, %.3f, %.3f, %.3f]\n", p_wts_2025[1], p_wts_2025[2], p_wts_2025[3], p_wts_2025[4]))

  ev_b25 <- bat_panel[projection_year == 2025L & is.finite(actual_woba) & actual_pa >= 150 & is.finite(m4_woba)]
  ev_p25 <- pitch_panel[projection_year == 2025L & is.finite(actual_fip) & actual_ip >= 40 & is.finite(m4_sc_fip)]

  eval_row <- function(dt, domain, metric, model_name, pred_col, act_col, wt_col) {
    data.table(
      track = "Locked_OOS_2025", domain = domain, metric = metric, model = model_name, n_eval = nrow(dt),
      w_rmse = round(w_rmse_safe(dt[[pred_col]], dt[[act_col]], dt[[wt_col]]), 5),
      w_mae  = round(w_mae_safe(dt[[pred_col]], dt[[act_col]], dt[[wt_col]]), 5),
      w_r    = round(w_cor_safe(dt[[pred_col]], dt[[act_col]], dt[[wt_col]]), 4)
    )
  }

  shootout <- rbindlist(list(
    eval_row(ev_b25, "Batter",  "wOBA", "B2_Marcel",             "b2_woba",       "actual_woba", "actual_pa"),
    eval_row(ev_b25, "Batter",  "wOBA", "M_ZiPS_Archetype",      "zips_woba",     "actual_woba", "actual_pa"),
    eval_row(ev_b25, "Batter",  "wOBA", "M_PECOTA_kNN",          "pecota_woba",   "actual_woba", "actual_pa"),
    eval_row(ev_b25, "Batter",  "wOBA", "M4_Statcast_Physics",   "m4_woba",       "actual_woba", "actual_pa"),
    eval_row(ev_b25, "Batter",  "wOBA", "M5_Consensus_Ensemble", "ensemble_woba", "actual_woba", "actual_pa"),
    eval_row(ev_p25, "Pitcher", "FIP",  "B2_Marcel",             "b2_fip",        "actual_fip",  "actual_ip"),
    eval_row(ev_p25, "Pitcher", "FIP",  "M_ZiPS_Archetype",      "zips_fip",      "actual_fip",  "actual_ip"),
    eval_row(ev_p25, "Pitcher", "FIP",  "M_PECOTA_kNN",          "pecota_fip",    "actual_fip",  "actual_ip"),
    eval_row(ev_p25, "Pitcher", "FIP",  "M4_Statcast_Physics",   "m4_sc_fip",     "actual_fip",  "actual_ip"),
    eval_row(ev_p25, "Pitcher", "FIP",  "M5_Consensus_Ensemble", "ensemble_fip",  "actual_fip",  "actual_ip")
  ))

  cat("\n=== 5-PARADIGM LOCKED 2024 -> 2025 OUT-OF-SAMPLE SHOOTOUT ===\n")
  print(shootout)

  cov_b80 <- mean(ev_b25$actual_woba >= ev_b25$pecota_p10 & ev_b25$actual_woba <= ev_b25$pecota_p90, na.rm = TRUE)
  cov_p80 <- mean(ev_p25$actual_fip  >= ev_p25$pecota_p10 & ev_p25$actual_fip  <= ev_p25$pecota_p90, na.rm = TRUE)
  cat(sprintf("\n   PECOTA [p10, p90] 80%% Empirical Coverage (2025 OOS): Batter wOBA = %.1f%% | Pitcher FIP = %.1f%%\n",
              100 * cov_b80, 100 * cov_p80))

  assert_true(nrow(ev_b25) >= 250L && nrow(ev_p25) >= 250L, "Too few 2025 OOS evaluation rows")
  assert_true(all(is.finite(shootout$w_rmse)), "Non-finite RMSE in shootout")
  assert_true(all(bat_panel$pecota_p10 <= bat_panel$pecota_p50 + 1e-6 & bat_panel$pecota_p50 <= bat_panel$pecota_p90 + 1e-6, na.rm = TRUE),
              "PECOTA batter percentiles violated monotonicity")
  assert_true(all(pitch_panel$pecota_p10 <= pitch_panel$pecota_p50 + 1e-6 & pitch_panel$pecota_p50 <= pitch_panel$pecota_p90 + 1e-6, na.rm = TRUE),
              "PECOTA pitcher percentiles violated monotonicity")

  cat("\n=== MARQUEE 2026 PECOTA HISTORICAL COMPS (TOP 3) ===\n")
  stars_b <- c("Aaron Judge", "Bobby Witt Jr.", "Shohei Ohtani", "Juan Soto")
  print(bat_comps_2026[player_name %in% stars_b & comp_rank <= 3L,
                       .(player_name, age_2026, comp_rank, comp_name, comp_year, comp_age, similarity)])

  if (smoke) {
    cat(sprintf("\n=== EXP-007 SMOKE TEST PASSED CLEANLY IN %.1f SECONDS ===\n",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    return(invisible(TRUE))
  }

  # ----------------------------------------------------------------------------
  # 6. FULL-LAKE GOLD PARQUET MATERIALIZATION, 4-ASSET VISUALS & REGISTRY LOG
  # ----------------------------------------------------------------------------
  cat("\n-> [6/6] Writing Gold ADR-001 ZSTD Parquets & 4-Asset Dark Cyber Visuals...\n")

  comps_all_2026 <- rbindlist(list(
    bat_comps_2026[, .(domain, mlbam = batter_mlbam, player_name, age_2026, group = pos_group,
                       comp_rank, comp_name, comp_year, comp_age, similarity, comp_delta, comp_surv,
                       pecota_p10, pecota_p50, pecota_p90, breakout_prob, collapse_prob, survival_prob)],
    pit_comps_2026[, .(domain, mlbam = pitcher_mlbam, player_name, age_2026, group = archetype,
                       comp_rank, comp_name, comp_year, comp_age, similarity, comp_delta, comp_surv,
                       pecota_p10, pecota_p50, pecota_p90, breakout_prob, collapse_prob, survival_prob)]
  ))

  trajs_all <- rbindlist(list(
    zips_5yr_bat[, .(domain, mlbam, player_name, group = pos_group, horizon_year, age_at_yr, proj_woba, proj_fip = NA_real_)],
    zips_5yr_pit[, .(domain, mlbam, player_name, group = archetype, horizon_year, age_at_yr, proj_woba = NA_real_, proj_fip)]
  ))

  ens_all <- rbindlist(list(
    bat_panel[, .(domain = "Batter", projection_year, mlbam, player_name, team_prev, group = pos_group, age_proj,
                  proj_exp = proj_pa, b2_pred = b2_woba, zips_pred = zips_woba, pecota_pred = pecota_woba,
                  m4_pred = m4_woba, ensemble_pred = ensemble_woba,
                  pecota_p10, pecota_p25, pecota_p50, pecota_p75, pecota_p90,
                  survival_prob, breakout_prob, collapse_prob, actual_exp = actual_pa, actual_val = actual_woba)],
    pitch_panel[, .(domain = "Pitcher", projection_year, mlbam, player_name, team_prev, group = archetype, age_proj,
                    proj_exp = proj_ip, b2_pred = b2_fip, zips_pred = zips_fip, pecota_pred = pecota_fip,
                    m4_pred = m4_sc_fip, ensemble_pred = ensemble_fip,
                    pecota_p10, pecota_p25, pecota_p50, pecota_p75, pecota_p90,
                    survival_prob, breakout_prob, collapse_prob, actual_exp = actual_ip, actual_val = actual_fip)]
  ))
  setorder(ens_all, domain, projection_year, mlbam)

  arrow::write_parquet(eda_rel,        file.path(processed_dir, "defense_baserunning_eda_2002_2025.parquet"), compression = "zstd")
  arrow::write_parquet(comps_all_2026, file.path(processed_dir, "pecota_knn_comps_2026.parquet"),            compression = "zstd")
  arrow::write_parquet(trajs_all,      file.path(processed_dir, "zips_5yr_trajectories_2026_2030.parquet"),  compression = "zstd")
  arrow::write_parquet(ens_all,        file.path(processed_dir, "ensemble_projections_2016_2026.parquet"),   compression = "zstd")
  arrow::write_parquet(shootout,       file.path(processed_dir, "ensemble_backtest_metrics.parquet"),        compression = "zstd")

  p1 <- make_pz01_eda(eda_rel)
  mlbr::save_plot_assets(p1, "pz01_defense_baserunning_stabilization_aging", data = eda_rel, width = 11.5, height = 6.8, dir = models_dir)
  file.copy(file.path(models_dir, "pz01_defense_baserunning_stabilization_aging.png"),
            file.path(assets_dir, "pz01_defense_baserunning_stabilization_aging.png"), overwrite = TRUE)

  b26_viz <- bat_panel[projection_year == 2026L & pa_t1 >= 250]
  top1_c <- bat_comps_2026[comp_rank == 1L, .(mlbam = batter_mlbam, top_comp_label = sprintf("%s (%d, Sim %.1f)", comp_name, comp_year, similarity))]
  b26_viz <- merge(b26_viz, top1_c, by = "mlbam", all.x = TRUE)
  setorder(b26_viz, -ensemble_woba)
  b26_top20 <- head(b26_viz, 20L)
  b26_top20[, player_label := factor(sprintf("%s (%d)", player_name, as.integer(age_proj)),
                                     levels = rev(sprintf("%s (%d)", player_name, as.integer(age_proj))))]

  p2_s <- make_pz02_pecota_static(b26_top20)
  mlbr::save_plot_assets(p2_s, "pz02_pecota_knn_percentile_fan", data = b26_top20, width = 12.0, height = 7.4, dir = models_dir)
  file.copy(file.path(models_dir, "pz02_pecota_knn_percentile_fan.png"),
            file.path(assets_dir, "pz02_pecota_knn_percentile_fan.png"), overwrite = TRUE)

  p2_ly <- make_pz02_pecota_plotly(b26_viz)
  htmlwidgets::saveWidget(p2_ly, file.path(models_dir, "pz02_pecota_knn_percentile_fan.html"), selfcontained = TRUE)
  dep_dir <- file.path(models_dir, "pz02_pecota_knn_percentile_fan_files")
  if (dir.exists(dep_dir)) unlink(dep_dir, recursive = TRUE)
  file.copy(file.path(models_dir, "pz02_pecota_knn_percentile_fan.html"),
            file.path(assets_dir, "pz02_pecota_knn_percentile_fan.html"), overwrite = TRUE)

  marquee <- c("Aaron Judge", "Shohei Ohtani", "Juan Soto", "Bobby Witt Jr.", "Gunnar Henderson", "Cal Raleigh")
  z5_viz <- zips_5yr_bat[player_name %in% marquee]
  z5_age <- z5_viz[horizon_year == 2026L, .(mlbam, a26 = as.integer(age_at_yr))]
  z5_viz <- merge(z5_viz, z5_age, by = "mlbam", all.x = TRUE)
  z5_viz[, player_label := sprintf("%s (Age %d, %s)", player_name, a26, pos_group)]
  p3 <- make_pz03_zips_5yr(z5_viz)
  mlbr::save_plot_assets(p3, "pz03_zips_5yr_career_trajectories", data = z5_viz, width = 11.5, height = 6.8, dir = models_dir)
  file.copy(file.path(models_dir, "pz03_zips_5yr_career_trajectories.png"),
            file.path(assets_dir, "pz03_zips_5yr_career_trajectories.png"), overwrite = TRUE)

  sh_viz <- copy(shootout)
  sh_viz[, panel_title := fifelse(domain == "Batter", "Batter wOBA (2025 OOS RMSE)", "Pitcher FIP (2025 OOS RMSE)")]
  sh_viz[, model_label := fcase(
    model == "B2_Marcel",             "Marcel (B2)",
    model == "M_ZiPS_Archetype",      "ZiPS Archetype",
    model == "M_PECOTA_kNN",          "PECOTA k-NN",
    model == "M4_Statcast_Physics",   "Statcast Physics (M4)",
    default = "M5 Consensus Ensemble"
  )]
  sh_viz[, is_best := (w_rmse == min(w_rmse)), by = domain]
  p4 <- make_pz04_shootout(sh_viz)
  mlbr::save_plot_assets(p4, "pz04_five_paradigm_oos_shootout", data = sh_viz, width = 12.0, height = 6.4, dir = models_dir)
  file.copy(file.path(models_dir, "pz04_five_paradigm_oos_shootout.png"),
            file.path(assets_dir, "pz04_five_paradigm_oos_shootout.png"), overwrite = TRUE)

  rds_files <- Sys.glob(file.path(models_dir, "pz0*.rds"))
  for (rf in rds_files) {
    sz <- file.info(rf)$size / 1e6
    cat(sprintf("   Saved %s: %.2f MB (Ceiling < 5.0 MB)\n", basename(rf), sz))
    assert_true(sz < 5.0, sprintf("Oversized RDS: %s (%.2f MB)", basename(rf), sz))
  }

  get_s <- function(dom, mod) shootout[domain == dom & model == mod]$w_rmse[1]
  mlbr::lake_log_experiment(
    id = "EXP-007-PECOTA-ZIPS-ENSEMBLE",
    title = "PECOTA k-NN Similarity Comps, ZiPS Archetype Multi-Year Aging, Defense/Baserunning EDA & 5-Paradigm Consensus Ensemble",
    tier = 2L, status = "VALIDATED",
    summary = paste0(
      "Completed Level 2B projection gamut across the full 1960-2025 historical lake (60,591 player-seasons) and 2002-2025 ",
      "fielding/baserunning EDA (166,850 fielder-seasons). Implemented ZiPS 8/5/4/3 archetype-specific aging & 5-year (2026-2030) ",
      "career trajectories, vectorized PECOTA k-NN era-neutralized similarity comps (k = 50, p10-p90 percentiles, breakout/collapse/survival ",
      "probabilities), and a strictly chronological M5 Consensus Meta-Ensemble uniting Marcel, ZiPS, PECOTA, and Statcast Physics."
    ),
    hypotheses = c(
      "Defensive (OAA/DRS/UZR) and baserunning (wBsR) skills exhibit markedly lower YoY reliability than plate discipline (K%, BB%), requiring heavier empirical shrinkage.",
      "Position- and role-archetype aging curves (ZiPS 8/5/4/3) capture distinct decay shapes across catchers, up-the-middle athletes, power starters, and relievers.",
      "Era-neutralized PECOTA k-NN similarity matching (1960-2025) yields well-calibrated non-parametric 10th-90th percentile uncertainty fans and explicit attrition/survival probabilities.",
      "A strictly chronological convex meta-ensemble (M5) combining Marcel, ZiPS, PECOTA, and Statcast Physics matches or outperforms every individual projection paradigm out of sample."
    ),
    formulas = list(
      era_neutral_zscore = "z_{i,s} = (x_{i,s} - mu_{s,g}) / sd_{s,g}",
      pecota_gaussian_w  = "w_j = exp(-0.5 * (d_{ij} / tau)^2), top k = 50 comps with season_j <= T - 2",
      zips_4yr_weights   = "8/5/4/3 recency weighting + archetype midpoint Delta-Method spline aging",
      convex_meta_stack  = "M5 = sum_m w_m * M_m, w_m >= 0, sum(w_m) = 1 (fit strictly on <= T - 1)"
    ),
    features = c("z_woba", "z_k", "z_bb", "z_iso", "z_babip", "z_sb", "z_fip", "z_hr9", "bmi", "pos_group", "archetype", "oaa", "drs", "uzr_150", "bsr_runs"),
    metrics = list(
      batter_2025_marcel_woba_rmse = get_s("Batter", "B2_Marcel"),
      batter_2025_zips_woba_rmse   = get_s("Batter", "M_ZiPS_Archetype"),
      batter_2025_pecota_woba_rmse = get_s("Batter", "M_PECOTA_kNN"),
      batter_2025_m4_woba_rmse     = get_s("Batter", "M4_Statcast_Physics"),
      batter_2025_m5_ens_woba_rmse = get_s("Batter", "M5_Consensus_Ensemble"),
      pitcher_2025_marcel_fip_rmse = get_s("Pitcher", "B2_Marcel"),
      pitcher_2025_zips_fip_rmse   = get_s("Pitcher", "M_ZiPS_Archetype"),
      pitcher_2025_pecota_fip_rmse = get_s("Pitcher", "M_PECOTA_kNN"),
      pitcher_2025_m4_fip_rmse     = get_s("Pitcher", "M4_Statcast_Physics"),
      pitcher_2025_m5_ens_fip_rmse = get_s("Pitcher", "M5_Consensus_Ensemble"),
      pecota_batter_80ci_coverage  = round(cov_b80, 4),
      pecota_pitcher_80ci_coverage = round(cov_p80, 4),
      yoy_r_oaa = eda_rel[metric_col == "oaa"]$yoy_r[1],
      yoy_r_drs = eda_rel[metric_col == "drs"]$yoy_r[1],
      yoy_r_bsr = eda_rel[metric_col == "bsr_runs"]$yoy_r[1]
    ),
    artifacts = list(
      spec = "specs/07_pecota_zips_ensemble.md", script = "models/07_pecota_zips_ensemble.R",
      gold_comps = "data/processed/pecota_knn_comps_2026.parquet",
      gold_zips  = "data/processed/zips_5yr_trajectories_2026_2030.parquet",
      gold_ens   = "data/processed/ensemble_projections_2016_2026.parquet",
      gold_eda   = "data/processed/defense_baserunning_eda_2002_2025.parquet"
    ),
    lake = lake
  )

  cat(sprintf("\n=== EXP-007 FULL-LAKE PIPELINE COMPLETED CLEANLY IN %.1f SECONDS ===\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  run_exp007(smoke = ("--smoke" %in% args))
}
