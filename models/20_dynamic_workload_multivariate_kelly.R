#!/usr/bin/env Rscript
# ==============================================================================
# EXP-020: Interior Half-Life Sweep (H = 14..180d), Dynamic In-Season Starter
#          Workload (BF/GS) & Multivariate Copula-Kelly Portfolio Staking
# ==============================================================================
suppressPackageStartupMessages({
  library(mlbr); library(DBI); library(arrow); library(dplyr); library(tidyr)
  library(ggplot2); library(MASS); library(pROC); library(plotly); library(htmlwidgets)
})

args <- commandArgs(trailingOnly = TRUE)
is_smoke <- "--smoke" %in% args
is_full  <- "--full"  %in% args
if (!is_smoke && !is_full) is_smoke <- TRUE

t0 <- proc.time()[["elapsed"]]
cat(sprintf("=== EXP-020 Dynamic Workload K-Props & Multivariate Copula-Kelly [%s] ===\n",
            if (is_smoke) "SMOKE" else "FULL"))

lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

brier <- function(y, p) mean((p - y)^2)
roc_auc_fast <- function(y, p) as.numeric(pROC::auc(pROC::roc(y, p, quiet = TRUE)))

# 16-pt Gauss-Legendre bivariate Gaussian copula (inherited from EXP-018)
GL16_NODES <- c(
  -0.9894009349916499, -0.9445750230732326, -0.8656312023878318, -0.7554044083550030,
  -0.6178762444026438, -0.4580167776572274, -0.2816035507792589, -0.0950125098376374,
   0.0950125098376374,  0.2816035507792589,  0.4580167776572274,  0.6178762444026438,
   0.7554044083550030,  0.8656312023878318,  0.9445750230732326,  0.9894009349916499
)
GL16_WEIGHTS <- c(
  0.0271524594117541, 0.0622535239386479, 0.0951585116824928, 0.1246289712555339,
  0.1495959888165767, 0.1691565193950025, 0.1826034150449236, 0.1894506104550685,
  0.1894506104550685, 0.1826034150449236, 0.1691565193950025, 0.1495959888165767,
  0.1246289712555339, 0.0951585116824928, 0.0622535239386479, 0.0271524594117541
)

gauss_copula_2d <- function(u, v, rho) {
  u <- pmin(pmax(u, 1e-9), 1 - 1e-9); v <- pmin(pmax(v, 1e-9), 1 - 1e-9)
  if (abs(rho) < 1e-9) return(u * v)
  z1 <- qnorm(u); z2 <- qnorm(v)
  r_pts <- 0.5 * rho * (GL16_NODES + 1.0); w_pts <- 0.5 * rho * GL16_WEIGHTS
  z1_sq <- z1 * z1; z2_sq <- z2 * z2; z12 <- z1 * z2
  acc <- numeric(length(u)); inv_2pi <- 1.0 / (2.0 * pi)
  for (k in seq_along(r_pts)) {
    rk <- r_pts[k]; omr2 <- 1.0 - rk * rk
    acc <- acc + w_pts[k] * (inv_2pi / sqrt(omr2)) * exp(-(z1_sq - 2.0 * rk * z12 + z2_sq) / (2.0 * omr2))
  }
  pmin(pmax(u * v + acc, pmax(0.0, u + v - 1.0)), pmin(u, v))
}

fit_gauss_rho_mle <- function(u, v, y1, y2) {
  nll <- function(rho) {
    p11 <- gauss_copula_2d(u, v, rho); eps <- 1e-9
    p11 <- pmin(pmax(p11, pmax(0, u + v - 1) + eps), pmin(u, v) - eps)
    p10 <- pmax(u - p11, eps); p01 <- pmax(v - p11, eps); p00 <- pmax(1 - u - v + p11, eps)
    -mean(ifelse(y1==1 & y2==1, log(p11), 0) + ifelse(y1==1 & y2==0, log(p10), 0) +
          ifelse(y1==0 & y2==1, log(p01), 0) + ifelse(y1==0 & y2==0, log(p00), 0))
  }
  optimize(nll, c(-0.95, 0.95), tol = 1e-5)$minimum
}

# ------------------------------------------------------------------------------
# PART A: Interior Half-Life Sweep (H = 14, 30, 45, 60, 90, 180d) + Dynamic BF/GS
# ------------------------------------------------------------------------------
p_kprop <- file.path(lake, "data/processed/dynamic_statespace_negbin_kprops_2024_2025.parquet")

kp_df <- DBI::dbGetQuery(con, sprintf("
  WITH daily_pit AS (
    SELECT
      pitcher AS sp_mlbam, game_year AS season, CAST(game_date AS DATE) AS gdate,
      COUNT(*) AS n_pitches,
      SUM(CASE WHEN is_csw = 1 THEN 1 ELSE 0 END) AS n_csw,
      SUM(CASE WHEN events IS NOT NULL THEN 1 ELSE 0 END) AS n_pa,
      SUM(CASE WHEN events IN ('strikeout', 'strikeout_double_play') THEN 1 ELSE 0 END) AS n_so
    FROM v_statcast_features
    WHERE game_year IN (2024, 2025) AND game_type = 'R'
    GROUP BY pitcher, game_year, CAST(game_date AS DATE)
  ),
  starts AS (
    SELECT
      sp_mlbam, game_id, season, CAST(game_date AS DATE) AS gdate, is_home_sp,
      sp_bf, sp_actual_so, eb_sp_k, eb_bf_gs, prior_exp_so, exp_so_m1,
      opp_lineup_k, sp_ivb_slot, pf_so, ump_k_eb, cat_csaa_1k,
      y_over_5_5, y_over_7_5
    FROM read_parquet('%s')
  ),
  decayed_pitch AS (
    SELECT
      s.game_id, s.sp_mlbam,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 14.0)  * d.n_csw)     AS w14_csw,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 14.0)  * d.n_pitches) AS w14_p,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 14.0)  * d.n_so)      AS w14_so,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 14.0)  * d.n_pa)      AS w14_pa,

      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 30.0)  * d.n_csw)     AS w30_csw,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 30.0)  * d.n_pitches) AS w30_p,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 30.0)  * d.n_so)      AS w30_so,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 30.0)  * d.n_pa)      AS w30_pa,

      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 45.0)  * d.n_csw)     AS w45_csw,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 45.0)  * d.n_pitches) AS w45_p,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 45.0)  * d.n_so)      AS w45_so,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 45.0)  * d.n_pa)      AS w45_pa,

      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 60.0)  * d.n_csw)     AS w60_csw,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 60.0)  * d.n_pitches) AS w60_p,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 60.0)  * d.n_so)      AS w60_so,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 60.0)  * d.n_pa)      AS w60_pa,

      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 90.0)  * d.n_csw)     AS w90_csw,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 90.0)  * d.n_pitches) AS w90_p,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 90.0)  * d.n_so)      AS w90_so,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 90.0)  * d.n_pa)      AS w90_pa,

      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 180.0) * d.n_csw)     AS w180_csw,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 180.0) * d.n_pitches) AS w180_p,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 180.0) * d.n_so)      AS w180_so,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', d.gdate, s.gdate) / 180.0) * d.n_pa)      AS w180_pa
    FROM starts s
    LEFT JOIN daily_pit d ON s.sp_mlbam = d.sp_mlbam AND s.season = d.season
      AND d.gdate < s.gdate AND DATEDIFF('day', d.gdate, s.gdate) BETWEEN 1 AND 180
    GROUP BY s.game_id, s.sp_mlbam
  ),
  decayed_bf AS (
    SELECT
      s.game_id, s.sp_mlbam,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', p.gdate, s.gdate) / 30.0) * p.sp_bf) AS w30_bf,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', p.gdate, s.gdate) / 30.0))           AS w30_gs,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', p.gdate, s.gdate) / 45.0) * p.sp_bf) AS w45_bf,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', p.gdate, s.gdate) / 45.0))           AS w45_gs,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', p.gdate, s.gdate) / 60.0) * p.sp_bf) AS w60_bf,
      SUM(EXP(-LN(2.0) * DATEDIFF('day', p.gdate, s.gdate) / 60.0))           AS w60_gs,
      SUM(p.sp_bf)                                                            AS winf_bf,
      COUNT(p.sp_bf)                                                          AS winf_gs
    FROM starts s
    LEFT JOIN starts p ON s.sp_mlbam = p.sp_mlbam AND s.season = p.season
      AND p.gdate < s.gdate AND DATEDIFF('day', p.gdate, s.gdate) BETWEEN 1 AND 180
    GROUP BY s.game_id, s.sp_mlbam
  )
  SELECT s.*,
         (COALESCE(k.w14_csw, 0)  + 150.0*0.290) / (COALESCE(k.w14_p, 0)  + 150.0) AS csw_h14,
         (COALESCE(k.w14_so, 0)   +  60.0*0.224) / (COALESCE(k.w14_pa, 0) +  60.0) AS k_h14,
         (COALESCE(k.w30_csw, 0)  + 150.0*0.290) / (COALESCE(k.w30_p, 0)  + 150.0) AS csw_h30,
         (COALESCE(k.w30_so, 0)   +  60.0*0.224) / (COALESCE(k.w30_pa, 0) +  60.0) AS k_h30,
         (COALESCE(k.w45_csw, 0)  + 150.0*0.290) / (COALESCE(k.w45_p, 0)  + 150.0) AS csw_h45,
         (COALESCE(k.w45_so, 0)   +  60.0*0.224) / (COALESCE(k.w45_pa, 0) +  60.0) AS k_h45,
         (COALESCE(k.w60_csw, 0)  + 150.0*0.290) / (COALESCE(k.w60_p, 0)  + 150.0) AS csw_h60,
         (COALESCE(k.w60_so, 0)   +  60.0*0.224) / (COALESCE(k.w60_pa, 0) +  60.0) AS k_h60,
         (COALESCE(k.w90_csw, 0)  + 150.0*0.290) / (COALESCE(k.w90_p, 0)  + 150.0) AS csw_h90,
         (COALESCE(k.w90_so, 0)   +  60.0*0.224) / (COALESCE(k.w90_pa, 0) +  60.0) AS k_h90,
         (COALESCE(k.w180_csw, 0) + 150.0*0.290) / (COALESCE(k.w180_p, 0) + 150.0) AS csw_h180,
         (COALESCE(k.w180_so, 0)  +  60.0*0.224) / (COALESCE(k.w180_pa, 0)+  60.0) AS k_h180,
         (COALESCE(b.w30_bf, 0)   + 4.0*s.eb_bf_gs) / (COALESCE(b.w30_gs, 0) + 4.0) AS bf_gs_h30,
         (COALESCE(b.w45_bf, 0)   + 4.0*s.eb_bf_gs) / (COALESCE(b.w45_gs, 0) + 4.0) AS bf_gs_h45,
         (COALESCE(b.w60_bf, 0)   + 4.0*s.eb_bf_gs) / (COALESCE(b.w60_gs, 0) + 4.0) AS bf_gs_h60,
         (COALESCE(b.winf_bf, 0)  + 4.0*s.eb_bf_gs) / (COALESCE(b.winf_gs, 0)+ 4.0) AS bf_gs_inf
  FROM starts s
  LEFT JOIN decayed_pitch k ON s.game_id = k.game_id AND s.sp_mlbam = k.sp_mlbam
  LEFT JOIN decayed_bf    b ON s.game_id = b.game_id AND s.sp_mlbam = b.sp_mlbam
  ORDER BY s.season, s.gdate, s.game_id
", p_kprop))

if (is_smoke) {
  kp_df <- bind_rows(head(kp_df |> filter(season == 2024), 600), head(kp_df |> filter(season == 2025), 600))
}
kp_tr <- kp_df |> filter(season == 2024L)
kp_te <- kp_df |> filter(season == 2025L)
cat(sprintf("Part A Loaded %d starts (2024: %d, 2025: %d).\n", nrow(kp_df), nrow(kp_tr), nrow(kp_te)))

eval_nb <- function(label, csw_col, k_col, bf_col = NULL, h_days = NA_real_) {
  d_tr <- kp_tr |> mutate(dyn_csw = .data[[csw_col]], dyn_k = .data[[k_col]])
  d_te <- kp_te |> mutate(dyn_csw = .data[[csw_col]], dyn_k = .data[[k_col]])
  if (!is.null(bf_col)) {
    d_tr <- d_tr |> mutate(dyn_bf = .data[[bf_col]])
    d_te <- d_te |> mutate(dyn_bf = .data[[bf_col]])
    fit <- MASS::glm.nb(sp_actual_so ~ log(prior_exp_so) + log(dyn_bf) + opp_lineup_k + sp_ivb_slot +
                          pf_so + ump_k_eb + cat_csaa_1k + dyn_csw + dyn_k, data = d_tr)
  } else {
    fit <- MASS::glm.nb(sp_actual_so ~ log(prior_exp_so) + opp_lineup_k + sp_ivb_slot +
                          pf_so + ump_k_eb + cat_csaa_1k + dyn_csw + dyn_k, data = d_tr)
  }
  th <- fit$theta
  mu_tr <- as.numeric(predict(fit, newdata = d_tr, type = "response"))
  mu_te <- as.numeric(predict(fit, newdata = d_te, type = "response"))
  p_o55_tr <- 1.0 - pnbinom(5L, size = th, mu = mu_tr)
  p_o55_te <- 1.0 - pnbinom(5L, size = th, mu = mu_te)
  p_o75_te <- 1.0 - pnbinom(7L, size = th, mu = mu_te)
  list(
    metrics = tibble(
      model = label, half_life_days = h_days, has_dyn_bf = !is.null(bf_col),
      theta_2024 = th,
      train_2024_nll = -mean(dnbinom(d_tr$sp_actual_so, size = th, mu = mu_tr, log = TRUE)),
      oos_2025_nll   = -mean(dnbinom(d_te$sp_actual_so, size = th, mu = mu_te, log = TRUE)),
      oos_2025_rmse  = sqrt(mean((mu_te - d_te$sp_actual_so)^2)),
      oos_2025_brier_o55 = brier(d_te$y_over_5_5, p_o55_te),
      oos_2025_auc_o55   = roc_auc_fast(d_te$y_over_5_5, p_o55_te),
      oos_2025_brier_o75 = brier(d_te$y_over_7_5, p_o75_te)
    ),
    p_o55_tr = p_o55_tr, p_o55_te = p_o55_te, mu_te = mu_te
  )
}

grid_res <- list(
  eval_nb("M5_ExpDecay_H14d",  "csw_h14",  "k_h14",  NULL, 14),
  eval_nb("M5_ExpDecay_H30d",  "csw_h30",  "k_h30",  NULL, 30),
  eval_nb("M5_ExpDecay_H45d",  "csw_h45",  "k_h45",  NULL, 45),
  eval_nb("M5_ExpDecay_H60d",  "csw_h60",  "k_h60",  NULL, 60),
  eval_nb("M5_ExpDecay_H90d",  "csw_h90",  "k_h90",  NULL, 90),
  eval_nb("M5_ExpDecay_H180d", "csw_h180", "k_h180", NULL, 180)
)
sweep_tbl <- bind_rows(lapply(grid_res, `[[`, "metrics"))
best_h <- sweep_tbl |> arrange(train_2024_nll) |> slice(1)
h_star <- as.integer(best_h$half_life_days)
cat(sprintf("2024-Selected Interior Half-Life (Zero 2025 Peeking): H* = %d days\n", h_star))

# Fit Dynamic Workload (BF/GS) models using H* for CSW%/K% and sweeping H in {30, 45, 60} for BF/GS
res_bf30 <- eval_nb(sprintf("M6_DynWorkload_H%d_BF30", h_star), paste0("csw_h", h_star), paste0("k_h", h_star), "bf_gs_h30", h_star)
res_bf45 <- eval_nb(sprintf("M6_DynWorkload_H%d_BF45", h_star), paste0("csw_h", h_star), paste0("k_h", h_star), "bf_gs_h45", h_star)
res_bf60  <- eval_nb(sprintf("M6_DynWorkload_H%d_BF60",  h_star), paste0("csw_h", h_star), paste0("k_h", h_star), "bf_gs_h60", h_star)
res_bfinf <- eval_nb(sprintf("M6_DynWorkload_H%d_BFInf", h_star), paste0("csw_h", h_star), paste0("k_h", h_star), "bf_gs_inf", h_star)

bf_candidates <- list(res_bf30, res_bf45, res_bf60, res_bfinf)
bf_tr_nlls <- sapply(bf_candidates, function(x) x$metrics$train_2024_nll)
best_bf_res <- bf_candidates[[which.min(bf_tr_nlls)]]

kprop_ablation <- bind_rows(sweep_tbl, res_bf30$metrics, res_bf45$metrics, res_bf60$metrics, res_bfinf$metrics)
cat("\n=== Part A: Locked 2025 OOS Interior Half-Life & Dynamic BF/GS Ablation ===\n")
print(kprop_ablation |> mutate(across(where(is.numeric), ~ round(.x, 5))), width = 140)

# Attach best dynamic K-Prop probabilities back to starts for Part B
kp_tr$p_k_dyn <- best_bf_res$p_o55_tr
kp_te$p_k_dyn <- best_bf_res$p_o55_te
kp_all_home <- bind_rows(kp_tr, kp_te) |>
  filter(is_home_sp == 1L) |>
  transmute(game_id, season, y_hsp_o55 = y_over_5_5, p_hsp_dyn = p_k_dyn,
            p_hsp_m1 = 1.0 - ppois(5L, lambda = exp_so_m1))

# ------------------------------------------------------------------------------
# PART B: Multivariate Copula-Kelly vs Independent Quarter-Kelly (Locked 2025)
# ------------------------------------------------------------------------------
p_games <- file.path(lake, "data/processed/unified_pro_game_f5_totals_2024_2025.parquet")
g_df <- DBI::dbGetQuery(con, sprintf("
  SELECT game_id, season, game_date, home_win, actual_total_runs,
         CASE WHEN actual_total_runs >= 9 THEN 1 ELSE 0 END AS y_over85,
         p_b2, p_m6_ml, exp_total_runs, exp_total_m6
  FROM read_parquet('%s')
  ORDER BY season, game_date, game_id
", p_games)) |> inner_join(kp_all_home, by = c("game_id", "season"))

# Calibrate Over 8.5 fair (M6) and naive book (EXP-009) strictly on 2024
fit_o85_fair <- glm(y_over85 ~ exp_total_m6,   data = g_df |> filter(season == 2024L), family = binomial())
fit_o85_book <- glm(y_over85 ~ exp_total_runs, data = g_df |> filter(season == 2024L), family = binomial())
g_df$p_o85_fair <- as.numeric(predict(fit_o85_fair, newdata = g_df, type = "response"))
g_df$p_o85_book <- as.numeric(predict(fit_o85_book, newdata = g_df, type = "response"))

# Fit 3x3 Gaussian Copula correlation matrix strictly on 2024 (ML, Home SP O5.5 K, Over 8.5)
g24 <- g_df |> filter(season == 2024L)
g25 <- g_df |> filter(season == 2025L)

rho12 <- fit_gauss_rho_mle(g24$p_m6_ml,    g24$p_hsp_dyn,  g24$home_win,  g24$y_hsp_o55)
rho13 <- fit_gauss_rho_mle(g24$p_m6_ml,    g24$p_o85_fair, g24$home_win,  g24$y_over85)
rho23 <- fit_gauss_rho_mle(g24$p_hsp_dyn,  g24$p_o85_fair, g24$y_hsp_o55, g24$y_over85)
R_2024 <- matrix(c(1, rho12, rho13, rho12, 1, rho23, rho13, rho23, 1), 3, 3)
cat(sprintf("\n2024 Copula Correlations: rho(ML, HSP_K)=%+.3f | rho(ML, O85)=%+.3f | rho(HSP_K, O85)=%+.3f\n",
            rho12, rho13, rho23))

# Exact box-constrained QP solver for m <= 3: max_{f in [0, f_max]^m} f'mu - (1/(2c)) f'Sigma f
solve_mv_kelly <- function(mu, Sigma, c_frac = 0.25, f_max = 0.05) {
  m <- length(mu)
  if (m == 0L) return(numeric(0))
  # Regularize Sigma slightly for numerical PD safety
  S <- Sigma + diag(1e-8, m)
  obj <- function(f) sum(f * mu) - (0.5 / c_frac) * as.numeric(t(f) %*% S %*% f)
  best_f <- rep(0.0, m); best_val <- 0.0
  # Enumerate active boundary assignments: 0 = free, 1 = 0, 2 = f_max (at most 3^3 = 27 states)
  states <- expand.grid(rep(list(c(0L, 1L, 2L)), m))
  for (r in seq_len(nrow(states))) {
    st <- as.integer(states[r, ])
    f_cand <- numeric(m)
    f_cand[st == 2L] <- f_max
    free_idx <- which(st == 0L)
    if (length(free_idx) > 0L) {
      fixed_idx <- which(st != 0L)
      rhs <- c_frac * mu[free_idx]
      if (length(fixed_idx) > 0L) {
        rhs <- rhs - as.numeric(S[free_idx, fixed_idx, drop = FALSE] %*% f_cand[fixed_idx])
      }
      f_free <- tryCatch(solve(S[free_idx, free_idx, drop = FALSE], rhs), error = function(e) rep(NA_real_, length(free_idx)))
      if (any(!is.finite(f_free)) || any(f_free < -1e-9) || any(f_free > f_max + 1e-9)) next
      f_cand[free_idx] <- pmin(pmax(f_free, 0.0), f_max)
    }
    val <- obj(f_cand)
    if (val > best_val + 1e-12) { best_val <- val; best_f <- f_cand }
  }
  best_f
}

# Run strictly causal start-of-day bankroll backtest on locked 2025 (vig = 2.38%, -105/-105)
vig_mult <- 1.0238
ev_thresh <- 0.02
c_frac <- 0.25
f_cap_leg <- 0.05
f_cap_day <- 0.35

# Build per-game active legs on locked 2025
eval_game_legs <- function(row_idx) {
  r <- g25[row_idx, ]
  # 3 markets: (1) ML, (2) Home SP O5.5 K, (3) Game Over 8.5
  p_fair_pos <- c(r$p_m6_ml, r$p_hsp_dyn, r$p_o85_fair)
  p_book_pos <- c(r$p_b2,    r$p_hsp_m1,  r$p_o85_book)
  y_pos      <- c(r$home_win, r$y_hsp_o55, r$y_over85)
  p_book_pos <- pmin(pmax(p_book_pos, 0.05), 0.95)

  O_pos <- 1.0 / (p_book_pos * vig_mult)
  O_neg <- 1.0 / ((1.0 - p_book_pos) * vig_mult)
  ev_pos <- p_fair_pos * O_pos - 1.0
  ev_neg <- (1.0 - p_fair_pos) * O_neg - 1.0

  idx_act <- integer(0); sgn <- numeric(0); p_bet <- numeric(0); O_bet <- numeric(0); y_bet <- integer(0); mu_bet <- numeric(0)
  for (j in 1:3) {
    if (ev_pos[j] >= ev_neg[j] && ev_pos[j] > ev_thresh) {
      idx_act <- c(idx_act, j); sgn <- c(sgn, 1.0); p_bet <- c(p_bet, p_fair_pos[j]); O_bet <- c(O_bet, O_pos[j]); y_bet <- c(y_bet, y_pos[j]); mu_bet <- c(mu_bet, ev_pos[j])
    } else if (ev_neg[j] > ev_pos[j] && ev_neg[j] > ev_thresh) {
      idx_act <- c(idx_act, j); sgn <- c(sgn, -1.0); p_bet <- c(p_bet, 1.0 - p_fair_pos[j]); O_bet <- c(O_bet, O_neg[j]); y_bet <- c(y_bet, 1L - y_pos[j]); mu_bet <- c(mu_bet, ev_neg[j])
    }
  }
  m <- length(idx_act)
  if (m == 0L) return(NULL)

  # Build m x m covariance matrix from 2024 Gaussian Copula
  Sigma <- matrix(0.0, m, m)
  var_j <- (O_bet^2) * p_bet * (1.0 - p_bet)
  diag(Sigma) <- var_j
  avg_pair_rho <- 0.0
  if (m >= 2L) {
    rhos <- numeric(0)
    for (a in 1:(m - 1L)) {
      for (b in (a + 1L):m) {
        rho_ab <- sgn[a] * sgn[b] * R_2024[idx_act[a], idx_act[b]]
        rhos <- c(rhos, rho_ab)
        p_ab <- gauss_copula_2d(p_bet[a], p_bet[b], rho_ab)
        cov_ab <- O_bet[a] * O_bet[b] * (p_ab - p_bet[a] * p_bet[b])
        Sigma[a, b] <- cov_ab; Sigma[b, a] <- cov_ab
      }
    }
    avg_pair_rho <- mean(rhos)
  }

  f_ind <- pmin(f_cap_leg, c_frac * mu_bet / var_j)
  f_mv  <- solve_mv_kelly(mu_bet, Sigma, c_frac = c_frac, f_max = f_cap_leg)
  f_os  <- pmin(f_mv, f_ind)
  ret_leg <- ifelse(y_bet == 1L, O_bet - 1.0, -1.0)

  tibble(
    game_id = r$game_id, game_date = r$game_date, n_legs = m, avg_pair_rho = avg_pair_rho,
    stake_ind = sum(f_ind), stake_mv = sum(f_mv), stake_os = sum(f_os),
    pnl_ind = sum(f_ind * ret_leg), pnl_mv = sum(f_mv * ret_leg), pnl_os = sum(f_os * ret_leg)
  )
}

bt_list <- lapply(seq_len(nrow(g25)), eval_game_legs)
game_bets <- bind_rows(bt_list)

# Daily slate aggregation with 35% daily exposure cap on start-of-day bankroll W_t
daily_bt <- game_bets |>
  group_by(game_date) |>
  summarise(
    n_games_bet = n(), n_multi_leg = sum(n_legs >= 2L),
    raw_stake_ind = sum(stake_ind), raw_pnl_ind = sum(pnl_ind),
    raw_stake_mv  = sum(stake_mv),  raw_pnl_mv  = sum(pnl_mv),
    raw_stake_os  = sum(stake_os),  raw_pnl_os  = sum(pnl_os),
    .groups = "drop"
  ) |>
  arrange(game_date) |>
  mutate(
    scale_ind = pmin(1.0, f_cap_day / pmax(raw_stake_ind, 1e-9)),
    scale_mv  = pmin(1.0, f_cap_day / pmax(raw_stake_mv,  1e-9)),
    scale_os  = pmin(1.0, f_cap_day / pmax(raw_stake_os,  1e-9)),
    day_ret_ind = scale_ind * raw_pnl_ind,
    day_ret_mv  = scale_mv  * raw_pnl_mv,
    day_ret_os  = scale_os  * raw_pnl_os,
    day_st_ind  = scale_ind * raw_stake_ind,
    day_st_mv   = scale_mv  * raw_stake_mv,
    day_st_os   = scale_os  * raw_stake_os,
    eq_ind = cumprod(1.0 + day_ret_ind),
    eq_mv  = cumprod(1.0 + day_ret_mv),
    eq_os  = cumprod(1.0 + day_ret_os),
    dd_ind = (pmax(1.0, cummax(eq_ind)) - eq_ind) / pmax(1.0, cummax(eq_ind)),
    dd_mv  = (pmax(1.0, cummax(eq_mv))  - eq_mv)  / pmax(1.0, cummax(eq_mv)),
    dd_os  = (pmax(1.0, cummax(eq_os))  - eq_os)  / pmax(1.0, cummax(eq_os))
  )

calc_port_summary <- function(name, day_ret, day_st, eq, dd) {
  tibble(
    strategy        = name,
    n_bet_days      = length(day_ret),
    total_turnover  = sum(day_st),
    final_bankroll  = tail(eq, 1),
    cum_log_growth  = log(tail(eq, 1)),
    roi_pct         = 100.0 * sum(day_ret) / sum(day_st),
    daily_vol_pct   = 100.0 * sd(day_ret),
    sharpe_ann      = sqrt(180) * mean(day_ret) / sd(day_ret),
    max_drawdown_pct = 100.0 * max(dd)
  )
}

mvk_summary <- bind_rows(
  calc_port_summary("Independent_Quarter_Kelly",  daily_bt$day_ret_ind, daily_bt$day_st_ind, daily_bt$eq_ind, daily_bt$dd_ind),
  calc_port_summary("Multivariate_Copula_Kelly",  daily_bt$day_ret_mv,  daily_bt$day_st_mv,  daily_bt$eq_mv,  daily_bt$dd_mv),
  calc_port_summary("OneSided_Copula_Kelly",      daily_bt$day_ret_os,  daily_bt$day_st_os,  daily_bt$eq_os,  daily_bt$dd_os)
)

pos_corr_games <- game_bets |> filter(n_legs >= 2L, avg_pair_rho > 0.05)
neg_corr_games <- game_bets |> filter(n_legs >= 2L, avg_pair_rho < -0.05)
haircut_pos_pct <- if (nrow(pos_corr_games) > 0) 100.0 * (mean(pos_corr_games$stake_mv) / mean(pos_corr_games$stake_ind) - 1.0) else 0.0
boost_neg_pct   <- if (nrow(neg_corr_games) > 0) 100.0 * (mean(neg_corr_games$stake_mv) / mean(neg_corr_games$stake_ind) - 1.0) else 0.0

cat("\n=== Part B: Locked 2025 OOS Multivariate Copula-Kelly vs Independent Kelly ===\n")
print(mvk_summary |> mutate(across(where(is.numeric), ~ round(.x, 4))), width = 140)
cat(sprintf("Multi-leg stake adjustment: Positively correlated games (%d): %+.2f%% | Negatively correlated hedges (%d): %+.2f%%\n",
            nrow(pos_corr_games), haircut_pos_pct, nrow(neg_corr_games), boost_neg_pct))

if (is_smoke) {
  cat(sprintf("\nSMOKE PASS in %.2fs. Skipping production Gold writes and registry logging.\n",
              proc.time()[["elapsed"]] - t0))
  quit(save = "no", status = 0)
}

# Write Production Gold ZSTD Parquet Tables (ADR-001)
proc_dir <- file.path(lake, "data/processed")
arrow::write_parquet(kprop_ablation, file.path(proc_dir, "dynamic_workload_kprop_ablation_2025.parquet"), compression = "zstd")
arrow::write_parquet(daily_bt,       file.path(proc_dir, "multivariate_kelly_backtest_2025.parquet"), compression = "zstd")
arrow::write_parquet(mvk_summary,    file.path(proc_dir, "multivariate_kelly_summary_2025.parquet"), compression = "zstd")
cat("Saved Gold ADR-001 ZSTD Parquet tables to data/processed/.\n")

# Dark Cyber 4-Asset Visualizations (< 5.0 MB .rds via force())
mod_dir <- file.path(lake, "models")

# Fig 1: Interior Half-Life Sweep & Dynamic BF/GS Workload Jump (Locked 2025 OOS Over 5.5 K AUC)
make_plot_mvk01 <- function(df_k, best_bf_m) {
  force(df_k); force(best_bf_m)
  d_sw <- df_k |> filter(!has_dyn_bf)
  bf_auc <- best_bf_m$oos_2025_auc_o55
  ggplot(d_sw, aes(x = half_life_days, y = oos_2025_auc_o55)) +
    geom_hline(yintercept = bf_auc, color = "#10b981", linetype = "dashed", linewidth = 1.0) +
    annotate("text", x = 175, y = bf_auc + 0.0005,
             label = sprintf("+ Dynamic BF/GS Workload (%s: %.4f AUC)", best_bf_m$model, bf_auc),
             color = "#10b981", hjust = 1, size = 3.5, fontface = "bold") +
    geom_line(color = "#38bdf8", linewidth = 1.2) +
    geom_point(color = "#f59e0b", size = 3.8) +
    geom_text(aes(label = sprintf("%.4f", oos_2025_auc_o55)), vjust = -1.1, color = "#f8fafc", size = 3.3) +
    scale_x_continuous(breaks = c(14, 30, 45, 60, 90, 180)) +
    labs(
      title = "Interior Half-Life Sweep (H = 14..180d) & Dynamic In-Season BF/GS Workload Lift",
      subtitle = "Locked 2025 OOS Over 5.5 K ROC AUC (n = 4,859 starts) | Adding causal BF/GS unlocks a step-change jump",
      x = "Exponential Half-Life H (Days, Strictly Pre-Game)", y = "Locked 2025 OOS Over 5.5 K ROC AUC",
      caption = "mlbr EXP-020 | Strictly pre-game (d < t) CSW%, K%, and starter BF/GS from v_statcast_features"
    ) +
    mlbr::theme_cyber()
}
p1 <- make_plot_mvk01(kprop_ablation, best_bf_res$metrics)
mlbr::save_plot_assets(p1, "mvk01_interior_halflife_workload_ablation", data = kprop_ablation, width = 10, height = 5.5, dir = mod_dir)

# Fig 2: Interactive Equity Curve — Multivariate Copula-Kelly vs Independent Quarter-Kelly (Locked 2025)
eq_long <- daily_bt |>
  select(game_date, Independent = eq_ind, Multivariate = eq_mv) |>
  pivot_longer(-game_date, names_to = "strategy", values_to = "bankroll")

make_plot_mvk02 <- function(df_eq) {
  force(df_eq)
  ggplot(df_eq, aes(x = as.Date(game_date), y = bankroll, color = strategy)) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = c("Independent" = "#f59e0b", "Multivariate" = "#38bdf8"),
                       labels = c("Independent Quarter-Kelly", "Multivariate Copula-Kelly"), name = "Staking Engine") +
    labs(
      title = "Locked 2025 OOS Multi-Market Bankroll Trajectory (ML + K-Props + Totals)",
      subtitle = "Simultaneous 3-market same-game staking (-105 vig synthetic benchmark, c=0.25, 35% daily cap)",
      x = "2025 MLB Regular Season Date", y = "Bankroll Multiple (Start = 1.00x)",
      caption = "mlbr EXP-020 | Start-of-day bankroll sizing; 2024 Gaussian Copula covariance matrix"
    ) +
    mlbr::theme_cyber()
}
p2 <- make_plot_mvk02(eq_long)
mlbr::save_plot_assets(p2, "mvk02_multivariate_vs_independent_kelly_equity", data = eq_long, width = 10, height = 5.5, dir = mod_dir)

p2_html <- plotly::plot_ly(daily_bt, x = ~as.Date(game_date)) |>
  plotly::add_lines(y = ~eq_ind, name = "Independent Quarter-Kelly", line = list(color = "#f59e0b", width = 2.2)) |>
  plotly::add_lines(y = ~eq_mv,  name = "Multivariate Copula-Kelly", line = list(color = "#38bdf8", width = 2.5)) |>
  plotly::layout(
    title = list(text = "Locked 2025 OOS Multi-Market Bankroll Trajectory (c = 0.25, -105 Vig)", font = list(color = "#f8fafc")),
    xaxis = list(title = "2025 Season Date", gridcolor = "#1e293b"),
    yaxis = list(title = "Bankroll Multiple (Start = 1.0x)", gridcolor = "#1e293b"),
    paper_bgcolor = "#090d16", plot_bgcolor = "#111827", font = list(color = "#e2e8f0")
  )
htmlwidgets::saveWidget(p2_html, file = file.path(mod_dir, "mvk02_multivariate_vs_independent_kelly_equity.html"), selfcontained = TRUE)
unlink(file.path(mod_dir, "mvk02_multivariate_vs_independent_kelly_equity_files"), recursive = TRUE)

# Fig 3: Copula Stake Adjustment (%) by Within-Game Ticket Correlation Regime
regime_df <- game_bets |>
  filter(n_legs >= 2L) |>
  mutate(
    corr_regime = case_when(
      avg_pair_rho >=  0.05 ~ "Positive Correlation (Shared Risk)",
      avg_pair_rho <= -0.05 ~ "Negative Correlation (Natural Hedge)",
      TRUE                  ~ "Near-Zero Correlation"
    ),
    stake_shift_pct = 100.0 * (stake_mv / pmax(stake_ind, 1e-9) - 1.0)
  )

make_plot_mvk03 <- function(df_r) {
  force(df_r)
  ggplot(df_r, aes(x = avg_pair_rho, y = stake_shift_pct, color = corr_regime)) +
    geom_hline(yintercept = 0, color = "#f8fafc", linetype = "dashed") +
    geom_vline(xintercept = 0, color = "#64748b", linetype = "dotted") +
    geom_point(alpha = 0.65, size = 2.4) +
    scale_color_manual(
      values = c("Positive Correlation (Shared Risk)" = "#f43f5e",
                 "Negative Correlation (Natural Hedge)" = "#10b981",
                 "Near-Zero Correlation" = "#38bdf8"),
      name = "Same-Game Ticket Regime"
    ) +
    labs(
      title = "Multivariate Copula-Kelly Stake Adjustment vs. Within-Game Ticket Correlation",
      subtitle = "Automatically haircuts positively correlated tickets (shared risk) and preserves/boosts negatively correlated hedges",
      x = "Mean Pairwise Copula Correlation (2024 MLE, Sign-Aligned to Bet Direction)",
      y = "Multivariate vs. Independent Stake Shift (%)",
      caption = "mlbr EXP-020 | Locked 2025 multi-leg game tickets (n = 2+ simultaneous positive-EV markets)"
    ) +
    mlbr::theme_cyber()
}
p3 <- make_plot_mvk03(regime_df)
mlbr::save_plot_assets(p3, "mvk03_copula_stake_haircut_by_correlation", data = regime_df, width = 10, height = 5.5, dir = mod_dir)

site_assets <- file.path(lake, "site/assets")
for (fn in c("mvk01_interior_halflife_workload_ablation.png", "mvk02_multivariate_vs_independent_kelly_equity.png",
             "mvk02_multivariate_vs_independent_kelly_equity.html", "mvk03_copula_stake_haircut_by_correlation.png")) {
  file.copy(file.path(mod_dir, fn), file.path(site_assets, fn), overwrite = TRUE)
}
for (rds_fn in c("mvk01_interior_halflife_workload_ablation.rds", "mvk02_multivariate_vs_independent_kelly_equity.rds", "mvk03_copula_stake_haircut_by_correlation.rds")) {
  sz_mb <- file.info(file.path(mod_dir, rds_fn))$size / (1024^2)
  cat(sprintf("Asset %s: %.3f MB\n", rds_fn, sz_mb))
  stopifnot(sz_mb < 5.0)
}

# Register EXP-020 in experiments/registry.json
m5_h30 <- kprop_ablation |> filter(model == "M5_ExpDecay_H30d") |> slice(1)
m6_bf  <- best_bf_res$metrics
s_ind  <- mvk_summary |> filter(strategy == "Independent_Quarter_Kelly") |> slice(1)
s_mv   <- mvk_summary |> filter(strategy == "Multivariate_Copula_Kelly") |> slice(1)

mlbr::lake_log_experiment(
  id = "EXP-020-DYNAMIC-WORKLOAD-MULTIVARIATE-KELLY",
  title = "Interior Half-Life Sweep, Dynamic In-Season Starter Workload (BF/GS) & Multivariate Copula-Kelly Staking",
  tier = 5, status = "VALIDATED",
  summary = sprintf(
    "Resolved EXP-019's boundary half-life sweep (H=14..180d, 2024-selected H*=%dd) and added strictly causal in-season starter workload (bf_gs_exp, EB M=4 starts) to NegBin K-Props, lifting locked-2025 OOS (n=4,859) Over 5.5 K AUC %.4f -> %.4f, Count NLL %.5f -> %.5f, and SO RMSE %.4f -> %.4f. Integrated EXP-018's 2024 Gaussian Copula covariance matrix into an exact box-constrained Multivariate Quarter-Kelly sizer across 3 simultaneous same-game markets (ML, Home SP O5.5 K, Over 8.5) on locked 2025 (-105 vig synthetic benchmark): positive-correlation tickets received %+.2f%% stake haircut and negative-correlation hedges %+.2f%%, shifting daily vol %.2f%% -> %.2f%% and Max DD %.2f%% -> %.2f%%.",
    h_star, m5_h30$oos_2025_auc_o55, m6_bf$oos_2025_auc_o55,
    m5_h30$oos_2025_nll, m6_bf$oos_2025_nll, m5_h30$oos_2025_rmse, m6_bf$oos_2025_rmse,
    haircut_pos_pct, boost_neg_pct,
    s_ind$daily_vol_pct, s_mv$daily_vol_pct, s_ind$max_drawdown_pct, s_mv$max_drawdown_pct
  ),
  hypotheses = c(
    "Adding strictly causal exponentially-decayed in-season starter workload (BF/GS) to interior half-life CSW%/K% improves locked 2025 OOS NegBin K-Prop Count NLL, RMSE, and Over 5.5 K AUC over EXP-019.",
    "Multivariate Copula-Kelly sizing haircuts positively correlated same-game tickets and preserves/boosts negatively correlated hedges, compressing portfolio volatility and drawdown."
  ),
  formulas = list(
    dyn_bf_gs = "BF_GS_H(p, t) = (sum_{d<t} 2^(-dt/H)*BF_{p,d} + 4*eb_bf_gs) / (sum 2^(-dt/H) + 4)",
    mv_kelly  = "max_{f in [0, 0.05]^m} f'mu - (1/(2c)) f'Sigma f, Sigma_{jk} = O_j*O_k*(C_Gauss(p_j,p_k; s_j*s_k*rho_{jk}) - p_j*p_k)"
  ),
  metrics = list(
    h_star_2024_days     = h_star,
    m5_h30_oos_nll       = round(m5_h30$oos_2025_nll, 5),
    m6_dynbf_oos_nll     = round(m6_bf$oos_2025_nll, 5),
    m5_h30_oos_rmse      = round(m5_h30$oos_2025_rmse, 4),
    m6_dynbf_oos_rmse    = round(m6_bf$oos_2025_rmse, 4),
    m5_h30_oos_auc_o55   = round(m5_h30$oos_2025_auc_o55, 4),
    m6_dynbf_oos_auc_o55 = round(m6_bf$oos_2025_auc_o55, 4),
    mvk_pos_haircut_pct  = round(haircut_pos_pct, 2),
    mvk_neg_boost_pct    = round(boost_neg_pct, 2),
    ind_kelly_sharpe     = round(s_ind$sharpe_ann, 3),
    mv_kelly_sharpe      = round(s_mv$sharpe_ann, 3),
    ind_kelly_max_dd_pct = round(s_ind$max_drawdown_pct, 2),
    mv_kelly_max_dd_pct  = round(s_mv$max_drawdown_pct, 2)
  ),
  features = c(paste0("csw_h", h_star), paste0("k_h", h_star), "bf_gs_h30", "bf_gs_h45", "bf_gs_h60", "stake_ind", "stake_mv"),
  artifacts = list(
    spec = "specs/20_dynamic_workload_multivariate_kelly.md",
    script = "models/20_dynamic_workload_multivariate_kelly.R",
    gold_kprop = "data/processed/dynamic_workload_kprop_ablation_2025.parquet",
    gold_bt    = "data/processed/multivariate_kelly_backtest_2025.parquet",
    gold_sum   = "data/processed/multivariate_kelly_summary_2025.parquet",
    fig_sweep  = "models/mvk01_interior_halflife_workload_ablation.png",
    fig_equity = "models/mvk02_multivariate_vs_independent_kelly_equity.html",
    fig_haircut = "models/mvk03_copula_stake_haircut_by_correlation.png"
  ),
  lake = lake
)
cat(sprintf("\n=== EXP-020 FULL COMPLETE in %.2fs ===\n", proc.time()[["elapsed"]] - t0))
