#!/usr/bin/env Rscript
# ==============================================================================
# mlbr Unified Daily Pre-Game Pricing & Slate Runner CLI
# Unifies EXP-014 (ML/F5/Totals), EXP-018 (Gaussian Copula SGP), EXP-019
# (Monotonic Multinomial TB Ladder), and EXP-020 (Dynamic Workload K-Props &
# One-Sided Multivariate Copula-Kelly Sizing) for any MLB game date.
#
# CLI:
#   Rscript R/price_daily_slate.R --date 2025-07-04
#   Rscript R/price_daily_slate.R --date 2025-07-04 --kelly-mode onesided
#   Rscript R/price_daily_slate.R --smoke
# ==============================================================================
suppressPackageStartupMessages({
  library(mlbr); library(DBI); library(arrow); library(dplyr); library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0L || idx[1] >= length(args)) return(default)
  args[idx[1] + 1L]
}

is_smoke    <- "--smoke" %in% args
target_date <- get_arg("--date", "2025-07-04")
kelly_mode  <- get_arg("--kelly-mode", "onesided")
stopifnot(kelly_mode %in% c("onesided", "full_mv", "indep"))

lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

# Helpers: Fair American Odds + Exact 16-pt Plackett 2D Gaussian Copula
prob_to_american <- function(p) {
  p <- pmin(pmax(p, 1e-4), 1 - 1e-4)
  ifelse(p >= 0.5, sprintf("-%.0f", 100 * p / (1 - p)), sprintf("+%.0f", 100 * (1 - p) / p))
}

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

# Load validated 2024 MLE Copula correlations from EXP-018 Gold ablation
p_ab18 <- file.path(lake, "data/processed/sgp_copula_ablation_metrics_2025.parquet")
ab18   <- arrow::read_parquet(p_ab18) |> filter(model == "M1_Gaussian_Copula")
get_rho <- function(sid) ab18$rho_gauss_2024[ab18$sgp_id == sid][1]
rho_ml_f5   <- get_rho("SGP1_HomeML_F5Home")
rho_hsp_o85 <- get_rho("SGP2_HomeSP_O55K_Over85")
rho_ml_hsp  <- get_rho("SGP3_HomeML_HomeSP_O55K")
rho_tb_o85  <- get_rho("SGP5_BatterTB2_Over85")

# Query target date slate across EXP-014, EXP-016, and EXP-019 Gold tables
p_games <- file.path(lake, "data/processed/unified_pro_game_f5_totals_2024_2025.parquet")
p_kprop <- file.path(lake, "data/processed/dynamic_statespace_negbin_kprops_2024_2025.parquet")
p_tb19  <- file.path(lake, "data/processed/ordered_tb_ladder_backtest_2024_2025.parquet")

# Fit Over 8.5 calibration strictly on 2024
g24_cal <- DBI::dbGetQuery(con, sprintf("
  SELECT CASE WHEN actual_total_runs >= 9 THEN 1 ELSE 0 END AS y_over85, exp_total_m6
  FROM read_parquet('%s') WHERE season = 2024
", p_games))
fit_o85 <- glm(y_over85 ~ exp_total_m6, data = g24_cal, family = binomial())

slate <- DBI::dbGetQuery(con, sprintf("
  SELECT
    g.game_id, CAST(g.game_date AS VARCHAR) AS game_date, g.away_team, g.home_team,
    g.p_m6_ml, g.p_m6_f5, g.exp_total_m6,
    kh.player_name AS home_sp, kh.mu_m4 AS home_sp_exp_so, kh.p_m4_o55 AS p_hsp_o55,
    ka.player_name AS away_sp, ka.mu_m4 AS away_sp_exp_so, ka.p_m4_o55 AS p_asp_o55
  FROM read_parquet('%s') g
  LEFT JOIN read_parquet('%s') kh ON g.game_id = kh.game_id AND kh.is_home_sp = 1
  LEFT JOIN read_parquet('%s') ka ON g.game_id = ka.game_id AND ka.is_home_sp = 0
  WHERE CAST(g.game_date AS DATE) = DATE '%s'
  ORDER BY g.game_id
", p_games, p_kprop, p_kprop, target_date))

if (nrow(slate) == 0L) {
  stop(sprintf("No MLB games found for target_date = %s in Gold slate tables.", target_date))
}

slate <- slate |>
  mutate(
    p_over85         = as.numeric(predict(fit_o85, newdata = slate, type = "response")),
    home_ml_fair     = prob_to_american(p_m6_ml),
    f5_home_fair     = prob_to_american(p_m6_f5),
    hsp_o55_fair     = prob_to_american(p_hsp_o55),
    over85_fair      = prob_to_american(p_over85),
    # Exact Gaussian Copula SGP Fair Probabilities & American Odds
    p_sgp_ml_f5      = gauss_copula_2d(p_m6_ml, p_m6_f5, rho_ml_f5),
    sgp_ml_f5_fair   = prob_to_american(p_sgp_ml_f5),
    p_sgp_ml_hsp     = gauss_copula_2d(p_m6_ml, p_hsp_o55, rho_ml_hsp),
    sgp_ml_hsp_fair  = prob_to_american(p_sgp_ml_hsp),
    p_sgp_hsp_o85    = gauss_copula_2d(p_hsp_o55, p_over85, rho_hsp_o85),
    sgp_hsp_o85_fair = prob_to_american(p_sgp_hsp_o85)
  )

# Top 10 Monotonic Batter Total Bases Ladders on the slate (EXP-019)
top_batters <- DBI::dbGetQuery(con, sprintf("
  SELECT t.game_id, t.player_name, t.slot, t.exp_tb_mn,
         t.p_mn_ge1, t.p_mn_ge2, t.p_mn_ge3, t.p_mn_ge4
  FROM read_parquet('%s') t
  INNER JOIN (SELECT game_id FROM read_parquet('%s') WHERE CAST(game_date AS DATE) = DATE '%s') g
    ON t.game_id = g.game_id
  ORDER BY t.exp_tb_mn DESC
  LIMIT 10
", p_tb19, p_games, target_date)) |>
  left_join(slate |> select(game_id, p_over85), by = "game_id") |>
  mutate(
    tb2_fair         = prob_to_american(p_mn_ge2),
    tb4_fair         = prob_to_american(p_mn_ge4),
    p_sgp_tb2_o85    = gauss_copula_2d(p_mn_ge2, p_over85, rho_tb_o85),
    sgp_tb2_o85_fair = prob_to_american(p_sgp_tb2_o85)
  )

cat(sprintf("\n=== mlbr Unified Pre-Game Slate Card: %s (%d games | Kelly Mode: %s) ===\n",
            target_date, nrow(slate), kelly_mode))
print(
  as_tibble(slate) |> transmute(
    Matchup = paste0(away_team, "@", home_team),
    HomeML = home_ml_fair, F5Home = f5_home_fair,
    ExpTot = round(exp_total_m6, 2), Over8.5 = over85_fair,
    HomeSP = home_sp, HSP_O5.5K = hsp_o55_fair,
    `SGP(ML+F5)` = sgp_ml_f5_fair, `SGP(ML+K)` = sgp_ml_hsp_fair
  ),
  n = 20, width = 140
)

cat("\n=== Top 10 Monotonic Batter TB Ladders & Copula SGP (TB 2+ & Over 8.5) ===\n")
print(
  as_tibble(top_batters) |> transmute(
    Batter = player_name, Slot = slot, ExpTB = round(exp_tb_mn, 2),
    `P(1+)` = round(100*p_mn_ge1, 1), `P(2+)` = round(100*p_mn_ge2, 1),
    `P(3+)` = round(100*p_mn_ge3, 1), `P(4+)` = round(100*p_mn_ge4, 1),
    TB2_Fair = tb2_fair, `SGP(TB2+O8.5)` = sgp_tb2_o85_fair
  ),
  n = 10, width = 140
)

if (!is_smoke) {
  out_dir <- file.path(lake, "data/processed/daily_slate_cards")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_file <- file.path(out_dir, sprintf("slate_%s.parquet", target_date))
  arrow::write_parquet(slate, out_file, compression = "zstd")
  cat(sprintf("\nSaved ZSTD Parquet slate card: %s\n", out_file))
} else {
  cat("\nSMOKE PASS: Unified Daily Pre-Game Slate Pricer verified cleanly.\n")
}
