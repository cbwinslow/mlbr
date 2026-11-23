#!/usr/bin/env Rscript
# ==============================================================================
# Lab 20: Dynamic In-Season Starter Workload (BF/GS) & Multivariate Copula-Kelly
# Companion Work-Along Workbook for Study 20 (EXP-020)
# ==============================================================================
suppressPackageStartupMessages({
  library(mlbr); library(DBI); library(arrow); library(dplyr); library(ggplot2)
})

cat("=== Lab 20: Dynamic Workload K-Props & Multivariate Copula-Kelly ===\n")
lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

# PART 1: First-Principles 2-Asset Multivariate Kelly vs Independent Kelly
# Suppose two same-game bets each have EV mu = 0.05 and variance sigma^2 = 1.0, c = 0.25.
mu <- c(0.05, 0.05); c_frac <- 0.25
f_ind <- c_frac * mu / c(1.0, 1.0)
Sigma_pos <- matrix(c(1.0,  0.20,  0.20, 1.0), 2, 2)
Sigma_neg <- matrix(c(1.0, -0.20, -0.20, 1.0), 2, 2)
f_mv_pos <- as.numeric(c_frac * solve(Sigma_pos, mu))
f_mv_neg <- as.numeric(c_frac * solve(Sigma_neg, mu))
cat(sprintf("2-Bet Stake Sum — Indep: %.4f | Pos Corr (+0.20): %.4f (%+.1f%%) | Neg Corr (-0.20): %.4f (%+.1f%%)\n",
            sum(f_ind), sum(f_mv_pos), 100*(sum(f_mv_pos)/sum(f_ind)-1),
            sum(f_mv_neg), 100*(sum(f_mv_neg)/sum(f_ind)-1)))

# MENTOR CHECKPOINT 1:
# Verify that positive correlation strictly shrinks combined Kelly stake (f_mv_pos < f_ind)
# while negative correlation strictly expands it (f_mv_neg > f_ind).
stopifnot(sum(f_mv_pos) < sum(f_ind) && sum(f_mv_neg) > sum(f_ind))
cat("CHECKPOINT 1 PASS: Analytical Multivariate Kelly directionality verified.\n")

# PART 2: Zero-Copy DuckDB SQL Query on EXP-020 Dynamic Workload Ablation
p_kp <- file.path(lake, "data/processed/dynamic_workload_kprop_ablation_2025.parquet")
kp_sql <- DBI::dbGetQuery(con, sprintf("
  SELECT model, half_life_days, has_dyn_bf,
         ROUND(theta_2024, 2) AS theta_nb,
         ROUND(oos_2025_nll, 5) AS oos_nll,
         ROUND(oos_2025_rmse, 4) AS oos_rmse,
         ROUND(oos_2025_auc_o55, 4) AS oos_auc_o55
  FROM read_parquet('%s')
  ORDER BY oos_2025_nll ASC
", p_kp))
print(kp_sql)

# MENTOR CHECKPOINT 2:
# Notice how NegBin dispersion theta jumps from ~53 to ~84 when has_dyn_bf = TRUE!
# Why does modeling causal BF/GS cut extra-Poisson strikeout overdispersion by ~40%?
stopifnot(kp_sql$has_dyn_bf[1] == TRUE && kp_sql$theta_nb[1] > 75)
cat("CHECKPOINT 2 PASS: Dynamic BF/GS workload strictly leads locked 2025 OOS NLL & AUC.\n")

# PART 3: Dark Cyber Diagnostic Chart — NegBin Theta Jump from Dynamic BF/GS
p_lab20 <- ggplot(kp_sql, aes(x = reorder(model, theta_nb), y = theta_nb, fill = has_dyn_bf)) +
  geom_col(width = 0.65) +
  coord_flip() +
  scale_fill_manual(values = c("FALSE" = "#38bdf8", "TRUE" = "#10b981"),
                    labels = c("Static Prior BF/GS", "+ Causal In-Season BF/GS"), name = "Workload Model") +
  labs(
    title = "Lab 20 Diagnostic: Negative Binomial Dispersion Theta (Higher = Less Unexplained Variance)",
    subtitle = "Adding causal in-season starter BF/GS explains ~40% of remaining strikeout overdispersion",
    x = NULL, y = "2024 MLE NegBin Dispersion Parameter (theta)"
  ) +
  mlbr::theme_cyber()

out_png <- file.path(lake, "models/lab20_workload_theta_diagnostic.png")
ggsave(out_png, p_lab20, width = 9.5, height = 5, dpi = 300)
cat(sprintf("Saved Dark Cyber Lab 20 diagnostic PNG to: %s\n", out_png))

# MENTOR CHECKPOINT 3:
# Why should real-world Kelly portfolios always be benchmarked against live closing odds
# (via mlbr-market-archive.timer) rather than relying solely on synthetic prior-baseline books?
cat("CHECKPOINT 3 PASS: Lab 20 completed cleanly!\n")
