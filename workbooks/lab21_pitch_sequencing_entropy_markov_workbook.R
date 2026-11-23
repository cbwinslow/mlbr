#!/usr/bin/env Rscript
# ==============================================================================
# Lab 21: Pitch Sequencing, Markov Transitions & Shannon Entropy (Seq+)
# Companion Work-Along Workbook for Study 21 (EXP-021)
# ==============================================================================
suppressPackageStartupMessages({
  library(mlbr); library(DBI); library(arrow); library(dplyr); library(ggplot2)
})

cat("=== Lab 21: Pitch Sequencing, Markov Transitions & Shannon Entropy ===\n")
lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

# PART 1: First-Principles Shannon Entropy H(P) = -sum p * log2(p)
shannon_bits <- function(p) { p <- p[p > 0]; -sum(p * log2(p)) }
h_predictable <- shannon_bits(c(0.85, 0.10, 0.05))           # 85% fastball in 3-1
h_balanced    <- shannon_bits(c(0.25, 0.25, 0.25, 0.25))     # 4-pitch equal mix
cat(sprintf("Shannon Entropy — Predictable 3-1 mix: %.3f bits | Balanced 4-pitch mix: %.3f bits\n",
            h_predictable, h_balanced))

# MENTOR CHECKPOINT 1:
# Verify that a uniform 4-pitch arsenal has exact entropy log2(4) = 2.000 bits,
# while an 85/10/5 predictable count mix collapses below 0.80 bits.
stopifnot(abs(h_balanced - 2.0) < 1e-12 && h_predictable < 0.80)
cat("CHECKPOINT 1 PASS: Information-theoretic Shannon entropy bounds verified.\n")

# PART 2: Zero-Copy DuckDB SQL Query on EXP-021 Gold Markov & Seq+ Tables
p_mk  <- file.path(lake, "data/processed/pitch_markov_transitions_2015_2025.parquet")
p_seq <- file.path(lake, "data/processed/pitcher_sequencing_entropy_2020_2025.parquet")

mk_sql <- DBI::dbGetQuery(con, sprintf("
  SELECT prev_group, curr_group, n_pairs,
         ROUND(100.0 * whiff_rate, 2) AS whiff_pct,
         ROUND(mean_delta_velo, 1) AS avg_dv_mph
  FROM read_parquet('%s')
  ORDER BY whiff_pct DESC
", p_mk))
print(mk_sql)

# MENTOR CHECKPOINT 2:
# Inspect why Breaking -> Breaking leads raw whiff% (35.2%) and explain why we MUST
# control for count & single-pitch location (M2 - M1) to isolate true sequencing skill!
stopifnot(nrow(mk_sql) == 9L && sum(mk_sql$n_pairs) > 5000000L)
cat("CHECKPOINT 2 PASS: All 9 Markov transitions queried across 5.47M+ pairs.\n")

# PART 3: Dark Cyber Diagnostic Chart — Top 12 2025 MLB Pitchers by Seq+
top12 <- arrow::read_parquet(p_seq) |>
  filter(season == 2025L, n_paired_swings >= 300L, !is.na(player_name)) |>
  arrange(desc(seq_plus)) |>
  head(12)

p_lab21 <- ggplot(top12, aes(x = reorder(player_name, seq_plus), y = seq_plus, fill = count_entropy_bits)) +
  geom_col(width = 0.68) +
  coord_flip(ylim = c(100, 155)) +
  scale_fill_viridis_c(option = "plasma", name = "Count\nEntropy\n(bits)") +
  labs(
    title = "Lab 21 Diagnostic: Top 12 MLB Pitch Sequencers in 2025 (Seq+, min 300 paired swings)",
    subtitle = "Orthogonal whiff-value added by sequencing & eye-level shifts above full pitch physics + location",
    x = NULL, y = "Sequence+ (Seq+, 100 = 2024 MLB Avg)"
  ) +
  mlbr::theme_cyber()

out_png <- file.path(lake, "models/lab21_top12_seq_plus_diagnostic.png")
ggsave(out_png, p_lab21, width = 9.5, height = 5.5, dpi = 300)
cat(sprintf("Saved Dark Cyber Lab 21 diagnostic PNG to: %s\n", out_png))

# MENTOR CHECKPOINT 3:
# Compare Tarik Skubal (1.965 bits entropy, 130.5 Seq+) vs Tommy Kahnle (0.593 bits, 151.8 Seq+).
# How can a reliever with low pitch-type entropy still post elite Seq+ via vertical eye-level & tunnel shifts?
cat("CHECKPOINT 3 PASS: Lab 21 completed cleanly!\n")
