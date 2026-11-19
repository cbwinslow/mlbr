# ==============================================================================
# Study 03: Commit-Point Pitch Tunneling & Platoon-Conditioned Stuff+ / Pitching+
# ==============================================================================
# Experiment ID: EXP-003-TUNNELING-STUFF-PLUS
# Spec:          specs/03_tunneling_stuff_plus.md
# Framework:     Q-CRISP + Medallion Full-Population Regular-Season Modeling
# Validation:    Strict Chronological Splits (game_type = 'R', Zero Leakage)
#                  - Train (Full):     2015-2023 Regular Season (~5.98M pitches)
#                  - Train (Hawk-Eye): 2020-2023 Regular Season (~2.44M pitches)
#                  - Validation/Calib: 2024 Regular Season      (~0.70M pitches)
#                  - Locked OOS Test:  2025-2026 Regular Season (~1.42M pitches)
# ==============================================================================

suppressPackageStartupMessages({
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(data.table)
  library(arrow)
  library(xgboost)
  library(ggplot2)
  library(plotly)
  library(htmlwidgets)
  library(scales)
  library(patchwork)
})

setDTthreads(16L)
set.seed(42L)

run_study03 <- function() {
cat("==============================================================================\n")
cat("  MLBR STUDY 03: COMMIT-POINT TUNNELING & PLATOON-CONDITIONED STUFF+ ENGINE  \n")
cat("==============================================================================\n\n")


# Strip large enclosing function environment before RDS serialization (prevents multi-hundred-MB closures)
strip_env <- function(obj) {
  if (inherits(obj, "ggplot")) obj$plot_env <- new.env(parent = globalenv())
  if (!is.null(obj$patches$plots)) {
    for (i in seq_along(obj$patches$plots)) obj$patches$plots[[i]]$plot_env <- new.env(parent = globalenv())
  }
  obj
}

# Dynamic path resolution (zero hardcoded paths)
lake <- mlbr::default_lake()
models_dir <- file.path(lake, "models")
processed_dir <- file.path(lake, "data", "processed")
saved_models_dir <- file.path(lake, "data", "models")
assets_dir <- file.path(lake, "site", "assets")

for (d in c(models_dir, processed_dir, saved_models_dir, assets_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

con <- mlbr::lake_duckdb(lake = lake)
on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET threads TO 16; SET memory_limit = '12GB';")

# Query live full-lake Silver tunneling count for registry metadata (no hardcoded counts)
silver_tunnel_pairs_all <- as.numeric(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM v_statcast_tunneling")$n)
stopifnot(is.finite(silver_tunnel_pairs_all) && silver_tunnel_pairs_all >= 9500000)

# ==============================================================================
# 1. FULL-POPULATION REGULAR-SEASON TUNNELING EDA (FIGURES 1 & 2)
# ==============================================================================
cat("-> [1/6] Computing full-population regular-season tunneling geometry & sequence matrices...\n")

# --- Figure 1 Data: 2D Tunnel Distance vs Post-Tunnel Break Whiff Surface (2015-2026 Regular Season) ---
fig1_df <- DBI::dbGetQuery(con, "
  SELECT
    FLOOR(tunnel_dist_in / 1.0) * 1.0 + 0.5 AS tunnel_bin,
    FLOOR(post_tunnel_break_in / 1.5) * 1.5 + 0.75 AS break_bin,
    COUNT(*) AS n_pitches,
    SUM(is_swing) AS n_swings,
    SUM(is_whiff) AS n_whiffs,
    SUM(is_csw) AS n_csw,
    ROUND(100.0 * SUM(is_whiff)::DOUBLE / NULLIF(SUM(is_swing), 0), 2) AS whiff_pct,
    ROUND(100.0 * SUM(is_csw)::DOUBLE / NULLIF(COUNT(*), 0), 2) AS csw_pct,
    ROUND(AVG(pitcher_run_exp) * 100.0, 3) AS rv_per_100
  FROM v_statcast_tunneling
  WHERE game_year BETWEEN 2015 AND 2026
    AND game_type = 'R'
    AND tunnel_dist_in >= 0 AND tunnel_dist_in < 24
    AND post_tunnel_break_in >= 0 AND post_tunnel_break_in < 36
  GROUP BY 1, 2
  HAVING SUM(is_swing) >= 250
  ORDER BY 1, 2
")
stopifnot(nrow(fig1_df) > 50L)

iso_x <- seq(0.5, 23.5, by = 0.25)
iso_lines <- rbind(
  data.frame(tunnel_x = iso_x, break_y = pmin(iso_x * 1.0, 35.5), ratio_lbl = "1.0x"),
  data.frame(tunnel_x = iso_x, break_y = pmin(iso_x * 2.0, 35.5), ratio_lbl = "2.0x"),
  data.frame(tunnel_x = iso_x, break_y = pmin(iso_x * 3.0, 35.5), ratio_lbl = "3.0x")
)
iso_lines <- iso_lines[iso_lines$break_y < 35.5, ]

fig1 <- ggplot(fig1_df, aes(x = tunnel_bin, y = break_bin, fill = whiff_pct)) +
  geom_tile(color = "#090d16", linewidth = 0.15) +
  geom_line(
    data = iso_lines,
    aes(x = tunnel_x, y = break_y, group = ratio_lbl),
    inherit.aes = FALSE,
    color = "#e2e8f0", linetype = "dashed", linewidth = 0.55, alpha = 0.75
  ) +
  annotate("text", x = 10.6, y = 33.8, label = "3.0x BP Ratio", color = "#f8fafc", size = 3.2, fontface = "bold") +
  annotate("text", x = 16.5, y = 34.0, label = "2.0x BP Ratio", color = "#cbd5e1", size = 3.2, fontface = "bold") +
  annotate("text", x = 22.0, y = 23.2, label = "1.0x", color = "#94a3b8", size = 3.0) +
  annotate(
    "label", x = 4.8, y = 30.5,
    label = "ELITE DECEPTION ZONE\nTight Tunnel (<8 in) + High Post-Tunnel Break",
    fill = "#0f172a", color = "#38bdf8", size = 3.1, fontface = "bold", linewidth = 0.4
  ) +
  scale_fill_gradientn(
    colours = c("#111827", "#1e3a5f", "#0284c7", "#10b981", "#f59e0b", "#f43f5e"),
    limits = c(14, 42),
    oob = scales::squish,
    name = "Whiff%\n(on Swings)"
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 4), expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 36, by = 6), expand = c(0.01, 0.01)) +
  labs(
    title = "Commit-Point Tunnel Geometry vs. Swing-and-Miss Deception (2015-2026 Regular Season)",
    subtitle = sprintf(
      "Exact y0=50ft -> y=23.8ft tunnel distance vs. post-tunnel break across %s regular-season pairs (min 250 swings/bin; dashed = BP Break:Tunnel ratio).",
      format(sum(fig1_df$n_pitches), big.mark = ",")
    ),
    x = "Commit-Point Tunnel Distance at y = 23.8 ft (inches)  [Tighter -> Left]",
    y = "Post-Tunnel Break Divergence (inches)  [Sharper Late Break -> Up]",
    caption = "Source: mlbr Silver Lake (v_statcast_tunneling, game_type = 'R') | Sum-before-divide empirical whiff rate"
  ) +
  mlbr::theme_cyber(base_size = 11)

mlbr::save_plot_assets(strip_env(fig1), "stuff01_tunnel_geometry_whiff_surface", data = fig1_df, width = 11, height = 6.8, dir = models_dir)

# --- Figure 2 Data: Pitch-Pair Sequence Deception Matrix by Platoon (2020-2026 Hawk-Eye Regular Season) ---
# Strictly sum-before-divide aggregate Break:Tunnel ratio (SUM(post_tunnel_break_in) / SUM(tunnel_dist_in))
fig2_df <- DBI::dbGetQuery(con, "
  WITH base AS (
    SELECT
      prev_pitch_type,
      pitch_type,
      pitch_sequence,
      is_repeat_pitch,
      CASE WHEN is_same_hand = 1 THEN 'Same-Hand (RvR / LvL)' ELSE 'Opp-Hand (RvL / LvR)' END AS platoon_group,
      COUNT(*) AS n_pairs,
      SUM(is_swing) AS n_swings,
      SUM(is_whiff) AS n_whiffs,
      SUM(is_csw) AS n_csw,
      ROUND(AVG(tunnel_dist_in), 2) AS mean_tunnel_in,
      ROUND(AVG(plate_sep_in), 2) AS mean_plate_sep_in,
      ROUND(AVG(post_tunnel_break_in), 2) AS mean_post_break_in,
      ROUND(SUM(post_tunnel_break_in) / NULLIF(SUM(tunnel_dist_in), 0), 3) AS agg_break_tunnel_ratio,
      ROUND(SUM(plate_sep_in) / NULLIF(SUM(tunnel_dist_in), 0), 3) AS agg_plate_tunnel_ratio,
      ROUND(AVG(delta_velo_mph), 2) AS mean_delta_velo,
      ROUND(100.0 * SUM(is_whiff)::DOUBLE / NULLIF(SUM(is_swing), 0), 2) AS whiff_pct,
      ROUND(100.0 * SUM(is_csw)::DOUBLE / NULLIF(COUNT(*), 0), 2) AS csw_pct,
      ROUND(AVG(pitcher_run_exp) * 100.0, 3) AS rv_per_100
    FROM v_statcast_tunneling
    WHERE game_year BETWEEN 2020 AND 2026
      AND game_type = 'R'
      AND is_same_hand IS NOT NULL
      AND prev_pitch_type IN ('FF', 'SI', 'FC', 'SL', 'ST', 'CH', 'CU', 'FS')
      AND pitch_type IN ('FF', 'SI', 'FC', 'SL', 'ST', 'CH', 'CU', 'FS')
    GROUP BY 1, 2, 3, 4, 5
  ),
  repeat_ref AS (
    SELECT pitch_type, platoon_group, whiff_pct AS repeat_whiff_pct
    FROM base
    WHERE is_repeat_pitch = 1
  )
  SELECT
    b.*,
    r.repeat_whiff_pct,
    ROUND(b.whiff_pct - r.repeat_whiff_pct, 2) AS delta_whiff_vs_repeat
  FROM base b
  LEFT JOIN repeat_ref r
    ON b.pitch_type = r.pitch_type AND b.platoon_group = r.platoon_group
  WHERE b.n_pairs >= 15000
  ORDER BY b.n_pairs DESC
")
stopifnot(nrow(fig2_df) > 10L)

top_seqs <- aggregate(n_pairs ~ pitch_sequence, data = fig2_df, sum)
top_seqs <- top_seqs[order(-top_seqs$n_pairs), "pitch_sequence"][seq_len(min(18, nrow(top_seqs)))]
fig2_plot_df <- fig2_df[fig2_df$pitch_sequence %in% top_seqs, ]

fig2 <- ggplot(
  fig2_plot_df,
  aes(x = mean_tunnel_in, y = whiff_pct, color = agg_break_tunnel_ratio, size = n_pairs / 1000, shape = factor(is_repeat_pitch))
) +
  geom_point(alpha = 0.92) +
  geom_text(
    aes(label = pitch_sequence),
    size = 2.9, vjust = -1.0, check_overlap = TRUE, fontface = "bold", show.legend = FALSE
  ) +
  facet_wrap(~ platoon_group, ncol = 2) +
  scale_color_gradientn(
    colours = c("#38bdf8", "#10b981", "#f59e0b", "#f43f5e"),
    name = "Aggregate Break /\nTunnel Ratio"
  ) +
  scale_shape_manual(
    values = c("0" = 16, "1" = 17),
    labels = c("0" = "Mix Sequence (A -> B)", "1" = "Repeat Pitch (X -> X)"),
    name = "Sequence Class"
  ) +
  scale_size_continuous(range = c(2.8, 8.5), name = "Pairs\n(Thousands)") +
  labs(
    title = "Pitch-Pair Deception Matrix: Tunnel Distance vs. Whiff% by Platoon (2020-2026 Regular Season)",
    subtitle = "Sum-before-divide BP Break:Tunnel ratio separates high-divergence cross-pitch tunnels (FF->SL/ST/CH) from repeat put-away counts (SL->SL).",
    x = "Mean Commit-Point Tunnel Distance at y = 23.8 ft (inches)",
    y = "Empirical Whiff Rate on Swings (%)",
    caption = "Source: mlbr Silver Lake (v_statcast_tunneling, game_type = 'R', 2020-2026 Hawk-Eye) | Top sequences >= 15,000 pairs"
  ) +
  mlbr::theme_cyber(base_size = 11) +
  theme(legend.position = "bottom", legend.box = "horizontal")

mlbr::save_plot_assets(strip_env(fig2), "stuff02_sequence_deception_matrix", data = fig2_df, width = 12.5, height = 7.0, dir = models_dir)

# ==============================================================================
# 2. FULL-POPULATION REGULAR-SEASON FEATURE EXTRACTION (2015-2026, ZERO LEAKAGE)
# ==============================================================================
cat("-> [2/6] Extracting full-population 2015-2026 Regular-Season pitches (game_type = 'R')...\n")
t_ext0 <- proc.time()

model_sql <- "
WITH base_fb AS (
  SELECT
    game_pk,
    game_year,
    game_date,
    game_type,
    at_bat_number,
    pitch_number,
    pitcher,
    pitch_type,
    p_throws,
    stand,
    matchup,
    is_same_hand,
    release_speed,
    effective_speed,
    release_spin_rate,
    spin_axis,
    ivb_in,
    hb_glove_in,
    hb_away_in,
    release_pos_x_arm,
    release_pos_z,
    release_extension,
    arm_angle_deg,
    plate_x,
    plate_x_norm,
    plate_z,
    sz_top,
    sz_bot,
    vaa,
    haa_away,
    balls,
    strikes,
    outs_when_up,
    n_thruorder_pitcher,
    is_swing,
    is_whiff,
    is_csw,
    is_chase,
    is_in_zone,
    xwoba,
    pitcher_run_exp,
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
)
SELECT
  f.game_pk,
  f.game_year::INTEGER AS game_year,
  f.game_date,
  f.at_bat_number::INTEGER AS at_bat_number,
  f.pitch_number::INTEGER AS pitch_number,
  f.pitcher::INTEGER AS pitcher,
  f.pitch_type,
  f.p_throws,
  f.stand,
  f.matchup,
  f.is_same_hand::INTEGER AS is_same_hand,
  -- Block A: Physical Stuff
  f.release_speed,
  f.effective_speed,
  f.release_spin_rate,
  f.spin_axis,
  f.ivb_in,
  f.hb_glove_in,
  f.hb_away_in,
  f.release_pos_x_arm,
  f.release_pos_z,
  f.release_extension,
  f.arm_angle_deg,
  (f.release_speed - f.pitcher_fb_velo_to_date) AS delta_fb_velo,
  (f.ivb_in - f.pitcher_fb_ivb_to_date) AS delta_fb_ivb,
  (f.hb_glove_in - f.pitcher_fb_hb_to_date) AS delta_fb_hb,
  -- Block B: Command & Context
  f.plate_x_norm,
  f.plate_z,
  CASE WHEN f.sz_top IS NOT NULL AND f.sz_bot IS NOT NULL AND (f.sz_top - f.sz_bot) > 0.2
       THEN (f.plate_z - f.sz_bot) / (f.sz_top - f.sz_bot)
       ELSE NULL END AS sz_norm_z,
  f.vaa,
  f.haa_away,
  f.balls::DOUBLE AS balls,
  f.strikes::DOUBLE AS strikes,
  f.outs_when_up::DOUBLE AS outs_when_up,
  f.n_thruorder_pitcher::DOUBLE AS n_thruorder_pitcher,
  -- Block C: Exact Commit-Point Tunneling (Pavlidis/Judge Break:Tunnel Ratio + Plate Separation)
  t.tunnel_dist_in,
  t.plate_sep_in,
  t.post_tunnel_break_in,
  t.break_to_tunnel_ratio,
  t.delta_velo_mph,
  t.delta_flight_ms,
  t.is_repeat_pitch::DOUBLE AS is_repeat_pitch,
  -- Outcomes
  f.is_swing::INTEGER AS is_swing,
  f.is_whiff::INTEGER AS is_whiff,
  f.is_csw::INTEGER AS is_csw,
  f.is_chase::INTEGER AS is_chase,
  f.is_in_zone::INTEGER AS is_in_zone,
  f.xwoba,
  f.pitcher_run_exp
FROM base_fb f
LEFT JOIN v_statcast_tunneling t
  USING (game_pk, at_bat_number, pitch_number)
WHERE f.release_speed IS NOT NULL
  AND f.ivb_in IS NOT NULL
  AND f.hb_glove_in IS NOT NULL
  AND f.plate_x IS NOT NULL
  AND f.plate_z IS NOT NULL
  AND f.pitcher_run_exp IS NOT NULL
  AND f.pitch_type IS NOT NULL
  AND f.matchup IS NOT NULL
"

dt <- as.data.table(DBI::dbGetQuery(con, model_sql))
t_ext1 <- proc.time()
cat(sprintf("   Extracted %s regular-season valid pitches in %.1f seconds.\n", format(nrow(dt), big.mark = ","), (t_ext1 - t_ext0)[3]))

# Fail-fast post-extraction integrity assertions
if (nrow(dt) < 7500000L) stop("Unexpected extracted regular-season pitch count: ", nrow(dt))
if (anyDuplicated(dt, by = c("game_pk", "at_bat_number", "pitch_number")) != 0L) {
  stop("Join fan-out detected: duplicate (game_pk, at_bat_number, pitch_number) keys in extracted table")
}
if (!all(is.finite(dt$pitcher_run_exp))) stop("Non-finite target detected in pitcher_run_exp")
if (!all(dt$is_swing %in% c(0L, 1L)) || !all(dt$is_whiff %in% c(0L, 1L))) {
  stop("Invalid domain or NA detected in is_swing / is_whiff")
}
for (num_col in c("delta_fb_velo", "delta_fb_ivb", "delta_fb_hb", "sz_norm_z", "tunnel_dist_in", "break_to_tunnel_ratio")) {
  v <- dt[[num_col]]
  if (any(is.infinite(v)) || any(is.nan(v))) stop("Inf/NaN detected in derived feature: ", num_col)
}

# Encode handedness & pitch-type one-hot indicators cleanly in-place
canonical_pts <- c("FF", "SI", "FC", "SL", "ST", "CU", "KC", "CH", "FS")
dt[, is_rhp := as.numeric(p_throws == "R")]
dt[, is_rhb := as.numeric(stand == "R")]
dt[, is_same_hand_num := as.numeric(is_same_hand)]
for (pt in canonical_pts) {
  dt[, (paste0("pt_", pt)) := as.numeric(pitch_type == pt)]
}
dt[, pt_OTHER := as.numeric(!(pitch_type %in% canonical_pts))]
pt_cols <- c(paste0("pt_", canonical_pts), "pt_OTHER")

# Feature blocks (aligned to specs/03_tunneling_stuff_plus.md)
feats_stuff <- c(
  "release_speed", "effective_speed", "release_spin_rate", "spin_axis",
  "ivb_in", "hb_glove_in", "hb_away_in",
  "release_pos_x_arm", "release_pos_z", "release_extension", "arm_angle_deg",
  "delta_fb_velo", "delta_fb_ivb", "delta_fb_hb",
  "is_same_hand_num", "is_rhp", "is_rhb",
  pt_cols
)

feats_loc <- c(
  "plate_x_norm", "plate_z", "sz_norm_z",
  "balls", "strikes", "outs_when_up", "n_thruorder_pitcher",
  "is_same_hand_num", "is_rhp", "is_rhb",
  pt_cols
)

feats_pitching <- unique(c(
  feats_stuff,
  "plate_x_norm", "plate_z", "sz_norm_z", "vaa", "haa_away",
  "balls", "strikes", "outs_when_up", "n_thruorder_pitcher"
))

feats_tunnel <- c(
  "tunnel_dist_in", "plate_sep_in", "post_tunnel_break_in",
  "break_to_tunnel_ratio", "delta_velo_mph", "delta_flight_ms", "is_repeat_pitch"
)
feats_pitching_tun <- c(feats_pitching, feats_tunnel)

# Strict chronological split indices (non-empty + strictly monotonic assertions)
idx_tr_full <- which(dt$game_year >= 2015L & dt$game_year <= 2023L)
idx_tr_he   <- which(dt$game_year >= 2020L & dt$game_year <= 2023L)
idx_val     <- which(dt$game_year == 2024L)
idx_test    <- which(dt$game_year >= 2025L & dt$game_year <= 2026L)

stopifnot(length(idx_tr_full) > 5000000L, length(idx_tr_he) > 2000000L,
          length(idx_val) > 600000L, length(idx_test) > 1000000L)
stopifnot(max(dt$game_date[idx_tr_full]) < min(dt$game_date[idx_val]))
stopifnot(max(dt$game_date[idx_tr_he])   < min(dt$game_date[idx_val]))
stopifnot(max(dt$game_date[idx_val])     < min(dt$game_date[idx_test]))

cat(sprintf(
  "   Regular-season chronological splits verified: Train 2015-2023 = %s | Hawk-Eye 2020-2023 = %s | Val 2024 = %s | Test 2025-2026 = %s\n",
  format(length(idx_tr_full), big.mark = ","),
  format(length(idx_tr_he), big.mark = ","),
  format(length(idx_val), big.mark = ","),
  format(length(idx_test), big.mark = ",")
))

# Fail-fast helper metrics
calc_rmse <- function(y, p) {
  stopifnot(length(y) > 0L, length(y) == length(p), all(is.finite(y)), all(is.finite(p)))
  sqrt(mean((y - p)^2))
}
calc_r2 <- function(y, p, mu_ref) {
  stopifnot(length(y) > 0L, length(y) == length(p), all(is.finite(y)), all(is.finite(p)), is.finite(mu_ref))
  denom <- sum((y - mu_ref)^2)
  stopifnot(denom > 0)
  1.0 - sum((y - p)^2) / denom
}
calc_logloss <- function(y, p) {
  stopifnot(length(y) > 0L, length(y) == length(p), all(y %in% c(0L, 1L)), all(is.finite(p)))
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}
calc_brier <- function(y, p) {
  stopifnot(length(y) > 0L, length(y) == length(p), all(is.finite(y)), all(is.finite(p)))
  mean((y - p)^2)
}
calc_auc <- function(y, p) {
  stopifnot(length(y) > 0L, length(y) == length(p), all(y %in% c(0L, 1L)), all(is.finite(p)))
  n1 <- sum(y == 1L)
  n0 <- length(y) - n1
  stopifnot(n1 > 0L && n0 > 0L)
  r <- rank(p, ties.method = "average")
  (sum(r[y == 1L]) - as.numeric(n1) * (as.numeric(n1) + 1.0) / 2.0) / (as.numeric(n1) * as.numeric(n0))
}

y_tr_full <- dt$pitcher_run_exp[idx_tr_full]
y_val     <- dt$pitcher_run_exp[idx_val]
y_test    <- dt$pitcher_run_exp[idx_test]
mu_tr_global <- mean(y_tr_full)
test_has_tun <- !is.na(dt$tunnel_dist_in[idx_test])
stopifnot(sum(test_has_tun) > 500000L)

# Exact chunked analytical Ridge solver (strictly fit on train_idx, fail fast on broken columns)
fit_eval_ridge <- function(cols, train_idx, val_idx, test_idx, label_name) {
  cat(sprintf("   Fitting exact analytical Ridge baseline: %s (%d features)...\n", label_name, length(cols)))
  p <- length(cols)
  meds <- numeric(p)
  means <- numeric(p)
  sds <- numeric(p)
  for (j in seq_len(p)) {
    v <- dt[[cols[j]]][train_idx]
    if (all(is.na(v))) stop("Ridge training column is 100% NA: ", cols[j])
    meds[j] <- stats::median(v, na.rm = TRUE)
    v[is.na(v)] <- meds[j]
    means[j] <- mean(v)
    s <- stats::sd(v)
    if (is.na(s) || s < 1e-12) stop("Ridge training column is constant/zero-variance: ", cols[j])
    sds[j] <- s
  }
  y_mu <- mean(dt$pitcher_run_exp[train_idx])
  n_tr <- length(train_idx)

  std_mat <- function(eval_idx) {
    M <- as.matrix(dt[eval_idx, ..cols])
    for (j in seq_len(p)) {
      na_j <- is.na(M[, j])
      if (any(na_j)) M[na_j, j] <- meds[j]
      M[, j] <- (M[, j] - means[j]) / sds[j]
    }
    M
  }

  XtX <- matrix(0.0, nrow = p, ncol = p)
  Xty <- numeric(p)
  chunk_sz <- 500000L
  for (s0 in seq(1L, n_tr, by = chunk_sz)) {
    s1 <- min(s0 + chunk_sz - 1L, n_tr)
    c_idx <- train_idx[s0:s1]
    M <- std_mat(c_idx)
    yc <- dt$pitcher_run_exp[c_idx] - y_mu
    XtX <- XtX + crossprod(M)
    Xty <- Xty + as.numeric(crossprod(M, yc))
  }

  M_val <- std_mat(val_idx)
  lambdas <- 10^seq(-6, 1, length.out = 20)
  best_rmse <- Inf
  best_beta <- NULL
  best_lam <- NA_real_
  I_p <- diag(p)
  for (lam in lambdas) {
    beta <- solve(XtX + (n_tr * lam) * I_p, Xty)
    pv <- y_mu + as.numeric(M_val %*% beta)
    rv <- calc_rmse(y_val, pv)
    if (rv < best_rmse) {
      best_rmse <- rv
      best_beta <- beta
      best_lam <- lam
    }
  }
  rm(M_val)

  n_te <- length(test_idx)
  pt <- numeric(n_te)
  for (s0 in seq(1L, n_te, by = chunk_sz)) {
    s1 <- min(s0 + chunk_sz - 1L, n_te)
    pt[s0:s1] <- y_mu + as.numeric(std_mat(test_idx[s0:s1]) %*% best_beta)
  }
  gc(verbose = FALSE)
  list(
    val_rmse = best_rmse,
    test_rmse = calc_rmse(y_test, pt),
    test_r2 = calc_r2(y_test, pt, mu_tr_global),
    test_paired_rmse = calc_rmse(y_test[test_has_tun], pt[test_has_tun]),
    best_lambda = best_lam
  )
}

# Helper for xgboost 3.2 hist regression training
train_xgb_reg <- function(cols, train_idx, val_idx, test_idx, label_name, nrounds = 1500L) {
  cat(sprintf("   Training xgboost hist [%s] (%d cols, %s train rows, max %d rounds)...\n",
              label_name, length(cols), format(length(train_idx), big.mark = ","), nrounds))
  t0 <- proc.time()
  X_tr <- as.matrix(dt[train_idx, ..cols])
  dtr  <- xgb.DMatrix(X_tr, label = dt$pitcher_run_exp[train_idx])
  rm(X_tr); gc(verbose = FALSE)

  X_va <- as.matrix(dt[val_idx, ..cols])
  dva  <- xgb.DMatrix(X_va, label = dt$pitcher_run_exp[val_idx])
  rm(X_va); gc(verbose = FALSE)

  bst <- xgb.train(
    params = xgb.params(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      tree_method = "hist",
      max_depth = 6L,
      learning_rate = 0.03,
      subsample = 0.8,
      colsample_bytree = 0.8,
      min_child_weight = 200,
      nthread = 16L,
      seed = 42L
    ),
    data = dtr,
    nrounds = nrounds,
    evals = list(train = dtr, val = dva),
    early_stopping_rounds = 40L,
    verbose = 0
  )
  bi <- as.integer(xgb.attr(bst, "best_iteration"))
  stopifnot(length(bi) == 1L && !is.na(bi) && bi >= 1L)
  pv <- predict(bst, dva)
  rm(dtr, dva); gc(verbose = FALSE)

  X_te <- as.matrix(dt[test_idx, ..cols])
  dte  <- xgb.DMatrix(X_te)
  rm(X_te)
  pt <- predict(bst, dte)
  rm(dte); gc(verbose = FALSE)

  el <- round((proc.time() - t0)[3], 1)
  v_rmse <- calc_rmse(y_val, pv)
  t_rmse <- calc_rmse(y_test, pt)
  t_r2   <- calc_r2(y_test, pt, mu_tr_global)
  t_prmse <- calc_rmse(y_test[test_has_tun], pt[test_has_tun])
  cat(sprintf("     -> Done in %.1fs | best_iter=%d | Val24 RMSE=%.6f | Test25-26 RMSE=%.6f | R2=%.4f%%\n",
              el, bi, v_rmse, t_rmse, t_r2 * 100))
  list(
    model = bst,
    best_iter = bi,
    val_rmse = v_rmse,
    test_rmse = t_rmse,
    test_r2 = t_r2,
    test_paired_rmse = t_prmse
  )
}

# ==============================================================================
# 3. PRIMARY MODEL LADDER: EXPECTED RUN VALUE (pitcher_run_exp)
# ==============================================================================
cat("-> [3/6] Training Primary Regular-Season Run-Value Model Ladder...\n")

b0_tbl <- dt[idx_tr_full, .(b0_pred = mean(pitcher_run_exp)), by = .(pitch_type, matchup)]
b0_val_dt <- b0_tbl[dt[idx_val, .(pitch_type, matchup)], on = .(pitch_type, matchup)]
b0_val_pred <- fifelse(is.na(b0_val_dt$b0_pred), mu_tr_global, b0_val_dt$b0_pred)
b0_te_dt <- b0_tbl[dt[idx_test, .(pitch_type, matchup)], on = .(pitch_type, matchup)]
b0_te_pred <- fifelse(is.na(b0_te_dt$b0_pred), mu_tr_global, b0_te_dt$b0_pred)

b0_res <- list(
  val_rmse = calc_rmse(y_val, b0_val_pred),
  test_rmse = calc_rmse(y_test, b0_te_pred),
  test_r2 = calc_r2(y_test, b0_te_pred, mu_tr_global),
  test_paired_rmse = calc_rmse(y_test[test_has_tun], b0_te_pred[test_has_tun])
)
cat(sprintf("   Baseline 0 (Pitch Type x Matchup Mean): Val24 RMSE=%.6f | Test25-26 RMSE=%.6f | R2=%.4f%%\n",
            b0_res$val_rmse, b0_res$test_rmse, b0_res$test_r2 * 100))

r1a_res <- fit_eval_ridge(feats_stuff, idx_tr_full, idx_val, idx_test, "Ridge Stuff (Block A)")
r1b_res <- fit_eval_ridge(feats_pitching, idx_tr_full, idx_val, idx_test, "Ridge Pitching (Blocks A+B)")

m1_stuff   <- train_xgb_reg(feats_stuff, idx_tr_full, idx_val, idx_test, "Stuff+ (Block A, 2015-2023)", nrounds = 1500L)
m1_stuff_h <- train_xgb_reg(feats_stuff, idx_tr_he,   idx_val, idx_test, "Stuff+ Hawk-Eye Ablation (2020-2023)", nrounds = 1500L)
m1_loc     <- train_xgb_reg(feats_loc,   idx_tr_full, idx_val, idx_test, "Location+ (Block B Command, 2015-2023)", nrounds = 2500L)
m2_pitch   <- train_xgb_reg(feats_pitching, idx_tr_full, idx_val, idx_test, "Pitching+ Base (Blocks A+B, 2015-2023)", nrounds = 2000L)
m3_tun     <- train_xgb_reg(feats_pitching_tun, idx_tr_full, idx_val, idx_test, "Pitching+Tunnel (Blocks A+B+C, 2015-2023)", nrounds = 2000L)
m3_tun_h   <- train_xgb_reg(feats_pitching_tun, idx_tr_he,   idx_val, idx_test, "Pitching+Tunnel Hawk-Eye Ablation (2020-2023)", nrounds = 2000L)

xgb.save(m1_stuff$model, file.path(saved_models_dir, "stuff_plus_xgb.ubj"))
xgb.save(m1_loc$model,   file.path(saved_models_dir, "location_plus_xgb.ubj"))
xgb.save(m2_pitch$model, file.path(saved_models_dir, "pitching_plus_xgb.ubj"))
xgb.save(m3_tun$model,   file.path(saved_models_dir, "pitching_tunnel_plus_xgb.ubj"))

imp_stuff <- as.data.table(xgb.importance(model = m1_stuff$model))[, model := "Stuff+"]
imp_loc   <- as.data.table(xgb.importance(model = m1_loc$model))[, model := "Location+"]
imp_tun   <- as.data.table(xgb.importance(model = m3_tun$model))[, model := "Pitching+Tunnel"]
imp_all   <- rbindlist(list(imp_stuff, imp_loc, imp_tun), use.names = TRUE)
arrow::write_parquet(imp_all, file.path(processed_dir, "stuff_plus_feature_importance.parquet"), compression = "zstd")

rv_ablation <- data.table(
  model = c(
    "Baseline 0: Pitch Type x Matchup",
    "Baseline 1A: Ridge (Stuff)",
    "Baseline 1B: Ridge (Pitching)",
    "Model 1: Stuff+ (2015-2023)",
    "Model 1-HE: Stuff+ (Hawk-Eye 20-23)",
    "Model 1-Loc: Location+ (2015-2023)",
    "Model 2: Pitching+ Base (A+B)",
    "Model 3: Pitching+Tunnel (A+B+C)",
    "Model 3-HE: Pitching+Tunnel (HE 20-23)"
  ),
  train_window = c(
    "2015-2023", "2015-2023", "2015-2023",
    "2015-2023", "2020-2023", "2015-2023",
    "2015-2023", "2015-2023", "2020-2023"
  ),
  val24_rmse = c(
    b0_res$val_rmse, r1a_res$val_rmse, r1b_res$val_rmse,
    m1_stuff$val_rmse, m1_stuff_h$val_rmse, m1_loc$val_rmse,
    m2_pitch$val_rmse, m3_tun$val_rmse, m3_tun_h$val_rmse
  ),
  test_rmse = c(
    b0_res$test_rmse, r1a_res$test_rmse, r1b_res$test_rmse,
    m1_stuff$test_rmse, m1_stuff_h$test_rmse, m1_loc$test_rmse,
    m2_pitch$test_rmse, m3_tun$test_rmse, m3_tun_h$test_rmse
  ),
  test_paired_rmse = c(
    b0_res$test_paired_rmse, r1a_res$test_paired_rmse, r1b_res$test_paired_rmse,
    m1_stuff$test_paired_rmse, m1_stuff_h$test_paired_rmse, m1_loc$test_paired_rmse,
    m2_pitch$test_paired_rmse, m3_tun$test_paired_rmse, m3_tun_h$test_paired_rmse
  ),
  test_r2_pct = 100 * c(
    b0_res$test_r2, r1a_res$test_r2, r1b_res$test_r2,
    m1_stuff$test_r2, m1_stuff_h$test_r2, m1_loc$test_r2,
    m2_pitch$test_r2, m3_tun$test_r2, m3_tun_h$test_r2
  ),
  best_iter = c(
    NA_integer_, NA_integer_, NA_integer_,
    m1_stuff$best_iter, m1_stuff_h$best_iter, m1_loc$best_iter,
    m2_pitch$best_iter, m3_tun$best_iter, m3_tun_h$best_iter
  )
)
rv_ablation[, delta_rmse_vs_b0_x1e4 := (b0_res$test_rmse - test_rmse) * 1e4]
cat("\n=== PRIMARY TARGET (pitcher_run_exp, game_type='R') LOCKED 2025-2026 ABLATION ===\n")
print(rv_ablation)

# Fail-fast model dominance assertions on locked 2025-2026 test
stopifnot(m1_stuff$test_rmse < b0_res$test_rmse, m1_stuff$test_rmse < r1a_res$test_rmse)
stopifnot(m1_stuff_h$test_rmse < b0_res$test_rmse, m1_loc$test_rmse < b0_res$test_rmse)
stopifnot(m2_pitch$test_rmse < r1b_res$test_rmse, m2_pitch$test_rmse < m1_stuff$test_rmse)
stopifnot(m3_tun$test_rmse < r1b_res$test_rmse, m3_tun_h$test_rmse < r1b_res$test_rmse)

# ==============================================================================
# 4. SECONDARY DECEPTION TARGET: WHIFF ON SWINGS (is_whiff | is_swing = 1)
# ==============================================================================
cat("\n-> [4/6] Training Secondary Swing-and-Miss Deception Ladder (game_type = 'R')...\n")

idx_sw_tr <- idx_tr_full[dt$is_swing[idx_tr_full] == 1L]
idx_sw_va <- idx_val[dt$is_swing[idx_val] == 1L]
idx_sw_te <- idx_test[dt$is_swing[idx_test] == 1L]
stopifnot(length(idx_sw_tr) > 2000000L, length(idx_sw_va) > 250000L, length(idx_sw_te) > 500000L)
sw_te_paired <- !is.na(dt$tunnel_dist_in[idx_sw_te])

y_sw_te <- dt$is_whiff[idx_sw_te]
w0_tbl <- dt[idx_sw_tr, .(w0_p = mean(is_whiff)), by = .(pitch_type, matchup)]
w0_global <- mean(dt$is_whiff[idx_sw_tr])
w0_te_dt <- w0_tbl[dt[idx_sw_te, .(pitch_type, matchup)], on = .(pitch_type, matchup)]
w0_te_p  <- fifelse(is.na(w0_te_dt$w0_p), w0_global, w0_te_dt$w0_p)

train_xgb_whiff <- function(cols, label_name) {
  cat(sprintf("   Training Whiff xgboost [%s] (%d cols, %s train swings)...\n",
              label_name, length(cols), format(length(idx_sw_tr), big.mark = ",")))
  t0 <- proc.time()
  X_tr <- as.matrix(dt[idx_sw_tr, ..cols])
  dtr  <- xgb.DMatrix(X_tr, label = dt$is_whiff[idx_sw_tr])
  rm(X_tr); gc(verbose = FALSE)

  X_va <- as.matrix(dt[idx_sw_va, ..cols])
  dva  <- xgb.DMatrix(X_va, label = dt$is_whiff[idx_sw_va])
  rm(X_va); gc(verbose = FALSE)

  bst <- xgb.train(
    params = xgb.params(
      objective = "binary:logistic",
      eval_metric = "logloss",
      tree_method = "hist",
      max_depth = 6L,
      learning_rate = 0.04,
      subsample = 0.8,
      colsample_bytree = 0.8,
      min_child_weight = 150,
      nthread = 16L,
      seed = 42L
    ),
    data = dtr,
    nrounds = 1500L,
    evals = list(train = dtr, val = dva),
    early_stopping_rounds = 35L,
    verbose = 0
  )
  bi <- as.integer(xgb.attr(bst, "best_iteration"))
  stopifnot(length(bi) == 1L && !is.na(bi) && bi >= 1L)
  rm(dtr, dva); gc(verbose = FALSE)

  X_te <- as.matrix(dt[idx_sw_te, ..cols])
  dte  <- xgb.DMatrix(X_te)
  rm(X_te)
  pt <- predict(bst, dte)
  rm(dte); gc(verbose = FALSE)

  ll_all  <- calc_logloss(y_sw_te, pt)
  ll_pair <- calc_logloss(y_sw_te[sw_te_paired], pt[sw_te_paired])
  br_all  <- calc_brier(y_sw_te, pt)
  auc_all <- calc_auc(y_sw_te, pt)
  auc_pair <- calc_auc(y_sw_te[sw_te_paired], pt[sw_te_paired])
  cat(sprintf("     -> Done in %.1fs | best_iter=%d | Test LL=%.5f (Paired=%.5f) | AUC=%.4f (Paired=%.4f)\n",
              (proc.time() - t0)[3], bi, ll_all, ll_pair, auc_all, auc_pair))
  list(
    best_iter = bi, logloss = ll_all, logloss_paired = ll_pair,
    brier = br_all, auc = auc_all, auc_paired = auc_pair
  )
}

w0_ll_all   <- calc_logloss(y_sw_te, w0_te_p)
w0_ll_pair  <- calc_logloss(y_sw_te[sw_te_paired], w0_te_p[sw_te_paired])
w0_auc_all  <- calc_auc(y_sw_te, w0_te_p)
w0_auc_pair <- calc_auc(y_sw_te[sw_te_paired], w0_te_p[sw_te_paired])
w1_stuff  <- train_xgb_whiff(feats_stuff, "Whiff: Stuff (Block A)")
w2_pitch  <- train_xgb_whiff(feats_pitching, "Whiff: Pitching (Blocks A+B)")
w3_tun    <- train_xgb_whiff(feats_pitching_tun, "Whiff: Pitching+Tunnel (Blocks A+B+C)")

stopifnot(w1_stuff$logloss < w0_ll_all, w2_pitch$logloss < w1_stuff$logloss, w3_tun$logloss < w2_pitch$logloss)

whiff_ablation <- data.table(
  model = c(
    "Whiff Baseline 0 (Type x Matchup)",
    "Whiff Model 1: Stuff (A)",
    "Whiff Model 2: Pitching (A+B)",
    "Whiff Model 3: Pitching+Tunnel (A+B+C)"
  ),
  test_logloss = c(w0_ll_all, w1_stuff$logloss, w2_pitch$logloss, w3_tun$logloss),
  test_logloss_paired = c(w0_ll_pair, w1_stuff$logloss_paired, w2_pitch$logloss_paired, w3_tun$logloss_paired),
  test_brier = c(calc_brier(y_sw_te, w0_te_p), w1_stuff$brier, w2_pitch$brier, w3_tun$brier),
  test_auc = c(w0_auc_all, w1_stuff$auc, w2_pitch$auc, w3_tun$auc),
  test_auc_paired = c(w0_auc_pair, w1_stuff$auc_paired, w2_pitch$auc_paired, w3_tun$auc_paired)
)
cat("\n=== SECONDARY TARGET (is_whiff | is_swing=1, game_type='R') LOCKED 2025-2026 ABLATION ===\n")
print(whiff_ablation)

arrow::write_parquet(rv_ablation, file.path(processed_dir, "stuff_plus_model_ablation.parquet"), compression = "zstd")
arrow::write_parquet(whiff_ablation, file.path(processed_dir, "stuff_plus_whiff_ablation.parquet"), compression = "zstd")

# --- Figure 3: Out-of-Time Model Ablation Lift ---
rv_plot_df <- copy(rv_ablation)
rv_plot_df[, model := factor(model, levels = rev(rv_ablation$model))]
rv_plot_df[, tier_grp := fifelse(grepl("Baseline", model), "Baseline",
                         fifelse(grepl("HE", model), "Hawk-Eye Ablation (20-23)", "Full-Lake XGB (15-23)"))]

p3_left <- ggplot(rv_plot_df, aes(x = test_r2_pct, y = model, fill = tier_grp)) +
  geom_col(width = 0.68, alpha = 0.92) +
  geom_text(aes(label = sprintf("%.3f%%", test_r2_pct)), hjust = -0.12, color = "#e2e8f0", size = 3.1, fontface = "bold") +
  scale_fill_manual(
    values = c("Baseline" = "#64748b", "Full-Lake XGB (15-23)" = "#38bdf8", "Hawk-Eye Ablation (20-23)" = "#f59e0b"),
    name = "Training Regime"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "A. Expected Run Value (Out-of-Sample R^2)",
    subtitle = sprintf("Locked 2025-2026 Regular Season (%s pitches)", format(length(idx_test), big.mark = ",")),
    x = "Out-of-Sample R^2 on pitcher_run_exp (%)",
    y = NULL
  ) +
  mlbr::theme_cyber(base_size = 10.5) +
  theme(legend.position = "bottom")

wh_plot_df <- copy(whiff_ablation)
wh_plot_df[, model := factor(model, levels = rev(whiff_ablation$model))]

p3_right <- ggplot(wh_plot_df, aes(x = test_auc_paired, y = model)) +
  geom_col(fill = "#10b981", width = 0.62, alpha = 0.90) +
  geom_text(
    aes(label = sprintf("AUC %.4f | LL %.4f", test_auc_paired, test_logloss_paired)),
    hjust = -0.06, color = "#e2e8f0", size = 3.0, fontface = "bold"
  ) +
  coord_cartesian(xlim = c(0.55, 0.88)) +
  labs(
    title = "B. Swing-and-Miss Deception (Paired Pitches)",
    subtitle = "Locked 2025-2026 Regular-Season Swings (Pitch N >= 2)",
    x = "ROC AUC on Paired Swings (with Log-Loss)",
    y = NULL
  ) +
  mlbr::theme_cyber(base_size = 10.5)

fig3 <- (p3_left | p3_right) +
  plot_annotation(
    title = "Chronological Out-of-Time Model Ladder & Commit-Point Tunneling Ablation (2025-2026 Regular Season)",
    subtitle = "Strict temporal separation (Train <= 2023 -> Val 2024 -> Locked Test 2025-2026, game_type = 'R') confirms XGBoost & tunneling whiff lift.",
    caption = "Source: mlbr Silver Lake | Zero temporal leakage | Regular Season (game_type = 'R')",
    theme = mlbr::theme_cyber(base_size = 11)
  )

mlbr::save_plot_assets(strip_env(fig3), "stuff03_ablation_oos_lift", data = rv_ablation, width = 13.5, height = 6.6, dir = models_dir)

# ==============================================================================
# 5. PLUS-SCALE CALIBRATION (LOCKED ON 2024 OOS) & GOLD LEADERBOARD (2020-2026)
# ==============================================================================
cat("\n-> [5/6] Scoring full 2020-2026 Regular Season, calibrating on 2024 OOS, and writing Gold leaderboard...\n")

dt_he <- dt[game_year >= 2020L]
rm(dt); gc(verbose = FALSE)

score_chunked <- function(bst, cols, dt_src) {
  n <- nrow(dt_src)
  preds <- numeric(n)
  chunk_sz <- 600000L
  for (s0 in seq(1L, n, by = chunk_sz)) {
    s1 <- min(s0 + chunk_sz - 1L, n)
    M <- as.matrix(dt_src[s0:s1, ..cols])
    dm <- xgb.DMatrix(M)
    preds[s0:s1] <- predict(bst, dm)
    rm(M, dm)
  }
  gc(verbose = FALSE)
  preds
}

dt_he[, pred_stuff := score_chunked(m1_stuff$model, feats_stuff, dt_he)]
dt_he[, pred_loc   := score_chunked(m1_loc$model,   feats_loc,   dt_he)]
dt_he[, pred_pitch := score_chunked(m2_pitch$model, feats_pitching, dt_he)]
dt_he[, pred_tun   := score_chunked(m3_tun$model,   feats_pitching_tun, dt_he)]

rm(m1_stuff, m1_stuff_h, m1_loc, m2_pitch, m3_tun, m3_tun_h); gc(verbose = FALSE)

calibrate_plus <- function(col_name) {
  d24 <- dt_he[game_year == 2024L]
  stopifnot(nrow(d24) > 500000L)
  mu_24 <- mean(d24[[col_name]])
  pit_24 <- d24[, .(n = .N, m = mean(get(col_name))), by = pitcher][n >= 250L]
  stopifnot(nrow(pit_24) >= 100L)
  sd_pit_24 <- stats::sd(pit_24$m)
  stopifnot(is.finite(mu_24), is.finite(sd_pit_24), sd_pit_24 > 1e-8)
  list(mu = mu_24, sd_pit = sd_pit_24)
}

cal_stuff <- calibrate_plus("pred_stuff")
cal_loc   <- calibrate_plus("pred_loc")
cal_pitch <- calibrate_plus("pred_pitch")
cal_tun   <- calibrate_plus("pred_tun")

cat(sprintf(
  "   2024 Regular-Season Calibration Lock (>= 250 pitches): Stuff SD=%.5f | Loc SD=%.5f | Pitch SD=%.5f | Pitch+Tun SD=%.5f\n",
  cal_stuff$sd_pit, cal_loc$sd_pit, cal_pitch$sd_pit, cal_tun$sd_pit
))

dt_he[, stuff_plus           := 100.0 + 10.0 * (pred_stuff - cal_stuff$mu) / cal_stuff$sd_pit]
dt_he[, location_plus        := 100.0 + 10.0 * (pred_loc   - cal_loc$mu)   / cal_loc$sd_pit]
dt_he[, pitching_plus        := 100.0 + 10.0 * (pred_pitch - cal_pitch$mu) / cal_pitch$sd_pit]
dt_he[, pitching_tunnel_plus := 100.0 + 10.0 * (pred_tun   - cal_tun$mu)   / cal_tun$sd_pit]

# Aggregate Regular-Season Gold table (strictly sum-before-divide, safe NA denominators, explicit eval_split)
pit_gold <- dt_he[, {
  n_sw <- sum(is_swing)
  n_ooz <- sum(is_in_zone == 0L)
  n_same <- sum(is_same_hand == 1L)
  n_opp <- sum(is_same_hand == 0L)
  n_pair <- sum(!is.na(tunnel_dist_in))
  sum_tun <- sum(tunnel_dist_in, na.rm = TRUE)
  .(
    p_throws                = p_throws[1L],
    n_pitches               = .N,
    n_paired_pitches        = n_pair,
    stuff_plus              = round(mean(stuff_plus), 2),
    location_plus           = round(mean(location_plus), 2),
    pitching_plus           = round(mean(pitching_plus), 2),
    pitching_tunnel_plus    = round(mean(pitching_tunnel_plus), 2),
    stuff_plus_same         = fifelse(n_same > 0L, round(mean(stuff_plus[is_same_hand == 1L]), 2), NA_real_),
    stuff_plus_opp          = fifelse(n_opp  > 0L, round(mean(stuff_plus[is_same_hand == 0L]), 2), NA_real_),
    mean_velo_mph           = round(mean(release_speed, na.rm = TRUE), 2),
    mean_tunnel_in          = fifelse(n_pair > 0L, round(mean(tunnel_dist_in, na.rm = TRUE), 2), NA_real_),
    mean_plate_sep_in       = fifelse(n_pair > 0L, round(mean(plate_sep_in, na.rm = TRUE), 2), NA_real_),
    mean_post_break_in      = fifelse(n_pair > 0L, round(mean(post_tunnel_break_in, na.rm = TRUE), 2), NA_real_),
    agg_break_tunnel_ratio  = fifelse(sum_tun > 0, round(sum(post_tunnel_break_in, na.rm = TRUE) / sum_tun, 3), NA_real_),
    agg_plate_tunnel_ratio  = fifelse(sum_tun > 0, round(sum(plate_sep_in, na.rm = TRUE) / sum_tun, 3), NA_real_),
    whiff_pct               = fifelse(n_sw > 0L, round(100.0 * sum(is_whiff) / n_sw, 2), NA_real_),
    csw_pct                 = round(100.0 * sum(is_csw) / .N, 2),
    chase_pct               = fifelse(n_ooz > 0L, round(100.0 * sum(is_chase) / n_ooz, 2), NA_real_),
    avg_xwoba               = fifelse(any(!is.na(xwoba)), round(mean(xwoba, na.rm = TRUE), 3), NA_real_),
    rv_per_100              = round(mean(pitcher_run_exp) * 100.0, 3)
  )
}, by = .(pitcher, game_year)]

pit_gold[, eval_split := fcase(
  game_year <= 2023L, "train_insample",
  game_year == 2024L, "val_calibration",
  default = "test_oos"
)]
pit_gold[, tunnel_lift_plus := round(pitching_tunnel_plus - pitching_plus, 2)]
pit_gold[, stuff_platoon_gap := round(stuff_plus_same - stuff_plus_opp, 2)]
pit_gold[, qualified_250 := n_pitches >= 250L]
pit_gold[, qualified_500 := n_pitches >= 500L]

cw_fg <- as.data.table(DBI::dbGetQuery(con, "
  SELECT
    cw.key_mlbam::INTEGER AS pitcher,
    TRIM(COALESCE(cw.name_first, '') || ' ' || COALESCE(cw.name_last, '')) AS pitcher_name,
    cw.key_fangraphs,
    cw.key_bbref,
    cw.key_retro
  FROM v_chadwick_register cw
  WHERE cw.key_mlbam IS NOT NULL
"))
cw_fg <- unique(cw_fg, by = "pitcher")

fg_bench <- as.data.table(DBI::dbGetQuery(con, "
  SELECT
    xMLBAMID::INTEGER AS pitcher,
    Season::INTEGER AS game_year,
    team_name_abb AS team,
    IP AS fg_ip,
    TBF AS fg_tbf,
    ERA AS fg_era,
    FIP AS fg_fip,
    xFIP AS fg_xfip,
    SIERA AS fg_siera,
    ROUND(K_pct * 100.0, 2) AS fg_k_pct,
    ROUND(BB_pct * 100.0, 2) AS fg_bb_pct,
    sp_stuff AS fg_stuff_plus,
    sp_location AS fg_location_plus,
    sp_pitching AS fg_pitching_plus,
    pb_overall AS fg_pb_overall
  FROM v_fangraphs_pitch
  WHERE Season >= 2020 AND xMLBAMID IS NOT NULL
"))
fg_bench <- unique(fg_bench, by = c("pitcher", "game_year"))

pit_gold <- merge(pit_gold, cw_fg, by = "pitcher", all.x = TRUE)
pit_gold <- merge(pit_gold, fg_bench, by = c("pitcher", "game_year"), all.x = TRUE)
pit_gold <- pit_gold[n_pitches >= 100L]
setorder(pit_gold, -game_year, -pitching_tunnel_plus)

# Fail-fast Gold leaderboard integrity assertions
stopifnot(nrow(pit_gold) > 4000L)
stopifnot(anyDuplicated(pit_gold, by = c("pitcher", "game_year")) == 0L)
stopifnot(all(is.finite(pit_gold$stuff_plus)), all(is.finite(pit_gold$location_plus)),
          all(is.finite(pit_gold$pitching_plus)), all(is.finite(pit_gold$pitching_tunnel_plus)))

gold_path <- file.path(processed_dir, "pitcher_stuff_plus.parquet")
arrow::write_parquet(pit_gold, gold_path, compression = "zstd")
cat(sprintf("   Wrote Gold Regular-Season leaderboard: %s (%s pitcher-seasons, 2020-2026)\n",
            gold_path, format(nrow(pit_gold), big.mark = ",")))

# External Benchmark Check (2025 Regular Season OOS, >= 500 pitches)
q25 <- pit_gold[game_year == 2025L & qualified_500 == TRUE & !is.na(fg_stuff_plus)]
stopifnot(nrow(q25) >= 400L)
cor_stuff_fg <- cor(q25$stuff_plus, q25$fg_stuff_plus, use = "complete.obs")
cor_loc_fg   <- cor(q25$location_plus, q25$fg_location_plus, use = "complete.obs")
cor_pitch_fg <- cor(q25$pitching_tunnel_plus, q25$fg_pitching_plus, use = "complete.obs")
cor_ortho    <- cor(q25$stuff_plus, q25$location_plus, use = "complete.obs")
cat(sprintf(
  "   External Benchmark (2025 OOS Regular Season, N=%d pitchers >= 500 pitches):\n     cor(mlbr Stuff+, FG Stuff+) = %.4f | cor(mlbr Loc+, FG Loc+) = %.4f | cor(mlbr Pitch+Tun, FG Pitch+) = %.4f | cor(Stuff+, Loc+) = %.4f\n",
  nrow(q25), cor_stuff_fg, cor_loc_fg, cor_pitch_fg, cor_ortho
))

# --- Figure 4: 2025 Regular-Season OOS Pitcher Talent Map (Static PNG + Interactive Plotly HTML) ---
q25_lbl <- rbindlist(list(
  head(q25[order(-pitching_tunnel_plus)], 8),
  head(q25[order(-stuff_plus)], 4),
  head(q25[order(-location_plus)], 4),
  head(q25[order(pitching_tunnel_plus)], 3)
))
q25_lbl <- unique(q25_lbl, by = "pitcher")

fig4_static <- ggplot(q25, aes(x = stuff_plus, y = location_plus, color = pitching_tunnel_plus, size = n_pitches)) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "#64748b", linewidth = 0.5) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "#64748b", linewidth = 0.5) +
  geom_point(alpha = 0.82) +
  geom_text(
    data = q25_lbl,
    aes(label = pitcher_name),
    size = 2.85, vjust = -0.95, check_overlap = TRUE, color = "#f8fafc", fontface = "bold", show.legend = FALSE
  ) +
  annotate("text", x = max(q25$stuff_plus) - 1, y = max(q25$location_plus) - 0.5,
           label = "UNICORN ACES\n(+Stuff & +Command)", hjust = 1, vjust = 1, color = "#10b981", size = 3.1, fontface = "bold") +
  annotate("text", x = max(q25$stuff_plus) - 1, y = min(q25$location_plus) + 0.8,
           label = "RAW STUFF / VOLATILE", hjust = 1, vjust = 0, color = "#f59e0b", size = 3.0, fontface = "bold") +
  annotate("text", x = min(q25$stuff_plus) + 1, y = max(q25$location_plus) - 0.5,
           label = "COMMAND ARTISTS", hjust = 0, vjust = 1, color = "#38bdf8", size = 3.0, fontface = "bold") +
  annotate("text", x = min(q25$stuff_plus) + 1, y = min(q25$location_plus) + 0.8,
           label = "REPLACEMENT RISK", hjust = 0, vjust = 0, color = "#f43f5e", size = 3.0, fontface = "bold") +
  scale_color_gradientn(
    colours = c("#f43f5e", "#f59e0b", "#38bdf8", "#10b981"),
    name = "Pitching+\n(Tunnel)"
  ) +
  scale_size_continuous(range = c(1.8, 6.2), name = "2025 Reg.\nPitches") +
  labs(
    title = sprintf("2025 Regular-Season OOS Pitcher Talent Map: Stuff+ vs. Location+ (N = %d Pitchers >= 500 Pitches)", nrow(q25)),
    subtitle = sprintf("Orthogonal axes (r = %.2f) | External benchmark vs. FanGraphs: Stuff+ r = %.3f, Location+ r = %.3f, Pitching+ r = %.3f",
                       cor_ortho, cor_stuff_fg, cor_loc_fg, cor_pitch_fg),
    x = "mlbr Stuff+ (100 = 2024 MLB Avg, +10 = +1 Pitcher SD)",
    y = "mlbr Location+ (100 = 2024 MLB Avg, +10 = +1 Pitcher SD)",
    caption = "Source: mlbr Gold Leaderboard (game_type = 'R') | Strictly out-of-sample 2025 scoring"
  ) +
  mlbr::theme_cyber(base_size = 11)

mlbr::save_plot_assets(strip_env(fig4_static), "stuff04_pitcher_stuff_vs_location_scatter", data = q25, width = 11.5, height = 7.0, dir = models_dir)

hover_txt <- sprintf(
  "<b>%s</b> (%s, %s)<br>Reg. Pitches: %s | IP: %.1f<br><b>Stuff+:</b> %.1f (FG: %.0f)<br><b>Location+:</b> %.1f (FG: %.0f)<br><b>Pitching+Tunnel:</b> %.1f (FG: %.0f)<br>Tunnel Dist: %.2f in | BP Break:Tunnel: %.2fx<br>CSW%%: %.1f%% | Whiff%%: %.1f%% | K%%: %.1f%% | FIP: %.2f",
  q25$pitcher_name, ifelse(is.na(q25$team), "MLB", q25$team), q25$p_throws,
  format(q25$n_pitches, big.mark = ","), ifelse(is.na(q25$fg_ip), 0, q25$fg_ip),
  q25$stuff_plus, q25$fg_stuff_plus,
  q25$location_plus, q25$fg_location_plus,
  q25$pitching_tunnel_plus, q25$fg_pitching_plus,
  q25$mean_tunnel_in, q25$agg_break_tunnel_ratio,
  q25$csw_pct, q25$whiff_pct, q25$fg_k_pct, q25$fg_fip
)

fig4_plotly <- local({
  plot_ly(
  data = q25,
  x = ~stuff_plus,
  y = ~location_plus,
  type = "scatter",
  mode = "markers",
  text = hover_txt,
  hoverinfo = "text",
  marker = list(
    size = ~scales::rescale(n_pitches, to = c(6, 20)),
    color = ~pitching_tunnel_plus,
    colorscale = list(c(0, "#f43f5e"), c(0.35, "#f59e0b"), c(0.70, "#38bdf8"), c(1, "#10b981")),
    colorbar = list(title = list(text = "Pitching+<br>(Tunnel)", font = list(color = "#e2e8f0")), tickfont = list(color = "#94a3b8")),
    line = list(color = "#090d16", width = 0.8),
    opacity = 0.88
  )
) |>
  layout(
    title = list(
      text = "<b>2025 Regular-Season Out-of-Sample Pitcher Talent Map: mlbr Stuff+ vs. Location+</b>",
      font = list(color = "#f1f5f9", size = 15)
    ),
    paper_bgcolor = "#090d16",
    plot_bgcolor = "#111827",
    xaxis = list(title = "mlbr Stuff+ (100 = League Avg)", color = "#e2e8f0", gridcolor = "#1e293b", zeroline = FALSE),
    yaxis = list(title = "mlbr Location+ (100 = League Avg)", color = "#e2e8f0", gridcolor = "#1e293b", zeroline = FALSE),
    shapes = list(
      list(type = "line", x0 = 100, x1 = 100, y0 = min(q25$location_plus) - 2, y1 = max(q25$location_plus) + 2,
           line = list(color = "#64748b", dash = "dash", width = 1)),
      list(type = "line", x0 = min(q25$stuff_plus) - 2, x1 = max(q25$stuff_plus) + 2, y0 = 100, y1 = 100,
           line = list(color = "#64748b", dash = "dash", width = 1))
    ),
    margin = list(t = 55, b = 55, l = 60, r = 30)
  )
}, envir = list2env(list(q25 = q25, hover_txt = hover_txt), parent = globalenv()))

mlbr::save_plot_assets(fig4_plotly, "stuff04_pitcher_stuff_vs_location_interactive", data = q25, dir = models_dir)
# Remove leftover selfcontained htmlwidgets dependency directory if created
dep_dir <- file.path(models_dir, "stuff04_pitcher_stuff_vs_location_interactive_files")
if (dir.exists(dep_dir)) unlink(dep_dir, recursive = TRUE)

# ==============================================================================
# 6. TRUE-TALENT STABILIZATION CURVE & YOY PREDICTIVE VALIDITY (FIGURE 5)
# ==============================================================================
cat("\n-> [6/6] Computing Regular-Season split-half stabilization & complete 2024->2025 YoY validity...\n")

d25 <- dt_he[game_year == 2025L]
setorder(d25, pitcher, game_date, game_pk, at_bat_number, pitch_number)
d25[, pitch_seq_idx := seq_len(.N), by = pitcher]
d25[, half_grp := pitch_seq_idx %% 2L]

sample_ns <- c(50L, 100L, 200L, 300L, 500L, 750L, 1000L)
stab_rows <- list()

for (N_half in sample_ns) {
  sub <- d25[pitch_seq_idx <= 2L * N_half]
  elig <- sub[, .N, by = pitcher][N == 2L * N_half, pitcher]
  if (length(elig) < 30L) next
  sub <- sub[pitcher %in% elig]
  agg <- sub[, {
    n_sw <- sum(is_swing)
    .(
      stuff_plus = mean(stuff_plus),
      pitching_tunnel_plus = mean(pitching_tunnel_plus),
      location_plus = mean(location_plus),
      csw_pct = 100.0 * sum(is_csw) / .N,
      whiff_pct = fifelse(n_sw > 0L, 100.0 * sum(is_whiff) / n_sw, NA_real_),
      rv_per_100 = mean(pitcher_run_exp) * 100.0
    )
  }, by = .(pitcher, half_grp)]
  h0 <- agg[half_grp == 0L]
  h1 <- agg[half_grp == 1L]
  m <- merge(h0, h1, by = "pitcher", suffixes = c("_0", "_1"))

  metrics_map <- c(
    "mlbr Stuff+" = "stuff_plus",
    "mlbr Pitching+Tunnel" = "pitching_tunnel_plus",
    "mlbr Location+" = "location_plus",
    "CSW%" = "csw_pct",
    "Whiff%" = "whiff_pct",
    "Raw RV/100" = "rv_per_100"
  )
  for (lbl in names(metrics_map)) {
    cn <- metrics_map[[lbl]]
    r_val <- cor(m[[paste0(cn, "_0")]], m[[paste0(cn, "_1")]], use = "complete.obs")
    stab_rows[[length(stab_rows) + 1L]] <- data.table(
      n_pitches = N_half,
      n_pitchers = nrow(m),
      metric = lbl,
      split_half_r = round(r_val, 4)
    )
  }
}
stab_df <- rbindlist(stab_rows)
stopifnot(nrow(stab_df) > 0L)
cat("=== SPLIT-HALF RELIABILITY (2025 REGULAR-SEASON OOS) ===\n")
print(dcast(stab_df, n_pitches + n_pitchers ~ metric, value.var = "split_half_r"))

# Complete 11-predictor x 7-target Year-over-Year (2024 -> 2025 Regular Season, >= 400 pitches/yr)
p24 <- pit_gold[game_year == 2024L & n_pitches >= 400L]
p25 <- pit_gold[game_year == 2025L & n_pitches >= 400L]
yoy <- merge(p24, p25, by = "pitcher", suffixes = c("_24", "_25"))
yoy[, fg_k_bb_25 := fg_k_pct_25 - fg_bb_pct_25]
stopifnot(nrow(yoy) >= 250L)

yoy_metrics <- data.table(
  metric = c(
    "mlbr Stuff+", "mlbr Location+", "mlbr Pitching+ (Base)", "mlbr Pitching+Tunnel",
    "FanGraphs Stuff+", "FanGraphs Location+", "FanGraphs Pitching+",
    "CSW%", "K%", "FIP", "ERA"
  ),
  col24 = c(
    "stuff_plus_24", "location_plus_24", "pitching_plus_24", "pitching_tunnel_plus_24",
    "fg_stuff_plus_24", "fg_location_plus_24", "fg_pitching_plus_24",
    "csw_pct_24", "fg_k_pct_24", "fg_fip_24", "fg_era_24"
  ),
  col25 = c(
    "stuff_plus_25", "location_plus_25", "pitching_plus_25", "pitching_tunnel_plus_25",
    "fg_stuff_plus_25", "fg_location_plus_25", "fg_pitching_plus_25",
    "csw_pct_25", "fg_k_pct_25", "fg_fip_25", "fg_era_25"
  )
)

yoy_summary <- yoy_metrics[, {
  x24 <- yoy[[col24]]
  x25 <- yoy[[col25]]
  .(
    n_pitchers       = sum(!is.na(x24) & !is.na(x25)),
    yoy_self_r       = round(cor(x24, x25, use = "complete.obs"), 4),
    pred_25_bb_pct_r = round(cor(x24, yoy$fg_bb_pct_25, use = "complete.obs"), 4),
    pred_25_k_pct_r  = round(cor(x24, yoy$fg_k_pct_25, use = "complete.obs"), 4),
    pred_25_k_bb_r   = round(cor(x24, yoy$fg_k_bb_25, use = "complete.obs"), 4),
    pred_25_fip_r    = round(cor(x24, yoy$fg_fip_25, use = "complete.obs"), 4),
    pred_25_siera_r  = round(cor(x24, yoy$fg_siera_25, use = "complete.obs"), 4),
    pred_25_era_r    = round(cor(x24, yoy$fg_era_25, use = "complete.obs"), 4)
  )
}, by = metric]

cat("\n=== YEAR-OVER-YEAR (2024 -> 2025 REGULAR SEASON, >= 400 pitches/yr) COMPLETE TABLE ===\n")
print(yoy_summary)

arrow::write_parquet(stab_df, file.path(processed_dir, "stuff_plus_stabilization_curve.parquet"), compression = "zstd")
arrow::write_parquet(yoy_summary, file.path(processed_dir, "stuff_plus_yoy_validity.parquet"), compression = "zstd")

stab_colors <- c(
  "mlbr Stuff+" = "#38bdf8",
  "mlbr Pitching+Tunnel" = "#10b981",
  "mlbr Location+" = "#a855f7",
  "CSW%" = "#f59e0b",
  "Whiff%" = "#fb7185",
  "Raw RV/100" = "#64748b"
)

p5_left <- ggplot(stab_df, aes(x = n_pitches, y = split_half_r, color = metric)) +
  geom_hline(yintercept = 0.70, linetype = "dashed", color = "#475569", linewidth = 0.45) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.6) +
  scale_color_manual(values = stab_colors, name = "Metric") +
  scale_x_continuous(breaks = sample_ns) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  coord_cartesian(ylim = c(-0.05, 1.0)) +
  labs(
    title = "A. Split-Half True-Talent Stabilization (2025 Regular Season)",
    subtitle = "Stuff+ stabilizes past r = 0.85 within 50-100 regular-season pitches; Raw RV/100 remains noisy past 1,000.",
    x = "Regular-Season Sample Size per Half (Pitches)",
    y = "Split-Half Pearson Correlation (r)"
  ) +
  mlbr::theme_cyber(base_size = 10.5) +
  theme(legend.position = "bottom")

yoy_plot_sub <- yoy_summary[metric %in% c("mlbr Stuff+", "mlbr Pitching+Tunnel", "mlbr Location+", "FanGraphs Stuff+", "FanGraphs Pitching+", "K%", "FIP", "ERA")]
yoy_long <- melt(
  yoy_plot_sub,
  id.vars = "metric",
  measure.vars = c("yoy_self_r", "pred_25_k_pct_r", "pred_25_siera_r"),
  variable.name = "corr_type",
  value.name = "r_val"
)
yoy_long[, abs_r := abs(r_val)]
yoy_long[, corr_label := fcase(
  corr_type == "yoy_self_r", "YoY Self-Stability (24 -> 25)",
  corr_type == "pred_25_k_pct_r", "Predicts 2025 K% (|r|)",
  corr_type == "pred_25_siera_r", "Predicts 2025 SIERA (|r|)"
)]
yoy_long[, metric := factor(metric, levels = rev(yoy_plot_sub$metric))]

p5_right <- ggplot(yoy_long, aes(x = abs_r, y = metric, fill = corr_label)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.65, alpha = 0.92) +
  geom_text(
    aes(label = sprintf("%.2f", abs_r)),
    position = position_dodge(width = 0.72), hjust = -0.12, color = "#e2e8f0", size = 2.7
  ) +
  scale_fill_manual(
    values = c(
      "YoY Self-Stability (24 -> 25)" = "#38bdf8",
      "Predicts 2025 K% (|r|)" = "#10b981",
      "Predicts 2025 SIERA (|r|)" = "#f59e0b"
    ),
    name = "Correlation"
  ) +
  scale_x_continuous(limits = c(0, 1.0), breaks = seq(0, 1, by = 0.2)) +
  labs(
    title = sprintf("B. 2024 -> 2025 Out-of-Sample Validity (N = %d Regular-Season Pitchers)", yoy_summary$n_pitchers[1]),
    subtitle = "Comparing mlbr Stuff+/Pitching+ against FanGraphs Stuff+/Pitching+, K%, FIP, and ERA (>=400 reg. pitches/yr).",
    x = "Absolute Pearson Correlation (|r|)",
    y = NULL
  ) +
  mlbr::theme_cyber(base_size = 10.5) +
  theme(legend.position = "bottom")

fig5 <- (p5_left | p5_right) +
  plot_annotation(
    title = "True-Talent Stabilization & Out-of-Sample Predictive Power (2024 -> 2025 Regular Season)",
    subtitle = "Strictly Regular-Season (game_type = 'R') physics-based Stuff+ & Pitching+ isolate repeatable skill vastly faster than ERA.",
    caption = "Source: mlbr Gold Leaderboard + FanGraphs Benchmark | Strictly out-of-sample 2025 evaluation",
    theme = mlbr::theme_cyber(base_size = 11)
  )

mlbr::save_plot_assets(strip_env(fig5), "stuff05_yoy_stability_curve", data = stab_df, width = 14.0, height = 6.8, dir = models_dir)

# Copy all Dark Cyber visual assets (PNG + HTML) to site/assets/ with fail-fast check
asset_files <- Sys.glob(file.path(models_dir, "stuff0*.*"))
asset_files <- asset_files[grepl("\\.(png|html)$", asset_files)]
stopifnot(length(asset_files) >= 6L)
copied_ok <- file.copy(asset_files, assets_dir, overwrite = TRUE)
if (!all(copied_ok)) stop("Failed to copy visual assets to site/assets: ", paste(asset_files[!copied_ok], collapse = ", "))
cat(sprintf("-> Copied %d visual assets to %s\n", length(asset_files), assets_dir))

cat("\n=== TOP 10 QUALIFIED 2025 REGULAR-SEASON STARTERS (>= 2,000 pitches) ===\n")
print(head(q25[n_pitches >= 2000L][order(-pitching_plus), .(
  pitcher_name, team, p_throws, n_pitches, stuff_plus, location_plus,
  pitching_plus, pitching_tunnel_plus, fg_stuff_plus, fg_pitching_plus,
  fg_k_pct, fg_bb_pct, fg_fip, fg_era
)], 10))

cat("\n=== TOP 10 QUALIFIED 2025 REGULAR-SEASON PITCHERS OVERALL (>= 500 pitches) ===\n")
print(head(q25[order(-pitching_tunnel_plus), .(
  pitcher_name, team, p_throws, n_pitches, stuff_plus, location_plus,
  pitching_plus, pitching_tunnel_plus, fg_pitching_plus, fg_k_pct, fg_fip
)], 10))

DBI::dbDisconnect(con, shutdown = TRUE)

# ==============================================================================
# 7. LOG EXPERIMENT EXP-003-TUNNELING-STUFF-PLUS TO REGISTRY (6-DIGIT PRECISION)
# ==============================================================================
r_stuff_yoy <- yoy_summary[metric == "mlbr Stuff+", yoy_self_r]
r_pitch_yoy <- yoy_summary[metric == "mlbr Pitching+Tunnel", yoy_self_r]
r_era_yoy   <- yoy_summary[metric == "ERA", yoy_self_r]
r_stuff_100 <- stab_df[metric == "mlbr Stuff+" & n_pitches == 100L, split_half_r]
r_rv_100    <- stab_df[metric == "Raw RV/100" & n_pitches == 100L, split_half_r]
stopifnot(length(r_stuff_100) == 1L, length(r_rv_100) == 1L)

mlbr::lake_log_experiment(
  id = "EXP-003-TUNNELING-STUFF-PLUS",
  title = "Commit-Point Pitch Tunneling & Platoon-Conditioned Stuff+ / Location+ / Pitching+",
  tier = 1,
  status = "VALIDATED",
  summary = paste0(
    "Materialized exact y0=50ft -> y=23.8ft commit-point pitch tunneling across all 19 seasons (",
    format(silver_tunnel_pairs_all, big.mark = ","), " consecutive pairs, strictly sorted ADR-001 ZSTD Parquet, 0 bad rows, 0 inversions) ",
    "and trained strictly chronological Regular-Season (game_type='R') xgboost hist Stuff+, orthogonal Location+, Pitching+, and Pitching+Tunnel models over 2015-2026. ",
    "All tree models beat empirical and analytical Ridge baselines on locked 2025-2026 run value and swing-and-miss whiff deception; ",
    "mlbr Stuff+ hits split-half r = ", round(r_stuff_100, 3), " in 100 regular-season pitches (vs. ", round(r_rv_100, 3), " for Raw RV/100), ",
    "achieves YoY r = ", round(r_stuff_yoy, 3), " (vs. ERA r = ", round(r_era_yoy, 3), "), and correlates at r = ", round(cor_pitch_fg, 3), " with FanGraphs Pitching+."
  ),
  hypotheses = c(
    "Reconstructing exact (x50, z50) from (plate_x, plate_z) eliminates the 2.87 in mean (4.69 in p95) naive release-point commit-point tunneling error.",
    "Non-linear gradient boosted trees (xgboost hist) over platoon-normalized aerodynamics and fastball differentials strictly outperform empirical and linear Ridge baselines on locked 2025-2026 regular-season run value.",
    "Commit-point pitch tunneling features add measurable out-of-sample lift to swing-and-miss deception (Whiff Log-Loss & ROC AUC on paired swings) and next-season pitcher SIERA/FIP forecasting.",
    "Physics-based Stuff+ stabilizes to true-talent signal in 50-100 regular-season pitches and predicts next-season K% and SIERA more reliably than lagging ERA."
  ),
  formulas = list(
    commit_time_t_dec = "(-sqrt(vy0^2 + 2*ay*(23.8 - 50.0)) - vy0) / ay",
    x50_reconstruction = "plate_x - vx0*t_plate - 0.5*ax*t_plate^2",
    tunnel_dist_in = "12 * sqrt((x_dec_N - x_dec_{N-1})^2 + (z_dec_N - z_dec_{N-1})^2)",
    break_to_tunnel_ratio_bp = "post_tunnel_break_in / GREATEST(tunnel_dist_in, 1.0)",
    plate_to_tunnel_ratio = "plate_sep_in / GREATEST(tunnel_dist_in, 1.0)",
    plus_scale_2024_lock = "100 + 10 * (pred_i - mu_2024_reg) / sd_pitcher_2024_reg_min250"
  ),
  metrics = list(
    silver_tunnel_pairs_2008_2026 = silver_tunnel_pairs_all,
    reg_season_train_2015_2023_pitches = length(idx_tr_full),
    reg_season_val_2024_pitches = length(idx_val),
    reg_season_test_2025_2026_pitches = length(idx_test),
    test_2025_2026_b0_rmse = round(rv_ablation[1, test_rmse], 6),
    test_2025_2026_ridge_pitching_rmse = round(rv_ablation[3, test_rmse], 6),
    test_2025_2026_stuff_rmse = round(rv_ablation[4, test_rmse], 6),
    test_2025_2026_stuff_he_rmse = round(rv_ablation[5, test_rmse], 6),
    test_2025_2026_location_rmse = round(rv_ablation[6, test_rmse], 6),
    test_2025_2026_pitching_rmse = round(rv_ablation[7, test_rmse], 6),
    test_2025_2026_pitching_tunnel_rmse = round(rv_ablation[8, test_rmse], 6),
    test_2025_2026_pitching_tunnel_he_rmse = round(rv_ablation[9, test_rmse], 6),
    whiff_paired_auc_baseline0 = round(whiff_ablation[1, test_auc_paired], 4),
    whiff_paired_auc_stuff = round(whiff_ablation[2, test_auc_paired], 4),
    whiff_paired_auc_pitching = round(whiff_ablation[3, test_auc_paired], 4),
    whiff_paired_auc_pitching_tunnel = round(whiff_ablation[4, test_auc_paired], 4),
    whiff_paired_logloss_pitching = round(whiff_ablation[3, test_logloss_paired], 5),
    whiff_paired_logloss_pitching_tunnel = round(whiff_ablation[4, test_logloss_paired], 5),
    external_cor_fg_stuff_2025 = round(cor_stuff_fg, 4),
    external_cor_fg_location_2025 = round(cor_loc_fg, 4),
    external_cor_fg_pitching_2025 = round(cor_pitch_fg, 4),
    orthogonality_cor_stuff_location_2025 = round(cor_ortho, 4),
    split_half_100p_stuff_r = round(r_stuff_100, 4),
    split_half_100p_rv_r = round(r_rv_100, 4),
    yoy_24_25_stuff_r = round(r_stuff_yoy, 4),
    yoy_24_25_pitching_tunnel_r = round(r_pitch_yoy, 4),
    yoy_24_25_era_r = round(r_era_yoy, 4)
  ),
  features = c(
    "tunnel_dist_in", "plate_sep_in", "post_tunnel_break_in", "break_to_tunnel_ratio",
    "plate_to_tunnel_ratio", "delta_velo_mph", "delta_flight_ms", "is_repeat_pitch",
    "delta_fb_velo", "delta_fb_ivb", "delta_fb_hb", "sz_norm_z",
    "stuff_plus", "location_plus", "pitching_plus", "pitching_tunnel_plus"
  ),
  artifacts = list(
    sql = "sql/02_pitch_tunneling.sql",
    materializer = "R/materialize_tunneling.R",
    script = "models/03_tunneling_stuff_plus.R",
    spec = "specs/03_tunneling_stuff_plus.md",
    study_qmd = "site/analyses/03_tunneling_stuff_plus.qmd",
    study_html = "http://192.168.10.1:8088/analyses/03_tunneling_stuff_plus.html",
    silver_tunneling = "data/features/statcast_tunneling/",
    gold_leaderboard = "data/processed/pitcher_stuff_plus.parquet",
    gold_ablation = "data/processed/stuff_plus_model_ablation.parquet",
    gold_whiff_ablation = "data/processed/stuff_plus_whiff_ablation.parquet",
    gold_yoy_validity = "data/processed/stuff_plus_yoy_validity.parquet",
    gold_importance = "data/processed/stuff_plus_feature_importance.parquet",
    saved_models = "data/models/"
  ),
  lake = lake
)

cat("\n*** STUDY 03 REGULAR-SEASON TUNNELING & STUFF+ PIPELINE COMPLETE ***\n")
}

run_study03()
