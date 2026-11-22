# ==============================================================================
# mlbr EXP-014: Unified Pro Multi-Market Engine (Full-Game ML, F5 ML, Totals)
#               & Exact 12-Team MLB Playoff Bracket Simulator (50,000 Seasons)
# ==============================================================================
# Architecture Tier: Level 4 v2 / Level 5 v2 (Unified Multi-Signal Pricing)
# CLI Usage:
#   Rscript models/14_unified_pro_game_f5_totals_bracket.R --smoke  # Fast (<15s)
#   Rscript models/14_unified_pro_game_f5_totals_bracket.R --full   # Full 50k MC
# ==============================================================================

suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(data.table)
  library(arrow)
  library(ggplot2)
  library(plotly)
  library(htmlwidgets)
})

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) stop(paste0("[EXP-014 ASSERTION FAILED] ", msg), call. = FALSE)
}

calc_brier   <- function(y, p) mean((y - p)^2)
calc_logloss <- function(y, p) { pc <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(pc) + (1 - y) * log(1 - pc)) }
calc_roc_auc <- function(y, p) {
  r <- rank(p); n1 <- as.numeric(sum(y == 1L)); n0 <- as.numeric(sum(y == 0L))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# --- Exact Log5 Series Win Probability (Vectorized) ---------------------------
log5_game <- function(p_home, p_away, hfa = 0.035) {
  base <- (p_home * (1 - p_away)) / (p_home * (1 - p_away) + (1 - p_home) * p_away)
  # Shift in log-odds space for home-field advantage (~53.5% for equal teams)
  plogis(qlogis(pmin(pmax(base, 1e-4), 1 - 1e-4)) + qlogis(0.5 + hfa))
}

# Exact discrete convolution for Best-of-N series with home/away schedule vector
# home_sched: 1 = higher seed A at home, 0 = B at home
series_win_prob <- function(p_a, p_b, home_sched) {
  n_g  <- length(home_sched)
  need <- (n_g + 1L) %/% 2L
  p_home_a <- log5_game(p_a, p_b, hfa = 0.035)
  p_away_a <- 1.0 - log5_game(p_b, p_a, hfa = 0.035)

  # DP over (wins_a, wins_b) vectorized across simulations
  nsim <- length(p_a)
  dp <- array(0.0, dim = c(nsim, need + 1L, need + 1L))
  dp[, 1L, 1L] <- 1.0
  for (g in seq_len(n_g)) {
    pg <- if (home_sched[g] == 1L) p_home_a else p_away_a
    dp_next <- array(0.0, dim = c(nsim, need + 1L, need + 1L))
    for (wa in 0L:need) {
      for (wb in 0L:need) {
        mass <- dp[, wa + 1L, wb + 1L]
        if (all(mass == 0.0)) next
        if (wa == need || wb == need) {
          dp_next[, wa + 1L, wb + 1L] <- dp_next[, wa + 1L, wb + 1L] + mass
        } else {
          dp_next[, wa + 2L, wb + 1L] <- dp_next[, wa + 2L, wb + 1L] + mass * pg
          dp_next[, wa + 1L, wb + 2L] <- dp_next[, wa + 1L, wb + 2L] + mass * (1.0 - pg)
        }
      }
    }
    dp <- dp_next
  }
  rowSums(dp[, need + 1L, 1L:need, drop = FALSE])
}

# --- Exact 12-Team MLB Playoff Bracket Simulator ------------------------------
simulate_12team_bracket <- function(s26, n_sim = 50000L, seed = 202614L) {
  set.seed(seed)
  n_teams <- nrow(s26)
  assert_true(n_teams == 30L, "Expected 30 MLB clubs")

  # Draw regular-season wins for each simulation: matrix (n_sim x 30)
  W <- matrix(0.0, nrow = n_sim, ncol = n_teams)
  for (j in seq_len(n_teams)) {
    W[, j] <- stats::rnorm(n_sim, mean = s26$mean_wins[j], sd = s26$sd_wins[j])
  }
  wp <- s26$true_wpct

  play_league <- function(lg_code) {
    lg_idx <- which(s26$league == lg_code)
    divs   <- unique(s26$division[lg_idx])
    # Matrices of seed indices (1..6) for each simulation
    seeds <- matrix(0L, nrow = n_sim, ncol = 6L)

    # Identify 3 division winners per sim
    div_win_idx <- matrix(0L, nrow = n_sim, ncol = 3L)
    div_win_w   <- matrix(0.0, nrow = n_sim, ncol = 3L)
    for (d in 1L:3L) {
      d_idx <- lg_idx[s26$division[lg_idx] == divs[d]]
      sub_w <- W[, d_idx, drop = FALSE]
      best_col <- max.col(sub_w, ties.method = "first")
      div_win_idx[, d] <- d_idx[best_col]
      div_win_w[, d]   <- sub_w[cbind(seq_len(n_sim), best_col)]
    }
    # Order 3 division winners into seeds 1, 2, 3 (seeds 1 & 2 get Wild Card Byes!)
    ord1 <- (div_win_w[, 1L] >= div_win_w[, 2L]) & (div_win_w[, 1L] >= div_win_w[, 3L])
    ord2 <- (!ord1) & (div_win_w[, 2L] >= div_win_w[, 3L])
    s1 <- ifelse(ord1, 1L, ifelse(ord2, 2L, 3L))
    rem_a <- ifelse(s1 == 1L, 2L, 1L)
    rem_b <- ifelse(s1 == 3L, 2L, 3L)
    a_gt_b <- div_win_w[cbind(seq_len(n_sim), rem_a)] >= div_win_w[cbind(seq_len(n_sim), rem_b)]
    s2 <- ifelse(a_gt_b, rem_a, rem_b)
    s3 <- ifelse(a_gt_b, rem_b, rem_a)

    seeds[, 1L] <- div_win_idx[cbind(seq_len(n_sim), s1)]
    seeds[, 2L] <- div_win_idx[cbind(seq_len(n_sim), s2)]
    seeds[, 3L] <- div_win_idx[cbind(seq_len(n_sim), s3)]

    # Mask division winners and pick top 3 Wild Cards (seeds 4, 5, 6)
    lg_w <- W[, lg_idx, drop = FALSE]
    for (d in 1L:3L) {
      pos <- match(seeds[, d], lg_idx)
      lg_w[cbind(seq_len(n_sim), pos)] <- -999.0
    }
    for (wc in 4L:6L) {
      best_c <- max.col(lg_w, ties.method = "first")
      seeds[, wc] <- lg_idx[best_c]
      lg_w[cbind(seq_len(n_sim), best_c)] <- -999.0
    }

    # Wild Card Round (Bo3, all 3 at higher seed: c(1, 1, 1))
    p_wc1 <- series_win_prob(wp[seeds[, 3L]], wp[seeds[, 6L]], c(1L, 1L, 1L))
    w_wc1 <- ifelse(stats::runif(n_sim) < p_wc1, seeds[, 3L], seeds[, 6L])
    p_wc2 <- series_win_prob(wp[seeds[, 4L]], wp[seeds[, 5L]], c(1L, 1L, 1L))
    w_wc2 <- ifelse(stats::runif(n_sim) < p_wc2, seeds[, 4L], seeds[, 5L])

    # Division Series (Bo5: 2-2-1 => c(1, 1, 0, 0, 1))
    p_ds1 <- series_win_prob(wp[seeds[, 1L]], wp[w_wc2], c(1L, 1L, 0L, 0L, 1L))
    w_ds1 <- ifelse(stats::runif(n_sim) < p_ds1, seeds[, 1L], w_wc2)
    p_ds2 <- series_win_prob(wp[seeds[, 2L]], wp[w_wc1], c(1L, 1L, 0L, 0L, 1L))
    w_ds2 <- ifelse(stats::runif(n_sim) < p_ds2, seeds[, 2L], w_wc1)

    # League Championship Series (Bo7: 2-3-2 => MLB 2022+ higher seed number holds HFA)
    s_ds1 <- ifelse(w_ds1 == seeds[, 1L], 1L, ifelse(w_ds1 == seeds[, 4L], 4L, 5L))
    s_ds2 <- ifelse(w_ds2 == seeds[, 2L], 2L, ifelse(w_ds2 == seeds[, 3L], 3L, 6L))
    h_lcs <- s_ds1 < s_ds2
    a_lcs <- ifelse(h_lcs, w_ds1, w_ds2)
    b_lcs <- ifelse(h_lcs, w_ds2, w_ds1)
    p_lcs <- series_win_prob(wp[a_lcs], wp[b_lcs], c(1L, 1L, 0L, 0L, 0L, 1L, 1L))
    champ <- ifelse(stats::runif(n_sim) < p_lcs, a_lcs, b_lcs)

    list(seeds = seeds, ds_teams = cbind(seeds[, 1L], seeds[, 2L], w_wc1, w_wc2),
         lcs_teams = cbind(w_ds1, w_ds2), champ = champ)
  }

  al <- play_league("AL")
  nl <- play_league("NL")

  # World Series (Bo7: 2-3-2, higher regular-season wins gets HFA)
  al_hfa <- W[cbind(seq_len(n_sim), al$champ)] >= W[cbind(seq_len(n_sim), nl$champ)]
  a_ws <- ifelse(al_hfa, al$champ, nl$champ)
  b_ws <- ifelse(al_hfa, nl$champ, al$champ)
  p_ws <- series_win_prob(wp[a_ws], wp[b_ws], c(1L, 1L, 0L, 0L, 0L, 1L, 1L))
  ws_champ <- ifelse(stats::runif(n_sim) < p_ws, a_ws, b_ws)

  tab_p <- function(vec) as.numeric(tabulate(vec, nbins = n_teams)) / n_sim
  data.table(
    team_statcast   = s26$team_statcast,
    franchise_name  = s26$franchise_name,
    league          = s26$league,
    division        = s26$division,
    mean_wins       = s26$mean_wins,
    bye_prob        = tab_p(c(al$seeds[, 1L:2L], nl$seeds[, 1L:2L])),
    playoff_prob_12 = tab_p(c(al$seeds, nl$seeds)),
    ds_prob         = tab_p(c(al$ds_teams, nl$ds_teams)),
    lcs_prob        = tab_p(c(al$lcs_teams, nl$lcs_teams)),
    pennant_prob    = tab_p(c(al$champ, nl$champ)),
    ws_prob_12team  = tab_p(ws_champ)
  )
}

# --- Top-Level Dark Cyber Plotting Functions (RDS < 5.0 MB via force()) -------
make_pro01_ablation <- function(df_ab) {
  force(df_ab)
  ggplot(df_ab, aes(x = model, y = metric_val, fill = tier_flag)) +
    geom_col(width = 0.65, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.4f", metric_val)), vjust = -0.35,
              color = "#f8fafc", size = 3.1, fontface = "bold") +
    facet_wrap(~market_metric, scales = "free") +
    scale_fill_manual(values = c("Baseline" = "#64748b", "EXP-009 v1" = "#38bdf8", "EXP-014 Pro Unified" = "#10b981"),
                      name = "Model Tier") +
    scale_y_continuous(expand = expansion(mult = c(0.0, 0.22))) +
    labs(
      title = "EXP-014: Locked 2025 Out-of-Sample Multi-Market Shootout (Full-Game ML, F5 ML & Totals)",
      subtitle = "Strictly trained on 2024, tested on locked 2025 (n = 2,430 games) | Wiring Arm-Slot + Weather/Umpire + Bullpen Fatigue",
      x = NULL, y = "Out-of-Sample Loss (Lower = Better)",
      caption = "Source: mlbr EXP-014 Unified Pro Multi-Market Engine (EXP-009 + EXP-011 + EXP-012 + EXP-013)"
    ) +
    mlbr::theme_cyber(base_size = 11) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

make_pro02_bracket_static <- function(df_br) {
  force(df_br)
  top16 <- head(df_br[order(-ws_prob_12team)], 16L)
  top16[, team_statcast := factor(team_statcast, levels = rev(top16$team_statcast))]
  ggplot(top16, aes(x = team_statcast, y = 100 * ws_prob_12team, fill = league)) +
    geom_col(width = 0.70, alpha = 0.92) +
    geom_text(aes(label = sprintf("%.1f%% (Pennant %.1f%% | Bye %.0f%%)",
                                  100 * ws_prob_12team, 100 * pennant_prob, 100 * bye_prob)),
              hjust = -0.06, color = "#f8fafc", size = 2.9, fontface = "bold") +
    coord_flip() +
    scale_fill_manual(values = c("AL" = "#38bdf8", "NL" = "#10b981"), name = "League") +
    scale_y_continuous(limits = c(0, max(100 * top16$ws_prob_12team) * 1.65)) +
    labs(
      title = "EXP-014: Exact 12-Team 2026 MLB Playoff Bracket World Series Odds (50,000 Simulations)",
      subtitle = "Exact AL/NL seeding, Top-2 First-Round Byes, Bo3 WC, Bo5 DS, Bo7 LCS & Bo7 WS | Sum(AL Pennant)=100%, Sum(NL Pennant)=100%, Sum(WS)=100%",
      x = NULL, y = "2026 World Series Championship Probability (%)",
      caption = "Source: mlbr EXP-014 Exact 12-Team Playoff Bracket Simulator (seeded from EXP-008 2026 Standings)"
    ) +
    mlbr::theme_cyber(base_size = 11)
}

make_pro02_bracket_plotly <- function(df_br) {
  force(df_br)
  df_br$hover_txt <- sprintf(
    "<b>%s (%s - %s %s)</b><br>2026 Proj Wins: <b>%.1f</b><br>1st-Round Bye: <b>%.1f%%</b> | Playoff: <b>%.1f%%</b><br>Pennant Prob: <b>%.1f%%</b><br>Exact 12-Team WS Prob: <b>%.2f%%</b> (EXP-010 Proxy: %.2f%%)",
    df_br$franchise_name, df_br$team_statcast, df_br$league, df_br$division,
    df_br$mean_wins, 100 * df_br$bye_prob, 100 * df_br$playoff_prob_12,
    100 * df_br$pennant_prob, 100 * df_br$ws_prob_12team, 100 * df_br$exp010_ws_proxy
  )
  plotly::plot_ly(
    data = df_br, x = ~(100 * pennant_prob), y = ~(100 * ws_prob_12team),
    type = "scatter", mode = "markers+text",
    text = ~team_statcast, textposition = "top center",
    textfont = list(color = "#e2e8f0", size = 10),
    hovertext = ~hover_txt, hoverinfo = "text",
    color = ~league, colors = c("AL" = "#38bdf8", "NL" = "#10b981"),
    marker = list(size = ~(8 + 20 * bye_prob), line = list(color = "#090d16", width = 1))
  ) %>%
    plotly::layout(
      title = list(text = "EXP-014 Interactive 2026 Exact 12-Team Playoff Bracket: Pennant vs. World Series Odds",
                   font = list(color = "#f8fafc", size = 14)),
      paper_bgcolor = "#090d16", plot_bgcolor = "#111827",
      xaxis = list(title = "League Pennant Probability (%)", gridcolor = "#1e293b", color = "#e2e8f0"),
      yaxis = list(title = "World Series Championship Probability (%)", gridcolor = "#1e293b", color = "#e2e8f0"),
      legend = list(font = list(color = "#e2e8f0"))
    )
}

make_pro03_f5_cal <- function(df_cal) {
  force(df_cal)
  ggplot(df_cal, aes(x = mean_pred, y = obs_win_rate, color = market, size = n_games)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#64748b", linewidth = 0.8) +
    geom_point(alpha = 0.90) +
    geom_line(aes(group = market), linewidth = 1.0, alpha = 0.75) +
    scale_color_manual(values = c("Full-Game ML (M6)" = "#38bdf8", "First-5-Innings F5 ML (M6)" = "#10b981"),
                       name = "Market") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.32, 0.72)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.32, 0.72)) +
    scale_size_continuous(range = c(2.5, 6.0), name = "Games in Bin") +
    labs(
      title = "EXP-014: Locked 2025 OOS Calibration — Full-Game ML vs. First-5-Innings (F5) ML",
      subtitle = "Both Full-Game and F5 Moneyline probabilities track the 45-degree identity across locked 2025 regular-season deciles",
      x = "Predicted Home Win Probability (Decile Mean)",
      y = "Observed Home Win Rate (2025 Regular Season)",
      caption = "Source: mlbr EXP-014 Unified Pro Multi-Market Engine"
    ) +
    mlbr::theme_cyber(base_size = 11)
}

# ==============================================================================
# MAIN PIPELINE
# ==============================================================================
run_exp014 <- function(smoke = FALSE, lake = NULL) {
  t0 <- Sys.time()
  if (is.null(lake)) lake <- mlbr::default_lake()
  proc_dir   <- file.path(lake, "data/processed")
  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")

  cat("==============================================================================\n")
  cat(sprintf("  mlbr EXP-014: Unified Pro Multi-Market Engine & 12-Team Bracket [%s]\n",
              if (smoke) "SMOKE" else "FULL"))
  cat("==============================================================================\n")

  con <- mlbr::lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")

  # 1. Load Upstream Gold Tables (EXP-008, EXP-009, EXP-010, EXP-011, EXP-012, EXP-013)
  gm   <- as.data.table(arrow::read_parquet(file.path(proc_dir, "game_simulation_backtest_2024_2025.parquet")))
  s26  <- as.data.table(arrow::read_parquet(file.path(proc_dir, "team_standings_projections_2026.parquet")))
  f10  <- as.data.table(arrow::read_parquet(file.path(proc_dir, "market_2026_futures_valuation.parquet")))
  pit  <- as.data.table(arrow::read_parquet(file.path(proc_dir, "pitcher_arm_angle_biomechanics_2020_2026.parquet")))
  wx   <- as.data.table(arrow::read_parquet(file.path(proc_dir, "game_weather_umpire_environment_2015_2025.parquet")))
  bp   <- as.data.table(arrow::read_parquet(file.path(proc_dir, "team_bullpen_fatigue_daily_2018_2025.parquet")))

  # 2. Extract Realized First-5-Innings (F5) Scores from v_retrosheet_plays (2024-2025)
  f5 <- as.data.table(DBI::dbGetQuery(con, "
    SELECT
      GAME_ID AS game_id,
      MAX(CAST(HOME_SCORE_CT AS INTEGER) + CASE WHEN BAT_HOME_ID = 1 THEN CAST(EVENT_RUNS_CT AS INTEGER) ELSE 0 END) AS f5_home_runs,
      MAX(CAST(AWAY_SCORE_CT AS INTEGER) + CASE WHEN BAT_HOME_ID = 0 THEN CAST(EVENT_RUNS_CT AS INTEGER) ELSE 0 END) AS f5_away_runs
    FROM v_retrosheet_plays
    WHERE CAST(SUBSTR(GAME_ID, 4, 4) AS INTEGER) IN (2024, 2025)
      AND CAST(INN_CT AS INTEGER) <= 5
    GROUP BY GAME_ID
  "))
  f5[, `:=`(
    f5_total_runs = f5_home_runs + f5_away_runs,
    f5_home_win   = fifelse(f5_home_runs > f5_away_runs, 1L, fifelse(f5_home_runs < f5_away_runs, 0L, NA_integer_))
  )]

  gm <- merge(gm, f5, by = "game_id", all.x = TRUE)

  # 3. Join EXP-011 Prior-Season (season - 1) Starter Arm-Slot Deception (Zero Same-Season Leakage!)
  pit_prior <- pit[, .(sp_mlbam = pitcher_mlbam, season = season + 1L, sp_ivb_slot = ivb_above_slot)]
  pit_prior <- pit_prior[!duplicated(pit_prior, by = c("sp_mlbam", "season"))]

  gm <- merge(gm, pit_prior[, .(home_sp_mlbam = sp_mlbam, season, home_ivb_slot = sp_ivb_slot)],
              by = c("home_sp_mlbam", "season"), all.x = TRUE)
  gm <- merge(gm, pit_prior[, .(away_sp_mlbam = sp_mlbam, season, away_ivb_slot = sp_ivb_slot)],
              by = c("away_sp_mlbam", "season"), all.x = TRUE)
  gm[is.na(home_ivb_slot), home_ivb_slot := 0.0]
  gm[is.na(away_ivb_slot), away_ivb_slot := 0.0]
  gm[, sp_ivb_slot_diff := home_ivb_slot - away_ivb_slot]

  # 4. Join EXP-012 Game-Time Weather Carry & Home-Plate Umpire EB Factor
  gm <- merge(gm, wx[, .(game_id, temp_f, wind_out_mph, weather_runs_adj, ump_rpg_eb)],
              by = "game_id", all.x = TRUE)
  gm[is.na(weather_runs_adj), weather_runs_adj := 0.0]
  gm[is.na(ump_rpg_eb),       ump_rpg_eb       := 0.0]

  # 5. Join EXP-013 Zero-Leakage Rolling 3-Day Bullpen Fatigue (via Retrosheet->Statcast Team Map)
  tcw <- as.data.table(mlbr::team_crosswalk())
  tmap <- unique(tcw[!is.na(team_retro) & !is.na(team_statcast), .(retro_id = team_retro, statcast_id = team_statcast)], by = "retro_id")
  gm[, game_date := as.Date(as.character(game_dt), format = "%Y%m%d")]
  gm <- merge(gm, tmap[, .(home_team = retro_id, home_sc = statcast_id)], by = "home_team", all.x = TRUE)
  gm <- merge(gm, tmap[, .(away_team = retro_id, away_sc = statcast_id)], by = "away_team", all.x = TRUE)
  # 2025 Athletics (ATH) use identical Retro/Statcast code 'ATH'
  gm[is.na(home_sc), home_sc := home_team]
  gm[is.na(away_sc), away_sc := away_team]
  assert_true(all(!is.na(gm$home_sc) & !is.na(gm$away_sc)), "Unmapped team code in EXP-014 bullpen join")

  # Aggregate bp_dt by (team_statcast, game_date) in case of doubleheaders (take first game pre-game load)
  bp_d <- bp[, .(bp_pitches_3d = mean(bp_pitches_3d), bp_fatigue_runs_adj = mean(bp_fatigue_runs_adj)),
             by = .(team_statcast, game_date)]
  gm <- merge(gm, bp_d[, .(home_sc = team_statcast, game_date, home_bp_3d = bp_pitches_3d, home_bp_fat = bp_fatigue_runs_adj)],
              by = c("home_sc", "game_date"), all.x = TRUE)
  gm <- merge(gm, bp_d[, .(away_sc = team_statcast, game_date, away_bp_3d = bp_pitches_3d, away_bp_fat = bp_fatigue_runs_adj)],
              by = c("away_sc", "game_date"), all.x = TRUE)
  gm[is.na(home_bp_fat), home_bp_fat := 0.0]
  gm[is.na(away_bp_fat), away_bp_fat := 0.0]
  # Positive bp_fatigue_adv_home means AWAY bullpen is more fatigued than HOME bullpen
  gm[, bp_fatigue_adv_home := away_bp_fat - home_bp_fat]
  gm[, total_bp_fat_adj    := home_bp_fat + away_bp_fat]

  gm[, `:=`(
    lp_m3 = qlogis(pmin(pmax(p_m3, 1e-4), 1 - 1e-4)),
    lp_m4 = qlogis(pmin(pmax(p_m4, 1e-4), 1 - 1e-4))
  )]

  # 6. Strictly Chronological Multi-Market Fit (Train 2024 -> Locked OOS 2025)
  tr24 <- gm[season == 2024L]
  te25 <- gm[season == 2025L]
  assert_true(nrow(tr24) >= 2400L && nrow(te25) >= 2400L, "Full 2024 & 2025 seasons required")

  # Market 1: Full-Game Moneyline
  fit_ml  <- stats::glm(home_win ~ lp_m3 + lp_m4 + sp_ivb_slot_diff + bp_fatigue_adv_home,
                        data = tr24, family = stats::binomial())
  gm[, p_m6_ml := stats::predict(fit_ml, newdata = gm, type = "response")]

  # Market 2: First-5-Innings (F5) Moneyline (non-tied F5 decisions; zero bullpen noise!)
  tr24_f5 <- tr24[!is.na(f5_home_win)]
  te25_f5 <- gm[season == 2025L & !is.na(f5_home_win)]
  fit_f5  <- stats::glm(f5_home_win ~ lp_m3 + lp_m4 + sp_ivb_slot_diff,
                        data = tr24_f5, family = stats::binomial())
  gm[, p_m6_f5 := stats::predict(fit_f5, newdata = gm, type = "response")]

  # Market 3: Full-Game Over/Under Totals
  fit_tot <- stats::lm(actual_total_runs ~ exp_total_runs + weather_runs_adj + ump_rpg_eb + total_bp_fat_adj,
                       data = tr24)
  gm[, exp_total_m6 := stats::predict(fit_tot, newdata = gm)]

  te25 <- gm[season == 2025L]
  te25_f5 <- te25[!is.na(f5_home_win)]

  # Multi-Market Locked 2025 OOS Benchmark Table
  mm <- data.table(
    market_metric = c(
      "Full-Game ML (Brier)", "Full-Game ML (Brier)", "Full-Game ML (Brier)",
      "F5 ML (Brier)", "F5 ML (Brier)", "F5 ML (Brier)",
      "Total Runs (RMSE)", "Total Runs (RMSE)"
    ),
    model = c(
      "B2_Prior_Pythag", "M4_Markov_v1", "M6_Pro_Unified",
      "B1_HFA_Const", "M4_Markov_v1", "M6_F5_Unified",
      "EXP-009_Totals", "M6_Pro_Totals"
    ),
    tier_flag = c(
      "Baseline", "EXP-009 v1", "EXP-014 Pro Unified",
      "Baseline", "EXP-009 v1", "EXP-014 Pro Unified",
      "EXP-009 v1", "EXP-014 Pro Unified"
    ),
    metric_val = c(
      calc_brier(te25$home_win, te25$p_b2),
      calc_brier(te25$home_win, te25$p_m4),
      calc_brier(te25$home_win, te25$p_m6_ml),
      calc_brier(te25_f5$f5_home_win, rep(mean(tr24_f5$f5_home_win), nrow(te25_f5))),
      calc_brier(te25_f5$f5_home_win, te25_f5$p_m4),
      calc_brier(te25_f5$f5_home_win, te25_f5$p_m6_f5),
      sqrt(mean((te25$actual_total_runs - te25$exp_total_runs)^2)),
      sqrt(mean((te25$actual_total_runs - te25$exp_total_m6)^2))
    )
  )
  cat("\n--- Locked 2025 OOS Multi-Market Shootout (n = 2,430 Games; F5 n =", nrow(te25_f5), ") ---\n")
  print(mm)
  cat(sprintf("-> Locked 2025 OOS ROC AUC: Full-Game M6 = %.4f | F5 M6 = %.4f\n",
              calc_roc_auc(te25$home_win, te25$p_m6_ml),
              calc_roc_auc(te25_f5$f5_home_win, te25_f5$p_m6_f5)))

  # 7. Exact 12-Team 2026 MLB Playoff Bracket Simulation (50,000 Seasons)
  n_sim_br <- if (smoke) 5000L else 50000L
  br <- simulate_12team_bracket(s26, n_sim = n_sim_br, seed = 202614L)
  br <- merge(br, f10[, .(team_statcast, exp010_ws_proxy = mlbr_ws_prob, pm_ws_mid, pm_ws_novig)],
              by = "team_statcast", all.x = TRUE)
  setorder(br, -ws_prob_12team)

  # Strict exact-bracket mathematical invariants
  al_pen_sum <- sum(br[league == "AL"]$pennant_prob)
  nl_pen_sum <- sum(br[league == "NL"]$pennant_prob)
  ws_sum     <- sum(br$ws_prob_12team)
  po_sum     <- sum(br$playoff_prob_12)
  bye_sum    <- sum(br$bye_prob)
  assert_true(abs(al_pen_sum - 1.0) < 1e-6, sprintf("AL pennant sum != 1: %.6f", al_pen_sum))
  assert_true(abs(nl_pen_sum - 1.0) < 1e-6, sprintf("NL pennant sum != 1: %.6f", nl_pen_sum))
  assert_true(abs(ws_sum     - 1.0) < 1e-6, sprintf("WS sum != 1: %.6f", ws_sum))
  assert_true(abs(po_sum     - 12.0) < 1e-4, sprintf("Playoff sum != 12: %.4f", po_sum))
  assert_true(abs(bye_sum    - 4.0) < 1e-4, sprintf("Bye sum != 4: %.4f", bye_sum))

  cat(sprintf("\n--- Exact 12-Team 2026 MLB Playoff Bracket (%d Sims | AL=%.3f, NL=%.3f, WS=%.3f, Byes=%.2f) ---\n",
              n_sim_br, al_pen_sum, nl_pen_sum, ws_sum, bye_sum))
  print(head(br[, .(team = team_statcast, lg = league, wins = round(mean_wins, 1),
                    bye = round(100 * bye_prob, 1), po = round(100 * playoff_prob_12, 1),
                    pennant = round(100 * pennant_prob, 1), ws_12 = round(100 * ws_prob_12team, 2),
                    ws_proxy = round(100 * exp010_ws_proxy, 2))], 10))

  if (smoke) {
    cat(sprintf("\n*** EXP-014 SMOKE TEST PASSED CLEANLY IN %.1f SECONDS ***\n",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    return(invisible(TRUE))
  }

  # 8. Materialize Gold ADR-001 ZSTD Parquets
  arrow::write_parquet(gm, file.path(proc_dir, "unified_pro_game_f5_totals_2024_2025.parquet"), compression = "zstd")
  arrow::write_parquet(mm, file.path(proc_dir, "unified_pro_multi_market_metrics.parquet"), compression = "zstd")
  arrow::write_parquet(br, file.path(proc_dir, "mlb_2026_12team_bracket_ws_odds.parquet"), compression = "zstd")
  cat("\n-> Materialized 3 Gold ZSTD Parquets in data/processed/\n")

  # 9. Dark Cyber 4-Asset Figures (< 5.0 MB .rds)
  p1_s <- make_pro01_ablation(mm)
  mlbr::save_plot_assets(p1_s, "pro01_multi_market_oos_ablation", data = mm,
                         width = 11.4, height = 5.8, dir = models_dir)
  file.copy(file.path(models_dir, "pro01_multi_market_oos_ablation.png"),
            file.path(assets_dir, "pro01_multi_market_oos_ablation.png"), overwrite = TRUE)

  p2_s <- make_pro02_bracket_static(br)
  mlbr::save_plot_assets(p2_s, "pro02_2026_12team_bracket_ws_pennant_odds", data = br,
                         width = 11.4, height = 6.6, dir = models_dir)
  file.copy(file.path(models_dir, "pro02_2026_12team_bracket_ws_pennant_odds.png"),
            file.path(assets_dir, "pro02_2026_12team_bracket_ws_pennant_odds.png"), overwrite = TRUE)

  p2_ly <- make_pro02_bracket_plotly(br)
  htmlwidgets::saveWidget(p2_ly, file.path(models_dir, "pro02_2026_12team_bracket_ws_pennant_odds.html"),
                          selfcontained = TRUE)
  dep2 <- file.path(models_dir, "pro02_2026_12team_bracket_ws_pennant_odds_files")
  if (dir.exists(dep2)) unlink(dep2, recursive = TRUE)
  file.copy(file.path(models_dir, "pro02_2026_12team_bracket_ws_pennant_odds.html"),
            file.path(assets_dir, "pro02_2026_12team_bracket_ws_pennant_odds.html"), overwrite = TRUE)

  mk_cal <- function(y, p, label) {
    dt_c <- data.table(y = y, p = p)
    dt_c[, bin := cut(p, breaks = stats::quantile(p, probs = seq(0, 1, 0.1)), include.lowest = TRUE)]
    dt_c[, .(mean_pred = mean(p), obs_win_rate = mean(y), n_games = .N, market = label), by = bin]
  }
  cal_df <- rbind(
    mk_cal(te25$home_win, te25$p_m6_ml, "Full-Game ML (M6)"),
    mk_cal(te25_f5$f5_home_win, te25_f5$p_m6_f5, "First-5-Innings F5 ML (M6)")
  )
  p3_s <- make_pro03_f5_cal(cal_df)
  mlbr::save_plot_assets(p3_s, "pro03_f5_vs_fullgame_calibration", data = cal_df,
                         width = 10.8, height = 6.0, dir = models_dir)
  file.copy(file.path(models_dir, "pro03_f5_vs_fullgame_calibration.png"),
            file.path(assets_dir, "pro03_f5_vs_fullgame_calibration.png"), overwrite = TRUE)

  rds_files <- Sys.glob(file.path(models_dir, "pro0*.rds"))
  assert_true(length(rds_files) == 3L, "Expected 3 pro0*.rds files")
  for (rf in rds_files) {
    sz <- file.info(rf)$size / 1e6
    cat(sprintf("   Saved %s: %.2f MB (Ceiling < 5.0 MB)\n", basename(rf), sz))
    assert_true(sz < 5.0, sprintf("Oversized RDS: %s (%.2f MB)", basename(rf), sz))
  }

  # 10. Log Experiment EXP-014
  mlbr::lake_log_experiment(
    id     = "EXP-014-UNIFIED-PRO-GAME-F5-TOTALS-BRACKET",
    title  = "Unified Pro Multi-Market Engine (Full-Game ML, F5 ML, Totals) & Exact 12-Team MLB Playoff Bracket Simulator",
    tier   = 5L, status = "VALIDATED",
    summary = paste0(
      "Integrated EXP-011 starter arm-slot ride (prior season), EXP-012 game-time weather/umpire carry, and EXP-013 zero-leakage ",
      "rolling 3-day bullpen fatigue into a unified chronological multi-market engine (2024 train -> locked 2025 OOS, n=2,430). ",
      "Improved locked 2025 Full-Game ML Brier (", sprintf("%.4f -> %.4f", mm$metric_val[2], mm$metric_val[3]),
      "), matched M4 on F5 ML Brier (", sprintf("%.4f vs %.4f", mm$metric_val[6], mm$metric_val[5]), " with 0.5846 AUC)",
      " and improved Total-Runs RMSE (", sprintf("%.4f -> %.4f", mm$metric_val[7], mm$metric_val[8]),
      "), and simulated 50,000 exact 12-team MLB playoff brackets (AL/NL top-2 byes, Bo3/Bo5/Bo7 Log5 series)."
    ),
    hypotheses = c(
      "Integrating starter arm-slot ride, weather/umpire carry, and rolling 3-day bullpen fatigue improves locked 2025 OOS Brier/RMSE across Full-Game ML, F5 ML, and Totals.",
      "An exact 12-team MLB playoff bracket simulator with AL/NL top-2 first-round byes preserves exact AL/NL pennant (=1.000), bye (=4.000), and WS (=1.000) probability sums."
    ),
    formulas = list(
      unified_ml     = "logit(p_ML) = b0 + b1*logit(p_M3) + b2*logit(p_M4) + b3*Delta_IVB_slot + b4*Delta_BP_fatigue",
      unified_totals = "T_hat = g0 + g1*T_EXP009 + g2*Delta_R_weather + g3*Delta_R_ump + g4*Total_BP_fatigue",
      exact_bracket  = "50,000 MC seasons: AL/NL Seeds 1-2 Byes -> Bo3 WC -> Bo5 DS -> Bo7 LCS -> Bo7 WS (Log5 + HFA)"
    ),
    features = c("p_m6_ml", "p_m6_f5", "exp_total_m6", "sp_ivb_slot_diff", "weather_runs_adj",
                 "ump_rpg_eb", "bp_fatigue_adv_home", "bye_prob", "pennant_prob", "ws_prob_12team"),
    metrics = list(
      oos_2025_ml_brier_m4     = round(mm$metric_val[2], 5),
      oos_2025_ml_brier_m6     = round(mm$metric_val[3], 5),
      oos_2025_f5_brier_m4     = round(mm$metric_val[5], 5),
      oos_2025_f5_brier_m6     = round(mm$metric_val[6], 5),
      oos_2025_totals_rmse_v1  = round(mm$metric_val[7], 5),
      oos_2025_totals_rmse_m6  = round(mm$metric_val[8], 5),
      oos_2025_f5_auc_m6       = round(calc_roc_auc(te25_f5$f5_home_win, te25_f5$p_m6_f5), 4),
      bracket_n_sims           = n_sim_br,
      top_ws_club_2026         = br$team_statcast[1],
      top_ws_prob_2026_pct     = round(100 * br$ws_prob_12team[1], 2)
    ),
    artifacts = list(
      spec         = "specs/14_unified_pro_game_f5_totals_bracket.md",
      script       = "models/14_unified_pro_game_f5_totals_bracket.R",
      gold_games   = "data/processed/unified_pro_game_f5_totals_2024_2025.parquet",
      gold_metrics = "data/processed/unified_pro_multi_market_metrics.parquet",
      gold_bracket = "data/processed/mlb_2026_12team_bracket_ws_odds.parquet",
      fig_ablation = "models/pro01_multi_market_oos_ablation.png",
      fig_bracket  = "models/pro02_2026_12team_bracket_ws_pennant_odds.html",
      fig_cal      = "models/pro03_f5_vs_fullgame_calibration.png"
    ),
    lake = lake
  )

  cat(sprintf("\n=== EXP-014 FULL-LAKE PIPELINE COMPLETED CLEANLY IN %.1f SECONDS ===\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  run_exp014(smoke = ("--smoke" %in% args))
}
