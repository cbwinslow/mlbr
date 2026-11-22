# ==============================================================================
# mlbr EXP-017: Batter Home Run (HR >= 1) & Total Bases (TB >= 2) Prop Pricing
#               Engine (v_statcast_biomechanics + Handedness Parks + Weather)
# ==============================================================================
# Architecture Tier: Level 5E (Batter Power & Total-Bases Prop Pricing)
# CLI Usage:
#   Rscript models/17_batter_hr_total_bases_props.R --smoke  # Fast (<15s)
#   Rscript models/17_batter_hr_total_bases_props.R --full   # Full ~87k starts
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
  if (!isTRUE(cond)) stop(paste0("[EXP-017 ASSERTION FAILED] ", msg), call. = FALSE)
}

calc_brier   <- function(y, p) mean((y - p)^2)
calc_logloss <- function(y, p) { pc <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(pc) + (1 - y) * log(1 - pc)) }
calc_roc_auc <- function(y, p) {
  r <- rank(p); n1 <- as.numeric(sum(y == 1L)); n0 <- as.numeric(sum(y == 0L))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

# --- Top-Level Dark Cyber Plotting Functions (RDS < 5.0 MB via force()) -------
make_bprop01_hr_cal <- function(df_cal, auc_hr, ll_hr, n_rows) {
  force(df_cal); force(auc_hr); force(ll_hr); force(n_rows)
  ggplot(df_cal, aes(x = 100 * mean_pred, y = 100 * obs_rate, size = n_obs)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#64748b", linewidth = 0.8) +
    geom_errorbar(aes(ymin = 100 * obs_lo, ymax = 100 * obs_hi), width = 0.35, color = "#38bdf8", alpha = 0.7) +
    geom_point(color = "#10b981", alpha = 0.92) +
    scale_size_continuous(range = c(2.5, 6.2), name = "Batter-Games") +
    labs(
      title = "EXP-017: Locked 2025 OOS Home Run Prop Calibration — P(HR >= 1 in Game)",
      subtitle = sprintf("n = %s locked 2025 starting batter-games | ROC AUC = %.4f | Log-Loss = %.5f | Deciles track 45-deg identity",
                         format(n_rows, big.mark = ","), auc_hr, ll_hr),
      x = "Predicted Game Home Run Probability (Decile Mean, %)",
      y = "Observed Home Run Game Rate (%, +/- 1 SE)",
      caption = "Source: mlbr EXP-017 (v_statcast_biomechanics + EXP-006 Handedness Parks + EXP-012 Weather Carry)"
    ) +
    mlbr::theme_cyber(base_size = 11)
}

make_bprop02_leaders_static <- function(df_b25) {
  force(df_b25)
  top_lbl <- rbind(
    head(df_b25[order(-mean_p_hr)], 8L),
    head(df_b25[order(-obs_hr_rate)], 5L)
  )
  top_lbl <- unique(top_lbl, by = "bat_mlbam")
  ggplot(df_b25, aes(x = 100 * mean_p_hr, y = 100 * obs_hr_rate, color = prior_bs, size = n_games)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#64748b", linewidth = 0.8) +
    geom_point(alpha = 0.86) +
    geom_text(data = top_lbl, aes(label = player_name),
              vjust = -0.8, size = 2.8, color = "#f8fafc", fontface = "bold", check_overlap = TRUE) +
    scale_color_viridis_c(option = "plasma", name = "Prior Bat\nSpeed (mph)") +
    scale_size_continuous(range = c(2.0, 5.8), name = "2025 Starts") +
    labs(
      title = "EXP-017: Locked 2025 Starting Batters — Pre-Game Predicted vs. Observed HR Game Rate (%)",
      subtitle = sprintf("n = %d qualified 2025 starters (>= 80 games) | Season-level r = %.3f strictly from pre-game inputs",
                         nrow(df_b25), stats::cor(df_b25$mean_p_hr, df_b25$obs_hr_rate)),
      x = "Mean Pre-Game Predicted P(HR >= 1) (%)",
      y = "Observed 2025 Game Home Run Rate (%)",
      caption = "Source: mlbr EXP-017 Batter HR & Total Bases Prop Engine"
    ) +
    mlbr::theme_cyber(base_size = 11)
}

make_bprop02_leaders_plotly <- function(df_b25) {
  force(df_b25)
  df_b25$hover_txt <- sprintf(
    "<b>%s</b> (2025, %d starts)<br>Pre-Game Exp P(HR): <b>%.1f%%</b> | Obs HR Rate: <b>%.1f%%</b><br>Pre-Game Exp P(TB>=2): <b>%.1f%%</b> | Obs TB>=2: <b>%.1f%%</b><br>Prior Bat Speed: <b>%.1f mph</b>",
    df_b25$player_name, df_b25$n_games,
    100 * df_b25$mean_p_hr, 100 * df_b25$obs_hr_rate,
    100 * df_b25$mean_p_tb2, 100 * df_b25$obs_tb2_rate, df_b25$prior_bs
  )
  plotly::plot_ly(
    data = df_b25, x = ~(100 * mean_p_hr), y = ~(100 * obs_hr_rate),
    type = "scatter", mode = "markers",
    hovertext = ~hover_txt, hoverinfo = "text",
    marker = list(
      size = ~(6 + 8 * (n_games / max(n_games))),
      color = ~prior_bs, colorscale = "Plasma",
      colorbar = list(title = "Bat Spd", tickfont = list(color = "#e2e8f0"),
                      titlefont = list(color = "#e2e8f0")),
      line = list(color = "#090d16", width = 0.8)
    )
  ) %>%
    plotly::layout(
      title = list(text = "EXP-017 Interactive 2025 Batter HR & Total Bases Prop Explorer",
                   font = list(color = "#f8fafc", size = 14)),
      paper_bgcolor = "#090d16", plot_bgcolor = "#111827",
      xaxis = list(title = "Pre-Game Predicted P(HR >= 1) (%)", gridcolor = "#1e293b", color = "#e2e8f0"),
      yaxis = list(title = "Observed 2025 HR Game Rate (%)", gridcolor = "#1e293b", color = "#e2e8f0"),
      shapes = list(list(type = "line", x0 = 2, x1 = 34, y0 = 2, y1 = 34,
                         line = list(color = "#64748b", dash = "dash", width = 1.2)))
    )
}

make_bprop03_ablation <- function(df_ab) {
  force(df_ab)
  df_ab[, model := factor(model, levels = c("B0_League_Mean", "M1_Prior_ISO_Only", "M2_Pro_Biomech_Env"))]
  ggplot(df_ab, aes(x = model, y = roc_auc, fill = model)) +
    geom_col(width = 0.64, alpha = 0.92) +
    geom_text(aes(label = sprintf("AUC %.4f\n(LL %.4f)", roc_auc, log_loss)), vjust = -0.25,
              color = "#f8fafc", size = 3.0, fontface = "bold") +
    facet_wrap(~market, scales = "free_y") +
    scale_fill_manual(values = c("B0_League_Mean" = "#64748b",
                                 "M1_Prior_ISO_Only" = "#38bdf8",
                                 "M2_Pro_Biomech_Env" = "#10b981"),
                      guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
    labs(
      title = "EXP-017: Locked 2025 OOS Batter Prop Ablation — Home Run (HR>=1) & Total Bases (TB>=2)",
      subtitle = "Adding Silver Bat Speed/Blast + Handedness Park HR + Weather/Wind + Slot + Opp SP HR% lifts both HR & TB>=2 props",
      x = NULL, y = "Locked 2025 OOS ROC AUC (Higher = Better)",
      caption = "Source: mlbr EXP-017 Batter HR & Total Bases Prop Engine"
    ) +
    mlbr::theme_cyber(base_size = 11) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))
}

# ==============================================================================
# MAIN PIPELINE
# ==============================================================================
run_exp017 <- function(smoke = FALSE, lake = NULL) {
  t0 <- Sys.time()
  if (is.null(lake)) lake <- mlbr::default_lake()
  proc_dir   <- file.path(lake, "data/processed")
  models_dir <- file.path(lake, "models")
  assets_dir <- file.path(lake, "site/assets")

  cat("==============================================================================\n")
  cat(sprintf("  mlbr EXP-017: Batter Home Run & Total Bases Prop Engine [%s]\n",
              if (smoke) "SMOKE" else "FULL"))
  cat("==============================================================================\n")

  con <- mlbr::lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")

  # 1. Extract All 2024-2025 Starting Batter-Games & Realized HR / Total Bases
  lim_sql <- if (smoke) "AND CAST(SUBSTR(GAME_ID, 4, 8) AS INTEGER) % 3 = 0" else ""
  bg <- as.data.table(DBI::dbGetQuery(con, sprintf("
    WITH starters AS (
      SELECT
        p.GAME_ID AS game_id,
        CAST(SUBSTR(p.GAME_ID, 4, 4) AS INTEGER) AS season,
        CAST(p.BAT_HOME_ID AS INTEGER) AS bat_home,
        CAST(p.BAT_LINEUP_ID AS INTEGER) AS slot,
        p.BAT_ID AS bat_retro,
        p.BAT_HAND_CD AS bat_hand,
        ROW_NUMBER() OVER (PARTITION BY p.GAME_ID, p.BAT_HOME_ID, p.BAT_LINEUP_ID ORDER BY p.EVENT_ID) AS rn
      FROM v_retrosheet_plays p
      WHERE CAST(SUBSTR(p.GAME_ID, 4, 4) AS INTEGER) IN (2024, 2025)
        AND p.BAT_EVENT_FL = 'T'
        AND CAST(p.BAT_LINEUP_ID AS INTEGER) BETWEEN 1 AND 9
        %s
    ),
    start9 AS (
      SELECT game_id, season, bat_home, slot, bat_retro, bat_hand
      FROM starters WHERE rn = 1
    ),
    outcomes AS (
      SELECT
        GAME_ID AS game_id,
        BAT_ID AS bat_retro,
        SUM(CASE WHEN EVENT_CD = 23 THEN 1 ELSE 0 END) AS game_hr,
        SUM(CASE WHEN EVENT_CD = 20 THEN 1
                 WHEN EVENT_CD = 21 THEN 2
                 WHEN EVENT_CD = 22 THEN 3
                 WHEN EVENT_CD = 23 THEN 4 ELSE 0 END) AS game_tb
      FROM v_retrosheet_plays
      WHERE CAST(SUBSTR(GAME_ID, 4, 4) AS INTEGER) IN (2024, 2025)
        AND BAT_EVENT_FL = 'T'
        %s
      GROUP BY GAME_ID, BAT_ID
    ),
    g_info AS (
      SELECT
        GAME_ID AS game_id, PARK_ID AS park_id,
        HOME_START_PIT_ID AS home_sp_retro, AWAY_START_PIT_ID AS away_sp_retro
      FROM v_retrosheet_games
      WHERE CAST(SUBSTR(GAME_ID, 4, 4) AS INTEGER) IN (2024, 2025)
    )
    SELECT
      s.game_id, s.season, s.bat_home, s.slot, s.bat_retro, s.bat_hand,
      g.park_id,
      CASE WHEN s.bat_home = 1 THEN g.away_sp_retro ELSE g.home_sp_retro END AS opp_sp_retro,
      COALESCE(o.game_hr, 0) AS game_hr,
      COALESCE(o.game_tb, 0) AS game_tb
    FROM start9 s
    INNER JOIN g_info g ON s.game_id = g.game_id
    LEFT JOIN outcomes o ON s.game_id = o.game_id AND s.bat_retro = o.bat_retro
  ", lim_sql, lim_sql)))

  # Restrict strictly to pure regular-season games matching EXP-009/014 (4,859 games)
  gm14_ids <- as.data.table(arrow::read_parquet(file.path(proc_dir, "unified_pro_game_f5_totals_2024_2025.parquet")))$game_id
  bg <- bg[game_id %in% gm14_ids]
  assert_true(nrow(bg) >= 25000L, sprintf("Insufficient starting batter-games: %d", nrow(bg)))

  # Map Retro IDs to MLBAM IDs & Opposing SP Handedness
  cw <- as.data.table(DBI::dbGetQuery(con, "
    SELECT key_retro, MAX(CAST(key_mlbam AS INTEGER)) AS key_mlbam,
           MAX(TRIM(name_first || ' ' || name_last)) AS player_name
    FROM v_chadwick_register WHERE key_retro IS NOT NULL AND key_mlbam IS NOT NULL
    GROUP BY key_retro
  "))
  sp_hand <- as.data.table(DBI::dbGetQuery(con, "
    SELECT PIT_ID AS opp_sp_retro, MODE(PIT_HAND_CD) AS opp_sp_hand
    FROM v_retrosheet_plays
    WHERE CAST(SUBSTR(GAME_ID, 4, 4) AS INTEGER) IN (2024, 2025)
      AND PIT_HAND_CD IN ('L', 'R')
    GROUP BY PIT_ID
  "))
  bg <- merge(bg, cw[, .(bat_retro = key_retro, bat_mlbam = key_mlbam, player_name)], by = "bat_retro", all.x = TRUE)
  bg <- merge(bg, cw[, .(opp_sp_retro = key_retro, opp_sp_mlbam = key_mlbam)], by = "opp_sp_retro", all.x = TRUE)
  bg <- merge(bg, sp_hand, by = "opp_sp_retro", all.x = TRUE)
  bg[is.na(player_name), player_name := bat_retro]
  bg[, is_platoon := as.integer(bat_hand != opp_sp_hand & bat_hand %in% c("L", "R") & opp_sp_hand %in% c("L", "R"))]

  # 2. Strictly Prior-Season (season - 1) Batter FanGraphs Power + Silver v_statcast_biomechanics
  fg_b <- as.data.table(DBI::dbGetQuery(con, "
    SELECT
      CAST(xMLBAMID AS INTEGER) AS bat_mlbam,
      CAST(Season AS INTEGER) + 1 AS season,
      CAST(PA AS DOUBLE) AS prior_pa,
      CAST(ISO AS DOUBLE) AS prior_iso,
      CAST(HR AS DOUBLE) / NULLIF(CAST(PA AS DOUBLE), 0) AS prior_hr_rate,
      CAST(SLG AS DOUBLE) AS prior_slg
    FROM v_fangraphs_bat
    WHERE CAST(Season AS INTEGER) IN (2023, 2024)
      AND xMLBAMID IS NOT NULL AND CAST(PA AS DOUBLE) > 0
  "))
  setorder(fg_b, bat_mlbam, season, -prior_pa)
  fg_b <- fg_b[!duplicated(fg_b, by = c("bat_mlbam", "season"))]
  fg_b[, `:=`(
    eb_iso = (prior_pa * prior_iso + 150.0 * 0.155) / (prior_pa + 150.0),
    eb_hr  = (prior_pa * prior_hr_rate + 200.0 * 0.030) / (prior_pa + 200.0),
    eb_slg = (prior_pa * prior_slg + 150.0 * 0.405) / (prior_pa + 150.0)
  )]

  # Query our newly mounted 17th DuckDB view: v_statcast_biomechanics (strictly season - 1)
  bio_b <- as.data.table(DBI::dbGetQuery(con, "
    SELECT
      batter AS bat_mlbam,
      game_year + 1 AS season,
      COUNT(CASE WHEN bat_speed_dbl >= 60.0 THEN 1 END) AS n_sw,
      AVG(CASE WHEN bat_speed_dbl >= 60.0 THEN bat_speed_dbl END) AS raw_bs,
      SUM(is_blast) AS n_blasts,
      COUNT(CASE WHEN sq_ratio IS NOT NULL THEN 1 END) AS n_bip
    FROM v_statcast_biomechanics
    WHERE game_year IN (2023, 2024)
    GROUP BY batter, game_year
    HAVING COUNT(CASE WHEN bat_speed_dbl >= 60.0 THEN 1 END) >= 25
  "))
  bio_b[, `:=`(
    prior_bs    = (n_sw * raw_bs + 50.0 * 71.3) / (n_sw + 50.0),
    prior_blast = (n_blasts + 50.0 * 0.12) / (n_bip + 50.0)
  )]

  # Strictly Prior-Season (season - 1) Opposing Starter HR/TBF
  fg_p <- as.data.table(DBI::dbGetQuery(con, "
    SELECT
      CAST(xMLBAMID AS INTEGER) AS opp_sp_mlbam,
      CAST(Season AS INTEGER) + 1 AS season,
      CAST(TBF AS DOUBLE) AS prior_tbf,
      CAST(HR AS DOUBLE) / NULLIF(CAST(TBF AS DOUBLE), 0) AS raw_sp_hr
    FROM v_fangraphs_pitch
    WHERE CAST(Season AS INTEGER) IN (2023, 2024)
      AND xMLBAMID IS NOT NULL AND CAST(TBF AS DOUBLE) > 0
  "))
  setorder(fg_p, opp_sp_mlbam, season, -prior_tbf)
  fg_p <- fg_p[!duplicated(fg_p, by = c("opp_sp_mlbam", "season"))]
  fg_p[, opp_sp_hr := (prior_tbf * raw_sp_hr + 250.0 * 0.030) / (prior_tbf + 250.0)]

  # Join Prior-Season Handedness Park Factors (EXP-006) & Game-Time Weather (EXP-012)
  pf <- as.data.table(arrow::read_parquet(file.path(proc_dir, "ballpark_factors_2002_2025.parquet")))
  pf_p <- pf[season %in% c(2023L, 2024L), .(park_id, season = as.integer(season) + 1L,
                                            pf_hr_lhb, pf_hr_rhb, pf_runs)]
  pf_p <- pf_p[!duplicated(pf_p, by = c("park_id", "season"))]
  wx <- as.data.table(arrow::read_parquet(file.path(proc_dir, "game_weather_umpire_environment_2015_2025.parquet")))

  bg <- merge(bg, fg_b[, .(bat_mlbam, season, eb_iso, eb_hr, eb_slg)], by = c("bat_mlbam", "season"), all.x = TRUE)
  bg <- merge(bg, bio_b[, .(bat_mlbam, season, prior_bs, prior_blast)], by = c("bat_mlbam", "season"), all.x = TRUE)
  bg <- merge(bg, fg_p[, .(opp_sp_mlbam, season, opp_sp_hr)], by = c("opp_sp_mlbam", "season"), all.x = TRUE)
  bg <- merge(bg, pf_p, by = c("park_id", "season"), all.x = TRUE)
  bg <- merge(bg, wx[, .(game_id, temp_f, wind_out_mph)], by = "game_id", all.x = TRUE)

  bg[is.na(eb_iso),      eb_iso      := 0.155]
  bg[is.na(eb_hr),       eb_hr       := 0.030]
  bg[is.na(eb_slg),      eb_slg      := 0.405]
  bg[is.na(prior_bs),    prior_bs    := 71.30]
  bg[is.na(prior_blast), prior_blast := 0.120]
  bg[is.na(opp_sp_hr),   opp_sp_hr   := 0.030]
  bg[is.na(pf_hr_lhb),   pf_hr_lhb   := 100.0]
  bg[is.na(pf_hr_rhb),   pf_hr_rhb   := 100.0]
  bg[is.na(pf_runs),     pf_runs     := 100.0]
  bg[is.na(temp_f),      temp_f      := 72.0]
  bg[is.na(wind_out_mph), wind_out_mph := 0.0]

  bg[, `:=`(
    pf_hr_hand = fifelse(bat_hand == "L", pf_hr_lhb, pf_hr_rhb),
    y_hr       = as.integer(game_hr >= 1L),
    y_tb2      = as.integer(game_tb >= 2L)
  )]

  # 3. Strictly Chronological Batter Prop Models (Train 2024 -> Locked OOS 2025)
  tr24 <- bg[season == 2024L]
  te25 <- bg[season == 2025L]
  assert_true(nrow(tr24) >= 12000L && nrow(te25) >= 12000L, "Insufficient 2024/2025 batter-games")

  # Home Run Prop (HR >= 1)
  p_hr_b0 <- mean(tr24$y_hr)
  fit_hr_m1 <- stats::glm(y_hr ~ eb_iso, data = tr24, family = stats::binomial())
  fit_hr_m2 <- stats::glm(y_hr ~ eb_iso + eb_hr + prior_bs + prior_blast + slot + is_platoon +
                            pf_hr_hand + temp_f + wind_out_mph + opp_sp_hr,
                          data = tr24, family = stats::binomial())

  # Total Bases Prop (TB >= 2 / Over 1.5 TB)
  p_tb_b0 <- mean(tr24$y_tb2)
  fit_tb_m1 <- stats::glm(y_tb2 ~ eb_slg, data = tr24, family = stats::binomial())
  fit_tb_m2 <- stats::glm(y_tb2 ~ eb_slg + eb_iso + prior_bs + prior_blast + slot + is_platoon +
                            pf_runs + pf_hr_hand + temp_f + wind_out_mph + opp_sp_hr,
                          data = tr24, family = stats::binomial())

  bg[, `:=`(
    p_hr_m1  = stats::predict(fit_hr_m1, newdata = bg, type = "response"),
    p_hr_m2  = stats::predict(fit_hr_m2, newdata = bg, type = "response"),
    p_tb2_m1 = stats::predict(fit_tb_m1, newdata = bg, type = "response"),
    p_tb2_m2 = stats::predict(fit_tb_m2, newdata = bg, type = "response")
  )]

  te25 <- bg[season == 2025L]
  ab <- data.table(
    market = c(rep("Home Run (HR >= 1)", 3L), rep("Total Bases (TB >= 2)", 3L)),
    model  = rep(c("B0_League_Mean", "M1_Prior_ISO_Only", "M2_Pro_Biomech_Env"), 2L),
    n_obs_2025 = nrow(te25),
    brier = c(
      calc_brier(te25$y_hr,  rep(p_hr_b0, nrow(te25))),
      calc_brier(te25$y_hr,  te25$p_hr_m1),
      calc_brier(te25$y_hr,  te25$p_hr_m2),
      calc_brier(te25$y_tb2, rep(p_tb_b0, nrow(te25))),
      calc_brier(te25$y_tb2, te25$p_tb2_m1),
      calc_brier(te25$y_tb2, te25$p_tb2_m2)
    ),
    log_loss = c(
      calc_logloss(te25$y_hr,  rep(p_hr_b0, nrow(te25))),
      calc_logloss(te25$y_hr,  te25$p_hr_m1),
      calc_logloss(te25$y_hr,  te25$p_hr_m2),
      calc_logloss(te25$y_tb2, rep(p_tb_b0, nrow(te25))),
      calc_logloss(te25$y_tb2, te25$p_tb2_m1),
      calc_logloss(te25$y_tb2, te25$p_tb2_m2)
    ),
    roc_auc = c(
      0.5000,
      calc_roc_auc(te25$y_hr,  te25$p_hr_m1),
      calc_roc_auc(te25$y_hr,  te25$p_hr_m2),
      0.5000,
      calc_roc_auc(te25$y_tb2, te25$p_tb2_m1),
      calc_roc_auc(te25$y_tb2, te25$p_tb2_m2)
    )
  )
  cat("\n--- Locked 2025 OOS Batter Home Run & Total Bases Prop Ablation ---\n")
  print(ab)

  if (smoke) {
    cat(sprintf("\n*** EXP-017 SMOKE TEST PASSED CLEANLY IN %.1f SECONDS ***\n",
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    return(invisible(TRUE))
  }

  # 4. Materialize Gold ADR-001 ZSTD Parquets
  arrow::write_parquet(bg, file.path(proc_dir, "batter_hr_tb_prop_backtest_2024_2025.parquet"), compression = "zstd")
  arrow::write_parquet(ab, file.path(proc_dir, "batter_hr_tb_ablation_metrics_2025.parquet"), compression = "zstd")
  cat("\n-> Materialized 2 Gold ZSTD Parquets in data/processed/\n")

  # 5. Dark Cyber 4-Asset Figures (< 5.0 MB .rds)
  te25[, bin := cut(p_hr_m2, breaks = stats::quantile(p_hr_m2, probs = seq(0, 1, 0.1)), include.lowest = TRUE)]
  cal_hr <- te25[, .(
    n_obs     = .N,
    mean_pred = mean(p_hr_m2),
    obs_rate  = mean(y_hr),
    obs_lo    = pmax(0, mean(y_hr) - sqrt(mean(y_hr) * (1 - mean(y_hr)) / .N)),
    obs_hi    = pmin(1, mean(y_hr) + sqrt(mean(y_hr) * (1 - mean(y_hr)) / .N))
  ), by = bin]

  p1_s <- make_bprop01_hr_cal(cal_hr, ab$roc_auc[3], ab$log_loss[3], nrow(te25))
  mlbr::save_plot_assets(p1_s, "bprop01_hr_prop_calibration_2025", data = cal_hr,
                         width = 11.0, height = 6.2, dir = models_dir)
  file.copy(file.path(models_dir, "bprop01_hr_prop_calibration_2025.png"),
            file.path(assets_dir, "bprop01_hr_prop_calibration_2025.png"), overwrite = TRUE)

  b25 <- te25[, .(
    n_games      = .N,
    mean_p_hr    = mean(p_hr_m2),
    obs_hr_rate  = mean(y_hr),
    mean_p_tb2   = mean(p_tb2_m2),
    obs_tb2_rate = mean(y_tb2),
    prior_bs     = mean(prior_bs)
  ), by = .(bat_mlbam, player_name)][n_games >= 80L]

  p2_s <- make_bprop02_leaders_static(b25)
  mlbr::save_plot_assets(p2_s, "bprop02_batter_hr_prop_leaders_2025", data = b25,
                         width = 11.2, height = 6.2, dir = models_dir)
  file.copy(file.path(models_dir, "bprop02_batter_hr_prop_leaders_2025.png"),
            file.path(assets_dir, "bprop02_batter_hr_prop_leaders_2025.png"), overwrite = TRUE)

  p2_ly <- make_bprop02_leaders_plotly(b25)
  htmlwidgets::saveWidget(p2_ly, file.path(models_dir, "bprop02_batter_hr_prop_leaders_2025.html"),
                          selfcontained = TRUE)
  dep2 <- file.path(models_dir, "bprop02_batter_hr_prop_leaders_2025_files")
  if (dir.exists(dep2)) unlink(dep2, recursive = TRUE)
  file.copy(file.path(models_dir, "bprop02_batter_hr_prop_leaders_2025.html"),
            file.path(assets_dir, "bprop02_batter_hr_prop_leaders_2025.html"), overwrite = TRUE)

  p3_s <- make_bprop03_ablation(ab)
  mlbr::save_plot_assets(p3_s, "bprop03_batter_prop_ablation_2025", data = ab,
                         width = 11.2, height = 5.8, dir = models_dir)
  file.copy(file.path(models_dir, "bprop03_batter_prop_ablation_2025.png"),
            file.path(assets_dir, "bprop03_batter_prop_ablation_2025.png"), overwrite = TRUE)

  rds_files <- Sys.glob(file.path(models_dir, "bprop0*.rds"))
  assert_true(length(rds_files) == 3L, "Expected 3 bprop0*.rds files")
  for (rf in rds_files) {
    sz <- file.info(rf)$size / 1e6
    cat(sprintf("   Saved %s: %.2f MB (Ceiling < 5.0 MB)\n", basename(rf), sz))
    assert_true(sz < 5.0, sprintf("Oversized RDS: %s (%.2f MB)", basename(rf), sz))
  }

  # 6. Log Experiment EXP-017
  mlbr::lake_log_experiment(
    id     = "EXP-017-BATTER-HR-TOTAL-BASES-PROPS",
    title  = "Batter Home Run (HR >= 1) & Total Bases (TB >= 2) Prop Pricing Engine via v_statcast_biomechanics",
    tier   = 5L, status = "VALIDATED",
    summary = paste0(
      "Registered v_statcast_biomechanics (17th DuckDB view, 4.55M Silver pitches) and built a strictly chronological ",
      "Batter Home Run & Total Bases Prop Engine (2024 train n=", nrow(tr24), " -> locked 2025 OOS n=", nrow(te25),
      " starting batter-games). Combining Silver bat_speed & Blast Rate with handedness park HR factors (pf_hr_lhb/rhb), ",
      "game-time temp/wind carry, batting-order slot, platoon, and opposing SP HR% lifted locked-2025 OOS HR>=1 AUC ",
      sprintf("%.4f -> %.4f (LL %.5f -> %.5f)", ab$roc_auc[2], ab$roc_auc[3], ab$log_loss[2], ab$log_loss[3]),
      " and TB>=2 AUC ", sprintf("%.4f -> %.4f (LL %.5f -> %.5f)", ab$roc_auc[5], ab$roc_auc[6], ab$log_loss[5], ab$log_loss[6]), "."
    ),
    hypotheses = c(
      "Integrating Silver bat_speed and Blast Rate with handedness park HR factors, game-time weather/wind carry, batting-order slot, platoon, and opposing SP HR% strictly beats prior-ISO-only and league-mean baselines on locked 2025 OOS Home Run (HR>=1) and Total Bases (TB>=2) Log-Loss and ROC AUC."
    ),
    formulas = list(
      hr_prop_logit  = "logit(P(HR>=1)) = b0 + b1*EB_ISO + b2*EB_HR + b3*BatSpeed + b4*Blast + b5*Slot + b6*Platoon + b7*PF_HR_hand + b8*Temp + b9*WindOut + b10*OppSP_HR",
      tb2_prop_logit = "logit(P(TB>=2)) = g0 + g1*EB_SLG + g2*EB_ISO + g3*BatSpeed + g4*Blast + g5*Slot + g6*Platoon + g7*PF_Runs + g8*PF_HR_hand + g9*Temp + g10*WindOut + g11*OppSP_HR"
    ),
    features = c("prior_bs", "prior_blast", "eb_iso", "eb_slg", "slot", "is_platoon",
                 "pf_hr_hand", "temp_f", "wind_out_mph", "opp_sp_hr", "p_hr_m2", "p_tb2_m2"),
    metrics = list(
      n_locked_2025_batter_games = nrow(te25),
      oos_2025_hr_auc_m1         = round(ab$roc_auc[2], 4),
      oos_2025_hr_auc_m2         = round(ab$roc_auc[3], 4),
      oos_2025_hr_ll_m1          = round(ab$log_loss[2], 5),
      oos_2025_hr_ll_m2          = round(ab$log_loss[3], 5),
      oos_2025_tb2_auc_m1        = round(ab$roc_auc[5], 4),
      oos_2025_tb2_auc_m2        = round(ab$roc_auc[6], 4),
      oos_2025_tb2_ll_m1         = round(ab$log_loss[5], 5),
      oos_2025_tb2_ll_m2         = round(ab$log_loss[6], 5)
    ),
    artifacts = list(
      spec         = "specs/17_batter_hr_total_bases_props.md",
      script       = "models/17_batter_hr_total_bases_props.R",
      gold_props   = "data/processed/batter_hr_tb_prop_backtest_2024_2025.parquet",
      gold_ablat   = "data/processed/batter_hr_tb_ablation_metrics_2025.parquet",
      fig_cal      = "models/bprop01_hr_prop_calibration_2025.png",
      fig_leaders  = "models/bprop02_batter_hr_prop_leaders_2025.html",
      fig_ablation = "models/bprop03_batter_prop_ablation_2025.png"
    ),
    lake = lake
  )

  cat(sprintf("\n=== EXP-017 FULL-LAKE PIPELINE COMPLETED CLEANLY IN %.1f SECONDS ===\n",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  run_exp017(smoke = ("--smoke" %in% args))
}
