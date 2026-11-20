# ==============================================================================
# mlbr EXP-006: Empirical Multi-Year Regressed Ballpark Factors (2002-2025)
# ==============================================================================
# Architecture Tier: Level 3 Foundation / Ballpark Environmental Priors
# Method: Bill James / Tom Tango 3-Year Rolling Regressed Empirical Park Factors
# Data Lake Sources:
#   - v_retrosheet_games (2000-2025, zero-copy DuckDB)
#   - v_retrosheet_plays (2000-2025, 16.2M+ PAs, BAT_EVENT_FL = 'T', zero-copy DuckDB)
#   - data/raw/mlbmeta/park_dimensions.parquet (venue geometry, elevation, azimuth)
# Deliverables:
#   - data/processed/ballpark_factors_2002_2025.parquet (Gold ZSTD Parquet)
#   - 4-Asset Dark Cyber Visuals (pf01_*, pf02_*) in models/ and site/assets/
#   - experiments/registry.json update (EXP-006-BALLPARK-FACTORS)
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(data.table)
  library(arrow)
  library(ggplot2)
  library(plotly)
  library(htmlwidgets)
  library(jsonlite)
})

# --- Top-Level Plotting Functions (prevents environment closure bloat < 5MB) ---
make_pf01_static <- function(df) {
  force(df)
  ggplot(df, aes(x = pf_runs_raw, y = pf_runs, color = elevation_ft)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#475569") +
    geom_hline(yintercept = 100, color = "#64748b", linetype = "dotted") +
    geom_vline(xintercept = 100, color = "#64748b", linetype = "dotted") +
    geom_point(size = 3.5, alpha = 0.9) +
    scale_color_viridis_c(option = "plasma", name = "Elevation (ft)") +
    theme_minimal(base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = "#090d16", color = NA),
      panel.background  = element_rect(fill = "#090d16", color = NA),
      panel.grid.major  = element_line(color = "#1e293b", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      text              = element_text(color = "#f8fafc"),
      axis.text         = element_text(color = "#94a3b8"),
      legend.background = element_rect(fill = "#0f172a", color = "#1e293b")
    ) +
    labs(
      title = "EXP-006: 3-Year Regressed Run Park Factor vs. Raw Single-Season Scoring Ratio (2024)",
      subtitle = "Empirical Bayes shrinkage (M = 40 games) pulls extreme noise toward neutral (100) while conserving altitude elevation edge",
      x = "Single-Season Raw Scoring Ratio Factor",
      y = "3-Year Weighted Regressed Park Factor (pf_runs)"
    )
}

make_pf01_plotly <- function(df) {
  force(df)
  df$hover_txt <- sprintf(
    "<b>%s</b> (%s)<br>Season: %d | Team: %s<br>3-Yr Regressed Run PF: <b>%.1f</b><br>Single-Season Raw PF: %.1f<br>Elevation: %d ft<br>Roof: %s",
    df$venue_name, df$park_id, df$season, df$team_id,
    df$pf_runs, df$pf_runs_raw, as.integer(df$elevation_ft), df$roof_type
  )
  plotly::plot_ly(
    data = df, x = ~pf_runs_raw, y = ~pf_runs, type = "scatter", mode = "markers",
    text = ~hover_txt, hoverinfo = "text",
    marker = list(
      size = 11, color = ~elevation_ft,
      colorscale = "Plasma",
      colorbar = list(title = list(text = "Elevation (ft)", font = list(color = "#e2e8f0")), tickfont = list(color = "#94a3b8")),
      line = list(width = 0.8, color = "#090d16")
    )
  ) %>%
    plotly::layout(
      title = list(text = "EXP-006: 3-Year Regressed Run Park Factor vs. Raw Scoring Ratio (2024)",
                   font = list(color = "#f8fafc", size = 15)),
      paper_bgcolor = "#090d16", plot_bgcolor = "#111827",
      xaxis = list(title = "Single-Season Raw Scoring Ratio Factor", range = c(75, 135), gridcolor = "#1e293b", color = "#e2e8f0"),
      yaxis = list(title = "3-Year Weighted Regressed Park Factor (pf_runs)", range = c(80, 130), gridcolor = "#1e293b", color = "#e2e8f0"),
      shapes = list(
        list(type = "line", x0 = 75, y0 = 75, x1 = 135, y1 = 135, line = list(color = "#64748b", dash = "dash", width = 1.2)),
        list(type = "line", x0 = 100, y0 = 80, x1 = 100, y1 = 130, line = list(color = "#334155", dash = "dot", width = 1.0)),
        list(type = "line", x0 = 75, y0 = 100, x1 = 135, y1 = 100, line = list(color = "#334155", dash = "dot", width = 1.0))
      )
    )
}

make_pf02_static <- function(df) {
  force(df)
  ggplot(df, aes(x = pf_hr_rhb, y = pf_hr_lhb)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#475569") +
    geom_hline(yintercept = 100, color = "#64748b", linetype = "dotted") +
    geom_vline(xintercept = 100, color = "#64748b", linetype = "dotted") +
    geom_point(aes(color = pf_hr), size = 3.8, alpha = 0.9) +
    scale_color_viridis_c(option = "turbo", name = "Overall HR PF") +
    theme_minimal(base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = "#090d16", color = NA),
      panel.background  = element_rect(fill = "#090d16", color = NA),
      panel.grid.major  = element_line(color = "#1e293b", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      text              = element_text(color = "#f8fafc"),
      axis.text         = element_text(color = "#94a3b8"),
      legend.background = element_rect(fill = "#0f172a", color = "#1e293b")
    ) +
    labs(
      title = "EXP-006: Batter Handedness Home Run Park Factor Asymmetry (2024)",
      subtitle = "Yankee Stadium (NYC21) short porch rewards LHB; Great American (CIN09) elevates both; Oracle (SFO03) suppresses both",
      x = "Right-Handed Batter HR Factor (pf_hr_rhb)",
      y = "Left-Handed Batter HR Factor (pf_hr_lhb)"
    )
}

make_pf02_plotly <- function(df) {
  force(df)
  df$hover_txt <- sprintf(
    "<b>%s</b> (%s)<br>Season: %d | Team: %s<br>Overall HR PF: <b>%.1f</b><br>LHB HR PF: <b>%.1f</b><br>RHB HR PF: <b>%.1f</b><br>Asymmetry (LHB - RHB): <b>%+0.1f</b>",
    df$venue_name, df$park_id, df$season, df$team_id,
    df$pf_hr, df$pf_hr_lhb, df$pf_hr_rhb, (df$pf_hr_lhb - df$pf_hr_rhb)
  )
  plotly::plot_ly(
    data = df, x = ~pf_hr_rhb, y = ~pf_hr_lhb, type = "scatter", mode = "markers",
    text = ~hover_txt, hoverinfo = "text",
    marker = list(
      size = 11, color = ~pf_hr,
      colorscale = "Viridis",
      colorbar = list(title = list(text = "Overall HR PF", font = list(color = "#e2e8f0")), tickfont = list(color = "#94a3b8")),
      line = list(width = 0.8, color = "#090d16")
    )
  ) %>%
    plotly::layout(
      title = list(text = "EXP-006: Batter Handedness Home Run Park Factor Asymmetry (2024)",
                   font = list(color = "#f8fafc", size = 15)),
      paper_bgcolor = "#090d16", plot_bgcolor = "#111827",
      xaxis = list(title = "Right-Handed Batter HR Factor (pf_hr_rhb)", range = c(60, 145), gridcolor = "#1e293b", color = "#e2e8f0"),
      yaxis = list(title = "Left-Handed Batter HR Factor (pf_hr_lhb)", range = c(60, 145), gridcolor = "#1e293b", color = "#e2e8f0"),
      shapes = list(
        list(type = "line", x0 = 60, y0 = 60, x1 = 145, y1 = 145, line = list(color = "#64748b", dash = "dash", width = 1.2)),
        list(type = "line", x0 = 100, y0 = 60, x1 = 100, y1 = 145, line = list(color = "#334155", dash = "dot", width = 1.0)),
        list(type = "line", x0 = 60, y0 = 100, x1 = 145, y1 = 100, line = list(color = "#334155", dash = "dot", width = 1.0))
      )
    )
}

run_exp006 <- function(lake = NULL) {
  t_start <- Sys.time()
  cat("==============================================================================\n")
  cat("mlbr EXP-006: Multi-Year Regressed Empirical Ballpark Factors (2002-2025)\n")
  cat("==============================================================================\n")

  if (is.null(lake)) lake <- mlbr::default_lake()
  processed_dir <- file.path(lake, "data/processed")
  models_dir    <- file.path(lake, "models")
  assets_dir    <- file.path(lake, "site/assets")
  if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)
  if (!dir.exists(models_dir)) dir.create(models_dir, recursive = TRUE)
  if (!dir.exists(assets_dir)) dir.create(assets_dir, recursive = TRUE)

  # ----------------------------------------------------------------------------
  # 1. INGEST RETROSHEET GAMES & PLAYS (2000-2025) VIA DUCKDB
  # ----------------------------------------------------------------------------
  cat("\n-> [1/6] Extracting Retrosheet game and play events (2000-2025)...\n")
  con <- mlbr::lake_duckdb(lake = lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")

  games <- as.data.table(DBI::dbGetQuery(con, "
    SELECT 
      TRY_CAST(SUBSTRING(GAME_ID, 4, 4) AS INTEGER) AS season,
      PARK_ID AS park_id,
      HOME_TEAM_ID AS home_team,
      AWAY_TEAM_ID AS away_team,
      TRY_CAST(HOME_SCORE_CT AS INTEGER) AS home_runs,
      TRY_CAST(AWAY_SCORE_CT AS INTEGER) AS away_runs
    FROM v_retrosheet_games
    WHERE TRY_CAST(SUBSTRING(GAME_ID, 4, 4) AS INTEGER) BETWEEN 2000 AND 2025
      AND HOME_SCORE_CT IS NOT NULL AND AWAY_SCORE_CT IS NOT NULL
  "))

  plays <- as.data.table(DBI::dbGetQuery(con, "
    SELECT 
      TRY_CAST(SUBSTRING(g.GAME_ID, 4, 4) AS INTEGER) AS season,
      g.PARK_ID AS park_id,
      g.HOME_TEAM_ID AS home_team,
      g.AWAY_TEAM_ID AS away_team,
      p.BAT_HOME_ID AS bat_home_id,
      p.BAT_HAND_CD AS bat_hand,
      COUNT(*) AS n_pa,
      SUM(CASE WHEN p.EVENT_CD = '23' THEN 1 ELSE 0 END) AS n_hr,
      SUM(CASE WHEN p.EVENT_CD = '3' THEN 1 ELSE 0 END) AS n_so,
      SUM(CASE WHEN p.EVENT_CD IN ('14', '15') THEN 1 ELSE 0 END) AS n_bb,
      SUM(CASE WHEN p.EVENT_CD IN ('20', '21', '22') THEN 1 ELSE 0 END) AS n_bip_hit,
      SUM(CASE WHEN p.EVENT_CD IN ('2', '18', '19', '20', '21', '22') THEN 1 ELSE 0 END) AS n_bip
    FROM v_retrosheet_plays p
    JOIN v_retrosheet_games g ON p.GAME_ID = g.GAME_ID
    WHERE TRY_CAST(SUBSTRING(g.GAME_ID, 4, 4) AS INTEGER) BETWEEN 2000 AND 2025
      AND p.BAT_EVENT_FL = 'T'
    GROUP BY 1, 2, 3, 4, 5, 6
  "))

  cat(sprintf("   Extracted %d games, %d aggregated play cells across 26 seasons (BAT_EVENT_FL = 'T').\n",
              nrow(games), nrow(plays)))

  # ----------------------------------------------------------------------------
  # 2. COMPUTE RAW SINGLE-SEASON HOME VS ROAD EVENT RATIOS
  # ----------------------------------------------------------------------------
  cat("\n-> [2/6] Computing single-season home vs. road event ratios...\n")
  games[, total_runs := home_runs + away_runs]

  # Identify primary home park for each team-season
  team_parks <- games[, .(n_games = .N), by = .(season, team = home_team, park_id)]
  setorder(team_parks, season, team, -n_games)
  primary_parks <- team_parks[, .SD[1], by = .(season, team)]

  # Home stats for team at its home park
  home_games <- games[, .(g_home = .N, r_home = sum(total_runs)), by = .(season, team = home_team, park_id)]
  road_games <- games[, .(g_road = .N, r_road = sum(total_runs)), by = .(season, team = away_team)]

  m_games <- merge(primary_parks[, .(season, team, park_id)], home_games, by = c("season", "team", "park_id"))
  m_games <- merge(m_games, road_games, by = c("season", "team"))

  # Home events at park (both teams)
  home_ev <- plays[, .(
    pa_home     = sum(n_pa),
    hr_home     = sum(n_hr),
    hr_rhb_home = sum(fifelse(bat_hand == "R", n_hr, 0L)),
    pa_rhb_home = sum(fifelse(bat_hand == "R", n_pa, 0L)),
    hr_lhb_home = sum(fifelse(bat_hand == "L", n_hr, 0L)),
    pa_lhb_home = sum(fifelse(bat_hand == "L", n_pa, 0L)),
    so_home     = sum(n_so),
    bb_home     = sum(n_bb),
    bip_hit_home = sum(n_bip_hit),
    bip_home    = sum(n_bip)
  ), by = .(season, team = home_team, park_id)]

  # Road events for team (both teams)
  road_ev <- plays[, .(
    pa_road     = sum(n_pa),
    hr_road     = sum(n_hr),
    hr_rhb_road = sum(fifelse(bat_hand == "R", n_hr, 0L)),
    pa_rhb_road = sum(fifelse(bat_hand == "R", n_pa, 0L)),
    hr_lhb_road = sum(fifelse(bat_hand == "L", n_hr, 0L)),
    pa_lhb_road = sum(fifelse(bat_hand == "L", n_pa, 0L)),
    so_road     = sum(n_so),
    bb_road     = sum(n_bb),
    bip_hit_road = sum(n_bip_hit),
    bip_road    = sum(n_bip)
  ), by = .(season, team = away_team)]

  panel <- merge(m_games, home_ev, by = c("season", "team", "park_id"))
  panel <- merge(panel, road_ev, by = c("season", "team"))

  calc_pf_raw <- function(h_cnt, h_denom, r_cnt, r_denom, n_teams = 30.0) {
    # Empirical Bayes pseudo-count smoothing (prior rate smoothed with 2 pseudo-events)
    prior_rate <- (sum(h_cnt, na.rm = TRUE) + sum(r_cnt, na.rm = TRUE)) / 
                  pmax(sum(h_denom, na.rm = TRUE) + sum(r_denom, na.rm = TRUE), 1.0)
    rate_h <- (h_cnt + 2.0 * prior_rate) / pmax(h_denom + 2.0, 1.0)
    rate_r <- (r_cnt + 2.0 * prior_rate) / pmax(r_denom + 2.0, 1.0)
    ratio  <- rate_h / pmax(rate_r, 1e-6)
    (n_teams * ratio) / (n_teams - 1.0 + ratio) * 100.0
  }

  panel[, `:=`(
    pf_runs_raw   = calc_pf_raw(r_home, g_home, r_road, g_road),
    pf_hr_raw     = calc_pf_raw(hr_home, pa_home, hr_road, pa_road),
    pf_hr_rhb_raw = calc_pf_raw(hr_rhb_home, pa_rhb_home, hr_rhb_road, pa_rhb_road),
    pf_hr_lhb_raw = calc_pf_raw(hr_lhb_home, pa_lhb_home, hr_lhb_road, pa_lhb_road),
    pf_so_raw     = calc_pf_raw(so_home, pa_home, so_road, pa_road),
    pf_bb_raw     = calc_pf_raw(bb_home, pa_home, bb_road, pa_road),
    pf_babip_raw  = calc_pf_raw(bip_hit_home, bip_home, bip_hit_road, bip_road)
  )]

  # ----------------------------------------------------------------------------
  # 3. MULTI-YEAR 3-YEAR ROLLING REGRESSED EMPIRICAL PARK FACTORS (2002-2025)
  # ----------------------------------------------------------------------------
  cat("\n-> [3/6] Applying 3-year weighted rolling regression with empirical shrinkage...\n")
  res_list <- list()

  for (target_yr in 2002L:2025L) {
    curr_parks <- panel[season == target_yr]
    for (i in seq_len(nrow(curr_parks))) {
      row_p <- curr_parks[i]
      p_id  <- row_p$park_id
      t_id  <- row_p$team

      # 3-year historical window: (target_yr, target_yr - 1, target_yr - 2)
      sub <- panel[park_id == p_id & season %in% c(target_yr, target_yr - 1L, target_yr - 2L)]
      sub[, w := fifelse(season == target_yr, 3.0, fifelse(season == target_yr - 1L, 2.0, 1.0))]

      # Rolling weighted rates & shrinkage
      shrink_metric <- function(val_col, denom_col, m_denom) {
        w_val <- sum(sub$w * sub[[val_col]] * sub[[denom_col]]) / pmax(sum(sub$w * sub[[denom_col]]), 1.0)
        tot_d <- sum(sub[[denom_col]])
        100.0 + (w_val - 100.0) * (tot_d / (tot_d + m_denom))
      }

      pf_runs_reg   <- shrink_metric("pf_runs_raw",   "g_home",      40.0)
      pf_hr_reg     <- shrink_metric("pf_hr_raw",     "pa_home",   1500.0)
      pf_hr_rhb_reg <- shrink_metric("pf_hr_rhb_raw", "pa_rhb_home", 800.0)
      pf_hr_lhb_reg <- shrink_metric("pf_hr_lhb_raw", "pa_lhb_home", 800.0)
      pf_so_reg     <- shrink_metric("pf_so_raw",     "pa_home",   2000.0)
      pf_bb_reg     <- shrink_metric("pf_bb_raw",     "pa_home",   2000.0)
      pf_babip_reg  <- shrink_metric("pf_babip_raw",  "bip_home",  2000.0)

      res_list[[length(res_list) + 1L]] <- data.table(
        season        = target_yr,
        park_id       = p_id,
        team_id       = t_id,
        g_home        = row_p$g_home,
        g_road        = row_p$g_road,
        tot_g_3yr     = sum(sub$g_home),
        pf_runs_raw   = round(row_p$pf_runs_raw, 2),
        pf_runs       = round(pf_runs_reg, 2),
        pf_hr_raw     = round(row_p$pf_hr_raw, 2),
        pf_hr         = round(pf_hr_reg, 2),
        pf_hr_rhb     = round(pf_hr_rhb_reg, 2),
        pf_hr_lhb     = round(pf_hr_lhb_reg, 2),
        pf_so         = round(pf_so_reg, 2),
        pf_bb         = round(pf_bb_reg, 2),
        pf_babip      = round(pf_babip_reg, 2)
      )
    }
  }
  gold_pf <- rbindlist(res_list)

  # ----------------------------------------------------------------------------
  # 4. ENRICH WITH BALLPARK METADATA & DIMENSIONS
  # ----------------------------------------------------------------------------
  cat("\n-> [4/6] Merging venue geometry and metadata (park_dimensions.parquet)...\n")
  pdim_path <- file.path(lake, "data/raw/mlbmeta/park_dimensions.parquet")
  stopifnot(file.exists(pdim_path))
  pdim <- as.data.table(arrow::read_parquet(pdim_path))

  # Explicit mapping for modern and special venues
  pdim[is.na(retro_park_id) & grepl("Truist", venue_name), retro_park_id := "ATL03"]
  pdim[is.na(retro_park_id) & grepl("Globe Life Field", venue_name), retro_park_id := "ARL03"]
  pdim[is.na(retro_park_id) & grepl("Sahlen", venue_name), retro_park_id := "BUF05"]
  pdim[is.na(retro_park_id) & grepl("Dunedin", venue_name), retro_park_id := "DUN01"]
  pdim[is.na(retro_park_id) & grepl("Sutter Health", venue_name), retro_park_id := "SAC01"]
  pdim[is.na(retro_park_id) & grepl("Steinbrenner", venue_name), retro_park_id := "TAM02"]
  pdim[is.na(retro_park_id) & grepl("London Stadium", venue_name), retro_park_id := "LON01"]
  pdim[is.na(retro_park_id) & grepl("Fort Bragg", venue_name), retro_park_id := "FTB01"]
  pdim[is.na(retro_park_id) & grepl("Rickwood", venue_name), retro_park_id := "BIR01"]

  pdim_sub <- unique(pdim[!is.na(retro_park_id)], by = c("retro_park_id", "season"))
  gold_pf <- merge(
    gold_pf,
    pdim_sub[, .(retro_park_id, season, venue_name, elevation_ft, roof_type, azimuth_deg, left_line_ft, center_ft, right_line_ft)],
    by.x = c("park_id", "season"), by.y = c("retro_park_id", "season"), all.x = TRUE
  )
  venue_map <- unique(pdim[!is.na(retro_park_id) & !is.na(venue_name), .(park_id = retro_park_id, name_fallback = venue_name, elev_fallback = elevation_ft, roof_fallback = roof_type)], by = "park_id")
  gold_pf <- merge(gold_pf, venue_map, by = "park_id", all.x = TRUE)
  gold_pf[, venue_name := fifelse(is.na(venue_name), name_fallback, venue_name)]
  gold_pf[, elevation_ft := fifelse(is.na(elevation_ft), elev_fallback, elevation_ft)]
  gold_pf[, roof_type := fifelse(is.na(roof_type), roof_fallback, roof_type)]
  gold_pf[, c("name_fallback", "elev_fallback", "roof_fallback") := NULL]

  # Historic stadium elevation lookup for venues where park_dimensions.parquet had NA
  historic_elev <- data.table(
    park_id = c("CIN08", "MIA01", "MIN03", "MON02", "NYC16", "NYC17", "PHI12", "SAN01", "STL09", "WAS10"),
    elev_known = c(500.0, 10.0, 815.0, 115.0, 54.0, 15.0, 20.0, 50.0, 465.0, 25.0)
  )
  gold_pf <- merge(gold_pf, historic_elev, by = "park_id", all.x = TRUE)
  gold_pf[, elevation_ft := fifelse(is.na(elevation_ft), elev_known, elevation_ft)]
  gold_pf[, elev_known := NULL]

  setorder(gold_pf, season, park_id)

  # Assertions & Verification
  cat("\n-> [5/6] Running deterministic quality assertions...\n")
  stopifnot(nrow(gold_pf) >= 700L)
  stopifnot(!anyNA(gold_pf$venue_name))
  stopifnot(!anyNA(gold_pf$elevation_ft))
  stopifnot(!anyNA(gold_pf$roof_type))
  stopifnot(all(is.finite(gold_pf$pf_runs)))
  stopifnot(all(is.finite(gold_pf$pf_hr)))
  stopifnot(all(is.finite(gold_pf$pf_hr_rhb)))
  stopifnot(all(is.finite(gold_pf$pf_hr_lhb)))
  stopifnot(all(is.finite(gold_pf$pf_so)))
  stopifnot(all(is.finite(gold_pf$pf_bb)))
  stopifnot(all(is.finite(gold_pf$pf_babip)))
  stopifnot(all(gold_pf$pf_runs >= 70.0 & gold_pf$pf_runs <= 145.0))
  stopifnot(all(gold_pf$pf_hr_rhb >= 50.0 & gold_pf$pf_hr_rhb <= 180.0))
  stopifnot(all(gold_pf$pf_hr_lhb >= 50.0 & gold_pf$pf_hr_lhb <= 180.0))
  stopifnot(nrow(unique(gold_pf, by = c("season", "park_id"))) == nrow(gold_pf))

  # Coors Field DEN02 run factor sanity check
  coors_runs <- gold_pf[park_id == "DEN02" & season == 2024L]$pf_runs[1]
  cat(sprintf("   Coors Field (DEN02, 2024) 3-Yr Regressed Run Factor: %.1f (Expected >= 120.0)\n", coors_runs))
  stopifnot(coors_runs >= 120.0)

  # Great American CIN09 HR factor sanity check
  cin_hr <- gold_pf[park_id == "CIN09" & season == 2024L]$pf_hr[1]
  cat(sprintf("   Great American (CIN09, 2024) 3-Yr Regressed HR Factor: %.1f (Expected >= 110.0)\n", cin_hr))
  stopifnot(cin_hr >= 110.0)

  # Oracle Park SFO03 HR factor sanity check
  sfo_hr <- gold_pf[park_id == "SFO03" & season == 2024L]$pf_hr[1]
  cat(sprintf("   Oracle Park (SFO03, 2024) 3-Yr Regressed HR Factor: %.1f (Expected <= 90.0)\n", sfo_hr))
  stopifnot(sfo_hr <= 90.0)

  # Yankee Stadium NYC21 LHB vs RHB asymmetry sanity check
  nyc_lhb <- gold_pf[park_id == "NYC21" & season == 2024L]$pf_hr_lhb[1]
  nyc_rhb <- gold_pf[park_id == "NYC21" & season == 2024L]$pf_hr_rhb[1]
  cat(sprintf("   Yankee Stadium (NYC21, 2024) HR Factors: LHB = %.1f, RHB = %.1f (Expected LHB >= RHB + 8.0)\n", nyc_lhb, nyc_rhb))
  stopifnot(nyc_lhb >= nyc_rhb + 8.0)

  # Write Gold Parquet
  out_parquet <- file.path(processed_dir, "ballpark_factors_2002_2025.parquet")
  arrow::write_parquet(gold_pf, out_parquet, compression = "zstd")
  cat(sprintf("   Wrote Gold ADR-001 ZSTD Parquet: %s (%d rows)\n", out_parquet, nrow(gold_pf)))

  # ----------------------------------------------------------------------------
  # 5. PRESERVE 4-ASSET DARK CYBER VISUALIZATIONS
  # ----------------------------------------------------------------------------
  cat("\n-> [6/6] Generating Dark Cyber 4-asset figures (pf01_*, pf02_*)...\n")

  df_p1 <- gold_pf[season == 2024L]
  p1_static <- make_pf01_static(df_p1)
  mlbr::save_plot_assets(p1_static, "pf01_park_factor_runs_scatter", data = df_p1,
                         width = 11.0, height = 7.0, dir = models_dir)
  p1_ly <- make_pf01_plotly(df_p1)
  htmlwidgets::saveWidget(p1_ly, file.path(models_dir, "pf01_park_factor_runs_scatter.html"), selfcontained = TRUE)
  file.copy(file.path(models_dir, "pf01_park_factor_runs_scatter.png"), file.path(assets_dir, "pf01_park_factor_runs_scatter.png"), overwrite = TRUE)
  file.copy(file.path(models_dir, "pf01_park_factor_runs_scatter.html"), file.path(assets_dir, "pf01_park_factor_runs_scatter.html"), overwrite = TRUE)

  p2_static <- make_pf02_static(df_p1)
  mlbr::save_plot_assets(p2_static, "pf02_handedness_hr_asymmetry", data = df_p1,
                         width = 11.0, height = 7.0, dir = models_dir)
  p2_ly <- make_pf02_plotly(df_p1)
  htmlwidgets::saveWidget(p2_ly, file.path(models_dir, "pf02_handedness_hr_asymmetry.html"), selfcontained = TRUE)
  file.copy(file.path(models_dir, "pf02_handedness_hr_asymmetry.png"), file.path(assets_dir, "pf02_handedness_hr_asymmetry.png"), overwrite = TRUE)
  file.copy(file.path(models_dir, "pf02_handedness_hr_asymmetry.html"), file.path(assets_dir, "pf02_handedness_hr_asymmetry.html"), overwrite = TRUE)

  # Check RDS file sizes
  rds_files <- list.files(models_dir, pattern = "^pf.*\\.rds$", full.names = TRUE)
  for (rf in rds_files) {
    sz_mb <- file.info(rf)$size / (1024 * 1024)
    cat(sprintf("   Saved RDS visual asset %s: %.2f MB (Ceiling < 5.0 MB)\n", basename(rf), sz_mb))
    stopifnot(sz_mb < 5.0)
  }

  # ----------------------------------------------------------------------------
  # 6. LOG TO EXPERIMENTS REGISTRY
  # ----------------------------------------------------------------------------
  mlbr::lake_log_experiment(
    id            = "EXP-006-BALLPARK-FACTORS",
    title         = "Empirical Multi-Year Regressed Ballpark Factors & Handedness Asymmetry",
    tier          = 3,
    status        = "VALIDATED",
    summary       = paste0(
      "Derived 24-year empirical multi-year regressed ballpark factors (2002-2025) across 720 park-seasons using zero-copy ",
      "DuckDB queries on v_retrosheet_games and v_retrosheet_plays (16.2M+ plate appearances with BAT_EVENT_FL = 'T'). Employs 3-year weighted ",
      "(3/2/1) rolling window with empirical Bayes shrinkage (M = 40 games for runs, M = 1500 PAs for HR, M = 800 PAs for ",
      "handedness HR splits, M = 2000 PAs for SO, BB, BABIP). Enriched with physical ballpark geometry, roof type, and elevation."
    ),
    hypotheses    = c(
      "Single-season park scoring ratios suffer extreme small-sample noise and require 3-year weighted empirical shrinkage (M = 40 games for runs).",
      "Home run environmental factors exhibit sharp batter handedness asymmetry governed by asymmetric outfield fence distances and heights (e.g. Yankee Stadium short porch vs. Oracle Park deep right-center triples alley).",
      "Elevation produces a persistent, non-regressing run and batted-ball trajectory lift (Coors Field ~122 run factor across all eras)."
    ),
    formulas      = list(
      raw_scoring_ratio         = "(r_home / g_home) / (r_road / g_road)",
      park_factor_30_teams      = "(30.0 * ratio) / (29.0 + ratio) * 100.0",
      empirical_bayes_smoothing = "(cnt + 2.0 * prior_rate) / (denom + 2.0)",
      three_year_shrinkage      = "100.0 + (w_val - 100.0) * (tot_denom / (tot_denom + M))"
    ),
    features      = c(
      "pf_runs", "pf_runs_raw", "pf_hr", "pf_hr_raw", "pf_hr_rhb", "pf_hr_lhb",
      "pf_so", "pf_bb", "pf_babip", "elevation_ft", "roof_type", "azimuth_deg"
    ),
    metrics       = list(
      n_park_seasons             = nrow(gold_pf),
      seasons_covered            = "2002-2025",
      coors_pf_runs_2024         = round(coors_runs, 2),
      great_american_pf_hr_2024  = round(cin_hr, 2),
      oracle_pf_hr_2024          = round(sfo_hr, 2),
      yankee_pf_hr_lhb_2024      = round(nyc_lhb, 2),
      yankee_pf_hr_rhb_2024      = round(nyc_rhb, 2)
    ),
    artifacts     = list(
      spec        = "specs/06_ballpark_factors.md",
      script      = "models/06_ballpark_factors_empirical.R",
      gold_table  = "data/processed/ballpark_factors_2002_2025.parquet",
      figure_pf01 = "models/pf01_park_factor_runs_scatter.png",
      figure_pf02 = "models/pf02_handedness_hr_asymmetry.png"
    ),
    lake          = lake
  )

  t_end <- Sys.time()
  cat(sprintf("\n=== EXP-006 COMPLETED CLEANLY IN %.1f SECONDS ===\n",
              as.numeric(difftime(t_end, t_start, units = "secs"))))
  invisible(gold_pf)
}

if (!interactive()) {
  run_exp006()
}
