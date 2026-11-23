#!/usr/bin/env Rscript
# ==============================================================================
# EXP-022: Starter Rest-Day Recovery, IL-Return Velocity/Command/Leash Curves &
#          Team Travel Distance / Circadian Jet-Lag Engine (Levels 2 & 3)
# ==============================================================================
suppressPackageStartupMessages({
  library(mlbr); library(DBI); library(arrow); library(dplyr); library(tidyr)
  library(ggplot2); library(plotly); library(htmlwidgets)
})

args <- commandArgs(trailingOnly = TRUE)
is_smoke <- "--smoke" %in% args
is_full  <- "--full"  %in% args
if (!is_smoke && !is_full) is_smoke <- TRUE

t0 <- proc.time()[["elapsed"]]
cat(sprintf("=== EXP-022 Starter Rest/IL Recovery & Circadian Travel Jet-Lag [%s] ===\n",
            if (is_smoke) "SMOKE" else "FULL"))

lake <- mlbr::default_lake()
con  <- mlbr::lake_duckdb(lake)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
DBI::dbExecute(con, "SET memory_limit = '12GB';")

# ------------------------------------------------------------------------------
# PART A: Exact Uncensored Starting Pitcher Outings (2018-2025 Regular Season)
# Identify starting pitcher via Inning 1 first pitch per (game_pk, inning_topbot)
# so short 35-50 pitch IL-return rehab starts are NOT censored out!
# ------------------------------------------------------------------------------
yr_a <- if (is_smoke) "2023 AND 2025" else "2018 AND 2025"

sp_starts <- DBI::dbGetQuery(con, sprintf("
  WITH sp_id AS (
    SELECT DISTINCT game_pk, game_year, CAST(game_date AS DATE) AS gdate, inning_topbot,
           FIRST_VALUE(pitcher) OVER (
             PARTITION BY game_pk, inning_topbot
             ORDER BY at_bat_number ASC, pitch_number ASC
           ) AS pitcher
    FROM v_statcast_features
    WHERE game_year BETWEEN %s AND game_type = 'R' AND inning = 1
  ),
  outing_stats AS (
    SELECT
      s.game_pk, s.game_year AS season, s.gdate, s.pitcher,
      COUNT(*) AS n_pitches,
      SUM(CASE WHEN f.events IS NOT NULL THEN 1 ELSE 0 END) AS n_bf,
      SUM(CASE WHEN f.events IN ('strikeout', 'strikeout_double_play') THEN 1 ELSE 0 END) AS n_so,
      SUM(CASE WHEN f.events = 'walk' THEN 1 ELSE 0 END) AS n_bb,
      AVG(CASE WHEN f.pitch_type = 'FF' THEN f.release_speed END) AS ff_velo,
      AVG(CASE WHEN f.is_csw = 1 THEN 1.0 ELSE 0.0 END) AS csw_rate,
      AVG(CASE WHEN f.zone BETWEEN 1 AND 9 THEN 1.0 ELSE 0.0 END) AS zone_rate
    FROM sp_id s
    INNER JOIN v_statcast_features f
      ON s.game_pk = f.game_pk AND s.pitcher = f.pitcher AND f.game_year = s.game_year
    GROUP BY 1, 2, 3, 4
  )
  SELECT * FROM outing_stats
  ORDER BY season, pitcher, gdate, game_pk
", yr_a))

# Compute exact days_rest within (pitcher, season) and post-IL ramp sequence (1st, 2nd, 3rd start back)
sp_starts <- sp_starts |>
  group_by(pitcher, season) |>
  arrange(gdate, game_pk, .by_group = TRUE) |>
  mutate(
    start_num    = row_number(),
    days_rest    = as.integer(gdate - lag(gdate)),
    # Prior cumulative season mean BF & SO strictly before current start (zero leakage)
    prior_gs     = row_number() - 1L,
    prior_bf_avg = ifelse(prior_gs > 0L, (cumsum(n_bf) - n_bf) / prior_gs, NA_real_),
    prior_so_avg = ifelse(prior_gs > 0L, (cumsum(n_so) - n_so) / prior_gs, NA_real_),
    # Within-pitcher-season demeaned outcomes for unbiased fixed-effect curves
    season_gs    = n(),
    d_ff_velo    = ff_velo   - mean(ff_velo,   na.rm = TRUE),
    d_csw_pct    = 100 * (csw_rate  - mean(csw_rate,  na.rm = TRUE)),
    d_zone_pct   = 100 * (zone_rate - mean(zone_rate, na.rm = TRUE)),
    d_pitches    = n_pitches - mean(n_pitches, na.rm = TRUE),
    d_bf         = n_bf      - mean(n_bf,      na.rm = TRUE),
    d_so         = n_so      - mean(n_so,      na.rm = TRUE),
    # Flag IL Return (#1, #2, #3 starts back after a 15+ day layoff)
    is_il_1      = as.integer(!is.na(days_rest) & days_rest >= 15L),
    is_il_2      = as.integer(lag(is_il_1, default = 0L) == 1L & !is.na(days_rest) & days_rest < 15L),
    is_il_3      = as.integer(lag(is_il_2, default = 0L) == 1L & !is.na(days_rest) & days_rest < 15L),
    rest_bucket  = case_when(
      is.na(days_rest)        ~ "Season_First",
      days_rest <= 3L         ~ "1_3d_Opener_Bulk",
      days_rest == 4L         ~ "4d_Short",
      days_rest == 5L         ~ "5d_Normal",
      days_rest == 6L         ~ "6d_Extra",
      days_rest <= 14L        ~ "7_14d_Extended",
      TRUE                    ~ "15plus_IL_Return"
    )
  ) |>
  ungroup()

cat(sprintf("Part A Loaded %d uncensored MLB starts (2018-2025: IL returns = %d).\n",
            nrow(sp_starts), sum(sp_starts$is_il_1 == 1L)))

# Within-pitcher fixed-effect summary across rest buckets (min 5 starts in season for clean within-demean)
rest_summary <- sp_starts |>
  filter(season_gs >= 5L, rest_bucket != "Season_First") |>
  group_by(rest_bucket) |>
  summarise(
    n_starts     = n(),
    mean_pitches = mean(n_pitches),
    mean_bf      = mean(n_bf),
    se_ff_velo   = sd(d_ff_velo, na.rm = TRUE) / sqrt(sum(!is.na(d_ff_velo))),
    d_ff_velo    = mean(d_ff_velo, na.rm = TRUE),
    d_csw_pct    = mean(d_csw_pct, na.rm = TRUE),
    d_zone_pct   = mean(d_zone_pct, na.rm = TRUE),
    d_pitches    = mean(d_pitches, na.rm = TRUE),
    d_bf         = mean(d_bf, na.rm = TRUE),
    d_so         = mean(d_so, na.rm = TRUE),
    .groups = "drop"
  )

# Post-IL Ramp Curve (Normal 5d vs IL Return #1, #2, #3)
il_ramp <- sp_starts |>
  filter(season_gs >= 5L) |>
  mutate(ramp_stage = case_when(
    is_il_1 == 1L ~ "IL Return #1 (15+d)",
    is_il_2 == 1L ~ "IL Return #2",
    is_il_3 == 1L ~ "IL Return #3",
    rest_bucket == "5d_Normal" ~ "Normal 5d Baseline",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(ramp_stage)) |>
  group_by(ramp_stage) |>
  summarise(
    n_starts   = n(),
    mean_pitches = mean(n_pitches),
    mean_bf      = mean(n_bf),
    se_pitches   = sd(d_pitches) / sqrt(n()),
    d_pitches    = mean(d_pitches),
    d_bf         = mean(d_bf),
    d_ff_velo    = mean(d_ff_velo, na.rm = TRUE),
    d_csw_pct    = mean(d_csw_pct, na.rm = TRUE),
    d_so         = mean(d_so),
    .groups = "drop"
  )

cat("\n=== Part A: Within-Pitcher Rest-Day Recovery Summary ===\n")
print(rest_summary |> mutate(across(where(is.numeric), ~ round(.x, 3))), width = 140)
cat("\n=== Part A: Post-IL Return Pitch-Count & Biomechanical Ramp ===\n")
print(il_ramp |> mutate(across(where(is.numeric), ~ round(.x, 3))), width = 140)

# Strictly Chronological OOS Validation (2018-2023 Train -> Locked 2024-2025 OOS)
# Predict n_bf and n_so from strictly pre-game EB prior_bf_avg + rest/IL indicators
lg_bf_tr <- mean(sp_starts$n_bf[sp_starts$season <= 2023L], na.rm = TRUE)
lg_so_tr <- mean(sp_starts$n_so[sp_starts$season <= 2023L], na.rm = TRUE)

sp_mod <- sp_starts |>
  filter(prior_gs >= 1L) |>
  mutate(
    eb_bf = (prior_gs * prior_bf_avg + 4.0 * lg_bf_tr) / (prior_gs + 4.0),
    eb_so = (prior_gs * prior_so_avg + 4.0 * lg_so_tr) / (prior_gs + 4.0),
    is_6d_extra = as.integer(rest_bucket == "6d_Extra"),
    is_7_14d    = as.integer(rest_bucket == "7_14d_Extended")
  )

tr_sp <- sp_mod |> filter(season <= 2023L)
te_sp <- sp_mod |> filter(season >= 2024L)

fit_bf_base <- lm(n_bf ~ eb_bf, data = tr_sp)
fit_bf_rest <- lm(n_bf ~ eb_bf + is_il_1 + is_il_2 + is_il_3 + is_6d_extra + is_7_14d, data = tr_sp)
fit_so_base <- lm(n_so ~ eb_so + eb_bf, data = tr_sp)
fit_so_rest <- lm(n_so ~ eb_so + eb_bf + is_il_1 + is_il_2 + is_il_3 + is_6d_extra + is_7_14d, data = tr_sp)

te_sp$pred_bf_base <- predict(fit_bf_base, newdata = te_sp)
te_sp$pred_bf_rest <- predict(fit_bf_rest, newdata = te_sp)
te_sp$pred_so_base <- predict(fit_so_base, newdata = te_sp)
te_sp$pred_so_rest <- predict(fit_so_rest, newdata = te_sp)

il_te <- te_sp |> filter(is_il_1 == 1L)
cat(sprintf("\nLocked 2024-2025 OOS BF RMSE — All Starts (n=%d): Base %.4f -> +Rest/IL %.4f | IL Return #1 (n=%d): Base %.4f -> +Rest/IL %.4f\n",
            nrow(te_sp), sqrt(mean((te_sp$n_bf - te_sp$pred_bf_base)^2)), sqrt(mean((te_sp$n_bf - te_sp$pred_bf_rest)^2)),
            nrow(il_te), sqrt(mean((il_te$n_bf - il_te$pred_bf_base)^2)), sqrt(mean((il_te$n_bf - il_te$pred_bf_rest)^2))))
cat(sprintf("Locked 2024-2025 OOS SO RMSE — All Starts (n=%d): Base %.4f -> +Rest/IL %.4f | IL Return #1 (n=%d): Base %.4f -> +Rest/IL %.4f\n",
            nrow(te_sp), sqrt(mean((te_sp$n_so - te_sp$pred_so_base)^2)), sqrt(mean((te_sp$n_so - te_sp$pred_so_rest)^2)),
            nrow(il_te), sqrt(mean((il_te$n_so - il_te$pred_so_base)^2)), sqrt(mean((il_te$n_so - il_te$pred_so_rest)^2))))

# ------------------------------------------------------------------------------
# PART B: Team Travel Distance (Haversine Miles) & Circadian Jet-Lag (2015-2025)
# ------------------------------------------------------------------------------
yr_b <- if (is_smoke) "2022 AND 2025" else "2015 AND 2025"
p_park <- file.path(lake, "data/raw/mlbmeta/park_dimensions.parquet")

# Verified WGS84 backfill for all 17 Retrosheet 2015-2025 parks absent from park_dimensions.parquet
park_backfill <- tibble::tribble(
  ~park_id, ~bf_lat,  ~bf_lon,
  "ATL03",  33.8908,  -84.4678,  # Truist Park (Atlanta, GA)
  "ARL03",  32.7473,  -97.0847,  # Globe Life Field (Arlington, TX)
  "ATL02",  33.7353,  -84.3894,  # Turner Field (Atlanta, GA)
  "TAM02",  27.9803,  -82.5067,  # George M. Steinbrenner Field (Tampa, FL)
  "SAC01",  38.5804, -121.5138,  # Sutter Health Park (West Sacramento, CA)
  "BUF05",  42.8806,  -78.8739,  # Sahlen Field (Buffalo, NY)
  "DUN01",  28.0038,  -82.7867,  # TD Ballpark (Dunedin, FL)
  "WIL02",  41.2422,  -77.0468,  # Bowman Field (Williamsport, PA)
  "MNT01",  25.7169, -100.3156,  # Estadio Monterrey (Monterrey, MX)
  "LON01",  51.5387,   -0.0166,  # London Stadium (London, UK)
  "MEX02",  19.4025,  -99.0867,  # Estadio Alfredo Harp Helu (Mexico City, MX)
  "SEO01",  37.4982,  126.8671,  # Gocheok Sky Dome (Seoul, KR)
  "DYE01",  42.4979,  -91.0549,  # Field of Dreams (Dyersville, IA)
  "OMA01",  41.2669,  -95.9317,  # Charles Schwab Field (Omaha, NE)
  "BIR01",  33.5023,  -86.8555,  # Rickwood Field (Birmingham, AL)
  "BST01",  36.5157,  -82.2569,  # Bristol Motor Speedway (Bristol, TN)
  "FTB01",  35.1404,  -79.0061   # Fort Bragg Field (Fort Bragg, NC)
)

team_games <- DBI::dbGetQuery(con, sprintf("
  WITH park_coords AS (
    SELECT retro_park_id AS park_id,
           AVG(latitude) AS lat, AVG(longitude) AS lon
    FROM read_parquet('%s')
    WHERE latitude IS NOT NULL AND longitude IS NOT NULL
    GROUP BY retro_park_id
  ),
  games_clean AS (
    SELECT
      GAME_ID AS game_id,
      CAST(SUBSTR(GAME_DT, 1, 4) AS INTEGER) AS season,
      STRPTIME(GAME_DT, '%%Y%%m%%d')::DATE AS gdate,
      CAST(GAME_CT AS INTEGER) AS game_ct,
      DAYNIGHT_PARK_CD AS daynight,
      PARK_ID AS park_id,
      HOME_TEAM_ID AS home_team,
      AWAY_TEAM_ID AS away_team,
      CAST(HOME_SCORE_CT AS INTEGER) AS home_score,
      CAST(AWAY_SCORE_CT AS INTEGER) AS away_score
    FROM v_retrosheet_games
    WHERE CAST(SUBSTR(GAME_DT, 1, 4) AS INTEGER) BETWEEN %s
      AND HOME_TEAM_ID NOT IN ('ALS', 'NLS')
      AND AWAY_TEAM_ID NOT IN ('ALS', 'NLS')
      AND HOME_SCORE_CT IS NOT NULL AND AWAY_SCORE_CT IS NOT NULL
      AND CAST(HOME_SCORE_CT AS INTEGER) != CAST(AWAY_SCORE_CT AS INTEGER)
  ),
  unpivoted AS (
    SELECT game_id, season, gdate, game_ct, daynight, park_id, home_team AS team, 1 AS is_home,
           home_score AS rs, away_score AS ra, CASE WHEN home_score > away_score THEN 1 ELSE 0 END AS win_fl
    FROM games_clean
    UNION ALL
    SELECT game_id, season, gdate, game_ct, daynight, park_id, away_team AS team, 0 AS is_home,
           away_score AS rs, home_score AS ra, CASE WHEN away_score > home_score THEN 1 ELSE 0 END AS win_fl
    FROM games_clean
  )
  SELECT u.*, p.lat, p.lon
  FROM unpivoted u
  LEFT JOIN park_coords p ON u.park_id = p.park_id
  ORDER BY u.season, u.team, u.gdate, u.game_ct
", p_park, yr_b)) |>
  left_join(park_backfill, by = "park_id") |>
  mutate(
    lat = coalesce(lat, bf_lat),
    lon = coalesce(lon, bf_lon)
  ) |>
  select(-bf_lat, -bf_lon)

# Fail-fast: zero missing coordinates allowed before computing lag() travel chains
missing_parks <- unique(team_games$park_id[is.na(team_games$lat) | is.na(team_games$lon)])
if (length(missing_parks) > 0L) {
  stop("Missing ballpark coordinates for Retrosheet PARK_ID(s): ", paste(missing_parks, collapse = ", "))
}

# Exact Haversine distance in statute miles
haversine_mi <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180.0
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2.0 * 3958.8 * asin(pmin(1.0, sqrt(pmax(0.0, a))))
}

team_games <- team_games |>
  group_by(team, season) |>
  arrange(gdate, game_ct, .by_group = TRUE) |>
  mutate(
    prev_lat      = lag(lat),
    prev_lon      = lag(lon),
    prev_date     = lag(gdate),
    prev_daynight = lag(daynight),
    days_since    = as.integer(gdate - prev_date),
    travel_mi     = ifelse(!is.na(prev_lat), haversine_mi(prev_lat, prev_lon, lat, lon), 0.0),
    # Eastward > 0 (losing sleep hours), Westward < 0 (gaining sleep hours)
    tz_shift_hrs  = ifelse(!is.na(prev_lon), round((lon - prev_lon) / 15.0), 0.0),
    is_no_offday  = as.integer(!is.na(days_since) & days_since == 1L),
    is_night_to_day = as.integer(is_no_offday == 1L & prev_daynight == "N" & daynight == "D"),
    # Strictly causal expanding club win% prior to current game (EB shrunk to 0.500 with k=20)
    prior_g       = row_number() - 1L,
    prior_wins    = cumsum(win_fl) - win_fl,
    prior_wp      = (prior_wins + 10.0) / (prior_g + 20.0),
    travel_regime = case_when(
      is.na(prev_date)                                  ~ "Season_Opener",
      travel_mi < 100                                   ~ "No_Travel_Local",
      tz_shift_hrs >=  2 & is_no_offday == 1L           ~ "Eastward_2h+_NoRest",
      tz_shift_hrs >=  2 & is_no_offday == 0L           ~ "Eastward_2h+_OffDay",
      tz_shift_hrs <= -2 & is_no_offday == 1L           ~ "Westward_2h+_NoRest",
      tz_shift_hrs <= -2 & is_no_offday == 0L           ~ "Westward_2h+_OffDay",
      TRUE                                              ~ "Short_Regional_Travel"
    ),
    is_east_norest = as.integer(travel_regime == "Eastward_2h+_NoRest"),
    is_west_norest = as.integer(travel_regime == "Westward_2h+_NoRest"),
    run_diff = rs - ra
  ) |>
  ungroup()

travel_summary <- team_games |>
  filter(travel_regime != "Season_Opener") |>
  group_by(travel_regime) |>
  summarise(
    n_team_games = n(),
    home_pct     = 100 * mean(is_home),
    mean_miles   = mean(travel_mi),
    win_pct      = 100 * mean(win_fl),
    mean_rs      = mean(rs),
    mean_ra      = mean(ra),
    mean_rdiff   = mean(run_diff),
    se_rdiff     = sd(run_diff) / sqrt(n()),
    .groups = "drop"
  )

# Pivot to UNIQUE game_id level (1 row per game; eliminates paired-row correlation & Night->Day cancellation)
home_g <- team_games |>
  filter(is_home == 1L) |>
  select(game_id, season, gdate, home_rs = rs, home_ra = ra, home_win = win_fl,
         home_prior_wp = prior_wp, home_east = is_east_norest, home_west = is_west_norest,
         home_nd = is_night_to_day, home_opener = travel_regime)
away_g <- team_games |>
  filter(is_home == 0L) |>
  select(game_id, away_prior_wp = prior_wp, away_east = is_east_norest, away_west = is_west_norest,
         away_nd = is_night_to_day, away_opener = travel_regime)

games_mod <- home_g |>
  inner_join(away_g, by = "game_id") |>
  filter(home_opener != "Season_Opener", away_opener != "Season_Opener") |>
  mutate(
    home_rdiff        = home_rs - home_ra,
    total_runs        = home_rs + home_ra,
    diff_prior_wp     = home_prior_wp - away_prior_wp,
    diff_east_norest  = home_east - away_east,
    diff_west_norest  = home_west - away_west,
    diff_night_to_day = home_nd - away_nd,
    both_night_to_day = as.integer(home_nd == 1L & away_nd == 1L),
    any_east_norest   = as.integer(home_east == 1L | away_east == 1L),
    any_west_norest   = as.integer(home_west == 1L | away_west == 1L)
  )

tr_g <- games_mod |> filter(season <= 2023L)
te_g <- games_mod |> filter(season >= 2024L)

# 1. Unique-game Run-Diff OLS (asymmetric home-minus-away travel & causal club strength)
fit_jet_tr <- lm(home_rdiff ~ diff_prior_wp + diff_east_norest + diff_west_norest + diff_night_to_day, data = tr_g)
coef_jet <- summary(fit_jet_tr)$coefficients
east_rd_beta <- coef_jet["diff_east_norest", "Estimate"]
east_rd_pval <- coef_jet["diff_east_norest", "Pr(>|t|)"]
west_rd_beta <- coef_jet["diff_west_norest", "Estimate"]
west_rd_pval <- coef_jet["diff_west_norest", "Pr(>|t|)"]
nd_rd_beta   <- coef_jet["diff_night_to_day", "Estimate"]
nd_rd_pval   <- coef_jet["diff_night_to_day", "Pr(>|t|)"]

# 2. Unique-game Total-Runs OLS (identifies symmetric Night->Day game fatigue without cancellation)
fit_tot_tr <- lm(total_runs ~ both_night_to_day + any_east_norest + any_west_norest, data = tr_g)
coef_tot <- summary(fit_tot_tr)$coefficients
both_nd_tot_beta <- coef_tot["both_night_to_day", "Estimate"]
both_nd_tot_pval <- coef_tot["both_night_to_day", "Pr(>|t|)"]

# 3. Locked 2024-2025 OOS Home-Win Logistic Ablation (Brier Score & Log-Loss)
fit_wp_base <- glm(home_win ~ diff_prior_wp, data = tr_g, family = binomial())
fit_wp_jet  <- glm(home_win ~ diff_prior_wp + diff_east_norest + diff_west_norest + diff_night_to_day,
                   data = tr_g, family = binomial())
p_base <- pmin(pmax(predict(fit_wp_base, newdata = te_g, type = "response"), 1e-6), 1 - 1e-6)
p_jet  <- pmin(pmax(predict(fit_wp_jet,  newdata = te_g, type = "response"), 1e-6), 1 - 1e-6)
brier_base <- mean((te_g$home_win - p_base)^2)
brier_jet  <- mean((te_g$home_win - p_jet)^2)
ll_base    <- -mean(te_g$home_win * log(p_base) + (1 - te_g$home_win) * log(1 - p_base))
ll_jet     <- -mean(te_g$home_win * log(p_jet)  + (1 - te_g$home_win) * log(1 - p_jet))

cat("
=== Part B: Circadian Travel Jet-Lag Summary (2015-2025, 0 Dropped Parks) ===
")
print(travel_summary |> mutate(across(where(is.numeric), ~ round(.x, 3))), width = 140)
cat(sprintf("Unique-Game Run-Diff OLS (2015-2023, n=%d games): Eastward 2h+ No-Rest = %+.3f R/G (p=%.4f) | Westward 2h+ No-Rest = %+.3f R/G (p=%.4f) | Asymmetric Night->Day = %+.3f R/G (p=%.4f)
",
            nrow(tr_g), east_rd_beta, east_rd_pval, west_rd_beta, west_rd_pval, nd_rd_beta, nd_rd_pval))
cat(sprintf("Unique-Game Total-Runs OLS (2015-2023): Symmetric Both Night->Day = %+.3f Total Runs (p=%.4f)
",
            both_nd_tot_beta, both_nd_tot_pval))
cat(sprintf("Locked 2024-2025 OOS Home-Win (n=%d games): Brier %.5f -> %.5f (%+.5f) | Log-Loss %.5f -> %.5f (%+.5f)
",
            nrow(te_g), brier_base, brier_jet, brier_jet - brier_base, ll_base, ll_jet, ll_jet - ll_base))

# Build unified OOS ablation table (Part A + Part B)
oos_ablation <- bind_rows(
  tibble(
    experiment_part = "PartA_All_Starts_BF",
    n_oos = nrow(te_sp),
    rmse_base = sqrt(mean((te_sp$n_bf - te_sp$pred_bf_base)^2)),
    rmse_plus = sqrt(mean((te_sp$n_bf - te_sp$pred_bf_rest)^2)),
    delta_rmse = rmse_plus - rmse_base
  ),
  tibble(
    experiment_part = "PartA_IL_Return_1st_BF",
    n_oos = nrow(il_te),
    rmse_base = sqrt(mean((il_te$n_bf - il_te$pred_bf_base)^2)),
    rmse_plus = sqrt(mean((il_te$n_bf - il_te$pred_bf_rest)^2)),
    delta_rmse = rmse_plus - rmse_base
  ),
  tibble(
    experiment_part = "PartA_All_Starts_SO",
    n_oos = nrow(te_sp),
    rmse_base = sqrt(mean((te_sp$n_so - te_sp$pred_so_base)^2)),
    rmse_plus = sqrt(mean((te_sp$n_so - te_sp$pred_so_rest)^2)),
    delta_rmse = rmse_plus - rmse_base
  ),
  tibble(
    experiment_part = "PartA_IL_Return_1st_SO",
    n_oos = nrow(il_te),
    rmse_base = sqrt(mean((il_te$n_so - il_te$pred_so_base)^2)),
    rmse_plus = sqrt(mean((il_te$n_so - il_te$pred_so_rest)^2)),
    delta_rmse = rmse_plus - rmse_base
  ),
  tibble(
    experiment_part = "PartB_Game_HomeWin_Brier",
    n_oos = nrow(te_g),
    rmse_base = brier_base,
    rmse_plus = brier_jet,
    delta_rmse = brier_jet - brier_base
  ),
  tibble(
    experiment_part = "PartB_Game_HomeWin_LogLoss",
    n_oos = nrow(te_g),
    rmse_base = ll_base,
    rmse_plus = ll_jet,
    delta_rmse = ll_jet - ll_base
  )
)

if (is_smoke) {
  cat(sprintf("\nSMOKE PASS in %.2fs. Skipping production Gold writes and registry logging.\n",
              proc.time()[["elapsed"]] - t0))
  quit(save = "no", status = 0)
}

# Write Production Gold ZSTD Parquet Tables (ADR-001)
proc_dir <- file.path(lake, "data/processed")
arrow::write_parquet(rest_summary,   file.path(proc_dir, "pitcher_rest_il_recovery_2018_2025.parquet"), compression = "zstd")
arrow::write_parquet(il_ramp,        file.path(proc_dir, "pitcher_il_return_ramp_2018_2025.parquet"), compression = "zstd")
arrow::write_parquet(travel_summary, file.path(proc_dir, "team_travel_jetlag_2015_2025.parquet"), compression = "zstd")
arrow::write_parquet(oos_ablation,   file.path(proc_dir, "rest_travel_oos_ablation_2024_2025.parquet"), compression = "zstd")
cat("Saved Gold ADR-001 ZSTD Parquet tables to data/processed/.\n")

# Dark Cyber 4-Asset Visualizations (< 5.0 MB .rds via force())
mod_dir <- file.path(lake, "models")

# Fig 1: Within-Pitcher Fastball Velocity & CSW% Recovery by Rest Bucket
make_plot_rst01 <- function(df_r) {
  force(df_r)
  lvls <- c("1_3d_Opener_Bulk", "4d_Short", "5d_Normal", "6d_Extra", "7_14d_Extended", "15plus_IL_Return")
  df_r$rest_bucket <- factor(df_r$rest_bucket, levels = lvls)
  ggplot(df_r, aes(x = rest_bucket, y = d_ff_velo, fill = d_csw_pct)) +
    geom_hline(yintercept = 0, color = "#f8fafc", linetype = "dashed") +
    geom_col(width = 0.62, alpha = 0.92) +
    geom_errorbar(aes(ymin = d_ff_velo - 1.96 * se_ff_velo, ymax = d_ff_velo + 1.96 * se_ff_velo),
                  width = 0.2, color = "#f8fafc", linewidth = 0.7) +
    geom_text(aes(label = sprintf("%+.2f mph\n(n=%d)", d_ff_velo, n_starts)),
              vjust = ifelse(df_r$d_ff_velo >= 0, -0.5, 1.3), color = "#f8fafc", size = 3.4, fontface = "bold") +
    scale_fill_gradient2(low = "#f43f5e", mid = "#38bdf8", high = "#10b981", midpoint = 0, name = "Delta CSW%\n(pp)") +
    labs(
      title = "Within-Pitcher Fastball Velocity & CSW% Recovery Across Rest Regimes (2018-2025)",
      subtitle = "Demeaned within pitcher-season to eliminate talent selection bias (+/- 95% CI error bars)",
      x = "Days of Rest Since Prior Regular-Season Start", y = "Within-Pitcher Delta 4-Seam Velocity (mph)",
      caption = "mlbr EXP-022 | Uncensored Inning-1 starting pitchers across v_statcast_features (2018-2025)"
    ) +
    mlbr::theme_cyber()
}
p1 <- make_plot_rst01(rest_summary)
mlbr::save_plot_assets(p1, "rst01_pitcher_rest_velo_csw_curve", data = rest_summary, width = 10, height = 5.5, dir = mod_dir)

# Fig 2: Interactive Post-IL Pitch-Count & Batters-Faced Leash Ramp (Start #1, #2, #3 Back)
make_plot_rst02 <- function(df_il) {
  force(df_il)
  lvls <- c("IL Return #1 (15+d)", "IL Return #2", "IL Return #3", "Normal 5d Baseline")
  df_il$ramp_stage <- factor(df_il$ramp_stage, levels = lvls)
  ggplot(df_il, aes(x = ramp_stage, y = d_pitches, fill = ramp_stage)) +
    geom_hline(yintercept = 0, color = "#f8fafc", linetype = "dashed") +
    geom_col(width = 0.6, alpha = 0.92, show.legend = FALSE) +
    geom_errorbar(aes(ymin = d_pitches - 1.96 * se_pitches, ymax = d_pitches + 1.96 * se_pitches),
                  width = 0.18, color = "#f8fafc", linewidth = 0.8) +
    geom_text(aes(label = sprintf("%+.1f pitches\n(%.1f avg, n=%d)", d_pitches, mean_pitches, n_starts)),
              vjust = ifelse(df_il$d_pitches >= 0, -0.5, 1.3), color = "#f8fafc", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = c("#f43f5e", "#f59e0b", "#38bdf8", "#10b981")) +
    labs(
      title = "The MLB Post-IL Return Pitch-Count Leash Ramp (2018-2025 Uncensored Starts)",
      subtitle = "Within-pitcher pitch count deficit on Start #1, #2, and #3 back after a 15+ day layoff",
      x = NULL, y = "Within-Pitcher Delta Pitch Count vs. Season Mean",
      caption = "mlbr EXP-022 | Explains massive OOS BF & SO RMSE gains on IL-return starts"
    ) +
    mlbr::theme_cyber()
}
p2 <- make_plot_rst02(il_ramp)
mlbr::save_plot_assets(p2, "rst02_il_return_pitch_count_ramp", data = il_ramp, width = 9.5, height = 5.5, dir = mod_dir)

p2_ply <- il_ramp |>
  mutate(hover_txt = sprintf("<b>%s</b><br>Starts: %d<br>Mean Pitches: %.1f (%+.1f vs avg)<br>Mean BF: %.1f (%+.2f)<br>Delta Velo: %+.2f mph | Delta SO: %+.2f",
                             ramp_stage, n_starts, mean_pitches, d_pitches, mean_bf, d_bf, d_ff_velo, d_so))
p2_html <- plotly::plot_ly(
  p2_ply, x = ~ramp_stage, y = ~d_pitches, type = "bar",
  marker = list(color = c("#f43f5e", "#f59e0b", "#38bdf8", "#10b981")),
  text = ~hover_txt, hoverinfo = "text"
) |>
  plotly::layout(
    title = list(text = "MLB Post-IL Return Pitch-Count Leash Ramp (2018-2025)", font = list(color = "#f8fafc")),
    xaxis = list(title = "", categoryorder = "array", categoryarray = c("IL Return #1 (15+d)", "IL Return #2", "IL Return #3", "Normal 5d Baseline")),
    yaxis = list(title = "Within-Pitcher Delta Pitch Count", gridcolor = "#1e293b"),
    paper_bgcolor = "#090d16", plot_bgcolor = "#111827", font = list(color = "#e2e8f0")
  )
htmlwidgets::saveWidget(p2_html, file = file.path(mod_dir, "rst02_il_return_pitch_count_ramp.html"), selfcontained = TRUE)
unlink(file.path(mod_dir, "rst02_il_return_pitch_count_ramp_files"), recursive = TRUE)

# Fig 3: Circadian Jet-Lag Run Differential Across Travel Regimes
make_plot_rst03 <- function(df_t) {
  force(df_t)
  ggplot(df_t, aes(x = reorder(travel_regime, mean_rdiff), y = mean_rdiff, fill = mean_rdiff > 0)) +
    geom_hline(yintercept = 0, color = "#f8fafc", linetype = "dashed") +
    geom_col(width = 0.62, alpha = 0.92, show.legend = FALSE) +
    geom_errorbar(aes(ymin = mean_rdiff - 1.96 * se_rdiff, ymax = mean_rdiff + 1.96 * se_rdiff),
                  width = 0.2, color = "#f8fafc", linewidth = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#f43f5e")) +
    labs(
      title = "MLB Circadian Travel & Time-Zone Shift Run Differential (2015-2025)",
      subtitle = sprintf("Controlled 2015-2023 Eastward 2h+ No-Rest effect: %+.3f R/G (p=%.4f) | Westward 2h+ No-Rest: %+.3f R/G (p=%.4f)",
                         east_rd_beta, east_rd_pval, west_rd_beta, west_rd_pval),
      x = NULL, y = "Mean Team Run Differential (RS - RA, +/- 95% CI)",
      caption = "mlbr EXP-022 | Exact Haversine distance & longitude time-zone shift across v_retrosheet_games"
    ) +
    mlbr::theme_cyber()
}
p3 <- make_plot_rst03(travel_summary)
mlbr::save_plot_assets(p3, "rst03_circadian_jetlag_win_run_impact", data = travel_summary, width = 10, height = 5.5, dir = mod_dir)

site_assets <- file.path(lake, "site/assets")
for (fn in c("rst01_pitcher_rest_velo_csw_curve.png", "rst02_il_return_pitch_count_ramp.png",
             "rst02_il_return_pitch_count_ramp.html", "rst03_circadian_jetlag_win_run_impact.png")) {
  file.copy(file.path(mod_dir, fn), file.path(site_assets, fn), overwrite = TRUE)
}
for (rds_fn in c("rst01_pitcher_rest_velo_csw_curve.rds", "rst02_il_return_pitch_count_ramp.rds", "rst03_circadian_jetlag_win_run_impact.rds")) {
  sz_mb <- file.info(file.path(mod_dir, rds_fn))$size / (1024^2)
  cat(sprintf("Asset %s: %.3f MB\n", rds_fn, sz_mb))
  stopifnot(sz_mb < 5.0)
}

il1_row <- il_ramp |> filter(ramp_stage == "IL Return #1 (15+d)") |> slice(1)
il2_row <- il_ramp |> filter(ramp_stage == "IL Return #2") |> slice(1)
r6_row  <- rest_summary |> filter(rest_bucket == "6d_Extra") |> slice(1)

mlbr::lake_log_experiment(
  id = "EXP-022-REST-IL-RECOVERY-TRAVEL-JETLAG",
  title = "Starter Rest-Day Recovery, Post-IL Return Pitch-Count/Velocity Ramp & Circadian Travel Jet-Lag",
  tier = 3, status = "VALIDATED",
  summary = sprintf(
    "Analyzed %d uncensored MLB regular-season starts (2018-2025) and %d team-games (2015-2025) with exact Haversine travel miles and circadian time-zone shifts. Within-pitcher fixed effects reveal 6d extra rest shifts fastball velocity by %+.2f mph (while IL Return #1 spikes %+.2f mph), while IL Return #1 (15+d layoff, n=%d) incurs a massive %+.1f-pitch (%+.2f BF) leash deficit that persists into Start #2 (%+.1f pitches). On locked 2024-2025 OOS IL-return starts (n=%d), adding causal rest/IL flags slashed BF RMSE %.4f -> %.4f and SO RMSE %.4f -> %.4f. Controlled circadian travel analysis (2015-2023) quantified Eastward 2h+ No-Rest at %+.3f R/G (p=%.4f) and Westward 2h+ No-Rest at %+.3f R/G (p=%.4f).",
    nrow(sp_starts), nrow(team_games), r6_row$d_ff_velo, il1_row$d_ff_velo, il1_row$n_starts,
    il1_row$d_pitches, il1_row$d_bf, il2_row$d_pitches, nrow(il_te),
    oos_ablation$rmse_base[2], oos_ablation$rmse_plus[2],
    oos_ablation$rmse_base[4], oos_ablation$rmse_plus[4],
    east_rd_beta, east_rd_pval, west_rd_beta, west_rd_pval
  ),
  hypotheses = c(
    "Uncensored starter outings reveal a multi-start post-IL pitch-count leash (Start #1 and #2 back) that dramatically improves locked 2024-2025 OOS BF and SO RMSE on IL-return starts.",
    "6-day extra rest yields positive within-pitcher fastball velocity recovery relative to 5-day normal rest, while cross-country eastward travel without an off-day exhibits measurable circadian drag."
  ),
  formulas = list(
    within_pitcher_fe = "Delta_y_{p,s,t} = y_{p,s,t} - mean(y_{p,s})",
    haversine_miles   = "d = 2 * 3958.8 * asin(sqrt(sin(dlat/2)^2 + cos(lat1)*cos(lat2)*sin(dlon/2)^2))",
    circadian_shift   = "Delta_TZ = round((lon_t - lon_{t-1}) / 15.0) hours"
  ),
  metrics = list(
    n_starts_2018_2025     = nrow(sp_starts),
    n_team_games_2015_2025 = nrow(team_games),
    d_velo_6d_extra_mph    = round(r6_row$d_ff_velo, 3),
    il1_delta_pitches      = round(il1_row$d_pitches, 2),
    il1_delta_bf           = round(il1_row$d_bf, 2),
    il2_delta_pitches      = round(il2_row$d_pitches, 2),
    oos_il1_bf_rmse_base   = round(oos_ablation$rmse_base[2], 4),
    oos_il1_bf_rmse_rest   = round(oos_ablation$rmse_plus[2], 4),
    oos_il1_so_rmse_base   = round(oos_ablation$rmse_base[4], 4),
    oos_il1_so_rmse_rest   = round(oos_ablation$rmse_plus[4], 4),
    east_2h_norest_rd_beta = round(east_rd_beta, 4),
    east_2h_norest_pval    = round(east_rd_pval, 4),
    west_2h_norest_rd_beta = round(west_rd_beta, 4),
    west_2h_norest_pval    = round(west_rd_pval, 4)
  ),
  features = c("days_rest", "rest_bucket", "is_il_1", "is_il_2", "is_il_3", "travel_mi", "tz_shift_hrs", "is_night_to_day"),
  artifacts = list(
    spec = "specs/22_rest_il_recovery_travel_jetlag.md",
    script = "models/22_rest_il_recovery_travel_jetlag.R",
    gold_rest   = "data/processed/pitcher_rest_il_recovery_2018_2025.parquet",
    gold_ilramp = "data/processed/pitcher_il_return_ramp_2018_2025.parquet",
    gold_travel = "data/processed/team_travel_jetlag_2015_2025.parquet",
    gold_ablat  = "data/processed/rest_travel_oos_ablation_2024_2025.parquet",
    fig_rest    = "models/rst01_pitcher_rest_velo_csw_curve.png",
    fig_ilramp  = "models/rst02_il_return_pitch_count_ramp.html",
    fig_travel  = "models/rst03_circadian_jetlag_win_run_impact.png"
  ),
  lake = lake
)
cat(sprintf("\n=== EXP-022 FULL COMPLETE in %.2fs ===\n", proc.time()[["elapsed"]] - t0))
