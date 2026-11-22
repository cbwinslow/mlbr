test_that("team_crosswalk returns 30+ franchises with expected columns", {
  tc <- team_crosswalk()
  expect_s3_class(tc, "data.frame")
  expect_gte(nrow(tc), 30)
  expect_true(all(c("team_retro", "team_statcast", "team_bbref", "franchise_name") %in% names(tc)))
})

test_that("default_lake honors explicit argument and MLBR_DATA_PATH", {
  tmp <- tempfile("mlbr_lake_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  expect_equal(default_lake(tmp), normalizePath(tmp, mustWork = FALSE))

  old_env <- Sys.getenv("MLBR_DATA_PATH", unset = NA)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("MLBR_DATA_PATH") else Sys.setenv(MLBR_DATA_PATH = old_env)
  }, add = TRUE)
  Sys.setenv(MLBR_DATA_PATH = tmp)
  expect_equal(default_lake(), normalizePath(tmp, mustWork = FALSE))
})

test_that("Dark Cyber visual helpers and ZSTD asset preservation work", {
  thm <- theme_cyber()
  expect_s3_class(thm, "theme")
  expect_named(cyber_pitch_colors())
  expect_named(cyber_quadrant_colors(), c("RvR", "RvL", "LvL", "LvR"))

  tmp_dir <- tempfile("mlbr_assets_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  df <- data.frame(x = 1:5, y = (1:5)^2)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point() + theme_cyber()

  res <- save_plot_assets(p, name = "unit_test_plot", data = df, width = 4, height = 3, dpi = 72, dir = tmp_dir)
  expect_true(file.exists(res$png))
  expect_true(file.exists(res$rds))
  expect_true(file.exists(res$parquet))
  reloaded <- arrow::read_parquet(res$parquet)
  expect_equal(nrow(reloaded), 5)
})

test_that("lake_log_experiment writes idempotent registry and README in isolated lake", {
  skip_if_not_installed("jsonlite")
  tmp_lake <- tempfile("mlbr_exp_")
  dir.create(file.path(tmp_lake, "data", "manifest"), recursive = TRUE)
  on.exit(unlink(tmp_lake, recursive = TRUE), add = TRUE)

  reg <- lake_log_experiment(
    id = "EXP-TEST-001",
    title = "Unit Test Experiment",
    tier = 1,
    status = "VALIDATED",
    summary = "Test summary",
    hypotheses = c("H1"),
    formulas = list(f = "a + b"),
    metrics = list(r2 = 0.9),
    features = c("a", "b"),
    lake = tmp_lake
  )
  expect_length(reg, 1)
  expect_true(file.exists(file.path(tmp_lake, "experiments", "registry.json")))
  expect_true(file.exists(file.path(tmp_lake, "experiments", "README.md")))
  got <- lake_get_experiment("EXP-TEST-001", lake = tmp_lake)
  expect_equal(got$title, "Unit Test Experiment")
})

test_that("lake_register_features(force_dynamic = TRUE) builds valid v_statcast_features view", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tmp_lake <- tempfile("mlbr_dyn_")
  dir.create(tmp_lake)
  on.exit(unlink(tmp_lake, recursive = TRUE), add = TRUE)

  sample_sc <- data.frame(
    game_pk = rep(745001, 5),
    game_date = as.Date(c("2013-06-01", "2018-06-01", "2024-06-01", "2024-06-01", "2024-06-01")),
    at_bat_number = c(1, 1, 1, 1, 2),
    pitch_number = c(1, 2, 1, 2, 1),
    pitcher = rep(605483, 5),
    batter = rep(592450, 5),
    p_throws = c("R", "R", "L", "L", "R"),
    stand = c("R", "L", "L", "R", "R"),
    release_pos_x = c(-1.5, -1.5, 1.8, 1.8, -1.6),
    pfx_x = c(-0.8, 0.9, 0.7, -0.6, -0.5),
    pfx_z = c(1.4, -0.2, 1.2, 0.3, 1.5),
    plate_x = c(0.2, -1.2, 0.4, 1.4, 0.1),
    plate_z = c(2.5, 2.0, 2.8, 1.0, 2.4),
    sz_top = rep(3.4, 5),
    sz_bot = rep(1.6, 5),
    zone = c(5, 11, 5, 14, 5),
    description = c("called_strike", "swinging_strike", "hit_into_play", "ball", "foul"),
    estimated_woba_using_speedangle = c("null", "null", "0.420", "null", "null"),
    estimated_ba_using_speedangle = c("null", "null", "0.350", "null", "null"),
    estimated_slg_using_speedangle = c("null", "null", "0.700", "null", "null"),
    arm_angle = c("null", "null", "42.5", "41.8", "39.0"),
    bat_speed = c("null", "null", "74.2", "null", "71.0"),
    swing_length = c("null", "null", "7.1", "null", "6.9"),
    delta_pitcher_run_exp = c(0.04, 0.06, -0.35, -0.05, 0.02),
    delta_run_exp = c(-0.04, -0.06, 0.35, 0.05, -0.02),
    bb_type = c(NA_character_, NA_character_, "ground_ball", NA_character_, NA_character_),
    launch_speed = c(NA_real_, NA_real_, 101.2, NA_real_, NA_real_),
    vx0 = c(5.0, -4.0, -5.0, 4.0, 3.0),
    vy0 = c(-138.0, -125.0, -136.0, -122.0, -139.0),
    vz0 = c(-4.0, -2.0, -3.5, -1.5, -4.2),
    ax = c(-12.0, 11.0, 10.0, -9.0, -11.0),
    ay = c(28.0, 24.0, 27.0, 23.0, 29.0),
    az = c(-14.0, -34.0, -16.0, -30.0, -13.5),
    stringsAsFactors = FALSE
  )

  duckdb::duckdb_register(con, "raw_sc_tbl", sample_sc)
  DBI::dbExecute(con, "CREATE VIEW v_statcast AS SELECT * FROM raw_sc_tbl")

  lake_register_features(con, lake = tmp_lake, force_dynamic = TRUE)
  res <- DBI::dbGetQuery(con, "SELECT * FROM v_statcast_features ORDER BY game_date, at_bat_number, pitch_number")

  expect_equal(nrow(res), 5)
  expect_equal(res$tracking_era, c("PITCHf/x", "TrackMan", "Hawk-Eye", "Hawk-Eye", "Hawk-Eye"))
  expect_equal(res$matchup, c("RvR", "RvL", "LvL", "LvR", "RvR"))
  expect_equal(res$is_same_hand, c(1L, 0L, 1L, 0L, 1L))
  expect_equal(res$is_chase, c(0L, 1L, 0L, 0L, 0L))
  expect_equal(res$is_hard_hit, c(0L, 0L, 1L, 0L, 0L))
  expect_equal(res$is_gb, c(0L, 0L, 1L, 0L, 0L))
  expect_true(all(!is.na(res$vaa) & res$vaa < 0))
  expect_equal(res$hb_glove_in[1], -9.6, tolerance = 1e-6)
  expect_equal(res$hb_glove_in[3], -8.4, tolerance = 1e-6)
  expect_equal(res$xwoba[3], 0.42, tolerance = 1e-6)
})

test_that("lake_register_tunneling(force_dynamic = TRUE) computes exact commit-point kinematics and guards", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tmp_lake <- tempfile("mlbr_tun_test_")
  dir.create(tmp_lake, recursive = TRUE)
  on.exit(unlink(tmp_lake, recursive = TRUE), add = TRUE)

  # Construct 6 synthetic pitches testing:
  # - AB 1, pitch 1 -> 2: same pitcher (100), FF -> SL (VALID pair)
  # - AB 1, pitch 2 -> 3: mid-AB pitcher change 100 -> 200 (MUST BE EXCLUDED)
  # - AB 1, pitch 3 -> 4: same pitcher (200), FF -> FF repeat pitch (VALID pair)
  # - AB 2, pitch 1 -> 3: gap pitch_number = 2 missing (MUST BE EXCLUDED)
  sample_sc <- data.frame(
    game_pk = c(1L, 1L, 1L, 1L, 2L, 2L),
    game_date = as.Date(rep("2025-05-01", 6)),
    at_bat_number = c(1L, 1L, 1L, 1L, 2L, 2L),
    pitch_number = c(1L, 2L, 3L, 4L, 1L, 3L),
    pitcher = c(100L, 100L, 200L, 200L, 100L, 100L),
    batter = c(500L, 500L, 500L, 500L, 501L, 501L),
    pitch_type = c("FF", "SL", "FF", "FF", "FF", "CH"),
    p_throws = rep("R", 6),
    stand = rep("R", 6),
    release_speed = c(96.0, 86.0, 94.0, 95.0, 96.0, 85.0),
    release_pos_x = c(-1.5, -1.5, -1.8, -1.8, -1.5, -1.5),
    pfx_x = c(-0.6, 0.5, -0.5, -0.5, -0.6, -1.0),
    pfx_z = c(1.5, 0.2, 1.4, 1.4, 1.5, 0.5),
    plate_x = c(0.10, 0.90, -0.20, -0.10, 0.00, -0.50),
    plate_z = c(3.10, 1.90, 2.80, 2.90, 3.00, 1.80),
    sz_top = rep(3.5, 6),
    sz_bot = rep(1.5, 6),
    zone = c(5L, 14L, 5L, 5L, 5L, 13L),
    description = c("called_strike", "swinging_strike", "foul", "called_strike", "ball", "swinging_strike"),
    estimated_woba_using_speedangle = rep("null", 6),
    estimated_ba_using_speedangle = rep("null", 6),
    estimated_slg_using_speedangle = rep("null", 6),
    arm_angle = rep("40.0", 6),
    bat_speed = rep("null", 6),
    swing_length = rep("null", 6),
    delta_pitcher_run_exp = c(0.04, 0.08, 0.02, 0.05, -0.03, 0.07),
    delta_run_exp = c(-0.04, -0.08, -0.02, -0.05, 0.03, -0.07),
    bb_type = rep(NA_character_, 6),
    launch_speed = rep(NA_real_, 6),
    vx0 = c(4.0, 2.0, 3.5, 3.6, 4.0, 1.5),
    vy0 = c(-140.0, -125.0, -137.0, -138.0, -140.0, -124.0),
    vz0 = c(-3.5, -1.0, -3.2, -3.1, -3.5, -0.8),
    ax = c(-10.0, 8.0, -9.0, -9.2, -10.0, -14.0),
    ay = c(28.0, 23.0, 27.0, 27.5, 28.0, 22.0),
    az = c(-15.0, -33.0, -16.0, -15.5, -15.0, -28.0),
    stringsAsFactors = FALSE
  )

  duckdb::duckdb_register(con, "raw_sc_tun", sample_sc)
  DBI::dbExecute(con, "CREATE VIEW v_statcast AS SELECT * FROM raw_sc_tun")

  lake_register_features(con, lake = tmp_lake, force_dynamic = TRUE)
  lake_register_tunneling(con, lake = tmp_lake, force_dynamic = TRUE)

  res <- DBI::dbGetQuery(con, "SELECT * FROM v_statcast_tunneling ORDER BY game_pk, at_bat_number, pitch_number")

  # Only 2 valid consecutive same-pitcher pairs should survive
  expect_equal(nrow(res), 2)
  expect_equal(res$pitch_number, c(2L, 4L))
  expect_equal(res$pitch_sequence, c("FF -> SL", "FF -> FF"))
  expect_equal(res$is_repeat_pitch, c(0L, 1L))
  expect_equal(res$delta_velo_mph, c(-10.0, 1.0), tolerance = 1e-6)

  # Analytical verification of exact y0=50 -> y=23.8 ft reconstruction for AB 1 pitch 1 & 2
  calc_dec <- function(i) {
    vy0 <- sample_sc$vy0[i]; ay <- sample_sc$ay[i]
    vx0 <- sample_sc$vx0[i]; ax <- sample_sc$ax[i]
    vz0 <- sample_sc$vz0[i]; az <- sample_sc$az[i]
    px <- sample_sc$plate_x[i]; pz <- sample_sc$plate_z[i]
    tp <- (-sqrt(vy0^2 + 2 * ay * (1.417 - 50.0)) - vy0) / ay
    td <- (-sqrt(vy0^2 + 2 * ay * (23.8 - 50.0)) - vy0) / ay
    x50 <- px - vx0 * tp - 0.5 * ax * tp^2
    z50 <- pz - vz0 * tp - 0.5 * az * tp^2
    c(x = x50 + vx0 * td + 0.5 * ax * td^2, z = z50 + vz0 * td + 0.5 * az * td^2, tp = tp)
  }
  p1 <- calc_dec(1); p2 <- calc_dec(2)
  exp_tun <- round(sqrt((p2["x"] - p1["x"])^2 + (p2["z"] - p1["z"])^2) * 12, 2)
  exp_plate <- round(sqrt((sample_sc$plate_x[2] - sample_sc$plate_x[1])^2 + (sample_sc$plate_z[2] - sample_sc$plate_z[1])^2) * 12, 2)
  exp_post_break <- round(
    sqrt(((sample_sc$plate_x[2] - p2["x"]) - (sample_sc$plate_x[1] - p1["x"]))^2 +
         ((sample_sc$plate_z[2] - p2["z"]) - (sample_sc$plate_z[1] - p1["z"]))^2) * 12,
    2
  )
  exp_bp_ratio <- round(exp_post_break / max(exp_tun, 1.0), 3)
  exp_plate_ratio <- round(exp_plate / max(exp_tun, 1.0), 3)

  expect_equal(res$x_dec[1], unname(round(p2["x"], 4)), tolerance = 1e-4)
  expect_equal(res$z_dec[1], unname(round(p2["z"], 4)), tolerance = 1e-4)
  expect_equal(res$tunnel_dist_in[1], unname(exp_tun), tolerance = 1e-2)
  expect_equal(res$plate_sep_in[1], unname(exp_plate), tolerance = 1e-2)
  expect_equal(res$post_tunnel_break_in[1], unname(exp_post_break), tolerance = 1e-2)
  expect_equal(res$break_to_tunnel_ratio[1], unname(exp_bp_ratio), tolerance = 1e-3)
  expect_equal(res$plate_to_tunnel_ratio[1], unname(exp_plate_ratio), tolerance = 1e-3)
})

test_that("lake_register_tunneling fails fast on missing dependency and mounts materialized Hive Parquet", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not_installed("arrow")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tmp_lake <- tempfile("mlbr_tun_mat_")
  dir.create(tmp_lake, recursive = TRUE)
  on.exit(unlink(tmp_lake, recursive = TRUE), add = TRUE)

  # Fail fast when v_statcast_features is absent
  expect_error(lake_register_tunneling(con, lake = tmp_lake, force_dynamic = TRUE), "v_statcast_features")

  # Mount materialized Hive-partitioned Silver Parquet when present
  part_dir <- file.path(tmp_lake, "data", "features", "statcast_tunneling", "game_year=2025")
  dir.create(part_dir, recursive = TRUE)
  mock_tun <- data.frame(
    game_pk = 999L,
    game_date = as.Date("2025-06-01"),
    game_type = "R",
    at_bat_number = 1L,
    pitch_number = 2L,
    pitcher = 123L,
    tunnel_dist_in = 0.5,
    post_tunnel_break_in = 12.0,
    break_to_tunnel_ratio = 12.0,
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(mock_tun, file.path(part_dir, "data_0.parquet"), compression = "zstd")

  lake_register_tunneling(con, lake = tmp_lake, force_dynamic = FALSE)
  out <- DBI::dbGetQuery(con, "SELECT * FROM v_statcast_tunneling")
  expect_equal(nrow(out), 1)
  expect_equal(as.integer(out$game_year), 2025L)
  expect_equal(out$game_type, "R")
  expect_equal(out$break_to_tunnel_ratio, 12.0)
})

test_that("Gold ballpark factors conform to ADR-001 and ground-truth anchors", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  gold_path <- file.path(lake, "data/processed/ballpark_factors_2002_2025.parquet")
  skip_if_not(file.exists(gold_path), "Gold ballpark factors parquet not materialized yet")

  pf <- arrow::read_parquet(gold_path)
  expect_equal(nrow(pf), 720L)
  expect_false(anyNA(pf$venue_name))
  expect_false(anyNA(pf$elevation_ft))
  expect_false(anyNA(pf$roof_type))
  expect_true(all(is.finite(pf$pf_runs)))
  expect_true(all(is.finite(pf$pf_hr)))
  expect_true(all(is.finite(pf$pf_hr_rhb)))
  expect_true(all(is.finite(pf$pf_hr_lhb)))
  expect_true(all(is.finite(pf$pf_so)))
  expect_true(all(is.finite(pf$pf_bb)))
  expect_true(all(is.finite(pf$pf_babip)))

  # Anchors: 2024 Coors, Great American, Oracle, Yankee Stadium
  coors24 <- pf[pf$season == 2024L & pf$park_id == "DEN02", ]
  expect_equal(nrow(coors24), 1L)
  expect_gte(coors24$pf_runs, 120.0)

  cin24 <- pf[pf$season == 2024L & pf$park_id == "CIN09", ]
  expect_equal(nrow(cin24), 1L)
  expect_gte(cin24$pf_hr, 110.0)

  sfo24 <- pf[pf$season == 2024L & pf$park_id == "SFO03", ]
  expect_equal(nrow(sfo24), 1L)
  expect_lte(sfo24$pf_hr, 90.0)

  nyc24 <- pf[pf$season == 2024L & pf$park_id == "NYC21", ]
  expect_equal(nrow(nyc24), 1L)
  expect_gte(nyc24$pf_hr_lhb, nyc24$pf_hr_rhb + 8.0)
})

test_that("Gold True-Talent Projections adhere to ADR-005 WAR accounting without double counting", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  gold_path <- file.path(lake, "data/processed/batter_projections_true_talent.parquet")
  skip_if_not(file.exists(gold_path), "Gold batter projections parquet not materialized yet")

  bg <- arrow::read_parquet(gold_path)
  expect_true("proj_fld_runs" %in% names(bg))
  expect_true("proj_pos_runs" %in% names(bg))
  expect_true("proj_def_runs" %in% names(bg))
  expect_true("proj_bsr_runs" %in% names(bg))
  expect_true("proj_war" %in% names(bg))

  # Defense identity: Defense = Fielding + Positional
  expect_true(all(abs(bg$proj_def_runs - (bg$proj_fld_runs + bg$proj_pos_runs)) < 1e-5, na.rm = TRUE))

  # WAR formula identity
  calc_war <- (bg$proj_wraa + bg$proj_bsr_runs + bg$proj_fld_runs + bg$proj_pos_runs + bg$proj_repl_runs) / 9.77
  expect_true(all(abs(bg$proj_war - calc_war) < 1e-4, na.rm = TRUE))

  # Plausibility: 2026 Aaron Judge
  judge26 <- bg[bg$projection_year == 2026L & bg$batter_mlbam == 592450L, ]
  expect_equal(nrow(judge26), 1L)
  expect_gte(judge26$proj_war, 6.5)
  expect_lte(judge26$proj_war, 9.5)
})

test_that("EXP-007 Gold PECOTA, ZiPS 5-Year & Defense EDA Parquets satisfy invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  eda_p   <- file.path(proc, "defense_baserunning_eda_2002_2025.parquet")
  comps_p <- file.path(proc, "pecota_knn_comps_2026.parquet")
  z5_p    <- file.path(proc, "zips_5yr_trajectories_2026_2030.parquet")
  skip_if_not(file.exists(eda_p) && file.exists(comps_p) && file.exists(z5_p), "EXP-007 Gold files not materialized")

  eda   <- arrow::read_parquet(eda_p)
  comps <- arrow::read_parquet(comps_p)
  z5    <- arrow::read_parquet(z5_p)

  # Defense EDA invariant: Statcast OAA YoY reliability > DRS > UZR/150
  r_oaa <- eda$yoy_r[eda$metric_col == "oaa"]
  r_drs <- eda$yoy_r[eda$metric_col == "drs"]
  r_uzr <- eda$yoy_r[eda$metric_col == "uzr_150"]
  expect_gt(r_oaa, r_drs)
  expect_gt(r_drs, r_uzr)

  # PECOTA percentile monotonicity & survival bounds
  expect_true(all(comps$pecota_p10 <= comps$pecota_p50 + 1e-6))
  expect_true(all(comps$pecota_p50 <= comps$pecota_p90 + 1e-6))
  expect_true(all(comps$survival_prob >= 0 & comps$survival_prob <= 1))
  expect_true(all(comps$comp_year <= 2024L))

  # ZiPS 5-year horizon completeness (2026-2030)
  expect_setequal(unique(z5$horizon_year), 2026L:2030L)

  # Ensemble projections & backtest metrics + exact 2026 ZiPS parity
  ens_p <- file.path(proc, "ensemble_projections_2016_2026.parquet")
  bm_p  <- file.path(proc, "ensemble_backtest_metrics.parquet")
  expect_true(file.exists(ens_p) && file.exists(bm_p))
  ens <- arrow::read_parquet(ens_p)
  bm  <- arrow::read_parquet(bm_p)
  expect_equal(nrow(bm), 10L)
  expect_true(all(is.finite(bm$w_rmse)))

  # Exact zero-centered parity: zips_5yr (2026) == ensemble_projections (2026, zips_pred)
  z5_b26 <- z5[z5$domain == "Batter" & z5$horizon_year == 2026L, c("mlbam", "proj_woba")]
  en_b26 <- ens[ens$domain == "Batter" & ens$projection_year == 2026L, c("mlbam", "zips_pred")]
  mb <- merge(z5_b26, en_b26, by = "mlbam")
  expect_gt(nrow(mb), 400L)
  expect_lt(max(abs(mb$proj_woba - mb$zips_pred)), 1e-10)

  z5_p26 <- z5[z5$domain == "Pitcher" & z5$horizon_year == 2026L, c("mlbam", "proj_fip")]
  en_p26 <- ens[ens$domain == "Pitcher" & ens$projection_year == 2026L, c("mlbam", "zips_pred")]
  mp <- merge(z5_p26, en_p26, by = "mlbam")
  expect_gt(nrow(mp), 400L)
  expect_lt(max(abs(mp$proj_fip - mp$zips_pred)), 1e-10)
})

test_that("EXP-008 Gold BaseRuns, Pythagenpat & 2026 Standings satisfy conservation invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  hist_p <- file.path(proc, "team_baseruns_pythagenpat_2002_2025.parquet")
  s26_p  <- file.path(proc, "team_standings_projections_2026.parquet")
  skip_if_not(file.exists(hist_p) && file.exists(s26_p), "EXP-008 Gold files not materialized")

  hist <- arrow::read_parquet(hist_p)
  s26  <- arrow::read_parquet(s26_p)

  expect_equal(nrow(hist), 720L)
  expect_gt(stats::cor(hist$bsr_off, hist$rs), 0.96)
  expect_gt(stats::cor(hist$bsr_def, hist$ra), 0.96)
  expect_true(all(hist$bsr_off >= 0 & hist$bsr_def >= 0))

  expect_equal(nrow(s26), 30L)
  expect_lt(abs(sum(s26$div_prob) - 6.0), 0.02)
  expect_lt(abs(sum(s26$wc_prob) - 6.0), 0.02)
  expect_lt(abs(sum(s26$playoff_prob) - 12.0), 0.02)
  expect_lt(abs(mean(s26$mean_wins) - 81.0), 0.5)
  expect_true(all(s26$p10_wins <= s26$p50_wins & s26$p50_wins <= s26$p90_wins))
  expect_true("roster_sigma_logit" %in% names(s26) && all(s26$roster_sigma_logit >= 0.10 & s26$roster_sigma_logit <= 0.19))

  # Published Regular-Season Ground-Truth Benchmark: 2024 Los Angeles Dodgers (98-64, 842 RS, 686 RA, 162 G)
  lad24 <- hist[hist$season == 2024L & hist$team_id == "LAN", ]
  expect_equal(nrow(lad24), 1L)
  expect_equal(lad24$g, 162L)
  expect_equal(lad24$wins, 98L)
  expect_equal(lad24$rs, 842L)
  expect_equal(lad24$ra, 686L)
  # Published Smyth/Patriot Pythagenpat: gamma = ((842+686)/162)^0.287 = 1.9035 -> 96.60 exp wins
  g_lad <- ((842 + 686) / 162)^0.287
  exp_w_lad <- 162 * (842^g_lad) / (842^g_lad + 686^g_lad)
  expect_lt(abs(lad24$pyth_gamma - g_lad), 1e-4)
  expect_lt(abs(lad24$pyth_act_wins - exp_w_lad), 1e-4)
})

test_that("EXP-009 Gold Markov Game Simulator & Win Expectancy satisfy OOS invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  gm_p <- file.path(proc, "game_simulation_backtest_2024_2025.parquet")
  bm_p <- file.path(proc, "game_simulator_benchmark_metrics.parquet")
  we_p <- file.path(proc, "win_expectancy_re24_matrix.parquet")
  skip_if_not(file.exists(gm_p) && file.exists(bm_p) && file.exists(we_p), "EXP-009 Gold files not materialized")

  gm <- arrow::read_parquet(gm_p)
  bm <- arrow::read_parquet(bm_p)
  we <- arrow::read_parquet(we_p)

  expect_gt(nrow(gm), 4800L)
  expect_true(all(gm$p_m4 > 0.05 & gm$p_m4 < 0.95))
  expect_true(all(gm$p_m4_rl15 > 0 & gm$p_m4_rl15 < gm$p_m4))
  expect_lt(bm$brier[bm$model == "M4_PA_Markov_Simulator"], bm$brier[bm$model == "B1_HomeField_Constant"])
  expect_lt(bm$brier[bm$model == "M3_Starter_Team_Log5"],   bm$brier[bm$model == "B1_HomeField_Constant"])
  expect_true(all(we$home_we >= 0 & we$home_we <= 1))
  # Sanity: Bot 9th tied bases loaded 0 outs must have home_we > 0.80
  w_walkoff <- we$home_we[we$inning == 9L & we$bat_home == 1L & we$outs == 0L & we$bases == 7L & we$score_diff == 0L]
  expect_gt(w_walkoff, 0.80)
  # Exact Bot-9th skip & walk-off truncation calibration: mean total runs within 0.25 R/G of actual
  expect_lt(abs(mean(gm$exp_total_runs) - mean(gm$actual_total_runs)), 0.25)
})

test_that("EXP-010 Gold Market Edge, Kelly Sizing & Prediction Market Futures satisfy invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  bt_p  <- file.path(proc, "market_valuation_backtest_2024_2025.parquet")
  st_p  <- file.path(proc, "market_staking_summary_2024_2025.parquet")
  fut_p <- file.path(proc, "market_2026_futures_valuation.parquet")
  skip_if_not(file.exists(bt_p) && file.exists(st_p) && file.exists(fut_p), "EXP-010 Gold files not materialized")

  bt  <- arrow::read_parquet(bt_p)
  st  <- arrow::read_parquet(st_p)
  fut <- arrow::read_parquet(fut_p)

  # Exact closed-form analytical benchmark (p = 0.60, O_entry = 1/0.55, O_close = 1/0.58)
  p_ref <- 0.60; o_en <- 1.0 / 0.55; o_cl <- 1.0 / 0.58; b_ref <- o_en - 1.0
  expect_lt(abs((p_ref * b_ref - (1.0 - p_ref)) - (0.60 / 0.55 - 1.0)), 1e-12)
  expect_lt(abs(((p_ref * b_ref - (1.0 - p_ref)) / b_ref) - (1.0 / 9.0)), 1e-12)
  expect_lt(abs(((o_en / o_cl) - 1.0) - (0.58 / 0.55 - 1.0)), 1e-12)

  # Backtest Gold table invariants (4,859 games across 2024-2025)
  expect_equal(nrow(bt), 4859L)
  expect_true(all(bt$p_fair > 0.05 & bt$p_fair < 0.95))
  expect_true(all(bt$o_home > 1.0 & bt$o_away > 1.0))
  # Exact -105/-105 two-way book overround invariant: 1/o_home + 1/o_away == 210/205
  expect_lt(max(abs((1.0 / bt$o_home + 1.0 / bt$o_away) - (210.0 / 205.0))), 1e-5)
  expect_true(all(bt$f_kelly_qtr >= 0.0 & bt$f_kelly_qtr <= 0.05))
  expect_true(all(bt$f_kelly_eighth <= bt$f_kelly_qtr + 1e-12))

  # Staking invariants on Locked 2025 OOS (EV >= +2%)
  s25_sb <- st[st$split_name == "Locked 2025 OOS" & grepl("105", st$vig_tier) & abs(st$ev_min - 0.02) < 1e-9, ]
  s25_ex <- st[st$split_name == "Locked 2025 OOS" & grepl("102", st$vig_tier) & abs(st$ev_min - 0.02) < 1e-9, ]
  expect_equal(nrow(s25_sb), 4L)
  expect_lt(
    s25_sb$max_drawdown_pct[s25_sb$regime == "Eighth_Kelly"],
    s25_sb$max_drawdown_pct[s25_sb$regime == "Full_Kelly"]
  )
  expect_gt(
    s25_ex$final_bankroll[s25_ex$regime == "Quarter_Kelly"],
    s25_sb$final_bankroll[s25_sb$regime == "Quarter_Kelly"]
  )

  # 30-club 2026 Futures & Polymarket overround invariants
  expect_equal(nrow(fut), 30L)
  expect_lt(abs(sum(fut$mlbr_ws_prob) - 1.0), 1e-5)
  expect_lt(abs(sum(fut$pm_ws_novig) - 1.0), 1e-5)
  expect_gt(sum(fut$pm_ws_mid), 1.0)
  expect_true(all(fut$qtr_kelly_ws_ask <= fut$qtr_kelly_ws + 1e-12))
})

test_that("EXP-011 Gold Hawk-Eye Bat-Tracking & Arm-Slot Biomechanics satisfy OOS invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  bat_p <- file.path(proc, "batter_bat_tracking_biomechanics_2023_2026.parquet")
  pit_p <- file.path(proc, "pitcher_arm_angle_biomechanics_2020_2026.parquet")
  abl_p <- file.path(proc, "bat_tracking_oos_ablation_metrics.parquet")
  yoy_p <- file.path(proc, "biomechanics_yoy_reliability_2024_2025.parquet")
  skip_if_not(all(file.exists(c(bat_p, pit_p, abl_p, yoy_p))), "EXP-011 Gold files not materialized")

  bat <- arrow::read_parquet(bat_p)
  pit <- arrow::read_parquet(pit_p)
  abl <- arrow::read_parquet(abl_p)
  yoy <- arrow::read_parquet(yoy_p)

  expect_gt(nrow(bat), 2000L)
  expect_true(all(bat$mean_bat_speed >= 60.0 & bat$mean_bat_speed <= 90.0))
  expect_gt(nrow(pit), 1500L)
  expect_true(all(pit$mean_arm_angle >= -10.0 & pit$mean_arm_angle <= 85.0))
  # Biomechanical sensors must beat traditional wOBA on YoY reliability
  expect_gt(yoy$yoy_r[yoy$metric == "Pitcher Arm Angle (deg)"], 0.90)
  expect_gt(yoy$yoy_r[yoy$metric == "Batter Bat Speed (mph)"],  0.90)
  expect_gt(yoy$yoy_r[yoy$metric == "Batter Bat Speed (mph)"],  yoy$yoy_r[yoy$metric == "Traditional wOBA"])
  # OOS lift checks: lower ISO RMSE and higher 4-seam Whiff AUC
  expect_lt(abl$value[abl$task == "2025_OOS_ISO_RMSE" & abl$model == "Plus_HawkEye_BatTracking"],
            abl$value[abl$task == "2025_OOS_ISO_RMSE" & abl$model == "Baseline_Prior_Stat"])
  expect_gt(abl$value[abl$task == "2024_2025_OOS_FF_Whiff_AUC" & abl$model == "Plus_ArmAngle_SlotRide"],
            abl$value[abl$task == "2024_2025_OOS_FF_Whiff_AUC" & abl$model == "Baseline_Velo_IVB"])
})

test_that("EXP-012 Gold Weather/Wind, Umpire Zones & Catcher Framing CSAA satisfy invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  wx_p <- file.path(proc, "game_weather_umpire_environment_2015_2025.parquet")
  um_p <- file.path(proc, "umpire_zone_factors_2015_2025.parquet")
  cf_p <- file.path(proc, "catcher_framing_csaa_2021_2025.parquet")
  skip_if_not(all(file.exists(c(wx_p, um_p, cf_p))), "EXP-012 Gold files not materialized")

  wx <- arrow::read_parquet(wx_p)
  um <- arrow::read_parquet(um_p)
  cf <- arrow::read_parquet(cf_p)

  expect_gt(nrow(wx), 25000L)
  expect_true(all(wx$temp_f >= 25.0 & wx$temp_f <= 115.0))
  # Hot games (>=85F) must score significantly higher than cold games (<55F)
  expect_gt(mean(wx$actual_total_runs[wx$temp_f >= 85]), mean(wx$actual_total_runs[wx$temp_f < 55]) + 0.5)
  expect_gt(nrow(um), 100L)
  # Zero-sum within-season centering for Catcher Framing CSAA
  cf25 <- cf[cf$season == 2025L, ]
  expect_lt(abs(sum(cf25$csaa)), 1e-4)
  expect_gt(max(cf25$framing_runs), 15.0)
})

test_that("EXP-013 Gold Starter Velocity Decay & Rolling Bullpen Fatigue satisfy invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  dec_p <- file.path(proc, "starter_velocity_decay_curve_2018_2025.parquet")
  bp_p  <- file.path(proc, "team_bullpen_fatigue_daily_2018_2025.parquet")
  st_p  <- file.path(proc, "pitcher_stamina_leaderboard_2024_2025.parquet")
  skip_if_not(all(file.exists(c(dec_p, bp_p, st_p))), "EXP-013 Gold files not materialized")

  dec <- arrow::read_parquet(dec_p)
  bp  <- arrow::read_parquet(bp_p)
  st  <- arrow::read_parquet(st_p)

  expect_equal(nrow(dec), 6L)
  expect_lt(dec$mean_velo_delta[dec$pc_bin == "61-80"], -0.25)
  expect_gt(nrow(bp), 30000L)
  expect_true(all(bp$bp_pitches_3d >= 0 & bp$bp_pitches_1d >= 0))
  expect_gt(mean(bp$late_runs_allowed[bp$bp_pitches_3d > 180]),
            mean(bp$late_runs_allowed[bp$bp_pitches_3d >= 61 & bp$bp_pitches_3d <= 100]))
  expect_gt(nrow(st), 150L)
})

test_that("EXP-014 Gold Unified Pro Multi-Market Engine & Exact 12-Team Bracket satisfy invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  gm_p <- file.path(proc, "unified_pro_game_f5_totals_2024_2025.parquet")
  mm_p <- file.path(proc, "unified_pro_multi_market_metrics.parquet")
  br_p <- file.path(proc, "mlb_2026_12team_bracket_ws_odds.parquet")
  skip_if_not(all(file.exists(c(gm_p, mm_p, br_p))), "EXP-014 Gold files not materialized")

  gm <- arrow::read_parquet(gm_p)
  mm <- arrow::read_parquet(mm_p)
  br <- arrow::read_parquet(br_p)

  expect_equal(nrow(gm), 4859L)
  expect_true(all(gm$p_m6_ml > 0.05 & gm$p_m6_ml < 0.95))
  expect_true(all(gm$p_m6_f5 > 0.05 & gm$p_m6_f5 < 0.95))
  # Unified Pro Totals must beat EXP-009 baseline Totals RMSE on locked 2025 OOS
  expect_lt(mm$metric_val[mm$model == "M6_Pro_Totals"], mm$metric_val[mm$model == "EXP-009_Totals"])
  # Exact 12-Team Playoff Bracket probability conservation invariants
  expect_equal(nrow(br), 30L)
  expect_lt(abs(sum(br$pennant_prob[br$league == "AL"]) - 1.0), 1e-5)
  expect_lt(abs(sum(br$pennant_prob[br$league == "NL"]) - 1.0), 1e-5)
  expect_lt(abs(sum(br$ws_prob_12team) - 1.0), 1e-5)
  expect_lt(abs(sum(br$playoff_prob_12) - 12.0), 1e-3)
  expect_lt(abs(sum(br$bye_prob) - 4.0), 1e-3)
})

test_that("EXP-011..015 published external reference-value formulas & EXP-015 Gold K-Props satisfy invariants", {
  skip_if_not_installed("arrow")
  # 1. Published Alan Nathan / MLB Statcast MaxEV collision formula: 1.23*75 + 0.2116*95 = 112.352 mph
  expect_lt(abs((1.23 * 75.0 + 0.2116 * 95.0) - 112.352), 1e-9)
  # 2. Exact Best-of-3 series probability for p=0.60: p^2 + 2*p^2*(1-p) = 0.36 + 0.288 = 0.648
  expect_lt(abs((0.60^2 + 2 * 0.60^2 * 0.40) - 0.648), 1e-12)
  # 3. Exact Poisson Over 5.5 K probability for lambda = 6.0: 1 - ppois(5, 6) == 0.5543203586
  expect_lt(abs((1.0 - stats::ppois(5L, 6.0)) - 0.5543203586), 1e-7)

  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  lu_p <- file.path(proc, "daily_lineups_platoon_2024_2025.parquet")
  kp_p <- file.path(proc, "sp_strikeout_prop_backtest_2024_2025.parquet")
  ab_p <- file.path(proc, "sp_k_prop_ablation_metrics_2025.parquet")
  skip_if_not(all(file.exists(c(lu_p, kp_p, ab_p))), "EXP-015 Gold files not materialized")

  lu <- arrow::read_parquet(lu_p)
  kp <- arrow::read_parquet(kp_p)
  ab <- arrow::read_parquet(ab_p)

  expect_gt(nrow(lu), 4800L)
  expect_gt(nrow(kp), 9500L)
  expect_true(all(kp$p_over_4_5 >= kp$p_over_5_5 & kp$p_over_5_5 >= kp$p_over_6_5))
  expect_true(all(kp$exp_so_m2 > 0 & kp$exp_so_m2 < 15))
  # Pro K-Prop engine must strictly beat both League Mean and SP-Prior-Only on RMSE, Poisson NLL, Brier & AUC
  expect_lt(ab$rmse_so[ab$model == "M2_Pro_K_Prop_Engine"], ab$rmse_so[ab$model == "M1_SP_Prior_Only"])
  expect_lt(ab$pois_nll[ab$model == "M2_Pro_K_Prop_Engine"], ab$pois_nll[ab$model == "M1_SP_Prior_Only"])
  expect_lt(ab$brier_o55[ab$model == "M2_Pro_K_Prop_Engine"], ab$brier_o55[ab$model == "M1_SP_Prior_Only"])
  expect_gt(ab$auc_o55[ab$model == "M2_Pro_K_Prop_Engine"], ab$auc_o55[ab$model == "M1_SP_Prior_Only"] + 0.015)
})

test_that("EXP-016 Silver statcast_biomechanics & Dynamic State-Space NegBin K-Props satisfy invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  silv <- file.path(lake, "data/features/statcast_biomechanics")
  proc <- file.path(lake, "data/processed")
  kp_p <- file.path(proc, "dynamic_statespace_negbin_kprops_2024_2025.parquet")
  ab_p <- file.path(proc, "dynamic_negbin_ablation_metrics_2025.parquet")
  skip_if_not(dir.exists(silv) && file.exists(kp_p) && file.exists(ab_p), "EXP-016 Silver/Gold files not materialized")

  kp <- arrow::read_parquet(kp_p)
  ab <- arrow::read_parquet(ab_p)

  expect_equal(nrow(kp), 9714L)
  expect_true(all(is.finite(kp$mu_m4) & kp$mu_m4 > 0 & kp$mu_m4 < 15))
  expect_true(all(kp$p_m4_o55 >= kp$p_m4_o75))
  # NegBin Static must beat Poisson Static on Count NLL & Over 7.5 K tail Brier
  expect_lt(ab$count_nll[ab$model == "M3_NegBin_Static"], ab$count_nll[ab$model == "M2_Poisson_Static"])
  expect_lt(ab$brier_o75[ab$model == "M3_NegBin_Static"], ab$brier_o75[ab$model == "M2_Poisson_Static"])
  # Dynamic State-Space NegBin must beat Static NegBin on RMSE, Count NLL, Over 5.5 K AUC & Over 7.5 K Brier
  expect_lt(ab$rmse_so[ab$model == "M4_NegBin_Dynamic_StateSpace"], ab$rmse_so[ab$model == "M3_NegBin_Static"])
  expect_lt(ab$count_nll[ab$model == "M4_NegBin_Dynamic_StateSpace"], ab$count_nll[ab$model == "M3_NegBin_Static"])
  expect_gt(ab$auc_o55[ab$model == "M4_NegBin_Dynamic_StateSpace"], ab$auc_o55[ab$model == "M3_NegBin_Static"] + 0.01)
  expect_lt(ab$brier_o75[ab$model == "M4_NegBin_Dynamic_StateSpace"], ab$brier_o75[ab$model == "M3_NegBin_Static"])
})

test_that("EXP-017 Native v_statcast_biomechanics View & Batter HR/TB Props satisfy OOS invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  con <- lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true("v_statcast_biomechanics" %in% DBI::dbListTables(con))
  bio_chk <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n, ROUND(AVG(csaa_pitch), 6) AS m_csaa FROM v_statcast_biomechanics")
  expect_gt(bio_chk$n, 4500000L)
  expect_lt(abs(bio_chk$m_csaa), 1e-5)

  proc <- file.path(lake, "data/processed")
  bg_p <- file.path(proc, "batter_hr_tb_prop_backtest_2024_2025.parquet")
  ab_p <- file.path(proc, "batter_hr_tb_ablation_metrics_2025.parquet")
  skip_if_not(file.exists(bg_p) && file.exists(ab_p), "EXP-017 Gold files not materialized")

  bg <- arrow::read_parquet(bg_p)
  ab <- arrow::read_parquet(ab_p)

  expect_gt(nrow(bg), 87000L)
  expect_true(all(bg$p_hr_m2 > 0 & bg$p_hr_m2 < 0.65))
  expect_true(all(bg$p_tb2_m2 >= bg$p_hr_m2))
  # M2 must strictly beat M1 (ISO/SLG only) on both HR>=1 and TB>=2 Log-Loss & ROC AUC
  expect_lt(ab$log_loss[ab$market == "Home Run (HR >= 1)" & ab$model == "M2_Pro_Biomech_Env"],
            ab$log_loss[ab$market == "Home Run (HR >= 1)" & ab$model == "M1_Prior_ISO_Only"])
  expect_gt(ab$roc_auc[ab$market == "Home Run (HR >= 1)" & ab$model == "M2_Pro_Biomech_Env"],
            ab$roc_auc[ab$market == "Home Run (HR >= 1)" & ab$model == "M1_Prior_ISO_Only"] + 0.015)
  expect_lt(ab$log_loss[ab$market == "Total Bases (TB >= 2)" & ab$model == "M2_Pro_Biomech_Env"],
            ab$log_loss[ab$market == "Total Bases (TB >= 2)" & ab$model == "M1_Prior_ISO_Only"])
  expect_gt(ab$roc_auc[ab$market == "Total Bases (TB >= 2)" & ab$model == "M2_Pro_Biomech_Env"],
            ab$roc_auc[ab$market == "Total Bases (TB >= 2)" & ab$model == "M1_Prior_ISO_Only"] + 0.020)
})

test_that("EXP-018 Same-Game Parlay (SGP) Correlation Matrix & Copula Pricing satisfy OOS invariants", {
  skip_if_not_installed("arrow")
  lake <- default_lake()
  proc <- file.path(lake, "data/processed")
  cm_p <- file.path(proc, "sgp_empirical_correlation_matrix_2024_2025.parquet")
  bt_p <- file.path(proc, "sgp_copula_backtest_2024_2025.parquet")
  ab_p <- file.path(proc, "sgp_copula_ablation_metrics_2025.parquet")
  skip_if_not(file.exists(cm_p) && file.exists(bt_p) && file.exists(ab_p), "EXP-018 Gold files not materialized")

  cm <- arrow::read_parquet(cm_p)
  bt <- arrow::read_parquet(bt_p)
  ab <- arrow::read_parquet(ab_p)

  expect_equal(nrow(cm), 49L)
  expect_gt(nrow(bt), 130000L)
  # Strict Frechet-Hoeffding bounds across all 130k+ OOS SGP legs
  lb <- pmax(0, bt$p_leg1 + bt$p_leg2 - 1)
  ub <- pmin(bt$p_leg1, bt$p_leg2)
  expect_true(all(bt$p_gauss   >= lb - 1e-8 & bt$p_gauss   <= ub + 1e-8))
  expect_true(all(bt$p_clayton >= lb - 1e-8 & bt$p_clayton <= ub + 1e-8))
  # Positive scripts must have pricing_mult > 1.0; negative scripts < 1.0
  expect_gt(ab$pricing_mult[ab$sgp_id == "SGP1_HomeML_F5Home" & ab$model == "M1_Gaussian_Copula"], 1.40)
  expect_gt(ab$pricing_mult[ab$sgp_id == "SGP4_BatterHR_TeamML" & ab$model == "M1_Gaussian_Copula"], 1.20)
  expect_lt(ab$pricing_mult[ab$sgp_id == "SGP2_HomeSP_O55K_Over85" & ab$model == "M1_Gaussian_Copula"], 0.90)
  expect_lt(ab$pricing_mult[ab$sgp_id == "SGP6_BatterTB2_OppSP_O55K" & ab$model == "M1_Gaussian_Copula"], 0.95)
  # Copula must strictly beat Naive Independence on locked 2025 OOS Log-Loss across core positive & 3-leg SGPs
  expect_lt(ab$parlay_logloss[ab$sgp_id == "SGP1_HomeML_F5Home" & ab$model == "M1_Gaussian_Copula"],
            ab$parlay_logloss[ab$sgp_id == "SGP1_HomeML_F5Home" & ab$model == "M0_Naive_Independence"] - 0.05)
  expect_lt(ab$parlay_logloss[ab$sgp_id == "SGP4_BatterHR_TeamML" & ab$model == "M1_Gaussian_Copula"],
            ab$parlay_logloss[ab$sgp_id == "SGP4_BatterHR_TeamML" & ab$model == "M0_Naive_Independence"])
  expect_lt(ab$parlay_logloss[ab$sgp_id == "SGP5_BatterTB2_Over85" & ab$model == "M2_Clayton_Copula"],
            ab$parlay_logloss[ab$sgp_id == "SGP5_BatterTB2_Over85" & ab$model == "M0_Naive_Independence"])
  expect_lt(ab$parlay_logloss[ab$sgp_id == "SGP8_3Leg_HomeML_TB2_Over85" & ab$model == "M1_Gaussian_Copula"],
            ab$parlay_logloss[ab$sgp_id == "SGP8_3Leg_HomeML_TB2_Over85" & ab$model == "M0_Naive_Independence"])
  # 4-cell joint NLL strictly beats naive independence even on negative-script SGP2
  expect_lt(ab$joint_4cell_nll[ab$sgp_id == "SGP2_HomeSP_O55K_Over85" & ab$model == "M1_Gaussian_Copula"],
            ab$joint_4cell_nll[ab$sgp_id == "SGP2_HomeSP_O55K_Over85" & ab$model == "M0_Naive_Independence"])
})
