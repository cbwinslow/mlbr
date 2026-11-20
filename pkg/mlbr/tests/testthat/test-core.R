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
