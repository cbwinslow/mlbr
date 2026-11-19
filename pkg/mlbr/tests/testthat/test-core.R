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
