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
