# ==============================================================================
# mlbr Silver Tier Materialization: Pitch-Level Biomechanics & Catcher Framing
# ==============================================================================
# Materializes validated EXP-011 (arm_angle slot-adjusted IVB, Alan Nathan
# MaxEV / Squared-Up / Blast) and EXP-012 (2D strike-zone Catcher Framing CSAA)
# into Hive-partitioned ADR-001 ZSTD Silver Parquet at:
#   data/features/statcast_biomechanics/game_year=YYYY/
#
# CLI Usage:
#   Rscript R/materialize_biomechanics_framing.R --smoke  # 2025 only (<5s)
#   Rscript R/materialize_biomechanics_framing.R --full   # 2020-2026 (~10s)
# ==============================================================================

suppressPackageStartupMessages({
  .libPaths(c("/mnt/storage/R/library", .libPaths()))
  library(mlbr)
  library(DBI)
  library(duckdb)
  library(data.table)
})

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) stop(paste0("[SILVER BIOMECH ASSERTION FAILED] ", msg), call. = FALSE)
}

materialize_biomechanics_framing <- function(smoke = FALSE, lake = NULL) {
  t0 <- Sys.time()
  if (is.null(lake)) lake <- mlbr::default_lake()
  out_dir <- file.path(lake, "data/features/statcast_biomechanics")

  cat("==============================================================================\n")
  cat(sprintf("  mlbr Silver Materialization: statcast_biomechanics [%s]\n",
              if (smoke) "SMOKE" else "FULL"))
  cat("==============================================================================\n")

  con <- mlbr::lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "SET memory_limit = '12GB';")

  # 1. Fit pre-test (2020-2023) quadratic arm_angle -> IVB expectation on 4-seam fastballs
  ff_tr <- as.data.table(DBI::dbGetQuery(con, "
    SELECT TRY_CAST(arm_angle AS DOUBLE) AS arm_angle,
           12.0 * TRY_CAST(pfx_z AS DOUBLE) AS ivb_in
    FROM v_statcast
    WHERE game_year BETWEEN 2020 AND 2023
      AND game_type = 'R' AND pitch_type = 'FF'
      AND TRY_CAST(arm_angle AS DOUBLE) BETWEEN -10.0 AND 85.0
      AND TRY_CAST(pfx_z AS DOUBLE) IS NOT NULL
  "))
  fit_slot <- stats::lm(ivb_in ~ arm_angle + I(arm_angle^2), data = ff_tr)
  b0 <- unname(stats::coef(fit_slot)[1L])
  b1 <- unname(stats::coef(fit_slot)[2L])
  b2 <- unname(stats::coef(fit_slot)[3L])
  cat(sprintf("-> Pre-test (2020-2023) Arm-Slot IVB curve: %.4f + %.4f*arm + %.6f*arm^2\n", b0, b1, b2))

  # 2. Materialize partitioned ZSTD Silver Parquet directly in DuckDB
  yr_where <- if (smoke) "s.game_year = 2025" else "s.game_year BETWEEN 2020 AND 2026"
  if (!smoke) {
    if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  target_path <- if (smoke) file.path(tempdir(), "mlbr_smoke_biomech") else out_dir
  if (smoke && dir.exists(target_path)) unlink(target_path, recursive = TRUE)
  if (smoke) dir.create(target_path, recursive = TRUE, showWarnings = FALSE)

  sql_mat <- sprintf("
    COPY (
      WITH train_grid AS (
        SELECT
          stand,
          CAST(FLOOR((LEAST(GREATEST(plate_x, -1.79), 1.79) + 1.80) / 0.15) AS INTEGER) AS x_bin,
          CAST(FLOOR((LEAST(GREATEST((plate_z - sz_bot) / NULLIF(sz_top - sz_bot, 0), -0.49), 1.49) + 0.50) / 0.083333) AS INTEGER) AS z_bin,
          AVG(CASE WHEN description = 'called_strike' THEN 1.0 ELSE 0.0 END) AS p_cs_exp
        FROM v_statcast
        WHERE game_year BETWEEN 2021 AND 2023
          AND game_type = 'R'
          AND description IN ('called_strike', 'ball')
          AND plate_x IS NOT NULL AND plate_z IS NOT NULL
          AND sz_top IS NOT NULL AND sz_bot IS NOT NULL
          AND (sz_top - sz_bot) > 0.5 AND stand IN ('L', 'R')
        GROUP BY stand, x_bin, z_bin
      ),
      base AS (
        SELECT
          CAST(s.game_year AS INTEGER) AS game_year,
          CAST(s.game_pk AS BIGINT) AS game_pk,
          CAST(s.at_bat_number AS INTEGER) AS at_bat_number,
          CAST(s.pitch_number AS INTEGER) AS pitch_number,
          CAST(s.game_date AS DATE) AS game_date,
          CAST(s.pitcher AS INTEGER) AS pitcher,
          CAST(s.batter AS INTEGER) AS batter,
          CAST(s.fielder_2 AS INTEGER) AS catcher,
          s.pitch_type,
          TRY_CAST(s.arm_angle AS DOUBLE) AS arm_angle,
          12.0 * TRY_CAST(s.pfx_z AS DOUBLE) AS ivb_in,
          CASE WHEN TRY_CAST(s.arm_angle AS DOUBLE) BETWEEN -10.0 AND 85.0 AND s.pfx_z IS NOT NULL
               THEN (12.0 * TRY_CAST(s.pfx_z AS DOUBLE)) - (%.8f + %.8f * TRY_CAST(s.arm_angle AS DOUBLE) + %.10f * POWER(TRY_CAST(s.arm_angle AS DOUBLE), 2))
               ELSE NULL END AS ivb_above_slot,
          TRY_CAST(s.bat_speed AS DOUBLE) AS bat_speed_dbl,
          TRY_CAST(s.swing_length AS DOUBLE) AS swing_length_dbl,
          TRY_CAST(s.attack_angle AS DOUBLE) AS attack_angle_dbl,
          CASE WHEN TRY_CAST(s.bat_speed AS DOUBLE) >= 60.0 AND TRY_CAST(s.release_speed AS DOUBLE) IS NOT NULL
               THEN 1.23 * TRY_CAST(s.bat_speed AS DOUBLE) + 0.2116 * TRY_CAST(s.release_speed AS DOUBLE)
               ELSE NULL END AS max_ev_mph,
          TRY_CAST(s.launch_speed AS DOUBLE) AS ev_dbl,
          CASE WHEN s.description IN ('called_strike', 'ball') AND s.plate_x IS NOT NULL AND s.plate_z IS NOT NULL
                    AND s.sz_top IS NOT NULL AND s.sz_bot IS NOT NULL AND (s.sz_top - s.sz_bot) > 0.5
               THEN (CASE WHEN s.description = 'called_strike' THEN 1.0 ELSE 0.0 END) - COALESCE(g.p_cs_exp, 0.0)
               ELSE NULL END AS csaa_raw
        FROM v_statcast s
        LEFT JOIN train_grid g
          ON s.stand = g.stand
         AND CAST(FLOOR((LEAST(GREATEST(s.plate_x, -1.79), 1.79) + 1.80) / 0.15) AS INTEGER) = g.x_bin
         AND CAST(FLOOR((LEAST(GREATEST((s.plate_z - s.sz_bot) / NULLIF(s.sz_top - s.sz_bot, 0), -0.49), 1.49) + 0.50) / 0.083333) AS INTEGER) = g.z_bin
        WHERE %s AND s.game_type = 'R'
      ),
      yr_drift AS (
        SELECT game_year, AVG(csaa_raw) AS drift
        FROM base WHERE csaa_raw IS NOT NULL GROUP BY game_year
      )
      SELECT
        b.game_year,
        b.game_pk,
        b.at_bat_number,
        b.pitch_number,
        b.game_date,
        b.pitcher,
        b.batter,
        b.catcher,
        b.pitch_type,
        b.arm_angle,
        b.ivb_in,
        b.ivb_above_slot,
        b.bat_speed_dbl,
        b.swing_length_dbl,
        b.attack_angle_dbl,
        b.max_ev_mph,
        CASE WHEN b.ev_dbl IS NOT NULL AND b.max_ev_mph > 0 THEN b.ev_dbl / b.max_ev_mph ELSE NULL END AS sq_ratio,
        CASE WHEN b.ev_dbl IS NOT NULL AND b.max_ev_mph > 0 AND (b.ev_dbl / b.max_ev_mph) >= 0.80 THEN 1 ELSE 0 END AS is_squared_up,
        CASE WHEN b.ev_dbl IS NOT NULL AND b.max_ev_mph > 0 AND (b.ev_dbl / b.max_ev_mph) >= 0.80
                  AND (b.bat_speed_dbl + 100.0 * (b.ev_dbl / b.max_ev_mph)) >= 164.0 THEN 1 ELSE 0 END AS is_blast,
        CASE WHEN b.csaa_raw IS NOT NULL THEN b.csaa_raw - COALESCE(d.drift, 0.0) ELSE NULL END AS csaa_pitch
      FROM base b
      LEFT JOIN yr_drift d ON b.game_year = d.game_year
      ORDER BY b.game_year, b.pitcher, b.game_date, b.game_pk, b.at_bat_number, b.pitch_number
    ) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD, PARTITION_BY (game_year), ROW_GROUP_SIZE 100000, OVERWRITE_OR_IGNORE 1)
  ", b0, b1, b2, yr_where, target_path)

  DBI::dbExecute(con, sql_mat)

  chk <- DBI::dbGetQuery(con, sprintf("
    SELECT COUNT(*) AS n_rows,
           COUNT(ivb_above_slot) AS n_slot,
           COUNT(bat_speed_dbl) AS n_bs,
           COUNT(csaa_pitch) AS n_csaa,
           ROUND(AVG(csaa_pitch), 6) AS mean_csaa
    FROM read_parquet('%s/**/*.parquet', hive_partitioning = true)
  ", target_path))
  print(chk)
  assert_true(chk$n_rows >= 500000L, "Insufficient Silver rows materialized")
  assert_true(abs(chk$mean_csaa) < 1e-4, "Zero-sum pitch-level CSAA centering violated")

  if (smoke) unlink(target_path, recursive = TRUE)

  cat(sprintf("\n=== SILVER statcast_biomechanics [%s] COMPLETED IN %.1f SECONDS ===\n",
              if (smoke) "SMOKE" else "FULL",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(TRUE)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  materialize_biomechanics_framing(smoke = ("--smoke" %in% args))
}
