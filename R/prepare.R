# prepare.R - build modeling-ready player-season tables in data/processed/
#
# Run (from project root):  Rscript R/prepare.R
# Reads data/raw (parquet), writes data/processed/batting_seasons.parquet
# and data/processed/pitching_seasons.parquet.
# Defensive by design: every join is wrapped so a missing source table
# degrades gracefully instead of crashing the build.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(janitor)
})
options(dplyr.summarise.inform = FALSE)

dir.create("data/processed", showWarnings = FALSE)
rd <- function(src, f) {
  p <- file.path("data", "raw", src, f)
  if (file.exists(p)) read_parquet(p) else NULL
}

# ---- 1. Lahman core: rate stats, age, salary, WAR ---------------------------
cat("== batting (Lahman) ==\n")
batting <- rd("lahman", "Batting.parquet")
people   <- rd("lahman", "People.parquet")
salaries <- rd("lahman", "Salaries.parquet")
war      <- rd("lahman", "WARValues.parquet")   # may not exist in all Lahman builds

stopifnot(!is.null(batting), !is.null(people))

bat <- batting |>
  filter(yearID >= 2015) |>
  mutate(
    PA  = AB + BB + HBP + SH + SF,
    OBP = ifelse(AB + BB + HBP + SF > 0, (H + BB + HBP) / (AB + BB + HBP + SF), NA_real_),
    SLG = ifelse(AB > 0,
                 ((H - X2B - X3B - HR) + 2 * X2B + 3 * X3B + 4 * HR) / AB, NA_real_),
    OPS = OBP + SLG
  ) |>
  left_join(people |> select(playerID, nameFirst, nameLast, birthYear, retroID),
            by = "playerID") |>
  mutate(age = yearID - birthYear)

if (!is.null(salaries)) {
  sal <- salaries |>
    group_by(playerID, yearID) |>
    summarise(salary = sum(salary, na.rm = TRUE)) |>
    ungroup()
  bat <- bat |> left_join(sal, by = c("playerID", "yearID"))
}
if (!is.null(war)) bat <- bat |> left_join(
  war |> select(playerID, yearID, b_WAR = WAR), by = c("playerID", "yearID"))

# next-season OPS target (the classic "aging curve" setup)
bat <- bat |>
  group_by(playerID) |>
  arrange(yearID, .by_group = TRUE) |>
  mutate(next_OPS = dplyr::lead(OPS)) |>
  ungroup()

write_parquet(bat, "data/processed/batting_seasons.parquet")
cat(sprintf("  batting_seasons: %d rows\n", nrow(bat)))

cat("== pitching (Lahman) ==\n")
pitching <- rd("lahman", "Pitching.parquet")
if (!is.null(pitching)) {
  pit <- pitching |>
    filter(yearID >= 2015) |>
    left_join(people |> select(playerID, nameFirst, nameLast, birthYear),
              by = "playerID") |>
    mutate(age = yearID - birthYear) |>
    group_by(playerID) |>
    arrange(yearID, .by_group = TRUE) |>
    mutate(next_ERA = dplyr::lead(ERA)) |>
    ungroup()
  write_parquet(pit, "data/processed/pitching_seasons.parquet")
  cat(sprintf("  pitching_seasons: %d rows\n", nrow(pit)))
}

# ---- 2. FanGraphs leaders joined via the Chadwick crosswalk ------------------
cat("== fangraphs + chadwick crosswalk ==\n")
lu <- rd("chadwick", "register.parquet")
fg_bat_files <- sort(list.files("data/raw/fangraphs", pattern = "^bat-.*parquet$",
                                full.names = TRUE))
if (!is.null(lu) && length(fg_bat_files) > 0) {
  keymap <- lu |>
    filter(!is.na(key_retro), !is.na(key_fangraphs)) |>
    select(retroID = key_retro, fgid = key_fangraphs) |>
    distinct()

  fg <- fg_bat_files |>
    setNames(gsub("bat-(\\d+)\\.parquet", "\\1", basename(fg_bat_files))) |>
    imap(\(f, yr) read_parquet(f) |> clean_names() |>
           mutate(yearID = as.integer(yr), .before = 1)) |>
    list_rbind() |>
    select(any_of(c("yearID", "playerid", "pa", "wrc_plus", "war", "hard_hit_pct",
                    "barrel_pct", "avg_ev", "sweet_spot_pct", "chase_pct", "whiff_pct"))) |>
    distinct(playerid, yearID, .keep_all = TRUE) |>
    left_join(keymap, by = c("playerid" = "fgid")) |>
    distinct(retroID, yearID, .keep_all = TRUE)

  bat <- read_parquet("data/processed/batting_seasons.parquet")
  if ("retroID" %in% names(bat)) {
    bat <- bat |>
      left_join(fg, by = c("retroID" = "retroID", "yearID" = "yearID"))
    write_parquet(bat, "data/processed/batting_seasons.parquet")
    cat(sprintf("  fangraphs cols joined for %d player-seasons\n",
                sum(!is.na(bat$wrc_plus))))
  } else {
    message("  (People table lacks retroID - skipping FanGraphs join)")
  }
} else {
  message("  (fangraphs or chadwick raw data not present yet - run ingest first)")
}

cat("prepare.R done.\n")
