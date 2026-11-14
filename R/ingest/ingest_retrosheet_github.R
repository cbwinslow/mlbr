# ingest_retrosheet_github.R - Retrosheet play-level data via the Chadwick Bureau
# GitHub mirror (chadwickbureau/retrosheet) -> data/raw/retrosheet/
#
# WHY THIS EXISTS: retrosheet.org rate-bans aggressive scrapers (this network is
# currently banned). The Chadwick Bureau maintains an "enhanced version" of the
# same data on GitHub, updated within days of Retrosheet releases. Cloning it is
# a SINGLE git connection (polite), then all parsing is LOCAL - zero web traffic.
#
# Outputs (per year, one row per play/game/player-game/sub - never aggregated):
#   plays-<year>.parquet        cwevent -f 0-96 -x 0-62   (one row per play)
#   games-<year>.parquet        cwgame  -f 0-83           (one row per game)
#   player-games-<year>.parquet cwdaily -f 0-79           (one row per player-game)
#   subs-<year>.parquet         cwsub                     (one row per substitution)
#   rosters-<year>.parquet      from *.ROS roster files
# Plus one-time reference tables from reference/ (teams, ballparks, biofile,
# ejections, coaches, relatives) -> data/raw/retrosheet/reference-*.parquet
#
# Usage: Rscript R/ingest/ingest_retrosheet_github.R <from_year> <to_year>
suppressPackageStartupMessages({ source("R/ingest/00_common.R"); library(arrow); library(data.table) })
args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) as.integer(args[1]) else 1871
to   <- if (length(args) >= 2) as.integer(args[2]) else as.integer(format(Sys.Date(), "%Y"))

repo <- normalizePath("data/raw/retrosheet/chadwick-repo", mustWork = FALSE)
if (!dir.exists(repo)) {
  message("retrosheet-github: cloning chadwickbureau/retrosheet (single git connection)...")
  system2("git", c("clone", "--depth", "1", "--single-branch", "--branch", "master",
                   "https://github.com/chadwickbureau/retrosheet.git", repo))
  if (!dir.exists(repo)) stop("clone failed")
} else {
  # refresh to latest (fast-forward only, one connection)
  system2("git", c("-C", repo, "pull", "--ff-only"), stdout = NULL, stderr = NULL)
}

read_tool_csv <- function(cmd, cwd) {
  oldwd <- getwd(); setwd(cwd); on.exit(setwd(oldwd))
  out <- tempfile(fileext = ".csv")
  errf <- tempfile(fileext = ".err")
  # NOTE: system2() mangles long command strings; classic system() passes them through
  status <- system(sprintf("%s > %s 2> %s", cmd, out, errf))
  message("    DBG status=", status, " exists=", file.exists(out),
          " size=", if (file.exists(out)) file.size(out) else -1)
  if (status != 0 || (!file.exists(out) || file.size(out) <= 0))
    message("    stderr: ", substr(paste(readLines(errf, warn = FALSE), collapse = " | "), 1, 200))
  sz <- if (file.exists(out)) file.size(out) else -1L
  if (status != 0 || sz <= 0)
    message("    tool status=", status, " outsize=", sz, ": ", substr(cmd, 1, 60), "...")
  if (status != 0) return(NULL)
  dt <- tryCatch(fread(out, sep = ",", header = TRUE, quote = '"',
                       na.strings = c("", "NA", "NA?"),
                       colClasses = "character"),  # read raw: keep every column verbatim
                 error = function(e) { message("    fread error: ", substr(conditionMessage(e), 1, 300)); NULL })
  unlink(out)
  dt
}

for (yr in from:to) {
  ydir <- normalizePath(file.path(repo, "seasons", yr), mustWork = FALSE)
  if (!dir.exists(ydir)) { message("retrosheet-github: no ", yr, " dir, skipping"); next }
  ev_files <- list.files(ydir, pattern = "\\.EV[A-Z]$", full.names = TRUE)
  if (!length(ev_files)) { message("retrosheet-github: ", yr, " has no event files"); next }
  if (!file.exists(file.path(raw_dir("retrosheet"), sprintf("plays-%d.parquet", yr)))) {
    # cwevent needs TEAM<year>.ROS (all rosters concatenated) in the CWD of the files
    ros_files <- list.files(ydir, pattern = "^\\w+\\d{4}\\.ROS$", full.names = TRUE)
    team_ros <- file.path(ydir, sprintf("TEAM%d.ROS", yr))
    if (length(ros_files) && !file.exists(team_ros)) {
      ok <- system(sprintf("cat %s > %s", paste(shQuote(ros_files), collapse = " "), shQuote(team_ros)))
      message("  built TEAM", yr, ".ROS status=", ok, " exists=", file.exists(team_ros))
    }
    message("retrosheet-github: parsing ", yr, " (", length(ev_files), " event files)...")
    rel <- basename(ev_files)
    pieces <- list(
      plays   = sprintf("cwevent -q -y %d -n -f 0-96 -x 0-62 %s", yr,
                        paste(sprintf("%s/%s", ydir, rel), collapse = " ")),
      games   = sprintf("cwgame  -q -y %d -n -f 0-83 %s", yr,
                        paste(sprintf("%s/%s", ydir, rel), collapse = " ")),
      pgames  = sprintf("cwdaily -q -y %d -n -f 0-79 %s", yr,
                        paste(sprintf("%s/%s", ydir, rel), collapse = " ")),
      subs    = sprintf("cwsub   -q -y %d -n %s", yr,
                        paste(sprintf("%s/%s", ydir, rel), collapse = " "))
    )
    for (nm in names(pieces)) {
      p <- file.path(raw_dir("retrosheet"), sprintf(
        c(plays = "plays-%d.parquet", games = "games-%d.parquet",
          pgames = "player-games-%d.parquet", subs = "subs-%d.parquet")[nm], yr))
      tryCatch({
        dt <- read_tool_csv(pieces[[nm]], ydir)
        if (!is.null(dt) && nrow(dt) > 0) write_df(as.data.frame(dt), p)
      }, error = function(e) message("  FAILED ", nm, " ", yr, " - ",
                                     substr(conditionMessage(e), 1, 90)))
    }
    if (length(ros_files)) {
      r <- rbindlist(lapply(ros_files, fread, sep = ",", header = FALSE,
                            colClasses = "character"), use.names = FALSE)
      setnames(r, c("player_id", "last", "first", "bats", "throws", "team_id", "position"))
      write_df(as.data.frame(r), file.path(raw_dir("retrosheet"),
                                           sprintf("rosters-%d.parquet", yr)))
    }
    if (file.exists(team_ros)) file.remove(team_ros)   # keep repo worktree clean
  }
}

# one-time reference tables (player bios, teams, parks, ejections...)
for (f in list.files(file.path(repo, "reference"), full.names = TRUE)) {
  nm <- sub("\\.csv$", "", basename(f))
  p  <- file.path(raw_dir("retrosheet", "reference"), sprintf("%s.parquet", nm))
  if (file.exists(p)) next
  tryCatch({ dt <- fread(f, colClasses = "character")
             write_df(as.data.frame(dt), p) },
           error = function(e) message("  FAILED reference/", nm, " - ",
                                       substr(conditionMessage(e), 1, 80)))
}
message("retrosheet-github: done (", from, "-", to, ")")
