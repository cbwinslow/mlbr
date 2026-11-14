# retrosheet_github_ingest.R - Retrosheet play-level data via the Chadwick
# Bureau GitHub mirror (chadwickbureau/retrosheet).
#
# WHY THIS PATH: retrosheet.org bans aggressive scrapers. The mirror is the
# same data as a git repo - cloning is ONE connection, then all parsing is
# local (chadwick C tools). Zero web traffic per year parsed.
#
# Outputs (per year, one row per play/game/player-game/sub - never aggregated):
#   plays-<year>.parquet        cwevent -n -f 0-96 -x 0-62
#   games-<year>.parquet        cwgame  -n -f 0-83
#   player-games-<year>.parquet cwdaily -n -f 0-79
#   subs-<year>.parquet         cwsub   -n
#   rosters-<year>.parquet      from *.ROS roster files
# Plus one-time reference tables -> data/raw/retrosheet/reference/*.parquet
#
# GOTCHAS (all learned the hard way):
#   - system2() mangles long command strings; use system() for cwevent pipes
#   - cwevent needs TEAM<year>.ROS (concatenated rosters) in the CWD
#   - the -n flag is required for CSV headers
#   - all columns are read as character; cast downstream in R

#' Ingest Retrosheet data via Chadwick GitHub mirror
#'
#' @rdname retrosheet_github_ingest
#' @param lake project root of the data lake
#' @param from,to year range (inclusive)
#' @param tools_dir directory containing cwevent/cwgame/cwdaily/cwsub
#' @return invisible TRUE; writes parquet files
#' @export
retrosheet_github_ingest <- function(lake, from = 1871L,
                                     to = as.integer(format(Sys.Date(), "%Y")),
                                     tools_dir = path.expand("~/.local/bin")) {
  lake <- default_lake(lake)
  repo <- normalizePath(lake_raw(lake, "retrosheet", "chadwick-repo"),
                        mustWork = FALSE)
  if (!dir.exists(repo)) {
    message("retrosheet-github: cloning chadwickbureau/retrosheet (one git connection)...")
    system2("git", c("clone", "--depth", "1", "--single-branch",
                     "--branch", "master",
                     "https://github.com/chadwickbureau/retrosheet.git", repo))
    if (!dir.exists(repo)) stop("clone failed")
  } else {
    system2("git", c("-C", repo, "pull", "--ff-only"),
            stdout = NULL, stderr = NULL)
  }

  # run a chadwick tool over the year's event files and read the CSV output
  read_tool_csv <- function(cmd, cwd) {
    oldwd <- getwd(); setwd(cwd); on.exit(setwd(oldwd))
    out <- tempfile(fileext = ".csv")
    errf <- tempfile(fileext = ".err")
    status <- system(sprintf("%s > %s 2> %s", cmd, out, errf))
    if (status != 0 || (!file.exists(out) || file.size(out) <= 0)) {
      message("    tool status=", status, " outsize=",
              if (file.exists(out)) file.size(out) else -1L)
      return(NULL)
    }
    dt <- tryCatch(
      data.table::fread(out, sep = ",", header = TRUE, quote = '"',
                        na.strings = c("", "NA", "NA?"),
                        colClasses = "character"),
      error = function(e) {
        message("    fread error: ", substr(conditionMessage(e), 1, 300)); NULL
      })
    unlink(out)
    dt
  }

  for (yr in from:to) {
    ydir <- normalizePath(file.path(repo, "seasons", yr), mustWork = FALSE)
    if (!dir.exists(ydir)) { message("retrosheet-github: no ", yr, " dir"); next }
    ev_files <- list.files(ydir, pattern = "\\.EV[A-Z]$", full.names = TRUE)
    if (!length(ev_files)) { message("retrosheet-github: ", yr, " no event files"); next }
    if (file.exists(lake_raw(lake, "retrosheet",
                             sprintf("plays-%d.parquet", yr)))) next

    # cwevent needs TEAM<year>.ROS (all rosters concatenated) in the CWD
    ros_files <- list.files(ydir, pattern = "^\\w+\\d{4}\\.ROS$", full.names = TRUE)
    team_ros <- file.path(ydir, sprintf("TEAM%d.ROS", yr))
    if (length(ros_files) && !file.exists(team_ros)) {
      ok <- system(sprintf("cat %s > %s",
                           paste(shQuote(ros_files), collapse = " "),
                           shQuote(team_ros)))
      if (ok != 0) message("  WARN: could not build TEAM", yr, ".ROS")
    }
    message("retrosheet-github: parsing ", yr, " (", length(ev_files),
            " event files)...")
    rel <- basename(ev_files)
    pieces <- list(
      plays  = sprintf("%s/cwevent -q -y %d -n -f 0-96 -x 0-62 %s", tools_dir, yr,
                       paste(sprintf("%s/%s", ydir, rel), collapse = " ")),
      games  = sprintf("%s/cwgame -q -y %d -n -f 0-83 %s", tools_dir, yr,
                       paste(sprintf("%s/%s", ydir, rel), collapse = " ")),
      pgames = sprintf("%s/cwdaily -q -y %d -n -f 0-79 %s", tools_dir, yr,
                       paste(sprintf("%s/%s", ydir, rel), collapse = " ")),
      subs   = sprintf("%s/cwsub -q -y %d -n %s", tools_dir, yr,
                       paste(sprintf("%s/%s", ydir, rel), collapse = " ")))
    for (nm in names(pieces)) {
      p <- lake_raw(lake, "retrosheet",
                    sprintf(c(plays = "plays-%d.parquet",
                              games = "games-%d.parquet",
                              pgames = "player-games-%d.parquet",
                              subs = "subs-%d.parquet")[nm], yr))
      tryCatch({
        dt <- read_tool_csv(pieces[[nm]], ydir)
        if (!is.null(dt) && nrow(dt) > 0) write_df(as.data.frame(dt), p)
      }, error = function(e) message("  FAILED ", nm, " ", yr, " - ",
                                     substr(conditionMessage(e), 1, 90)))
    }
    if (length(ros_files)) {
      r <- data.table::rbindlist(
        lapply(ros_files, data.table::fread, sep = ",", header = FALSE,
               colClasses = "character"), use.names = FALSE)
      data.table::setnames(r, c("player_id", "last", "first", "bats",
                                "throws", "team_id", "position"))
      write_df(as.data.frame(r),
               lake_raw(lake, "retrosheet", sprintf("rosters-%d.parquet", yr)))
    }
    if (file.exists(team_ros)) file.remove(team_ros)  # keep worktree clean
  }

  # one-time reference tables (player bios, teams, parks, ejections...)
  refdir <- file.path(repo, "reference")
  if (dir.exists(refdir)) {
    for (f in list.files(refdir, full.names = TRUE)) {
      nm <- sub("\\.csv$", "", basename(f))
      p <- lake_raw(lake, "retrosheet", "reference", sprintf("%s.parquet", nm))
      if (file.exists(p)) next
      tryCatch({
        dt <- data.table::fread(f, colClasses = "character")
        write_df(as.data.frame(dt), p)
      }, error = function(e) message("  FAILED reference/", nm, " - ",
                                     substr(conditionMessage(e), 1, 80)))
    }
  }
  message("retrosheet-github: done (", from, "-", to, ")")
  invisible(TRUE)
}
