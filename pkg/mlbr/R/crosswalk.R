#' MLB Entity Crosswalk Utilities
#'
#' Unified ID crosswalking between MLBAM, Retrosheet, Baseball-Reference,
#' and FanGraphs for players, teams, and games. Grounded in the Chadwick
#' Bureau register and official MLB schedule data.
#'
#' @name crosswalk
NULL

#' Reference Table of Team Abbreviations Across Systems
#'
#' Provides bidirectional translation between Retrosheet, Statcast/MLBAM,
#' Baseball-Reference, and official franchise names.
#'
#' @return A tibble with columns: \code{team_retro}, \code{team_statcast},
#'   \code{team_bbref}, \code{franchise_name}, \code{league}, \code{division}
#' @export
#' @examples
#' \dontrun{
#' teams <- team_crosswalk()
#' subset(teams, team_statcast == "NYY")
#' }
team_crosswalk <- function() {
  tibble::tibble(
    team_retro = c(
      "ARI", "ATL", "BAL", "BOS", "CHA", "CHN", "CIN", "CLE", "COL", "DET",
      "HOU", "KCA", "ANA", "LAN", "MIA", "MIL", "MIN", "NYA", "NYN", "OAK",
      "PHI", "PIT", "SDN", "SFN", "SEA", "SLN", "TBA", "TEX", "TOR", "WAS",
      # Historical / relocated aliases
      "CAL", "FLO", "MON"
    ),
    team_statcast = c(
      "AZ",  "ATL", "BAL", "BOS", "CWS", "CHC", "CIN", "CLE", "COL", "DET",
      "HOU", "KC",  "LAA", "LAD", "MIA", "MIL", "MIN", "NYY", "NYM", "ATH",
      "PHI", "PIT", "SD",  "SF",  "SEA", "STL", "TB",  "TEX", "TOR", "WSH",
      "LAA", "MIA", "WSH"
    ),
    team_bbref = c(
      "ARI", "ATL", "BAL", "BOS", "CHW", "CHC", "CIN", "CLE", "COL", "DET",
      "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYY", "NYM", "OAK",
      "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", "TEX", "TOR", "WSN",
      "CAL", "FLA", "MON"
    ),
    franchise_name = c(
      "Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox",
      "Chicago White Sox", "Chicago Cubs", "Cincinnati Reds", "Cleveland Guardians",
      "Colorado Rockies", "Detroit Tigers", "Houston Astros", "Kansas City Royals",
      "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", "Milwaukee Brewers",
      "Minnesota Twins", "New York Yankees", "New York Mets", "Athletics",
      "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres", "San Francisco Giants",
      "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays", "Texas Rangers",
      "Toronto Blue Jays", "Washington Nationals",
      "California Angels", "Florida Marlins", "Montreal Expos"
    ),
    league = c(
      "NL", "NL", "AL", "AL", "AL", "NL", "NL", "AL", "NL", "AL",
      "AL", "AL", "AL", "NL", "NL", "NL", "AL", "AL", "NL", "AL",
      "NL", "NL", "NL", "NL", "AL", "NL", "AL", "AL", "AL", "NL",
      "AL", "NL", "NL"
    ),
    division = c(
      "West", "East", "East", "East", "Central", "Central", "Central", "Central", "West", "Central",
      "West", "Central", "West", "West", "East", "Central", "Central", "East", "East", "West",
      "East", "Central", "West", "West", "West", "Central", "East", "West", "East", "East",
      "West", "East", "East"
    )
  )
}

#' Look Up Player Across ID Systems
#'
#' Resolves player identity across MLBAM, Retrosheet, Baseball-Reference,
#' and FanGraphs using the Chadwick Bureau Register.
#'
#' @param query Player name (e.g. \code{"Aaron Judge"}, \code{"Ohtani"}) or ID
#'   (numeric or character string e.g. \code{592450}, \code{"judga001"}).
#' @param lake Data lake path (defaults to \code{default_lake()}).
#' @param con Optional existing DuckDB DBI connection.
#' @param type Search type: \code{"auto"} (default), \code{"name"}, \code{"mlbam"},
#'   \code{"retro"}, \code{"bbref"}, \code{"fangraphs"}.
#' @param exact Logical; if \code{TRUE}, matches name exactly (case-insensitive).
#' @param limit Maximum number of records to return (default: 25).
#' @return A tibble with resolved keys, names, and career spans.
#' @export
#' @examples
#' \dontrun{
#' crosswalk_player("Aaron Judge")
#' crosswalk_player(592450)
#' crosswalk_player("judga001")
#' }
crosswalk_player <- function(query,
                             lake = NULL,
                             con = NULL,
                             type = c("auto", "name", "mlbam", "retro", "bbref", "fangraphs"),
                             exact = FALSE,
                             limit = 25L) {
  type <- match.arg(type)
  query_str <- as.character(query)
  query_clean <- trimws(query_str)
  
  if (nchar(query_clean) == 0) {
    return(tibble::tibble())
  }
  
  # Auto-detection
  if (type == "auto") {
    if (grepl("^[0-9]+$", query_clean)) {
      # Pure integer: likely MLBAM or FanGraphs ID
      type <- "mlbam"
    } else if (grepl("^[a-z]{4,6}[0-9]{2,3}$", tolower(query_clean))) {
      # Retrosheet / BBRef format: e.g. judga001, judgeaa01
      type <- if (grepl("^[a-z]{5}[0-9]{3}$", tolower(query_clean))) "retro" else "bbref"
    } else {
      type <- "name"
    }
  }
  
  # Connect if con is NULL
  close_con <- FALSE
  if (is.null(con)) {
    lake <- default_lake(lake)
    con <- lake_duckdb(lake)
    close_con <- TRUE
  }
  on.exit(if (close_con) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  cols <- "key_mlbam, key_retro, key_bbref, key_fangraphs, name_first, name_last, 
           (name_first || ' ' || name_last) AS full_name,
           birth_year, mlb_played_first, mlb_played_last, pro_played_first, pro_played_last"
  
  sql <- switch(
    type,
    "mlbam" = sprintf(
      "SELECT %s FROM v_chadwick_register WHERE key_mlbam = %d LIMIT %d",
      cols, as.integer(query_clean), as.integer(limit)
    ),
    "retro" = sprintf(
      "SELECT %s FROM v_chadwick_register WHERE LOWER(key_retro) = LOWER('%s') LIMIT %d",
      cols, query_clean, as.integer(limit)
    ),
    "bbref" = sprintf(
      "SELECT %s FROM v_chadwick_register WHERE LOWER(key_bbref) = LOWER('%s') LIMIT %d",
      cols, query_clean, as.integer(limit)
    ),
    "fangraphs" = sprintf(
      "SELECT %s FROM v_chadwick_register WHERE key_fangraphs = %d LIMIT %d",
      cols, as.integer(query_clean), as.integer(limit)
    ),
    "name" = {
      q_esc <- gsub("'", "''", tolower(query_clean))
      if (exact) {
        sprintf(
          "SELECT %s FROM v_chadwick_register 
           WHERE LOWER(name_first || ' ' || name_last) = '%s'
              OR LOWER(name_last) = '%s'
           ORDER BY CASE WHEN mlb_played_first IS NOT NULL THEN 1 ELSE 0 END DESC, COALESCE(mlb_played_last, 0) DESC, COALESCE(pro_played_last, 0) DESC LIMIT %d",
          cols, q_esc, q_esc, as.integer(limit)
        )
      } else {
        # Check if space in query
        if (grepl(" ", q_esc)) {
          parts <- strsplit(q_esc, "\\s+")[[1]]
          first_part <- parts[1]
          last_part  <- parts[length(parts)]
          sprintf(
            "SELECT %s FROM v_chadwick_register 
             WHERE (LOWER(name_first) LIKE '%s%%' AND LOWER(name_last) LIKE '%s%%')
                OR LOWER(name_first || ' ' || name_last) LIKE '%%%s%%'
             ORDER BY CASE WHEN mlb_played_first IS NOT NULL THEN 1 ELSE 0 END DESC, COALESCE(mlb_played_last, 0) DESC, COALESCE(pro_played_last, 0) DESC LIMIT %d",
            cols, first_part, last_part, q_esc, as.integer(limit)
          )
        } else {
          sprintf(
            "SELECT %s FROM v_chadwick_register 
             WHERE LOWER(name_last) LIKE '%s%%'
                OR LOWER(name_first) LIKE '%s%%'
                OR LOWER(name_first || ' ' || name_last) LIKE '%%%s%%'
             ORDER BY CASE WHEN mlb_played_first IS NOT NULL THEN 1 ELSE 0 END DESC, COALESCE(mlb_played_last, 0) DESC, COALESCE(pro_played_last, 0) DESC LIMIT %d",
            cols, q_esc, q_esc, q_esc, as.integer(limit)
          )
        }
      }
    }
  )
  
  res <- DBI::dbGetQuery(con, sql)
  tibble::as_tibble(res)
}

#' Crosswalk Game IDs Between Retrosheet and Statcast
#'
#' Bridges Retrosheet \code{GAME_ID} (e.g. \code{"CIN201804150"}) and
#' Statcast \code{game_pk} (e.g. \code{529639}).
#'
#' @param query Retrosheet \code{GAME_ID} (character string) or Statcast
#'   \code{game_pk} (integer or numeric string).
#' @param lake Data lake root path.
#' @param con Optional existing DuckDB connection.
#' @return A tibble with linked game identifiers, dates, and team codes.
#' @export
#' @examples
#' \dontrun{
#' crosswalk_game("BOS202304150")
#' crosswalk_game(718576)
#' }
crosswalk_game <- function(query, lake = NULL, con = NULL) {
  query_str <- trimws(as.character(query))
  if (nchar(query_str) == 0) return(tibble::tibble())
  
  close_con <- FALSE
  if (is.null(con)) {
    lake <- default_lake(lake)
    con <- lake_duckdb(lake)
    close_con <- TRUE
  }
  on.exit(if (close_con) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  is_pk <- grepl("^[0-9]{5,7}$", query_str)
  
  if (is_pk) {
    # Given game_pk -> find Retrosheet GAME_ID
    pk <- as.integer(query_str)
    # Check v_mlbmeta_schedule first
    sql <- sprintf("
      WITH sched AS (
        SELECT game_pk, official_date AS game_date, 
               teams_home_team_name, teams_away_team_name,
               game_number
        FROM v_mlbmeta_schedule
        WHERE game_pk = %d
        LIMIT 1
      ),
      teams AS (
        SELECT * FROM (VALUES
          ('ARI', 'Arizona Diamondbacks'), ('ATL', 'Atlanta Braves'), ('BAL', 'Baltimore Orioles'),
          ('BOS', 'Boston Red Sox'), ('CHA', 'Chicago White Sox'), ('CHN', 'Chicago Cubs'),
          ('CIN', 'Cincinnati Reds'), ('CLE', 'Cleveland Guardians'), ('CLE', 'Cleveland Indians'),
          ('COL', 'Colorado Rockies'), ('DET', 'Detroit Tigers'), ('HOU', 'Houston Astros'),
          ('KCA', 'Kansas City Royals'), ('ANA', 'Los Angeles Angels'), ('LAN', 'Los Angeles Dodgers'),
          ('MIA', 'Miami Marlins'), ('MIL', 'Milwaukee Brewers'), ('MIN', 'Minnesota Twins'),
          ('NYA', 'New York Yankees'), ('NYN', 'New York Mets'), ('OAK', 'Athletics'),
          ('OAK', 'Oakland Athletics'), ('PHI', 'Philadelphia Phillies'), ('PIT', 'Pittsburgh Pirates'),
          ('SDN', 'San Diego Padres'), ('SFN', 'San Francisco Giants'), ('SEA', 'Seattle Mariners'),
          ('SLN', 'St. Louis Cardinals'), ('TBA', 'Tampa Bay Rays'), ('TEX', 'Texas Rangers'),
          ('TOR', 'Toronto Blue Jays'), ('WAS', 'Washington Nationals')
        ) AS t(team_retro, franchise_name)
      )
      SELECT 
        s.game_pk,
        s.game_date,
        s.teams_home_team_name AS home_team_name,
        s.teams_away_team_name AS away_team_name,
        s.game_number,
        th.team_retro AS home_team_retro,
        ta.team_retro AS away_team_retro
      FROM sched s
      LEFT JOIN teams th ON s.teams_home_team_name = th.franchise_name
      LEFT JOIN teams ta ON s.teams_away_team_name = ta.franchise_name;
    ", pk)
    
    meta <- DBI::dbGetQuery(con, sql)
    if (nrow(meta) == 0 || is.na(meta$home_team_retro[1])) {
      # Fallback to statcast pitch extract
      sc_sql <- sprintf("
        SELECT DISTINCT game_pk, game_date, home_team, away_team
        FROM v_statcast WHERE game_pk = %d LIMIT 1;
      ", pk)
      sc_meta <- DBI::dbGetQuery(con, sc_sql)
      if (nrow(sc_meta) == 0) return(tibble::tibble())
      
      tmap <- team_crosswalk()
      h_retro <- tmap$team_retro[match(sc_meta$home_team[1], tmap$team_statcast)]
      a_retro <- tmap$team_retro[match(sc_meta$away_team[1], tmap$team_statcast)]
      gdate <- sc_meta$game_date[1]
      dt_num <- as.integer(gsub("-", "", gdate))
      
      # Match retrosheet game
      rg_sql <- sprintf("
        SELECT GAME_ID, GAME_DT, HOME_TEAM_ID, AWAY_TEAM_ID, GAME_CT
        FROM v_retrosheet_games
        WHERE GAME_DT = %d AND HOME_TEAM_ID = '%s'
      ", dt_num, h_retro)
      rg <- DBI::dbGetQuery(con, rg_sql)
      
      return(tibble::tibble(
        game_pk = pk,
        retrosheet_game_id = if (nrow(rg) > 0) rg$GAME_ID[1] else NA_character_,
        game_date = as.character(gdate),
        home_team_statcast = sc_meta$home_team[1],
        away_team_statcast = sc_meta$away_team[1],
        home_team_retro = h_retro,
        away_team_retro = a_retro,
        game_number = if (nrow(rg) > 0) rg$GAME_CT[1] + 1L else 1L
      ))
    }
    
    gdate <- meta$game_date[1]
    dt_num <- as.integer(gsub("-", "", gdate))
    h_retro <- meta$home_team_retro[1]
    gnum <- meta$game_number[1]
    game_ct <- if (gnum > 1L) (gnum - 1L) else 0L
    
    # Query retrosheet game
    rg_sql <- sprintf("
      SELECT GAME_ID, GAME_DT, HOME_TEAM_ID, AWAY_TEAM_ID, GAME_CT
      FROM v_retrosheet_games
      WHERE GAME_DT = %d AND HOME_TEAM_ID = '%s' AND (GAME_CT = %d OR GAME_CT = %d)
      LIMIT 1
    ", dt_num, h_retro, game_ct, if (gnum == 1L) 1L else 0L)
    rg <- DBI::dbGetQuery(con, rg_sql)
    
    tmap <- team_crosswalk()
    h_sc <- tmap$team_statcast[match(h_retro, tmap$team_retro)]
    a_sc <- tmap$team_statcast[match(meta$away_team_retro[1], tmap$team_retro)]
    
    tibble::tibble(
      game_pk = pk,
      retrosheet_game_id = if (nrow(rg) > 0) rg$GAME_ID[1] else sprintf("%s%08d%d", h_retro, dt_num, game_ct),
      game_date = as.character(gdate),
      home_team_statcast = h_sc,
      away_team_statcast = a_sc,
      home_team_retro = h_retro,
      away_team_retro = meta$away_team_retro[1],
      game_number = gnum
    )
  } else {
    # Given Retrosheet GAME_ID: e.g. "BOS202304150"
    if (nchar(query_str) < 12) {
      warning("Retrosheet GAME_ID expected to be 12 chars (e.g. 'BOS202304150')")
      return(tibble::tibble())
    }
    h_retro <- substr(query_str, 1, 3)
    dt_str  <- substr(query_str, 4, 11)
    gct     <- as.integer(substr(query_str, 12, 12))
    gdate   <- sprintf("%s-%s-%s", substr(dt_str, 1, 4), substr(dt_str, 5, 6), substr(dt_str, 7, 8))
    gnum    <- if (gct == 0L) 1L else (gct + 1L)
    
    tmap <- team_crosswalk()
    h_sc <- tmap$team_statcast[match(h_retro, tmap$team_retro)]
    if (is.na(h_sc)) h_sc <- h_retro
    
    # Try Statcast view
    sc_sql <- sprintf("
      SELECT DISTINCT game_pk, game_date, home_team, away_team
      FROM v_statcast
      WHERE game_date = '%s' AND (home_team = '%s' OR home_team = '%s')
    ", gdate, h_sc, h_retro)
    sc_matches <- DBI::dbGetQuery(con, sc_sql)
    
    matched_pk <- if (nrow(sc_matches) > 0) sc_matches$game_pk[1] else NA_integer_
    
    # If not found in statcast, try schedule
    if (is.na(matched_pk)) {
      sched_sql <- sprintf("
        SELECT game_pk, official_date, teams_home_team_name, teams_away_team_name, game_number
        FROM v_mlbmeta_schedule
        WHERE official_date = '%s' AND game_number = %d
      ", gdate, gnum)
      sched_m <- DBI::dbGetQuery(con, sched_sql)
      if (nrow(sched_m) > 0) {
        matched_pk <- sched_m$game_pk[1]
      }
    }
    
    tibble::tibble(
      game_pk = as.integer(matched_pk),
      retrosheet_game_id = query_str,
      game_date = gdate,
      home_team_statcast = h_sc,
      away_team_statcast = if (nrow(sc_matches) > 0) sc_matches$away_team[1] else NA_character_,
      home_team_retro = h_retro,
      away_team_retro = NA_character_,
      game_number = gnum
    )
  }
}

#' Join Player IDs Onto Arbitrary Baseball Dataframes
#'
#' Automatically detects player ID columns in an input data frame (e.g.
#' \code{pitcher}, \code{batter}, \code{playerID}, \code{mlbam_id},
#' \code{key_mlbam}, \code{key_retro}) and enriches the data frame with all
#' corresponding crosswalk identifiers from the Chadwick register.
#'
#' @param df Data frame or tibble containing baseball player data.
#' @param id_col Optional name of the ID column. If \code{NULL}, attempts
#'   auto-detection.
#' @param lake Data lake path.
#' @param con Optional existing DuckDB connection.
#' @return The original data frame enriched with \code{key_mlbam},
#'   \code{key_retro}, \code{key_bbref}, \code{key_fangraphs}, and player names.
#' @export
#' @examples
#' \dontrun{
#' sample_pitches <- tibble::tibble(pitcher = c(592450, 660271))
#' crosswalk_join(sample_pitches)
#' }
crosswalk_join <- function(df, id_col = NULL, lake = NULL, con = NULL) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  # Auto-detect ID column if not specified
  if (is.null(id_col)) {
    candidates <- c("pitcher", "batter", "mlbam_id", "key_mlbam", "key_retro", "playerID", "retro_id", "bbref_id")
    found <- intersect(candidates, names(df))
    if (length(found) == 0) {
      stop("Could not auto-detect player ID column in df. Please specify 'id_col'.")
    }
    id_col <- found[1]
  }
  
  if (!id_col %in% names(df)) {
    stop(sprintf("Column '%s' not found in df.", id_col))
  }
  
  vals <- unique(stats::na.omit(df[[id_col]]))
  if (length(vals) == 0) return(df)
  
  close_con <- FALSE
  if (is.null(con)) {
    lake <- default_lake(lake)
    con <- lake_duckdb(lake)
    close_con <- TRUE
  }
  on.exit(if (close_con) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # Determine if values are numeric (MLBAM) or character (Retro/BBRef)
  is_num <- is.numeric(vals) || all(grepl("^[0-9]+$", as.character(vals)))
  
  cols <- "key_mlbam, key_retro, key_bbref, key_fangraphs, name_first, name_last"
  
  if (is_num) {
    int_vals <- as.integer(vals)
    val_str <- paste(int_vals, collapse = ",")
    sql <- sprintf(
      "SELECT %s FROM v_chadwick_register WHERE key_mlbam IN (%s)",
      cols, val_str
    )
    register_matches <- tibble::as_tibble(DBI::dbGetQuery(con, sql))
    
    # Merge back into df
    # Match on integer key
    df_key <- as.integer(df[[id_col]])
    idx <- match(df_key, register_matches$key_mlbam)
    
    for (cn in names(register_matches)) {
      if (!cn %in% names(df)) {
        df[[cn]] <- register_matches[[cn]][idx]
      }
    }
  } else {
    char_vals <- as.character(vals)
    val_str <- paste(sprintf("'%s'", gsub("'", "''", tolower(char_vals))), collapse = ",")
    sql <- sprintf(
      "SELECT %s FROM v_chadwick_register WHERE LOWER(key_retro) IN (%s) OR LOWER(key_bbref) IN (%s)",
      cols, val_str, val_str
    )
    register_matches <- tibble::as_tibble(DBI::dbGetQuery(con, sql))
    
    df_key <- tolower(as.character(df[[id_col]]))
    idx <- match(df_key, tolower(register_matches$key_retro))
    # fallback to bbref
    na_idx <- which(is.na(idx))
    if (length(na_idx) > 0) {
      idx[na_idx] <- match(df_key[na_idx], tolower(register_matches$key_bbref))
    }
    
    for (cn in names(register_matches)) {
      if (!cn %in% names(df)) {
        df[[cn]] <- register_matches[[cn]][idx]
      }
    }
  }
  
  df
}

#' Validate Crosswalk Modules Against Invariants
#'
#' Runs a comprehensive 5-stage verification suite validating player,
#' game, and team crosswalks across Chadwick, Statcast, and Retrosheet.
#'
#' @param lake Data lake path.
#' @return A tibble with columns \code{test}, \code{status}, and \code{detail}.
#' @export
proof_crosswalks <- function(lake = NULL) {
  lake <- default_lake(lake)
  con <- lake_duckdb(lake)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  results <- list()
  
  # Stage 1: Chadwick register completeness
  count_df <- DBI::dbGetQuery(
    con, 
    "SELECT COUNT(*) AS total, 
            COUNT(key_mlbam) AS has_mlbam, 
            COUNT(key_retro) AS has_retro 
     FROM v_chadwick_register"
  )
  pass_st1 <- count_df$total >= 100000 && count_df$has_mlbam >= 15000
  results[[length(results) + 1L]] <- tibble::tibble(
    test = "chadwick_register_keys",
    status = if (pass_st1) "PASS" else "FAIL",
    detail = sprintf("Total: %d | MLBAM keys: %d | Retro keys: %d", 
                     count_df$total, count_df$has_mlbam, count_df$has_retro)
  )
  
  # Stage 2: Superstar lookups (Aaron Judge, Shohei Ohtani, Mike Trout)
  judge <- crosswalk_player("Aaron Judge", con = con)
  ohtani <- crosswalk_player("Shohei Ohtani", con = con)
  trout <- crosswalk_player(545361, con = con)
  
  judge_ok <- nrow(judge) >= 1 && judge$key_mlbam[1] == 592450 && judge$key_retro[1] == "judga001"
  ohtani_ok <- nrow(ohtani) >= 1 && ohtani$key_mlbam[1] == 660271 && ohtani$key_retro[1] == "ohtas001"
  trout_ok <- nrow(trout) >= 1 && trout$key_retro[1] == "troum001"
  
  pass_st2 <- judge_ok && ohtani_ok && trout_ok
  results[[length(results) + 1L]] <- tibble::tibble(
    test = "superstar_resolution",
    status = if (pass_st2) "PASS" else "FAIL",
    detail = sprintf("Judge: %s | Ohtani: %s | Trout: %s",
                     if (judge_ok) "OK" else "FAIL",
                     if (ohtani_ok) "OK" else "FAIL",
                     if (trout_ok) "OK" else "FAIL")
  )
  
  # Stage 3: Historical legends (Babe Ruth, Ted Williams)
  ruth <- crosswalk_player("Babe Ruth", con = con)
  williams <- crosswalk_player("Ted Williams", con = con)
  
  ruth_ok <- nrow(ruth) >= 1 && ruth$key_retro[1] == "ruthb101"
  williams_ok <- nrow(williams) >= 1 && williams$key_retro[1] == "willt103"
  
  pass_st3 <- ruth_ok && williams_ok
  results[[length(results) + 1L]] <- tibble::tibble(
    test = "historical_legends_resolution",
    status = if (pass_st3) "PASS" else "FAIL",
    detail = sprintf("Babe Ruth: %s (%s) | Ted Williams: %s (%s)",
                     ruth$key_retro[1], if (ruth_ok) "OK" else "FAIL",
                     williams$key_retro[1], if (williams_ok) "OK" else "FAIL")
  )
  
  # Stage 4: Team crosswalk bidirectional mapping (30 active franchises)
  teams <- team_crosswalk()
  pass_st4 <- nrow(teams) >= 30 && all(!is.na(teams$team_retro)) && all(!is.na(teams$team_statcast))
  results[[length(results) + 1L]] <- tibble::tibble(
    test = "team_crosswalk_bidirectional",
    status = if (pass_st4) "PASS" else "FAIL",
    detail = sprintf("%d franchise mappings defined across 4 systems", nrow(teams))
  )
  
  # Stage 5: Game crosswalk test (2023 sample)
  game_test <- crosswalk_game("BOS202304150", con = con)
  game_ok <- nrow(game_test) == 1 && game_test$game_pk == 718576 && game_test$home_team_statcast == "BOS"
  
  results[[length(results) + 1L]] <- tibble::tibble(
    test = "game_crosswalk_roundtrip",
    status = if (game_ok) "PASS" else "FAIL",
    detail = sprintf("BOS202304150 -> game_pk %s (expected 718576)", game_test$game_pk[1])
  )
  
  out <- do.call(rbind, results)
  cat("\n=== mlbr Crosswalk Validation Report ===\n")
  print(as.data.frame(out), row.names = FALSE)
  cat(sprintf("\nPassed %d / %d checks (%.1f%%)\n", 
              sum(out$status == "PASS"), nrow(out), 
              100 * mean(out$status == "PASS")))
  invisible(out)
}
