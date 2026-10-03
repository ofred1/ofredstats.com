# Hoops Lab: average of per-game points / max(turnovers, 1).
# Save this file in your RStudio project folder.
# Install packages and configure your cbbdr API key separately, once.
# Existing cache files from our earlier work are reused.

SEASON <- 2026L                 # 2025-26; change to 2027 for 2026-27
MODE <- "cached"                # "cached", "daily", or "rebuild"
LOOKBACK_DAYS <- 7L

update_rankings <- function(season, mode = "cached", lookback_days = 7L) {
  stopifnot(mode %in% c("cached", "daily", "rebuild"),
            length(season) == 1L, is.finite(season),
            lookback_days >= 1L)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Install jsonlite once before running this script.")
  }
  if (mode != "cached" && !requireNamespace("cbbdr", quietly = TRUE)) {
    stop("Install cbbdr and configure your API key before downloading.")
  }
  dir.create("data", showWarnings = FALSE)
  games_path <- sprintf("data/all_games_%s.rds", season)
  teams_path <- sprintf("data/team_stats_%s.rds", season)
  state_path <- sprintf("data/update_state_%s.rds", season)

  get_number <- function(x, ...) {
    for (name in list(...)) {
      if (!is.list(x)) return(NA_real_)
      x <- x[[name]]
      if (is.null(x)) return(NA_real_)
    }
    if (length(x) != 1L) return(NA_real_)
    suppressWarnings(as.numeric(x))
  }
  check_columns <- function(x, required, label) {
    if (!is.data.frame(x) || !all(required %in% names(x))) {
      stop(label, " has an unexpected structure; existing exports are unchanged.")
    }
  }
  # Remove locally computed columns before combining old and new API records.
  raw_columns <- function(x) {
    x[, setdiff(names(x), c("points", "turnovers", "game_rating")), drop = FALSE]
  }
  combine_records <- function(old, new) {
    if (is.null(old) || !nrow(old)) return(new)
    if (is.null(new) || !nrow(new)) return(old)
    if (!setequal(names(old), names(new))) {
      stop("The box-score schema changed. Review it before rebuilding the cache.")
    }
    rbind(old, new[, names(old), drop = FALSE])
  }
  replace_file <- function(temp, destination) {
    # Same-directory rename is atomic on supported systems. A copy fallback
    # accommodates systems where rename cannot replace an existing file.
    if (!file.rename(temp, destination)) {
      if (!file.copy(temp, destination, overwrite = TRUE)) stop("Could not save ", destination)
      unlink(temp)
    }
  }
  save_rds <- function(x, path) {
    temp <- tempfile(tmpdir = dirname(path))
    saveRDS(x, temp)
    replace_file(temp, path)
  }

  all_games <- if (file.exists(games_path)) raw_columns(readRDS(games_path)) else NULL
  team_stats <- if (file.exists(teams_path)) readRDS(teams_path) else NULL
  state <- if (file.exists(state_path)) readRDS(state_path) else NULL
  now <- Sys.time()
  finish <- min(as.Date(now, tz = "UTC") + 1L, as.Date(sprintf("%s-06-01", season)))
  season_start <- as.Date(sprintf("%s-10-01", season - 1L))

  if (mode == "cached") {
    if (is.null(all_games) || is.null(team_stats)) {
      stop("Cached mode needs both saved RDS files. Use MODE = 'daily' for the first download.")
    }
  } else {
    if (finish <= season_start) stop("The selected season has not started; no export was replaced.")
    team_stats <- cbbdr::get_team_season_stats(season = season)
    # First run/rebuild downloads the full season to date. Later daily runs
    # overlap seven days and catch up from the last successful update.
    if (mode == "rebuild" || is.null(all_games)) {
      all_games <- NULL
      start <- season_start
    } else {
      last_check <- if (!is.null(state$checked_through)) as.Date(state$checked_through) else finish - 1L
      start <- max(season_start, min(finish - 1L, last_check) - lookback_days)
    }
    boundaries <- sort(unique(c(seq(start, finish, by = "7 days"), finish)))
    for (i in seq_len(length(boundaries) - 1L)) {
      from <- paste0(boundaries[i], "T00:00:00Z")
      to <- paste0(boundaries[i + 1L], "T00:00:00Z")
      message("Downloading ", boundaries[i], " through ", boundaries[i + 1L])
      new <- cbbdr::get_game_teams(season = season,
                                 start_date_range = from, end_date_range = to)
      schedule <- cbbdr::get_games(season = season,
                                  start_date_range = from, end_date_range = to)
      # Never include live games in the rating.
      if (nrow(schedule)) {
        check_columns(schedule, c("id", "status"), "Game schedule")
        if (length(unique(schedule$id)) >= 3000L) stop("Date batch reached the API limit; use smaller batches.")
        final_ids <- schedule$id[!is.na(schedule$status) & schedule$status == "final"]
      } else final_ids <- integer()
      if (nrow(new)) {
        check_columns(new, c("game_id", "team_id", "team_stats"), "Game box scores")
        new <- raw_columns(new[new$game_id %in% final_ids, , drop = FALSE])
        all_games <- combine_records(all_games, new)
      }
    }
  }

  if (is.null(all_games) || !nrow(all_games)) stop("No completed game data; existing exports are unchanged.")
  check_columns(all_games, c("game_id", "team_id", "team_stats"), "Saved games")
  check_columns(team_stats, c("team_id", "team", "conference", "games", "wins", "losses"), "Team statistics")
  if (anyNA(all_games$game_id) || anyNA(all_games$team_id)) stop("Missing game/team IDs.")
  if (anyDuplicated(team_stats$team_id)) stop("Duplicate team IDs in season statistics.")
  # Keep the NEWEST copy of every team/game record.
  all_games <- all_games[!duplicated(all_games[c("game_id", "team_id")], fromLast = TRUE), , drop = FALSE]
  raw_games <- all_games
  all_games$points <- vapply(all_games$team_stats, get_number, numeric(1), "points", "total")
  all_games$turnovers <- vapply(all_games$team_stats, get_number, numeric(1), "turnovers", "total")

  # Provisional D1 membership filter; verify membership before public launch.
  teams <- team_stats[!is.na(team_stats$conference) & nzchar(trimws(team_stats$conference)),
                      c("team_id", "team", "conference", "games", "wins", "losses"), drop = FALSE]
  usable <- with(all_games, team_id %in% teams$team_id &
                   is.finite(points) & points > 0 & is.finite(turnovers) & turnovers >= 0)
  rating_games <- all_games[usable, , drop = FALSE]
  if (!nrow(rating_games)) stop("No usable box scores; existing exports are unchanged.")
  rating_games$game_rating <- with(rating_games, points / pmax(turnovers, 1))
  averages <- aggregate(cbind(points, turnovers, game_rating) ~ team_id, rating_games, mean)
  names(averages) <- c("team_id", "ppg", "turnovers_per_game", "rating")
  counts <- aggregate(game_rating ~ team_id, rating_games, length)
  names(counts) <- c("team_id", "games_rated")
  rankings <- merge(merge(teams, averages, by = "team_id", all.x = TRUE), counts,
                    by = "team_id", all.x = TRUE)
  rankings$games_rated[is.na(rankings$games_rated)] <- 0L
  rankings$rank <- rank(-rankings$rating, ties.method = "min", na.last = "keep")
  rankings <- rankings[order(rankings$rank, rankings$team, na.last = TRUE),
                       c("rank", "team_id", "team", "conference", "games", "games_rated",
                         "wins", "losses", "ppg", "turnovers_per_game", "rating")]
  rownames(rankings) <- NULL
  if (any(rankings$games_rated != rankings$games)) {
    warning("Some games_rated counts differ from season games. Check coverage before publication.")
  }

  # Export time is separate from source refresh time: cached mode does not
  # pretend to have downloaded fresh data. Old caches have unknown freshness.
  source_updated <- if (mode == "cached") state$source_updated_at else
    format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  payload <- list(updated_at = if (is.null(source_updated)) NULL else source_updated,
                  exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                  season = season, formula = "mean(points / max(turnovers, 1))",
                  rankings = rankings)
  temp_json <- tempfile(tmpdir = ".", fileext = ".json")
  jsonlite::write_json(payload, temp_json, dataframe = "rows", auto_unbox = TRUE,
                       pretty = TRUE, na = "null", digits = NA)
  exported <- jsonlite::read_json(temp_json, simplifyVector = TRUE)
  stopifnot(nrow(exported$rankings) == nrow(rankings), !anyNA(exported$rankings$team_id))

  # Downloads/calculations/validation must succeed before replacing outputs.
  if (mode != "cached") {
    save_rds(raw_games, games_path)
    save_rds(team_stats, teams_path)
    save_rds(list(checked_through = as.character(finish - 1L), source_updated_at = source_updated), state_path)
  }
  replace_file(temp_json, "rankings.json")
  message("Exported ", nrow(rankings), " teams to rankings.json (", mode, " mode).")
  rankings
}

rankings <- update_rankings(SEASON, MODE, LOOKBACK_DAYS)
print(head(rankings, 25))
