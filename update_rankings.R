# OFred Stats PPT: points per game / turnovers per game.
# Save this file in your RStudio project folder.
# Install packages and configure your cbbdr API key separately, once.
# Existing cache files from our earlier work are reused.

SEASON <- 2027L                 # 2026-27; 2026 is the previous season
PUBLISH_ACTIVE <- TRUE         # TRUE explicitly updates the website rankings.json
MODE <- "daily"                # "cached", "daily", or "rebuild"
LOOKBACK_DAYS <- 7L

update_rankings <- function(season, mode = "cached", lookback_days = 7L, publish_active = FALSE) {
  stopifnot(mode %in% c("cached", "daily", "rebuild"),
            length(season) == 1L, is.finite(season), season == as.integer(season),
            is.logical(publish_active), length(publish_active) == 1L, !is.na(publish_active),
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

  export_path <- sprintf("rankings_%s.json", season)
  status_path <- sprintf("season_status_%s.json", season)

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

  write_json_safe <- function(payload, path) {
    temp <- tempfile(tmpdir = dirname(path), fileext = ".json")
    on.exit(unlink(temp), add = TRUE)
    jsonlite::write_json(payload, temp, dataframe = "rows", auto_unbox = TRUE,
                         pretty = TRUE, na = "null", digits = NA)
    replace_file(temp, path)
  }
  pending <- function(status, detail) {
    # Status files are not rankings files and never contain a rankings array.
    write_json_safe(list(season = season, status = status, message = detail,
                         checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                    status_path)
    message(detail, " No rankings export was replaced. Status: ", status_path)
    invisible(NULL)
  }
  # Preserve the current website snapshot under its actual season before any
  # new-season download. Never infer the season from today's date.
  if (file.exists("rankings.json")) {
    existing <- jsonlite::read_json("rankings.json", simplifyVector = TRUE)
    if (length(existing$season) != 1L || !is.numeric(existing$season) ||
        !is.finite(existing$season) || existing$season != as.integer(existing$season) ||
        !is.data.frame(existing$rankings) || !nrow(existing$rankings) ||
        (!any(is.finite(existing$rankings$rating)) &&
         !any(is.finite(existing$rankings$overall_score)))) {
      stop("Cannot safely archive rankings.json: inspect its season and rankings first.")
    }
    archive <- sprintf("rankings_%s.json", existing$season)
    if (!file.exists(archive) && !file.copy("rankings.json", archive)) {
      stop("Could not preserve the previous season; no new export was written.")
    }
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
    if (finish <= season_start) {
      return(pending("not_started", "The selected season is outside the download window."))
    }
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
      if (!is.null(schedule) && nrow(schedule)) {
        check_columns(schedule, c("id", "status"), "Game schedule")
        if (length(unique(schedule$id)) >= 3000L) stop("Date batch reached the API limit; use smaller batches.")
        final_ids <- schedule$id[!is.na(schedule$status) & schedule$status == "final"]
      } else final_ids <- integer()
      if (!is.null(new) && nrow(new)) {
        check_columns(new, c("game_id", "team_id", "team_stats"), "Game box scores")
        new <- raw_columns(new[new$game_id %in% final_ids, , drop = FALSE])
        all_games <- combine_records(all_games, new)
      }
    }
  }

  if (is.null(all_games) || !nrow(all_games)) {
    return(pending("no_completed_games", "No completed game data is available for this season."))
  }
  check_columns(all_games, c("game_id", "team_id", "team_stats"), "Saved games")
  check_columns(team_stats, c("team_id", "team", "conference", "games", "wins", "losses"), "Team statistics")
  for (x in list(all_games, team_stats)) {
    if ("season" %in% names(x) && any(is.na(x$season) | x$season != season)) {
      stop("Season mismatch in input data; existing rankings are unchanged.")
    }
  }
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
  if (!nrow(rating_games)) {
    return(pending("no_usable_box_scores", "Completed games exist, but none have usable D1 box scores."))
  }
  averages <- aggregate(cbind(points, turnovers) ~ team_id, rating_games, mean)
  names(averages) <- c("team_id", "ppg", "turnovers_per_game")
  averages$rating <- with(averages, ifelse(turnovers_per_game > 0,
                                         ppg / turnovers_per_game, NA_real_))
  if (!any(is.finite(averages$rating))) stop("No finite PPT values; exports unchanged.")
  counts <- aggregate(points ~ team_id, rating_games, length)
  names(counts) <- c("team_id", "games_rated")
  rankings <- merge(merge(teams, averages, by = "team_id", all.x = TRUE), counts,
                    by = "team_id", all.x = TRUE)
  rankings$games_rated[is.na(rankings$games_rated)] <- 0L
  # Overall rankings are independent of PPT. Use the frozen preseason snapshot.
  if (!requireNamespace("gh", quietly = TRUE) ||
      !requireNamespace("base64enc", quietly = TRUE)) {
    stop("Install gh and base64enc to load the preseason snapshot.")
  }
  preseason_path <- sprintf("data/preseason_%s.json", season)
  if (!file.exists(preseason_path)) {
    snapshot_file <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
      owner = "ofred1", repo = "ofredstats.com",
      path = sprintf("preseason_%s.json", season))
    snapshot_text <- rawToChar(base64enc::base64decode(
      gsub("\\s", "", snapshot_file$content)))
    snapshot <- jsonlite::fromJSON(snapshot_text)
    if (snapshot$season != season || !nrow(snapshot$rankings)) {
      stop("Invalid preseason snapshot; rankings unchanged.")
    }
    write_json_safe(snapshot, preseason_path)
  }
  snapshot <- jsonlite::read_json(preseason_path, simplifyVector = TRUE)
  if (snapshot$season != season) stop("Preseason season mismatch.")
  base <- snapshot$rankings
  team_key <- function(x) gsub("[^a-z0-9]", "",
    tolower(iconv(x, to = "ASCII//TRANSLIT")))
  match_id <- match(team_key(rankings$team), team_key(base$team))
  if (anyNA(match_id)) {
    stop("Teams missing from preseason snapshot: ",
         paste(rankings$team[is.na(match_id)], collapse = ", "))
  }
  for (column in c("kenpom_preseason_rank", "evanmiya_roster_rank",
                   "preseason_average_rank", "preseason_score")) {
    rankings[[column]] <- base[[column]][match_id]
  }
  # Completed season games drive the decay, including games missing PPT data.
  rankings$preseason_weight <- pmax(0, (12 - rankings$games) / 12)
  rankings$inseason_score <- NA_real_
  scores_path <- sprintf("data/inseason_scores_%s.csv", season)
  played <- rankings$games > 0
  if (any(played)) {
    if (!file.exists(scores_path)) {
      stop("Overall in-season model not configured. No rankings published. ",
           "PPT will NOT be used as the overall rating. Define the in-season ",
           "model and supply ", scores_path, " with team and inseason_score (0-100).")
    }
    scores <- read.csv(scores_path, stringsAsFactors = FALSE)
    check_columns(scores, c("team", "inseason_score"), "In-season scores")
    if (anyDuplicated(team_key(scores$team)) || !is.numeric(scores$inseason_score) ||
        any(!is.finite(scores$inseason_score) | scores$inseason_score < 0 |
            scores$inseason_score > 100)) stop("Invalid in-season scores.")
    rankings$inseason_score <- scores$inseason_score[
      match(team_key(rankings$team), team_key(scores$team))]
    if (any(!is.finite(rankings$inseason_score[played]))) {
      stop("Missing in-season scores for played teams; no rankings published.")
    }
  }
  rankings$overall_score <- rankings$preseason_score
  rankings$overall_score[played] <- with(rankings[played, ],
    preseason_weight * preseason_score + (1 - preseason_weight) * inseason_score)
  rankings$rank <- rank(-rankings$overall_score, ties.method = "min", na.last = "keep")
  rankings <- rankings[order(rankings$rank, rankings$team), ]
  rownames(rankings) <- NULL
  if (any(rankings$games_rated != rankings$games)) {
    warning("Some games_rated counts differ from season games. Check coverage before publication.")
  }

  # Export time is separate from source refresh time: cached mode does not
  # pretend to have downloaded fresh data. Old caches have unknown freshness.
  source_updated <- if (mode == "cached") state$source_updated_at else
    format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  payload <- list(status = "ready", metric = "OFred overall composite", updated_at = if (is.null(source_updated)) NULL else source_updated,
                  exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                  season = season, formula = "preseason_weight * preseason_score + (1 - preseason_weight) * inseason_score",
                  ppt_formula = "ppg / turnovers_per_game",
                  preseason_weight_formula = "max(0, (12 - games) / 12)",
                  rankings = rankings)
  temp_json <- tempfile(tmpdir = ".", fileext = ".json")
  on.exit(unlink(temp_json), add = TRUE)
  jsonlite::write_json(payload, temp_json, dataframe = "rows", auto_unbox = TRUE,
                       pretty = TRUE, na = "null", digits = NA)
  exported <- jsonlite::read_json(temp_json, simplifyVector = TRUE)
  stopifnot(nrow(exported$rankings) == nrow(rankings), !anyNA(exported$rankings$team_id),
            any(is.finite(exported$rankings$overall_score)), exported$season == season)

  # Downloads/calculations/validation must succeed before replacing outputs.
  if (mode != "cached") {
    save_rds(raw_games, games_path)
    save_rds(team_stats, teams_path)
    save_rds(list(checked_through = as.character(finish - 1L), source_updated_at = source_updated), state_path)
  }
  replace_file(temp_json, export_path)
  write_json_safe(list(season = season, status = "ready", export = export_path,
                       exported_at = payload$exported_at), status_path)
  if (publish_active) {
    # Publish only the already validated, nonempty season export.
    temp_active <- tempfile(tmpdir = ".", fileext = ".json")
    on.exit(unlink(temp_active), add = TRUE)
    if (!file.copy(export_path, temp_active)) stop("Could not stage the active export.")
    replace_file(temp_active, "rankings.json")
    message("Active website data updated to season ", season, ".")
  }
  message("Exported ", nrow(rankings), " teams to ", export_path, " (", mode, " mode).")
  rankings
}


upload_rankings <- function(path = "rankings.json", expected_season) {
  for (package in c("gh", "base64enc", "jsonlite")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Install ", package, " first.")
  }
  local <- jsonlite::read_json(path, simplifyVector = TRUE)
  if (!identical(as.integer(local$season), as.integer(expected_season)) ||
      !is.data.frame(local$rankings) || !nrow(local$rankings) ||
      !any(is.finite(local$rankings$overall_score)) ||
      !identical(local$formula, "preseason_weight * preseason_score + (1 - preseason_weight) * inseason_score")) {
    stop("Upload blocked: wrong season, formula, or empty rankings.")
  }
  owner <- "ofred1"
  repo <- "ofredstats.com"
  repository <- gh::gh("GET /repos/{owner}/{repo}", owner = owner, repo = repo)
  branch <- repository$default_branch
  get_file <- function(file) {
    gh::gh("GET /repos/{owner}/{repo}/contents/{path}",
           owner = owner, repo = repo, path = file, ref = branch)
  }
  decode <- function(file) {
    rawToChar(base64enc::base64decode(gsub("\\s", "", file$content)))
  }
  put_file <- function(file, content, sha) {
    gh::gh("PUT /repos/{owner}/{repo}/contents/{path}",
           owner = owner, repo = repo, path = file, branch = branch, sha = sha,
           message = paste("Update PPT rankings for season", expected_season),
           content = base64enc::base64encode(charToRaw(enc2utf8(content))))
  }
  remote_json <- get_file("rankings.json")
  remote_page <- get_file("index.html")
  html <- decode(remote_page)
  opening <- '<script id="rankings-data" type="application/json">'
  begin <- regexpr(opening, html, fixed = TRUE)[1]
  if (begin < 1L) stop("Website is missing the embedded-data slot; nothing uploaded.")
  data_start <- begin + nchar(opening)
  remainder <- substring(html, data_start)
  closing <- regexpr("</script>", remainder, fixed = TRUE)[1]
  if (closing < 1L) stop("Website data slot is invalid; nothing uploaded.")
  data_end <- data_start + closing - 2L
  # Compare data, not timestamps; repair either file independently on retries.
  same_data <- function(other) {
    isTRUE(all.equal(local$season, other$season)) &&
      isTRUE(all.equal(local$rankings, other$rankings)) &&
      identical(local$formula, other$formula)
  }
  previous_embedded <- jsonlite::fromJSON(substr(html, data_start, data_end))
  remote_data <- jsonlite::fromJSON(decode(remote_json))
  # Escape '<' so a data string cannot close its HTML script container.
  json <- as.character(jsonlite::toJSON(local, dataframe = "rows",
                        auto_unbox = TRUE, na = "null", null = "null", digits = NA))
  safe_json <- paste(strsplit(json, "<", fixed = TRUE)[[1]], collapse = "\\u003c")
  updated_html <- paste0(substr(html, 1L, data_start - 1L),
                         safe_json, substring(html, data_end + 1L))
  if (!same_data(remote_data)) {
    put_file("rankings.json", json, remote_json$sha)
    message("Updated rankings.json on GitHub.")
  }
  if (!same_data(previous_embedded)) {
    put_file("index.html", updated_html, remote_page$sha)
    message("Updated the website's embedded rankings. GitHub Pages will deploy it.")
  } else {
    message("Website rankings are already current.")
  }
}

# Run once. No-data runs return NULL and do not upload anything.
rankings <- update_rankings(SEASON, MODE, LOOKBACK_DAYS, PUBLISH_ACTIVE)
if (!is.null(rankings)) {
  print(head(rankings, 25))
  if (isTRUE(PUBLISH_ACTIVE)) upload_rankings("rankings.json", SEASON)
}

