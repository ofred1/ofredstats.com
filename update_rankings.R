# OFred Stats PPT: points per game / turnovers per game.
# Save this file in your RStudio project folder.
# Install packages and configure your cbbdr API key separately, once.
# Existing cache files from our earlier work are reused.

SEASON <- 2027L                 # 2026-27; 2026 is the previous season
PUBLISH_ACTIVE <- TRUE         # TRUE explicitly updates the website rankings.json
MODE <- "daily"                # "cached", "daily", or "rebuild"
LOOKBACK_DAYS <- 7L


# Count this script's request attempts without logging credentials or response data.
.api_usage <- new.env(parent = emptyenv())
.api_usage$run_id <- paste(format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), Sys.getpid(), sep = "-")

tracked_api_request <- function(endpoint, request) {
  dir.create("data", showWarnings = FALSE)
  path <- "data/api_request_log.csv"
  entry <- data.frame(run_id = .api_usage$run_id,
    requested_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    endpoint = endpoint, stringsAsFactors = FALSE)
  exists <- file.exists(path)
  write.table(entry, path, sep = ",", row.names = FALSE,
    col.names = !exists, append = exists, quote = TRUE)
  force(request)
}

report_api_requests <- function() {
  path <- "data/api_request_log.csv"
  log <- if (file.exists(path)) read.csv(path, stringsAsFactors = FALSE) else
    data.frame(run_id = character(), requested_at = character(), endpoint = character())
  this_run <- log[log$run_id == .api_usage$run_id, , drop = FALSE]
  month <- format(Sys.time(), "%Y-%m", tz = "UTC")
  monthly <- sum(substr(log$requested_at, 1L, 7L) == month)
  message("API request attempts this run: ", nrow(this_run))
  if (nrow(this_run)) {
    counts <- table(this_run$endpoint)
    for (endpoint in names(counts)) message("  ", endpoint, ": ", counts[[endpoint]])
  }
  message("Logged attempts this calendar month: ", monthly,
    ". This script's log only; earlier requests and other programs are not included.")
  invisible(list(this_run = nrow(this_run), logged_month = monthly))
}

# Optional one-request account check. Never prints or stores your key.
# CFBD/CBBD share their provider's API quota pool.
check_api_allowance <- function() {
  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("keyring", quietly = TRUE)) stop("Install httr2 and keyring first.")
  response <- tracked_api_request("/info (shared quota check)",
    httr2::request("https://api.collegefootballdata.com/info") |>
      httr2::req_headers(Authorization = paste("Bearer", keyring::key_get("CBBD_API_KEY"))) |>
      httr2::req_timeout(30) |>
      httr2::req_perform())
  account <- httr2::resp_body_json(response)
  if (is.null(account) || !all(c("monthlyLimit", "remainingCalls") %in% names(account)))
    stop("Account allowance was not returned. Check your account on the provider website.")
  summary <- account[intersect(c("tierName", "monthlyLimit", "usedCalls",
                                "remainingCalls", "resetAt", "sharedPool"), names(account))]
  print(summary)
  message("This account check used one additional API request.")
  invisible(summary)
}


# Raw season efficiencies: ratio of totals, not an average of game ratings.
# Each side excludes only games missing that side's points or possessions.
calculate_raw_efficiencies <- function(games) {
  output <- data.frame(team_id = unique(games$team_id),
    offensive_efficiency = NA_real_, defensive_efficiency = NA_real_,
    offense_eff_games = 0L, defense_eff_games = 0L)
  number <- function(x, ...) {
    for (key in list(...)) {
      if (!is.list(x) || is.null(x[[key]])) return(NA_real_)
      x <- x[[key]]
    }
    if (length(x) != 1L) return(NA_real_)
    suppressWarnings(as.numeric(x))
  }
  extract <- function(column, ...) {
    if (!column %in% names(games)) return(rep(NA_real_, nrow(games)))
    vapply(games[[column]], number, numeric(1), ...)
  }
  points <- extract("team_stats", "points", "total")
  possessions <- extract("team_stats", "possessions")
  allowed <- extract("opponent_stats", "points", "total")
  opponent_possessions <- extract("opponent_stats", "possessions")
  valid_offense <- is.finite(points) & points >= 0 &
    is.finite(possessions) & possessions > 0
  valid_defense <- is.finite(allowed) & allowed >= 0 &
    is.finite(opponent_possessions) & opponent_possessions > 0
  for (i in seq_len(nrow(output))) {
    team <- games$team_id == output$team_id[i]
    offense <- team & valid_offense
    defense <- team & valid_defense
    output$offense_eff_games[i] <- sum(offense)
    output$defense_eff_games[i] <- sum(defense)
    if (any(offense)) output$offensive_efficiency[i] <-
      100 * sum(points[offense]) / sum(possessions[offense])
    if (any(defense)) output$defensive_efficiency[i] <-
      100 * sum(allowed[defense]) / sum(opponent_possessions[defense])
  }
  output
}

# OFred additive opponent adjustment (not KenPom's proprietary model).
# D1-vs-D1 games only; equal game weights; one national-average pseudo-game
# anchors disconnected schedules. This anchor is not the preseason ranking.
calculate_adjusted_efficiencies <- function(games, team_ids, tolerance = 0.001,
                                            max_iterations = 1000L, prior_games = 1) {
  stopifnot(tolerance > 0, max_iterations >= 1, prior_games > 0)
  ids <- unique(team_ids)
  out <- data.frame(team_id = ids, adjusted_offensive_efficiency = NA_real_,
    adjusted_defensive_efficiency = NA_real_, adjusted_efficiency_margin = NA_real_,
    adjusted_games = 0L)
  diagnostics <- list(status = "no_usable_games", converged = FALSE,
    iterations = 0L, tolerance = tolerance, max_change = NA_real_,
    national_average = NA_real_, prior_games = prior_games)
  finish <- function() list(ratings = out, diagnostics = diagnostics)
  required <- c("team_id", "opponent_id", "team_stats", "opponent_stats")
  if (!all(required %in% names(games))) {
    diagnostics$status <- "missing_columns"
    warning("Adjusted efficiencies unavailable: missing game columns.")
    return(finish())
  }
  number <- function(x, ...) {
    for (key in list(...)) {
      if (!is.list(x) || is.null(x[[key]])) return(NA_real_)
      x <- x[[key]]
    }
    if (length(x) != 1L) return(NA_real_)
    suppressWarnings(as.numeric(x))
  }
  points <- vapply(games$team_stats, number, numeric(1), "points", "total")
  poss <- vapply(games$team_stats, number, numeric(1), "possessions")
  allowed <- vapply(games$opponent_stats, number, numeric(1), "points", "total")
  opp_poss <- vapply(games$opponent_stats, number, numeric(1), "possessions")
  valid <- games$team_id %in% ids & games$opponent_id %in% ids &
    is.finite(points) & points >= 0 & is.finite(allowed) & allowed >= 0 &
    is.finite(poss) & poss > 0 & is.finite(opp_poss) & opp_poss > 0
  if (!any(valid)) return(finish())
  a <- match(games$team_id[valid], ids)
  b <- match(games$opponent_id[valid], ids)
  off <- 100 * points[valid] / poss[valid]
  def <- 100 * allowed[valid] / opp_poss[valid]
  league <- 100 * (sum(points[valid]) + sum(allowed[valid])) /
    (sum(poss[valid]) + sum(opp_poss[valid]))
  groups <- split(seq_along(a), factor(a, levels = seq_along(ids)))
  counts <- lengths(groups)
  # Initialize with raw game-average ratings; unplayed teams start at baseline.
  offense <- defense <- rep(league, length(ids))
  for (i in which(counts > 0)) {
    offense[i] <- mean(off[groups[[i]]])
    defense[i] <- mean(def[groups[[i]]])
  }
  for (iteration in seq_len(max_iterations)) {
    next_offense <- next_defense <- rep(league, length(ids))
    for (i in which(counts > 0)) {
      rows <- groups[[i]]
      next_offense[i] <- (sum(off[rows] - (defense[b[rows]] - league)) +
        prior_games * league) / (counts[i] + prior_games)
      next_defense[i] <- (sum(def[rows] - (offense[b[rows]] - league)) +
        prior_games * league) / (counts[i] + prior_games)
    }
    # Damping prevents oscillation; both sides use the previous iteration.
    next_offense <- 0.5 * offense + 0.5 * next_offense
    next_defense <- 0.5 * defense + 0.5 * next_defense
    change <- max(abs(next_offense - offense), abs(next_defense - defense))
    offense <- next_offense
    defense <- next_defense
    if (!is.finite(change)) break
    if (change < tolerance) break
  }
  converged <- is.finite(change) && change < tolerance
  diagnostics <- list(status = if (converged) "ready" else "not_converged",
    converged = converged, iterations = iteration, tolerance = tolerance,
    max_change = change, national_average = league, prior_games = prior_games)
  out$adjusted_games <- as.integer(counts)
  if (converged) {
    played <- counts > 0
    out$adjusted_offensive_efficiency[played] <- offense[played]
    out$adjusted_defensive_efficiency[played] <- defense[played]
    out$adjusted_efficiency_margin[played] <- offense[played] - defense[played]
  } else warning("Opponent adjustment did not converge; adjusted statistics left blank.")
  finish()
}

# Pace uses only each team's own box-score possession estimate.
calculate_pace <- function(games) {
  out <- data.frame(team_id = unique(games$team_id), pace = NA_real_, pace_games = 0L)
  number <- function(x, ...) {
    for (key in list(...)) {
      if (!is.list(x) || is.null(x[[key]])) return(NA_real_)
      x <- x[[key]]
    }
    if (length(x) != 1L) return(NA_real_)
    suppressWarnings(as.numeric(x))
  }
  possessions <- function(column) {
    if (!column %in% names(games)) return(rep(NA_real_, nrow(games)))
    extract <- function(...) vapply(games[[column]], number, numeric(1), ...)
    fga <- extract("fieldGoals", "attempted")
    fta <- extract("freeThrows", "attempted")
    orb <- extract("rebounds", "offensive")
    turnovers <- extract("turnovers", "total")
    valid <- is.finite(fga) & fga >= 0 & is.finite(fta) & fta >= 0 &
      is.finite(orb) & orb >= 0 & is.finite(turnovers) & turnovers >= 0
    estimate <- fga + 0.44 * fta - orb + turnovers
    estimate[!valid | !is.finite(estimate) | estimate <= 0] <- NA_real_
    estimate
  }
  team_possessions <- possessions("team_stats")
  minutes <- if ("game_minutes" %in% names(games))
    suppressWarnings(as.numeric(games$game_minutes)) else rep(NA_real_, nrow(games))
  valid <- is.finite(team_possessions) &
    is.finite(minutes) & minutes >= 40
  for (i in seq_len(nrow(out))) {
    rows <- games$team_id == out$team_id[i] & valid
    out$pace_games[i] <- sum(rows)
    if (any(rows)) out$pace[i] <- 40 * sum(team_possessions[rows]) / sum(minutes[rows])
  }
  out
}

# Frozen pre-day opponent values; later games never change saved contributions.
calculate_frozen_sos <- function(games, roster, saved = NULL) {
  version <- "pregame-average-v1"
  if (!is.null(saved) && !identical(saved$version, version))
    stop("SOS cache has a different model version; review before publishing.")
  required <- c("game_id", "team_id", "opponent_id", "start_date")
  if (!all(required %in% names(games))) stop("SOS needs game IDs, opponent IDs and start dates.")
  dates <- if (inherits(games$start_date, "POSIXt")) games$start_date else
    as.POSIXct(as.character(games$start_date), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  if (anyNA(dates)) stop("Could not parse SOS game start dates; exports unchanged.")
  days <- as.Date(dates, tz = "America/New_York")
  ids <- roster$team_id[is.finite(roster$team_id)]
  eligible <- games$team_id %in% ids
  ledger <- if (is.null(saved)) data.frame(game_id = numeric(), team_id = numeric(),
    opponent_id = numeric(), snapshot_date = character(), opponent_rank = integer(),
    contribution = numeric()) else saved$ledger
  snapshots <- if (is.null(saved)) list() else saved$snapshots
  key <- function(x) paste(x$game_id, x$team_id, sep = ":")
  if (anyDuplicated(key(ledger)) || any(!is.finite(ledger$contribution) |
      abs(ledger$contribution) > 1)) stop("Invalid frozen SOS ledger.")
  pending <- which(eligible & !key(games) %in% key(ledger))
  for (day in sort(unique(as.character(days[pending])))) {
    rows <- pending[as.character(days[pending]) == day]
    if (is.null(snapshots[[day]])) {
      # Exclude every result on the game date; snapshots are pre-day, Eastern time.
      prior <- games[days < as.Date(day), , drop = FALSE]
      n <- nrow(roster)
      net <- rep(NA_real_, n)
      if (nrow(prior)) {
        adjusted <- calculate_adjusted_efficiencies(prior, ids)
        if (identical(adjusted$diagnostics$status, "not_converged"))
          stop("Historical SOS adjustment did not converge; frozen values unchanged.")
        net <- adjusted$ratings$adjusted_efficiency_margin[
          match(roster$team_id, adjusted$ratings$team_id)]
      }
      net_rank <- rank(-net, ties.method = "average", na.last = "keep")
      # No usable D1 efficiency data yet: fall back to the team's preseason rank.
      net_rank[!is.finite(net_rank)] <- roster$preseason_rank[!is.finite(net_rank)]
      counts <- vapply(roster$team_id, function(id) {
        if (!is.finite(id)) return(0L)
        as.integer(sum(prior$team_id == id))
      }, integer(1))
      weight <- pmax(0.5, 1 - counts / 12)
      blended <- weight * roster$preseason_rank + (1 - weight) * net_rank
      order_ids <- order(blended, roster$preseason_rank, roster$team)
      opponent_rank <- integer(n)
      opponent_rank[order_ids] <- seq_len(n)
      snapshots[[day]] <- data.frame(team_id = roster$team_id, team = roster$team,
        preseason_rank = roster$preseason_rank, net_rank = net_rank,
        prior_games = counts, preseason_weight = weight, blended_rank = blended,
        opponent_rank = opponent_rank,
        contribution = 1 - 2 * (opponent_rank - 1) / (n - 1))
      message("Saved pregame SOS snapshot for ", day)
    }
    snapshot <- snapshots[[day]]
    for (i in rows) {
      if (is.na(games$opponent_id[i])) stop("Missing SOS opponent ID.")
      opponent <- match(games$opponent_id[i], snapshot$team_id)
      if (is.na(opponent) && "opponent_conference" %in% names(games) &&
          !is.na(games$opponent_conference[i]) && nzchar(trimws(games$opponent_conference[i])))
        stop("Unmatched D1 SOS opponent: ", games$opponent_id[i])
      ledger <- rbind(ledger, data.frame(game_id = games$game_id[i],
        team_id = games$team_id[i], opponent_id = games$opponent_id[i],
        snapshot_date = day,
        opponent_rank = if (is.na(opponent)) NA_integer_ else snapshot$opponent_rank[opponent],
        contribution = if (is.na(opponent)) -0.99 else snapshot$contribution[opponent]))
    }
  }
  output <- data.frame(team_id = ids, strength_of_schedule = NA_real_, sos_games = 0L)
  current_keys <- key(games[eligible, , drop = FALSE])
  for (i in seq_len(nrow(output))) {
    values <- ledger$contribution[ledger$team_id == output$team_id[i] & key(ledger) %in% current_keys]
    output$sos_games[i] <- length(values)
    if (length(values)) output$strength_of_schedule[i] <- mean(values)
  }
  list(ratings = output, state = list(version = version, ledger = ledger,
    snapshots = snapshots, timezone = "America/New_York",
    snapshot_rule = "Before each Eastern day; only earlier completed games"))
}


# Clutch qualification uses regulation's last three minutes, not final margin alone.
classify_clutch_game <- function(plays, home_points, away_points, game_minutes = NA_real_) {
  overtime <- if (is.finite(game_minutes) && game_minutes >= 40) game_minutes > 40 else NA
  flag <- NA
  if (isTRUE(overtime)) return(list(clutch = TRUE, overtime = TRUE))
  if (is.finite(home_points) && is.finite(away_points) &&
      abs(home_points - away_points) <= 3) flag <- TRUE
  if (!is.data.frame(plays) || !nrow(plays) ||
      !all(c("period", "clock", "home_score", "away_score") %in% names(plays)))
    return(list(clutch = flag, overtime = overtime))
  period <- suppressWarnings(as.numeric(plays$period))
  if (any(period >= 3, na.rm = TRUE)) return(list(clutch = TRUE, overtime = TRUE))
  # Read the period clock directly; seconds_remaining may describe the whole game.
  clocks <- strsplit(as.character(plays$clock), ":", fixed = TRUE)
  seconds <- vapply(clocks, function(x) {
    if (length(x) != 2L) return(NA_real_)
    v <- suppressWarnings(as.numeric(x))
    if (any(!is.finite(v)) || v[1] < 0 || v[2] < 0 || v[2] >= 60) return(NA_real_)
    60 * v[1] + v[2]
  }, numeric(1))
  home <- suppressWarnings(as.numeric(plays$home_score))
  away <- suppressWarnings(as.numeric(plays$away_score))
  valid <- is.finite(period) & period == 2 & is.finite(seconds) &
    seconds >= 0 & seconds <= 1200 & is.finite(home) & home >= 0 &
    is.finite(away) & away >= 0
  # Keep API order at equal clock times (e.g. consecutive free throws).
  rows <- which(valid)
  rows <- rows[order(-seconds[rows], rows)]
  inside <- rows[seconds[rows] <= 180]
  before <- rows[seconds[rows] > 180]
  if (length(inside) && any(abs(home[inside] - away[inside]) <= 3)) flag <- TRUE
  # The last state above 3:00 carries forward until the next recorded play.
  if (length(before) && length(inside)) {
    boundary <- tail(before, 1L)
    if (abs(home[boundary] - away[boundary]) <= 3) flag <- TRUE
  }
  ends <- rows[seconds[rows] == 0]
  end_matches <- length(ends) > 0 && is.finite(home_points) && is.finite(away_points) &&
    home[tail(ends, 1L)] == home_points && away[tail(ends, 1L)] == away_points
  # A final regulation score matching the box score establishes no overtime.
  if (is.na(overtime) && end_matches) overtime <- FALSE
  # Never call a game non-clutch without coverage spanning the full window.
  window_rows <- which(is.finite(period) & period == 2 &
    is.finite(seconds) & seconds <= 180 & seconds >= 0)
  window_valid <- length(window_rows) > 0 && all(valid[window_rows])
  covered <- length(before) > 0 || any(seconds[rows] == 180)
  if (is.na(flag) && covered && window_valid && end_matches && identical(overtime, FALSE))
    flag <- FALSE
  list(clutch = flag, overtime = overtime)
}

calculate_clutch <- function(games, team_ids, season, mode, cache_path, save_cache) {
  number <- function(x, ...) {
    for (key in list(...)) {
      if (!is.list(x) || is.null(x[[key]])) return(NA_real_)
      x <- x[[key]]
    }
    if (length(x) != 1L) return(NA_real_)
    suppressWarnings(as.numeric(x))
  }
  version <- "clutch-final-three-v1"
  cache <- if (file.exists(cache_path)) readRDS(cache_path) else
    list(version = version, games = list(), date_checks = list())
  if (!identical(cache$version, version)) stop("Clutch cache version mismatch.")
  relevant <- games[games$team_id %in% team_ids, , drop = FALSE]
  metadata <- relevant[!duplicated(relevant$game_id), , drop = FALSE]
  if (!all(c("is_home", "opponent_stats", "start_date") %in% names(metadata)))
    stop("Clutch needs home/away identity, opponent box scores and game dates.")
  metadata$own_points <- vapply(metadata$team_stats, number, numeric(1), "points", "total")
  metadata$other_points <- vapply(metadata$opponent_stats, number, numeric(1), "points", "total")
  metadata$home_points <- ifelse(metadata$is_home, metadata$own_points, metadata$other_points)
  metadata$away_points <- ifelse(metadata$is_home, metadata$other_points, metadata$own_points)
  metadata$minutes <- if ("game_minutes" %in% names(metadata))
    as.numeric(metadata$game_minutes) else rep(NA_real_, nrow(metadata))
  metadata$utc_date <- substr(as.character(metadata$start_date), 1L, 10L)
  ids <- as.character(metadata$game_id)
  complete <- function(x) !is.null(x) && !is.na(x$clutch) &&
    (!isTRUE(x$clutch) || !is.na(x$overtime))
  for (i in seq_len(nrow(metadata))) {
    old <- cache$games[[ids[i]]]
    # Re-evaluate if a provider correction changes the final score or duration.
    signature <- paste(metadata$home_points[i], metadata$away_points[i], metadata$minutes[i])
    if (!is.null(old) && !identical(old$signature, signature)) old <- NULL
    if (!complete(old)) {
      result <- classify_clutch_game(if (is.null(old)) NULL else old$plays, metadata$home_points[i],
        metadata$away_points[i], metadata$minutes[i])
      result$signature <- signature
      cache$games[[ids[i]]] <- result
    }
  }
  missing <- which(!vapply(ids, function(id) complete(cache$games[[id]]), logical(1)))
  today <- as.character(as.Date(Sys.time(), tz = "UTC"))
  if (mode != "cached" && length(missing)) {
    requests <- 0L
    # Prioritize recent dates; cap catch-up work to protect the API allowance.
    for (day in sort(unique(metadata$utc_date[missing]), decreasing = TRUE)) {
      if (requests >= 14L) { message("Clutch download cap reached; catch-up continues next run."); break }
      # A missing day's data is retried at most once per calendar day.
      last_check <- cache$date_checks[[day]]
      if (identical(last_check, today)) next
      # Old missing dates retry weekly, rather than consuming requests every day.
      if (!is.null(last_check) && as.Date(day) < as.Date(today) - 7L &&
          as.Date(last_check) > as.Date(today) - 7L) next
      requests <- requests + 1L
      message("Downloading clutch play-by-play for ", day)
      fetched <- tryCatch(tracked_api_request("/plays/date",
        cbbdr::get_plays_by_date(date = day, shooting_plays_only = FALSE)),
        error = function(e) {
          warning("Clutch play-by-play unavailable for ", day, ": ", conditionMessage(e))
          NULL
        })
      cache$date_checks[[day]] <- today
      for (i in which(metadata$utc_date == day)) {
        if (complete(cache$games[[ids[i]]])) next
        plays <- if (is.data.frame(fetched) && "game_id" %in% names(fetched))
          fetched[fetched$game_id == metadata$game_id[i] & !is.na(fetched$game_id), , drop = FALSE] else NULL
        # Retain only fields needed for classification and offline checks.
        if (is.data.frame(plays)) plays <- plays[, intersect(names(plays),
          c("id", "game_id", "period", "clock", "home_score", "away_score")), drop = FALSE]
        result <- classify_clutch_game(plays, metadata$home_points[i],
          metadata$away_points[i], metadata$minutes[i])
        result$signature <- cache$games[[ids[i]]]$signature
        result$plays <- plays
        cache$games[[ids[i]]] <- result
      }
      save_cache(cache, cache_path)
    }
  }
  save_cache(cache, cache_path)
  out <- data.frame(team_id = unique(team_ids), clutch_factor = NA_real_,
    clutch_games = 0L, clutch_wins = 0L, clutch_losses = 0L,
    clutch_overtime_games = 0L, clutch_bonus = 0,
    clutch_games_checked = 0L, clutch_games_pending = 0L)
  for (i in seq_len(nrow(out))) {
    rows <- relevant[relevant$team_id == out$team_id[i], , drop = FALSE]
    if (!nrow(rows)) next
    own <- vapply(rows$team_stats, number, numeric(1), "points", "total")
    other <- vapply(rows$opponent_stats, number, numeric(1), "points", "total")
    usable <- is.finite(own) & is.finite(other) & own != other
    checked <- vapply(as.character(rows$game_id), function(id) complete(cache$games[[id]]), logical(1)) & usable
    out$clutch_games_checked[i] <- sum(checked)
    out$clutch_games_pending[i] <- sum(!checked)
    flags <- vapply(as.character(rows$game_id), function(id) isTRUE(cache$games[[id]]$clutch), logical(1)) & checked
    n <- sum(flags)
    ot <- sum(vapply(as.character(rows$game_id[flags]), function(id)
      isTRUE(cache$games[[id]]$overtime), logical(1)))
    wins <- sum(own[flags] > other[flags])
    losses <- n - wins
    bonus <- 0.2 * (n - ot) + 0.4 * ot
    out$clutch_games[i] <- n
    out$clutch_wins[i] <- wins
    out$clutch_losses[i] <- losses
    out$clutch_overtime_games[i] <- ot
    out$clutch_bonus[i] <- bonus
    # Avoid publishing a biased average when some games remain unclassified.
    if (!out$clutch_games_pending[i]) out$clutch_factor[i] <-
      if (n) (wins - losses) / n + bonus else 0
  }
  pending <- sum(out$clutch_games_pending)
  if (pending) warning("CLUTCH remains blank for teams with unclassified games (",
                       pending, " team-game records). Other statistics still update.")
  out
}


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
  sos_path <- sprintf("data/frozen_sos_%s.rds", season)
  clutch_path <- sprintf("data/clutch_%s.rds", season)

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
    team_stats <- tracked_api_request("/stats/team/season",
      cbbdr::get_team_season_stats(season = season))
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
      new <- tracked_api_request("/games/teams", cbbdr::get_game_teams(season = season,
                                 start_date_range = from, end_date_range = to))
      schedule <- tracked_api_request("/games", cbbdr::get_games(season = season,
                                  start_date_range = from, end_date_range = to))
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
    if (mode != "cached") {
      save_rds(if (is.null(all_games)) data.frame() else all_games, games_path)
      save_rds(team_stats, teams_path)
      save_rds(list(checked_through = as.character(finish - 1L),
        source_updated_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), state_path)
    }
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
  # Keep season points/turnovers on the same usable-game basis for PPT.
  if (nrow(rating_games)) {
    averages <- aggregate(cbind(points, turnovers) ~ team_id, rating_games, mean)
    names(averages) <- c("team_id", "ppg", "turnovers_per_game")
    averages$rating <- with(averages, ifelse(turnovers_per_game > 0,
                                           ppg / turnovers_per_game, NA_real_))
    counts <- aggregate(points ~ team_id, rating_games, length)
    names(counts) <- c("team_id", "games_rated")
  } else {
    # Efficiency must still update when turnover data is missing.
    averages <- data.frame(team_id = numeric(), ppg = numeric(),
                           turnovers_per_game = numeric(), rating = numeric())
    counts <- data.frame(team_id = numeric(), games_rated = integer())
  }
  rankings <- merge(merge(teams, averages, by = "team_id", all.x = TRUE), counts,
                    by = "team_id", all.x = TRUE)
  rankings$games_rated[is.na(rankings$games_rated)] <- 0L
  rankings <- merge(rankings, calculate_pace(all_games), by = "team_id", all.x = TRUE)
  clutch <- calculate_clutch(all_games, teams$team_id, season, mode, clutch_path, save_rds)
  rankings <- merge(rankings, clutch, by = "team_id", all.x = TRUE)
  efficiency <- calculate_raw_efficiencies(all_games)
  rankings <- merge(rankings, efficiency, by = "team_id", all.x = TRUE)
  rankings$offense_eff_games[is.na(rankings$offense_eff_games)] <- 0L
  rankings$defense_eff_games[is.na(rankings$defense_eff_games)] <- 0L
  adjusted <- calculate_adjusted_efficiencies(all_games, teams$team_id)
  rankings <- merge(rankings, adjusted$ratings, by = "team_id", all.x = TRUE)
  rankings$net_rating <- rankings$adjusted_efficiency_margin
  message("Opponent adjustment: ", adjusted$diagnostics$status,
          " (", adjusted$diagnostics$iterations, " iterations).")
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
  # Rebuild the fixed preseason score using the current no-ties convention,
  # including when an older local snapshot is cached.
  base <- base[order(base$preseason_average_rank, base$kenpom_preseason_rank,
                    base$evanmiya_roster_rank, base$team), ]
  base$rank <- seq_len(nrow(base))
  base$preseason_score <- 100 * (nrow(base) - base$rank) / (nrow(base) - 1)
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
  # SOS has its own permanent 50% preseason floor; overall ranking decay is unchanged.
  sos_roster <- data.frame(team = base$team, team_id = base$team_id,
                           preseason_rank = base$rank)
  current_ids <- team_stats$team_id[match(team_key(sos_roster$team), team_key(team_stats$team))]
  sos_roster$team_id[!is.na(current_ids)] <- current_ids[!is.na(current_ids)]
  if (anyDuplicated(sos_roster$team_id[is.finite(sos_roster$team_id)]))
    stop("Duplicate team IDs in SOS roster.")
  sos_saved <- if (file.exists(sos_path)) readRDS(sos_path) else NULL
  if (is.null(sos_saved)) message("Initializing frozen SOS history from pre-day results.")
  sos <- calculate_frozen_sos(all_games, sos_roster, sos_saved)
  rankings <- merge(rankings, sos$ratings, by = "team_id", all.x = TRUE)
  # Completed season games drive the decay, including games missing PPT data.
  rankings$preseason_weight <- pmax(0, (12 - rankings$games) / 12)
  rankings$inseason_score <- NA_real_
  scores_path <- sprintf("data/inseason_scores_%s.csv", season)
  played <- rankings$games > 0
  model_configured <- file.exists(scores_path)
  rankings$planned_preseason_weight <- rankings$preseason_weight
  if (any(played) && !model_configured) {
    message("Game statistics updated. Overall order remains the preseason projection ",
            "until a separate in-season overall model is configured.")
    rankings$preseason_weight <- 1
  }
  if (any(played) && model_configured) {
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
  blend_rows <- played & model_configured
  rankings$overall_score[blend_rows] <- with(rankings[blend_rows, ],
    preseason_weight * preseason_score + (1 - preseason_weight) * inseason_score)
  # Unique ranks: composite score, then original source ranks resolve exact ties.
  rankings <- rankings[order(-rankings$overall_score,
    rankings$kenpom_preseason_rank, rankings$evanmiya_roster_rank, rankings$team), ]
  rankings$rank <- seq_len(nrow(rankings))
  rownames(rankings) <- NULL
  if (any(rankings$games_rated != rankings$games)) {
    warning("Some games_rated counts differ from season games. Check coverage before publication.")
  }

  # Export time is separate from source refresh time: cached mode does not
  # pretend to have downloaded fresh data. Old caches have unknown freshness.
  source_updated <- if (mode == "cached") state$source_updated_at else
    format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  payload <- list(status = if (model_configured) "ready" else "preseason_projection",
                  overall_model_configured = model_configured,
                  clutch_formula = "(clutch_wins - clutch_losses) / clutch_games + 0.2 * regulation_clutch_games + 0.4 * overtime_clutch_games",
                  clutch_definition = "Margin <= 3 at any point with 3:00 or less in regulation second half; overtime qualifies",
                  clutch_no_qualifying_games = 0,
                  clutch_incomplete_coverage = "Unavailable until all completed team games are classified",
                  sos_formula = "mean(frozen pregame opponent contributions)",
                  sos_opponent_value_formula = "1 - 2 * (opponent_rank - 1) / (D1_team_count - 1)",
                  sos_preseason_weight_formula = "max(0.5, 1 - opponent_prior_games / 12)",
                  sos_snapshot_rule = "Before each Eastern-calendar day; only earlier completed games",
                  sos_non_d1_value = -0.99,
                  adjusted_efficiency_model = "OFred additive, D1-only, equal game weights, one baseline pseudo-game",
                  adjusted_efficiency_diagnostics = adjusted$diagnostics,
                  pace_formula = "40 * sum(team_estimated_possessions) / sum(game_minutes)",
                  estimated_possessions_formula = "FGA + 0.44 * FTA - OR + TO",
                  net_rating_formula = "adjusted_offensive_efficiency - adjusted_defensive_efficiency",
                  adjusted_efficiency_margin_formula = "adjusted_offensive_efficiency - adjusted_defensive_efficiency",
                  offensive_efficiency_formula = "100 * total_points / total_possessions",
                  defensive_efficiency_formula = "100 * total_points_allowed / total_opponent_possessions",
                  metric = "OFred overall composite", updated_at = if (is.null(source_updated)) NULL else source_updated,
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
  # Retain this file: it preserves opponent values already locked for played games.
  save_rds(sos$state, sos_path)
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
           message = paste("Update OFred statistics for season", expected_season),
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
run_rankings_update <- function() {
  on.exit(report_api_requests(), add = TRUE)
  result <- update_rankings(SEASON, MODE, LOOKBACK_DAYS, PUBLISH_ACTIVE)
  if (!is.null(result)) {
    print(head(result, 25))
    if (isTRUE(PUBLISH_ACTIVE)) upload_rankings("rankings.json", SEASON)
  }
  invisible(result)
}
rankings <- run_rankings_update()


