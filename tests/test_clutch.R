# Run from the repository root: Rscript tests/test_clutch.R
# Load only function definitions, never the updater's API/upload entry point.
expressions <- parse("update_rankings.R")
for (expression in expressions) {
  if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      is.symbol(expression[[2]]) &&
      as.character(expression[[2]]) %in% c("classify_clutch_game", "calculate_clutch"))
    eval(expression, envir = .GlobalEnv)
}
plays <- function(clock, home, away, period = rep(2L, length(clock))) {
  data.frame(period = period, clock = clock, home_score = home, away_score = away)
}
# Close at a point in the last three minutes, even if the final margin is large.
x <- plays(c("3:20", "2:40", "0:00"), c(60, 62, 80), c(55, 60, 65))
stopifnot(isTRUE(classify_clutch_game(x, 80, 65, 40)$clutch))
# Carry the last pre-3:00 score into the window.
x <- plays(c("3:10", "2:50", "0:00"), c(60, 63, 80), c(58, 58, 65))
stopifnot(isTRUE(classify_clutch_game(x, 80, 65, 40)$clutch))
# Exactly 3:00 and a three-point margin qualify.
x <- plays(c("3:00", "0:00"), c(60, 80), c(57, 65))
stopifnot(isTRUE(classify_clutch_game(x, 80, 65, 40)$clutch))
# Close in the first half alone does not qualify.
x <- plays(c("1:00", "3:10", "2:50", "0:00"),
           c(30, 60, 65, 80), c(30, 50, 50, 65), c(1, 2, 2, 2))
stopifnot(identical(classify_clutch_game(x, 80, 65, 40)$clutch, FALSE))
# Missing late-game coverage is unknown, not non-clutch.
stopifnot(is.na(classify_clutch_game(plays("3:10", 60, 50), 80, 65, 40)$clutch))
stopifnot(is.na(classify_clutch_game(NULL, 80, 65, 40)$clutch))
# Overtime implies a regulation tie and receives one overtime bonus.
stopifnot(isTRUE(classify_clutch_game(NULL, 90, 80, 45)$clutch))
stopifnot(isTRUE(classify_clutch_game(NULL, 90, 80, 50)$overtime))
stopifnot(isTRUE(classify_clutch_game(NULL, 71, 70, 40)$clutch))

# Seven clutch games, five wins and two losses, with one overtime game.
own <- c(rep(80, 5), 78, 78)
other <- c(rep(78, 5), 80, 80)
minutes <- c(45, rep(40, 6))
games <- data.frame(game_id = 1:7, team_id = 72,
  is_home = TRUE, start_date = "2026-11-10T00:00:00Z", game_minutes = minutes)
games$team_stats <- lapply(own, function(x) list(points = list(total = x)))
games$opponent_stats <- lapply(other, function(x) list(points = list(total = x)))
cache <- list(version = "clutch-final-three-v1", games = list(), date_checks = list())
for (i in 1:7) cache$games[[as.character(i)]] <- list(clutch = TRUE,
  overtime = minutes[i] > 40, signature = paste(own[i], other[i], minutes[i]))
path <- tempfile(fileext = ".rds")
saveRDS(cache, path)
answer <- calculate_clutch(games, c(72, 73), 2027L, "cached", path, saveRDS)
stopifnot(answer$clutch_games[1] == 7, answer$clutch_wins[1] == 5,
  answer$clutch_losses[1] == 2, answer$clutch_overtime_games[1] == 1,
  abs(answer$clutch_bonus[1] - 1.6) < 1e-12,
  abs(answer$clutch_factor[1] - (3/7 + 1.6)) < 1e-12,
  is.na(answer$clutch_factor[2]))
# An unclassified game blocks only CLUTCH, not the other updater statistics.
cache$games[["1"]] <- list(clutch = NA, overtime = FALSE,
  signature = paste(own[1], other[1], minutes[1]))
# Make this game neither overtime nor close at the final buzzer.
games$game_minutes[1] <- 40
games$opponent_stats[[1]] <- list(points = list(total = 60))
saveRDS(cache, path)
answer <- suppressWarnings(calculate_clutch(games, 72, 2027L, "cached", path, saveRDS))
stopifnot(is.na(answer$clutch_factor), answer$clutch_games_pending == 1L)
unlink(path)
cat("Clutch classification and scoring tests passed.\n")
