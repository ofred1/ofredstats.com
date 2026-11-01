script <- if (file.exists('update_rankings.R')) 'update_rankings.R' else '../update_rankings.R'
model_env <- new.env(parent=baseenv())
for (expr in parse(script)) {
 if (is.call(expr) && identical(expr[[1]], as.name('<-')) &&
     identical(expr[[2]], as.name('calculate_net_rankings'))) eval(expr,model_env)
}
calculate_net_rankings <- model_env$calculate_net_rankings
stopifnot(is.function(calculate_net_rankings))
x <- data.frame(team_id=1:4,games=c(0,6,12,14),
 preseason_score=c(90,20,80,100),net_rating=c(40,30,20,10))
y <- calculate_net_rankings(x)$ratings
stopifnot(y$overall_score[1]==90,y$preseason_weight[1]==1,
 y$overall_score[2]==60,y$preseason_weight[2]==0.5,
 y$overall_score[3]==50,y$overall_score[4]==0,
 all(y$preseason_weight[3:4]==0))
z <- x;z$games <- 12
full <- calculate_net_rankings(z)$ratings
stopifnot(identical(order(-full$overall_score),order(-z$net_rating)))
z$net_rating[1] <- NA_real_
missing <- calculate_net_rankings(z)$ratings
stopifnot(is.na(missing$inseason_score[1]),missing$overall_score[1]==90)
z$net_rating <- NA_real_
none <- calculate_net_rankings(z)
stopifnot(none$diagnostics$status=='preseason_projection',
 identical(none$ratings$overall_score,z$preseason_score))
z$net_rating[1] <- 20
stopifnot(calculate_net_rankings(z)$ratings$inseason_score[1]==50)
z$net_rating <- 0
stopifnot(all(calculate_net_rankings(z)$ratings$inseason_score==50))
z$clutch_factor <- 100;z$strength_of_schedule <- -1;z$ofs <- 75
stopifnot(identical(calculate_net_rankings(z)$ratings$overall_score,
 calculate_net_rankings(z[,c('team_id','games','preseason_score','net_rating')])$ratings$overall_score))
invisible(parse(script))
cat('PASS: preseason at zero games, 50% at six, net-only after twelve, missing/one/tied net ratings, no influence from other metrics, updater syntax.\n')
