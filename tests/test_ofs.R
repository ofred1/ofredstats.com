script <- if (file.exists('update_rankings.R')) 'update_rankings.R' else '../update_rankings.R'
expressions <- parse(script)
model_env <- new.env(parent=baseenv())
for (expr in expressions) {
 if (is.call(expr) && identical(expr[[1]], as.name('<-')) &&
     identical(expr[[2]], as.name('calculate_ofs'))) eval(expr, model_env)
}
calculate_ofs <- model_env$calculate_ofs
stopifnot(is.function(calculate_ofs))
x <- data.frame(team_id=1:20,team=paste0('Team ',1:20),
 net_rating=c(100,48.9,47,45,44,40.4,40.3,30.4,20.4,10.4,0.4,-20,-60,-120,-200,-250,-300,-350,-400,-450),
 strength_of_schedule=seq(1,-1,length.out=20),clutch_factor=seq(-5,5,length.out=20))
y <- calculate_ofs(x)
stopifnot(y$diagnostics$status=='ready',abs(y$ratings$ofs[6]-25)<1e-10,
 all(y$ratings$ofs>=0 & y$ratings$ofs<=75),
 max(y$ratings$ofs_sos_component)==15,max(y$ratings$ofs_clutch_component)==5,
 y$ratings$ofs_net_raw[1]==55,y$ratings$ofs_net_raw[6]==40,
 abs(y$ratings$ofs_net_raw[8]-21)<1e-10,
 abs(y$ratings$ofs_net_raw[9]-13)<1e-10,
 abs(y$ratings$ofs_net_raw[10]-12)<1e-10,
 y$ratings$ofs_net_raw[20]==0)
z <- x;z$clutch_factor <- 0
tie <- calculate_ofs(z)
stopifnot(all(abs(tie$ratings$ofs_clutch_component-2.5)<1e-12))
z$clutch_factor[6] <- NA_real_
stopifnot(all(is.na(calculate_ofs(z)$ratings$ofs)))
z <- x;z$clutch_factor[1] <- NA_real_
partial <- calculate_ofs(z)
stopifnot(partial$diagnostics$status=='partial',is.na(partial$ratings$ofs[1]),
 abs(partial$ratings$ofs[6]-25)<1e-10)
stopifnot(all(is.na(calculate_ofs(x[1:5,])$ratings$ofs)))
invisible(parse(script))
cat('PASS: OFS anchor, bounds, piecewise breakpoints, ties, missing inputs, partial coverage, and updater syntax.\n')

z <- x; z$clutch_factor <- z$strength_of_schedule
stopifnot(abs(calculate_ofs(z)$ratings$ofs[1]-75)<1e-10)

