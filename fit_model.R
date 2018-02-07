## MILAD KHARRATZADEH - FEB 2018

library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
sm <- stan_model("epl_model.stan")
nsamples <- 2000
for (i in 0:15) {
  epl <- readRDS(paste('DATA/epl_', sprintf("%02d%02d", i, i+1), '.rds',sep=""))
  a_sims <- array(NA, c(nsamples, epl$nweeks, epl$nteams))
  for (w in 1:38) {
    epl_w <- epl
    idx <- c(1:(w*10))
    epl_w$home_team  <- epl$home_team[idx]
    epl_w$away_team  <- epl$away_team[idx]
    epl_w$home_goals <- epl$home_goals[idx]
    epl_w$away_goals <- epl$away_goals[idx]
    epl_w$score_diff <- epl$score_diff[idx]
    epl_w$home_week  <- epl$home_week[idx]
    epl_w$away_week  <- epl$away_week[idx]
    epl_w$ngames     <- w*10
    epl_w$nweeks     <- max(c(epl_w$home_week, epl_w$away_week))
    fit <- sampling(sm, chains = 4, iter = (nsamples/2), data = epl_w, 
                    control = list(adapt_delta=0.95))
    saveRDS(fit, paste('FITS/fit_', sprintf("%02d%02d_%02d", i, i+1, w), '.rds',sep=""))
    sims <- extract(fit)
    for (g in ((w-1)*10 + 1):(w*10)) {
      a_sims[,epl$home_week[g],epl$home_team[g]] <- 
        sims$a[,epl$home_week[g],epl$home_team[g]]
      a_sims[,epl$away_week[g],epl$away_team[g]] <- 
        sims$a[,epl$away_week[g],epl$away_team[g]]
    }
  }
  saveRDS(a_sims,paste('FITS/sims_', sprintf("%02d%02d", i, i+1), '.rds',sep=""))
}