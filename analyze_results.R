## PLOTTING TEAM ABILITIES AFTER A GIVEN WEEK IN A GIVEN SEASON

library (arm)
library(rstan)
library(matrixStats)
season<-07
week<-38
epl <- readRDS(paste('DATA/epl_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
a_sims<-readRDS(paste('FITS/sims_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
curr_perf <- epl$team_pts;
sort_perf <- order(curr_perf);
a_hat <- colMeans(a_sims[,week,])
a_se <- sqrt(colVars(a_sims[,week,]))
coefplot (a_hat[sort_perf], a_se[order(sort_perf)], 
          CI=1, varnames=as.character(epl$team_names[sort_perf]),
          main=paste("Team abilities after week ",week, 
                     ", season 20", sprintf("%02d",season), 
                     "-20", sprintf("%02d",season+1),
                     "\n (estimate +/- 1 s.e.)\n",sep=""), 
          cex.var=.9, mar=c(1,6,5.1,2), xlim=c(-2,2))




##########################################################

## PLOTTING THE EVOLUTION OF ABILITIES OVER THE COURSE OF THE SEASON

library(rstan)
library (arm)
library(matrixStats)
season<-05
epl <- readRDS(paste('DATA/epl_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
a_sims<-readRDS(paste('FITS/sims_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
a_hat <- matrix(NA, nrow=38, ncol=20)
a_se <- matrix(NA, nrow=38, ncol=20)
for (w in 1:38) { 
  a_hat[w,] <- colMeans(a_sims[,w,])
  a_se[w,] <- sqrt(colVars(a_sims[,w,]))
}
a_min <- a_hat-a_se 
a_max <- a_hat+a_se
curr_perf <- epl$team_pts;
sort_perf <- rev(order(curr_perf));
png ("ab_evol.png", height=10, width=8, units = 'in', res = 200)
attach(mtcars)
op <- par(mfrow = c(5,4),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0.8,0.8,4,4) + 0.1)
x<-c(1:38)
for (i in 1:20) {
  teamname <- epl$team_names[sort_perf[i]];  
  ind <- sort_perf[i]
  plot(a_hat[,ind], type="l", ylim=c(-2,2),
       lty = 1, lwd = 3, bty='l')
  polygon(c(x, rev(x)), c(a_min[,ind], rev(a_max[,ind])), col = 'grey80', border = NA)
  lines(a_hat[,ind], type="l", ylim=c(-2,2),
        lty = 1, lwd = 3, bty='l')
  title(teamname, line=0)
  
  par(new = T)
  g1 <- lapply(ind, function(x) which(epl$home_team %in% x))
  g2 <- lapply(ind, function(x) which(epl$away_team %in% x))
  g <- c(g1[[1]],g2[[1]])
  scd <- epl$score_diff[g] * rep(c(1,-1), each=19)
  aa <- a_hat[,ind]
  scd <- scd[order(g)]
  plot(scd, col = 2, pch=16, axes=F, xlab=NA, ylab=NA, cex=0.7, ylim=c(-6,6))
  axis(side = 4, col="red",col.axis="red",las=1)
}
title(xlab = "week",
      ylab = "team ability",
      outer = TRUE, line = 3, cex.lab=1.5)
mtext("score difference",side=4,col="red",line=-1.5, outer = TRUE) 
par(op)
invisible(dev.off())


############################################################################

## PLOTTIN ABILITY FOR INDIVIDUAL TEAM

library(rstan)
library (arm)
library(matrixStats)
season<-11
teamname <- "Man United"  
epl <- readRDS(paste('DATA/epl_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
a_sims<-readRDS(paste('FITS/sims_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
a_hat <- matrix(NA, nrow=38, ncol=20)
a_se <- matrix(NA, nrow=38, ncol=20)
for (w in 1:38) { 
  a_hat[w,] <- colMeans(a_sims[,w,])
  a_se[w,] <- sqrt(colVars(a_sims[,w,]))
}
a_min <- a_hat-a_se 
a_max <- a_hat+a_se
x<-c(1:38)
op <- par(oma = c(5,4,0,0) + 0.1,
          mar = c(0.8,0.8,4,4) + 0.1)
ind <- match(teamname, epl$team_names)
plot(a_hat[,ind], type="l", ylim=c(-2,2),
     lty = 1, lwd = 3, bty='l', xlab=NA, ylab=NA)
polygon(c(x, rev(x)), c(a_min[,ind], rev(a_max[,ind])), col = 'grey80', border = NA)
lines(a_hat[,ind], type="l", ylim=c(-2,2),
      lty = 1, lwd = 3, bty='l')
title(paste(teamname, "  -  season 20", sprintf("%02d",season), 
            "-20", sprintf("%02d",season+1),sep=""), line=0)
par(new = T)
g1 <- lapply(ind, function(x) which(epl$home_team %in% x))
g2 <- lapply(ind, function(x) which(epl$away_team %in% x))
g <- c(g1[[1]],g2[[1]])
scd <- epl$score_diff[g] * rep(c(1,-1), each=19)
aa <- a_hat[,ind]
scd <- scd[order(g)]
plot(scd, col = 2, pch=16, axes=F, xlab=NA, ylab=NA, cex=0.7, ylim=c(-6,6))
axis(side = 4, col="red",col.axis="red",las=1)
title(xlab = "week",
      ylab = "team ability",
      outer = TRUE, line = 2, cex.lab=1.2)
mtext("score difference",side=4,col="red",line=-1.5, outer = TRUE) 
par(op)



############################################################################

## INVESTIGATING PARAMETER ESTIMATES

nsamples=2000
w<-38
b_home <- array(NA, c(16,nsamples))
b_prev <- array(NA, c(16,nsamples))
tau_a <- array(NA, c(16,nsamples))
nu <- array(NA, c(16,nsamples))
sigma_y <- array(NA, c(16,nsamples))
sigma_a <- array(NA, c(16,nsamples,20))
for (season in 0:15) { 
  fit <- readRDS(paste('FITS/fit_', sprintf("%02d%02d_%02d", season, season+1, w), '.rds',sep=""))
  sims <- extract(fit)
  b_home[(season+1),] <- sims$b_home
  b_prev[(season+1),] <- sims$b_prev
  tau_a[(season+1),] <- sims$tau_a
  nu[(season+1),] <- sims$nu
  sigma_y[(season+1),] <- sims$sigma_y
  sigma_a[(season+1),,] <- sims$sigma_a
}


png ("b_home.png", height=4, width=6, units = 'in', res = 200)
plot(c(0:15), rowMeans(b_home),
     ylim=range(c(rowMeans(b_home)-rowSds(b_home), rowMeans(b_home)+rowSds(b_home))),
     pch=19, xlab="Season", ylab="B_home",
     main="Home Team Effect", xaxt = "n"
)
axis(1, at=c(0,5,10,15), labels=c("00-01", "05-06", "10-11", "15-16"))
# hack: we draw arrows but with very special "arrowheads"
arrows(c(0:15), rowMeans(b_home)-rowSds(b_home), 
       c(0:15), rowMeans(b_home)+rowSds(b_home), 
       length=0.05, angle=90, code=3)
dev.off()


png ("sigma_a.png", height=4, width=6, units = 'in', res = 200)
sigma_a<-apply(sigma_a, c(1,2), mean)
plot(c(0:15), rowMeans(sigma_a),
     ylim=range(c(rowMeans(sigma_a)-rowSds(sigma_a), rowMeans(sigma_a)+rowSds(sigma_a))),
     pch=19, xlab="Season", ylab="sigma_a",
     main="sigma_a", xaxt = "n"
)
axis(1, at=c(0,5,10,15), labels=c("00-01", "05-06", "10-11", "15-16"))
# hack: we draw arrows but with very special "arrowheads"
arrows(c(0:15), rowMeans(sigma_a)-rowSds(sigma_a), 
       c(0:15), rowMeans(sigma_a)+rowSds(sigma_a), 
       length=0.05, angle=90, code=3)
dev.off()


png ("sigma_y.png", height=4, width=6, units = 'in', res = 200)
plot(c(0:15), rowMeans(sigma_y),
     ylim=range(c(rowMeans(sigma_y)-rowSds(sigma_y), rowMeans(sigma_y)+rowSds(sigma_y))),
     pch=19, xlab="Season", ylab="sigma_y",
     main="Score Difference S.E.", xaxt = "n"
)
axis(1, at=c(0,5,10,15), labels=c("00-01", "05-06", "10-11", "15-16"))
# hack: we draw arrows but with very special "arrowheads"
arrows(c(0:15), rowMeans(sigma_y)-rowSds(sigma_y), 
       c(0:15), rowMeans(sigma_y)+rowSds(sigma_y), 
       length=0.05, angle=90, code=3)
dev.off()


png ("b_prev.png", height=4, width=6, units = 'in', res = 200)
b_prev <- b_prev[2:16,]
plot(c(1:15), rowMeans(b_prev),
     ylim=range(c(rowMeans(b_prev)-rowSds(b_prev), rowMeans(b_prev)+rowSds(b_prev))),
     pch=19, xlab="Season", ylab="B_prev",
     main="Coef. of Previous Performance", xaxt = "n"
)
axis(1, at=c(1,5,10,15), labels=c("01-02", "05-06", "10-11", "15-16"))
# hack: we draw arrows but with very special "arrowheads"
arrows(c(1:15), rowMeans(b_prev)-rowSds(b_prev), 
       c(1:15), rowMeans(b_prev)+rowSds(b_prev), 
       length=0.05, angle=90, code=3)
dev.off()


png ("tau_a.png", height=4, width=6, units = 'in', res = 200)
plot(c(0:15), rowMeans(tau_a),
     ylim=range(c(rowMeans(tau_a)-rowSds(tau_a), rowMeans(tau_a)+rowSds(tau_a))),
     pch=19, xlab="Season", ylab="tau_a",
     main="Game-to-game Variation S.E.", xaxt = "n"
)
axis(1, at=c(0,5,10,15), labels=c("00-01", "05-06", "10-11", "15-16"))
# hack: we draw arrows but with very special "arrowheads"
arrows(c(0:15), rowMeans(tau_a)-rowSds(tau_a), 
       c(0:15), rowMeans(tau_a)+rowSds(tau_a), 
       length=0.05, angle=90, code=3)
dev.off()





season<-15
b_home <- array(NA, c(38,nsamples))
b_prev <- array(NA, c(38,nsamples))
nu <- array(NA, c(38,nsamples))
sigma_y <- array(NA, c(38,nsamples))
for (w in 1:38) { 
  fit <- readRDS(paste('FITS/fit_', sprintf("%02d%02d_%02d", season, season+1, w), '.rds',sep=""))
  sims <- extract(fit)
  b_home[w,] <- sims$b_home
  b_prev[w,] <- sims$b_prev
  nu[w,] <- sims$nu
  sigma_y[w,] <- sims$sigma_y
}

png ("b_home_15.png", height=4, width=6, units = 'in', res = 200)
plot(c(1:38), rowMeans(b_home), type="l", lwd=3,
     ylim=range(c(rowMeans(b_home)-rowSds(b_home), rowMeans(b_home)+rowSds(b_home))),
     pch=19, xlab="Week", ylab="B_home")
# hack: we draw arrows but with very special "arrowheads"
arrows(c(1:38), rowMeans(b_home)-rowSds(b_home), 
       c(1:38), rowMeans(b_home)+rowSds(b_home), 
       length=0.03, angle=90, code=3, lwd=1.5)
dev.off()


png ("b_prev_15.png", height=4, width=6, units = 'in', res = 200)
plot(c(1:38), rowMeans(b_prev), type="l", lwd=3,
     ylim=range(c(rowMeans(b_prev)-rowSds(b_prev), rowMeans(b_prev)+rowSds(b_prev))),
     pch=19, xlab="Week", ylab="B_prev")
# hack: we draw arrows but with very special "arrowheads"
arrows(c(1:38), rowMeans(b_prev)-rowSds(b_prev), 
       c(1:38), rowMeans(b_prev)+rowSds(b_prev), 
       length=0.03, angle=90, code=3, lwd=1.5)
dev.off()

png ("sigma_y_15.png", height=4, width=6, units = 'in', res = 200)
plot(c(1:38), rowMeans(sigma_y), type="l", lwd=3,
     ylim=range(c(rowMeans(sigma_y)-rowSds(sigma_y), rowMeans(sigma_y)+rowSds(sigma_y))),
     pch=19, xlab="Week", ylab="sigma_y")
# hack: we draw arrows but with very special "arrowheads"
arrows(c(1:38), rowMeans(sigma_y)-rowSds(sigma_y), 
       c(1:38), rowMeans(sigma_y)+rowSds(sigma_y), 
       length=0.03, angle=90, code=3, lwd=1.5)
dev.off()


############################################################################

## PREDICTION ANALYSIS FOR SPECIFIC MATCHES

nsamples=2000
library(rstan)
library(matrixStats)
season<-15
epl <- readRDS(paste('DATA/epl_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
a_sims<-readRDS(paste('FITS/sims_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
b_home <- array(NA, c(38,nsamples))
nu <- array(NA, c(38,nsamples))
sigma_y <- array(NA, c(38,nsamples))
for (w in 1:38) { 
  fit <- readRDS(paste('FITS/fit_', sprintf("%02d%02d_%02d", season, season+1, w), '.rds',sep=""))
  sims <- extract(fit)
  b_home[w,] <- sims$b_home
  nu[w,] <- sims$nu
  sigma_y[w,] <- sims$sigma_y
}
score_diff_pred <- array(NA, c(380,nsamples))
set.seed(1);
rt_ls <- function(n, df, mu, a) rt(n,df)*a + mu
for (i in 11:380) {
  w <- ceiling(i/10)
  for (j in 1:nsamples) {
    score_diff_pred[i,j] <- 
      rt_ls(1, nu[w-1,j], 
            a_sims[j,epl$home_week[i]-1, epl$home_team[i]] - 
              a_sims[j,epl$away_week[i]-1, epl$away_team[i]] +
              b_home[w-1,j],
            sigma_y[w-1,j]);
  }
}


game_ind1 <- which(epl$home_team==1 & epl$away_team==12) # Arsenal vs Norwich

png ("ars_nor.png", height=4, width=6, units = 'in', res = 200)
opar <- par(lwd=3)
hist(score_diff_pred[game_ind1,], breaks=seq(-4.5,7.5,by=1), xlab = "Arsenal Goals - Norwich Goals", main="")
axis(side=1,lwd=3)
axis(side=2,lwd=3)
lines(c(epl$score_diff[game_ind1],epl$score_diff[game_ind1]),c(0,500), col="red", lwd=6)
par(opar)
dev.off()

game_ind2 <- which(epl$home_team==2 & epl$away_team==4) # Aston Villa vs Chelsea


png ("asv_che.png", height=4, width=6, units = 'in', res = 200)
opar <- par(lwd=3)
hist(score_diff_pred[game_ind2,], breaks=seq(-10.5,10.5,by=1), xlab = "Aston Villa Goals - Chelsea Goals", main="", xlim = c(-7,5))
axis(side=1,lwd=3)
axis(side=2,lwd=3)
lines(c(epl$score_diff[game_ind2],epl$score_diff[game_ind2]),c(0,500), col="red", lwd=6)
par(opar)
dev.off()


##############################################################################################################

## MODEL CHECKING VIA POSTERIOR PREDICTIVE CHECKS

accuracy <- array(NA,16)
c1 <- array(NA,16)
c2 <- array(NA,16)

nsamples=2000
library(rstan)
library(matrixStats)
for (season in 0:15) {
  epl <- readRDS(paste('DATA/epl_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
  a_sims<-readRDS(paste('FITS/sims_', sprintf("%02d%02d", season, season+1), '.rds',sep=""))
  b_home <- array(NA, c(38,nsamples))
  nu <- array(NA, c(38,nsamples))
  sigma_y <- array(NA, c(38,nsamples))
  for (w in 1:38) { 
    fit <- readRDS(paste('FITS/fit_', sprintf("%02d%02d_%02d", season, season+1, w), '.rds',sep=""))
    sims <- extract(fit)
    b_home[w,] <- sims$b_home
    nu[w,] <- sims$nu
    sigma_y[w,] <- sims$sigma_y
  }
  score_diff_pred <- array(0, c(380,nsamples))
  set.seed(1);
  rt_ls <- function(n, df, mu, a) rt(n,df)*a + mu
  for (i in 11:380) {
    w <- ceiling(i/10)
    for (j in 1:nsamples) {
      if ((epl$home_week[i]-1)>0 & epl$away_week[i]-1>0)
      score_diff_pred[i,j] <- 
        rt_ls(1, nu[w-1,j], 
              a_sims[j,epl$home_week[i]-1, epl$home_team[i]] - 
                a_sims[j,epl$away_week[i]-1, epl$away_team[i]] +
                b_home[w-1,j],
              sigma_y[w-1,j]);
    }
  }
  

  
  scd <- epl$score_diff[191:380]
  scd_sims <- t(score_diff_pred[191:380,])
  alpha <- 0.95
  scd_ub <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2, na.rm = TRUE)
  scd_lb <- colQuantiles(scd_sims, probs = (1-alpha)/2, na.rm = TRUE)
  cip95 <- sum(scd < scd_ub & scd_lb<scd)/190
  alpha <- 0.5
  scd_ub <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2, na.rm = TRUE)
  scd_lb <- colQuantiles(scd_sims, probs = (1-alpha)/2, na.rm = TRUE)
  cip50 <- sum(scd < scd_ub & scd_lb<scd)/190
  c1[season+1] <- cip50
  c2[season+1] <- cip95
  
  scd_sims_hat <- round(score_diff_pred[191:380,])
  est_probs <- array(NA,c(190,3))
  est_probs[,1] <- rowSums(scd_sims_hat>0)/nsamples;
  est_probs[,2] <- rowSums(scd_sims_hat==0)/nsamples;
  est_probs[,3] <- rowSums(scd_sims_hat<0)/nsamples;
 
  count_est<-0
  for (i in 1:190) {
    if (scd[i]>0 & est_probs[i,1]>est_probs[i,3] & est_probs[i,1]>est_probs[i,2]) {
      count_est<-count_est+1;
    }
    if (scd[i]<0 & est_probs[i,1]<est_probs[i,3] & est_probs[i,2]<est_probs[i,3]) {
      count_est<-count_est+1;
    }
    if (scd[i]==0 & est_probs[i,1]<est_probs[i,2] & est_probs[i,3]<est_probs[i,2]){
      count_est<-count_est+1;
    }
  }
  accuracy[season+1] <- count_est/190
}




bet_probs <- array(NA,c(190,3))
bet_probs[,1] <- 1/epl$bet_home[191:380]
bet_probs[,2] <- 1/epl$bet_draw[191:380]
bet_probs[,3] <- 1/epl$bet_away[191:380]
rowsum <- rowSums(bet_probs);
bet_probs[,1] <- bet_probs[,1]/rowsum
bet_probs[,2] <- bet_probs[,2]/rowsum
bet_probs[,3] <- bet_probs[,3]/rowsum

png ("scatter.png", height=5, width=5, units = 'in', res = 200)
opar <- par(lwd=2)
plot(bet_probs[,1], est_probs[,1], ylab="Estimated probability of home win", xlab="BET365 probability of home win",
     xlim = c(0.1,0.85),ylim = c(0.1,0.85))
lines(c(0,0.9), c(0,0.9), col="red")
axis(side=1,lwd=2)
axis(side=2,lwd=2)
par(opar)
dev.off()

cor(est_probs[,1],bet_probs[,1])
cor(est_probs[,2],bet_probs[,2])
cor(est_probs[,3],bet_probs[,3])


##############################################################################################################

## PLOTTING POSTERIOR PR#EDICTIVE DIST

library(matrixStats)
scd <- epl$score_diff[191:380]
scd_sims <- t(score_diff_pred[191:380,])
scd_hat <- colMedians(scd_sims, na.rm=TRUE)
scd_se <- sqrt(colVars(scd_sims, na.rm=TRUE))
scd_ub <- scd_hat + 1.95 * scd_se;
scd_lb <- scd_hat - 1.95 * scd_se;
scd_ub2 <- scd_hat + 0.67 * scd_se;
scd_lb2 <- scd_hat - 0.67 * scd_se;

sort_scd <- scd[order(scd)]
sort_scd_hat <- scd_hat[order(scd)]
sort_scd_se <- scd_se[order(scd)]
sort_scd_ub <- scd_ub[order(scd)]
sort_scd_lb <- scd_lb[order(scd)]
sort_scd_ub2 <- scd_ub2[order(scd)]
sort_scd_lb2 <- scd_lb2[order(scd)]
df <- data.frame(list(scd = sort_scd, scd_hat = sort_scd_hat, scd_se = sort_scd_se, 
                      scd_ub = sort_scd_ub, scd_lb = sort_scd_lb, 
                      scd_ub2 = sort_scd_ub2, scd_lb2 = sort_scd_lb2))

png ("ppc.png", height=5, width=12, units = 'in', res = 200)
ggplot(df, aes(x = c(1:190))) +
  geom_ribbon(aes(ymin = scd_lb,
                  ymax = scd_ub),
              fill="dodgerblue2") + 
  geom_ribbon(aes(ymin = scd_lb2,
                  ymax = scd_ub2),
              fill="dodgerblue4") + 
  geom_line(aes(y=scd_hat),colour="white") + 
  geom_point(aes(y=scd), size = 0.7, colour="gold") +
  scale_x_continuous(name="match") +
  scale_y_continuous(name="score difference", minor_breaks = seq(-6, 6, 1), 
                     sec.axis = dup_axis()) #+
  #ggtitle("Predicted score differences (red) with 95% intervals (light yellow), \n  50% intervals (dark yellow), and the actual score differences (black)");
dev.off()

scd <- epl$score_diff[191:380]
scd_sims <- t(score_diff_pred[191:380,])
scd_hat <- colMedians(scd_sims, na.rm = TRUE)
alpha <- 0.95
scd_ub <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2, na.rm = TRUE)
scd_lb <- colQuantiles(scd_sims, probs = (1-alpha)/2, na.rm = TRUE)
cip95 <- sum(scd < scd_ub & scd_lb<scd)/190
alpha <- 0.5
scd_ub <- colQuantiles(scd_sims, probs = 1-(1-alpha)/2, na.rm = TRUE)
scd_lb <- colQuantiles(scd_sims, probs = (1-alpha)/2, na.rm = TRUE)
cip50 <- sum(scd < scd_ub & scd_lb<scd)/190