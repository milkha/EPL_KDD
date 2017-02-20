library(plyr)
map_to_score <- function(x) { 
  x_max <- max(x);   x_min <- min(x);
  return(2*x/(x_max-x_min) - (x_max+x_min)/(x_max-x_min))
}
for (i in 0:15) {
  url_csv <- paste("http://www.football-data.co.uk/mmz4281/", 
                   sprintf("%02d%02d", i, i+1), "/E0.csv", sep=""); 
  # Data downloaded from football-data.co.uk
  mydat   <- read.csv(url(url_csv)); epl <- c();
  mydat <- mydat[!is.na(mydat$FTHG),]
  mydat$HomeTeam <- factor(mydat$HomeTeam)
  mydat$AwayTeam <- factor(mydat$AwayTeam)
  # teams are assigned IDs 1, 2, ...:
  epl$home_team       <- as.numeric(mydat$HomeTeam)  
  epl$away_team       <- as.numeric(mydat$AwayTeam)
  epl$team_names      <- levels(mydat$HomeTeam)
  epl$home_goals      <- mydat$FTHG # FTHG: full time home goals
  epl$away_goals      <- mydat$FTAG # FTHG: full time away goals
  epl$score_diff      <- epl$home_goals - epl$away_goals
  epl$nteams          <- length(unique(epl$home_team))
  epl$ngames          <- length(epl$score_diff)
  epl$nweeks          <- floor(2*epl$ngames/epl$nteams)
  # The following code computes the week for each team in their games:
  epl$home_week <- c();   epl$away_week <- c();
  epl$team_GF <- array(0, epl$nteams)
  epl$team_GA <- array(0, epl$nteams)
  epl$team_pts <- array(0, epl$nteams)
  for (g in 1:epl$ngames) {
    epl$home_week[g]  <-  sum(epl$home_team[1:g] == epl$home_team[g]) + 
      sum(epl$away_team[1:g] == epl$home_team[g]) 
    epl$away_week[g]  <-  sum(epl$away_team[1:g] == epl$away_team[g]) +
      sum(epl$home_team[1:g] == epl$away_team[g])
    epl$team_GF[epl$home_team[g]] <- epl$team_GF[epl$home_team[g]] + epl$home_goals[g]
    epl$team_GF[epl$away_team[g]] <- epl$team_GF[epl$away_team[g]] + epl$away_goals[g]
    epl$team_GA[epl$home_team[g]] <- epl$team_GA[epl$home_team[g]] + epl$away_goals[g]
    epl$team_GA[epl$away_team[g]] <- epl$team_GA[epl$away_team[g]] + epl$home_goals[g]
    if (epl$score_diff[g]>0)
      epl$team_pts[epl$home_team[g]] <- epl$team_pts[epl$home_team[g]] + 3
    else if (epl$score_diff[g]<0)
      epl$team_pts[epl$away_team[g]] <- epl$team_pts[epl$away_team[g]] + 3
    else if (epl$score_diff[g]==0) {
      epl$team_pts[epl$home_team[g]] <- epl$team_pts[epl$home_team[g]] + 1
      epl$team_pts[epl$away_team[g]] <- epl$team_pts[epl$away_team[g]] + 1
    }
  }
  epl$bet_home <- mydat$B365H; # Betting odds for home team win
  epl$bet_draw <- mydat$B365D; # Betting odds for draw
  epl$bet_away <- mydat$B365A; # Betting odds for away team win
  if (i==0) {
    epl$prev_perf <- array(0,20)
    epl_prev <- epl
  }
  else {
    for (j in 1:epl$nteams) {
      ind <- which(epl$team_names[j] == epl_prev$team_names)
      if (length(ind)>0) {epl$prev_perf[j] <- epl_prev$team_pts[ind]}
      else {epl$prev_perf[j] <- min(epl_prev$team_pts)}
    }
    epl$prev_perf <- map_to_score(epl$prev_perf)
    epl_prev <- epl    
  }
  saveRDS(epl, paste('DATA/epl_', sprintf("%02d%02d", i, i+1), '.rds',sep=""))
}

