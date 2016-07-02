# Basic machine learning using data from Kaggles March Madness 2016 Competition
# Written in Ubuntu64 16.10/R 3.2.3/RStudio 0.99.902

## LOAD DATA ####
data_Regular <- read.csv("./data/RegularSeasonDetailedResults.csv",
                            stringsAsFactors = FALSE)
data_TourneyResults <- read.csv("./data/TourneyCompactResults.csv",
                                stringsAsFactors = FALSE)

## LOAD PACKAGES ####
library(dplyr)
library(tidyr)

## CLEAN DATASETS ####
# Convert the regular season game results so that the lowest team ID is team 1
season.games <- function(season = 1985){
  dfx <- data_Regular[data_Regular$Season == season,]
  dfx$Wloc <- as.character(dfx$Wloc)
  
  temp <- dfx %>%
    dplyr::mutate(team1 = as.factor(ifelse(Wteam < Lteam, Wteam, Lteam)),
                  team2 = as.factor(ifelse(Wteam < Lteam, Lteam, Wteam)),
                  team1.pts = ifelse(Wteam < Lteam, Wscore, Lscore),
                  team2.pts = ifelse(Wteam < Lteam, Lscore, Wscore),
                  team1.fgm = ifelse(Wteam < Lteam, Wfgm, Lfgm),
                  team2.fgm = ifelse(Wteam < Lteam, Lfgm, Wfgm),
                  team1.fga = ifelse(Wteam < Lteam, Wfga, Lfga),
                  team2.fga = ifelse(Wteam < Lteam, Lfga, Wfga),
                  team1.fgm3 = ifelse(Wteam < Lteam, Wfgm3, Lfgm3),
                  team2.fgm3 = ifelse(Wteam < Lteam, Lfgm3, Wfgm3),
                  team1.fga3 = ifelse(Wteam < Lteam, Wfga3, Lfga3),
                  team2.fga3 = ifelse(Wteam < Lteam, Lfga3, Wfga3),
                  team1.ftm = ifelse(Wteam < Lteam, Wftm, Lftm),
                  team2.ftm = ifelse(Wteam < Lteam, Lftm, Wftm),
                  team1.fta = ifelse(Wteam < Lteam, Wfta, Lfta),
                  team2.fta = ifelse(Wteam < Lteam, Lfta, Wfta),
                  result = ifelse(Wteam < Lteam, 1, 0),
                  team1.loc = Wloc,
                  team1.loc = ifelse(result == 0 & Wloc == "H", "A", 
                                    ifelse(result == 0 & Wloc == "A",
                                           "H", team1loc))
            ) %>%
    dplyr::select(season = Season, daynum = Daynum, 
                 team1, team1.pts, team2, team2.pts, result, team1.loc, Numot,
                 team1.fgm, team2.fgm, team1.fga, team2.fga, 
                 team1.fgm3, team2.fgm3, team1.fga3, team2.fga3, 
                 team1.ftm, team2.ftm, team1.fta, team2.fta
                 ) %>%
    as.data.frame()
    
  return(temp)
}

season.stats <- function(season = 1985){
  
  df <- season.games(season)
  
  temp <- rbind(
    df %>%
      dplyr::select(season, team = team1, opp = team2, 
                    pf = team1.pts, pa = team2.pts,
                    fgmade = team1.fgm, fgallowed = team2.fgm,
                    fgatt = team1.fga, fgattall = team2.fga,
                    made3 = team1.fgm3, all3 = team2.fgm3,
                    att3 = team1.fga3, against3 = team2.fga3,
                    ftmade = team1.ftm, ftallowed = team2.ftm,
                    ftfor = team1.fta, ftagainst = team2.fta),
    df %>%
      dplyr::select(season, team = team2, opp = team1, 
                    pf = team2.pts, pa = team1.pts,
                    fgmade = team2.fgm, fgallowed = team1.fgm,
                    fgatt = team2.fga, fgattall = team1.fga,
                    made3 = team2.fgm3, all3 = team1.fgm3,
                    att3 = team2.fga3, against3 = team1.fga3,
                    ftmade = team2.ftm, ftallowed = team1.ftm,
                    ftfor = team2.fta, ftagainst = team1.fta)
  ) %>%
    dplyr::mutate(ptDiff = pf - pa) %>%
    dplyr::group_by(season, team) %>%
    dplyr::summarise(
      G = sum(ptDiff <= 300),
      W = sum(ptDiff > 0),
      L = sum(ptDiff < 0),
      pt.for = sum(pf),
      pt.against = sum(pa),
      fgm.for = sum(fgmade),
      fgm.against = sum(fgallowed),
      fga.for = sum(fgatt),
      fga.against = sum(fgattall),
      fgm3.for = sum(made3),
      fgm3.against = sum(all3),
      fga3.for = sum(att3),
      fga3.against = sum(against3),
      ftm.for = sum(ftmade),
      ftm.against = sum(ftallowed),
      fta.for = sum(ftfor),
      fta.against = sum(ftagainst)
    ) %>%
    ungroup() %>%
    as.data.frame()
  
  return(temp)
  
}

season <- season.stats(2015)
games <- season.games(2015)