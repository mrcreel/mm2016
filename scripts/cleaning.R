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
                  team1pts = ifelse(Wteam < Lteam, Wscore, Lscore),
                  team2pts = ifelse(Wteam < Lteam, Lscore, Wscore),
                  team1fgm = ifelse(Wteam < Lteam, Wfgm, Lfgm),
                  team2fgm = ifelse(Wteam < Lteam, Lfgm, Wfgm),
                  team1fga = ifelse(Wteam < Lteam, Wfga, Lfga),
                  team2fga = ifelse(Wteam < Lteam, Lfga, Wfga),
                  team1fgm3 = ifelse(Wteam < Lteam, Wfgm3, Lfgm3),
                  team2fgm3 = ifelse(Wteam < Lteam, Lfgm3, Wfgm3),
                  team1fga3 = ifelse(Wteam < Lteam, Wfga3, Lfga3),
                  team2fga3 = ifelse(Wteam < Lteam, Lfga3, Wfga3),
                  result = ifelse(Wteam < Lteam, 1, 0),
                  team1loc = Wloc,
                  team1loc = ifelse(result == 0 & Wloc == "H", "A", 
                                    ifelse(result == 0 & Wloc == "A",
                                           "H", team1loc))
            ) %>%
    dplyr::select(season = Season, daynum = Daynum, 
                 team1, team1pts, team2, team2pts, result, team1loc, Numot,
                 team1fgm, team2fgm, team1fga, team2fga, team1fgm3, team2fgm3,
                 team1fga3, team2fga3
                 ) %>%
    as.data.frame()
    
  return(temp)
}

season.stats <- function(season = 1985){
  
  df <- season.games(season)
  
  temp <- rbind(
    df %>%
      dplyr::select(season, team = team1, opp = team2, 
                    pf = team1pts, pa = team2pts,
                    fgmade = team1fgm, fgallowed = team2fgm,
                    fgatt = team1fga, fgattall = team2fga,
                    made3 = team1fgm3, all3 = team2fgm3,
                    att3 = team1fga3, against3 = team2fga3),
    df %>%
      dplyr::select(season, team = team2, opp = team1, 
                    pf = team2pts, pa = team1pts,
                    fgmade = team2fgm, fgallowed = team1fgm,
                    fgatt = team2fga, fgattall = team1fga,
                    made3 = team2fgm3, all3 = team1fgm3,
                    att3 = team2fga3, against3 = team1fga3)
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
      fga3.against = sum(against3)
    ) %>%
    ungroup() %>%
    as.data.frame()
  
  return(temp)
  
  }