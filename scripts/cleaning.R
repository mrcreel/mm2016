# Basic machine learning using data from Kaggles March Madness 2016 Competition
# Written in Ubuntu64 16.10/R 3.2.3/RStudio 0.99.902

## LOAD DATA ####
data_RegCompact <- read.csv("./data/RegularSeasonCompactResults.csv")
data_TourneyResults <- read.csv("./data/TourneyCompactResults.csv")

## LOAD PACKAGES ####
library(dplyr)
library(tidyr)

## CLEAN DATASETS ####
func_season <- function(season = 1985){
  dfx <- data_RegCompact[data_RegCompact$Season == season,]
  
  temp <- dfx %>%
    dplyr::mutate(team1 = ifelse(Wteam < Lteam, Wteam, Lteam),
                  team2 = ifelse(Wteam < Lteam, Lteam, Wteam),
                  team1pts = ifelse(Wteam < Lteam, Wscore, Lscore),
                  team2pts = ifelse(Wteam < Lteam, Lscore, Wscore)
           ) %>%
    dplyr::select(season = Season, daynum = Daynum, 
                  team1, team1pts, team2, team2pts)
  
  return(temp)
}
