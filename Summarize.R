#Put Necessary Libraries Here
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(plotly)
library(ggplot2)
library(tidyverse)
library(modelr)
library(broom)
library(glmnet)
library(kableExtra)
library(scales)

#Load cleaned data from Variable_Addition.R
nhl.clean=read.csv('nhl-game-data/cleaned_NHL.csv')

#Summarize Betting Variablity by Team
season.fgw=nhl.clean %>%
  group_by(Season) %>%
  summarize(rate=mean(first_goal_win)) %>%
  mutate(Season=Season %/% 10000) %>%
  cbind(Team="All") %>%
  rename(`First Goal Wins`=rate)

for(i in unique(nhl.clean$team_ID)){
  data=filter(nhl.clean,team_ID==i) %>%
          select(Season,away_team_id,home_team_id, first_goal_win) %>%
          group_by(Season) %>%
          summarize(rate=mean(first_goal_win)) %>%
          cbind(as.character(i)) %>%
          mutate(Season=Season %/% 10000) %>%
          rename(`First Goal Wins`=rate,Team="as.character(i)")
  season.fgw=rbind(season.fgw,data)
}

#Summarize Betting Variable by H vs A
nhl.clean %>%
  select(away_team_id,home_team_id,first_goal_win) %>%
  group_by(away_team_id,home_team_id) %>%
  summarize(rate=mean(first_goal_win)) %>%
  spread(away_team_id,rate)



