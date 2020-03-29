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


for(i in unique(nhl.clean$team_id)){
  data=filter(nhl.clean,team_id==i) %>%
          select(Season,away_team_id,home_team_id, first_goal_win) %>%
          group_by(Season) %>%
          summarize(rate=mean(first_goal_win)) %>%
          cbind(as.character(i)) %>%
          mutate(Season=Season %/% 10000) %>%
          rename(`First Goal Wins`=rate,Team="as.character(i)")
  season.fgw=rbind(season.fgw,data)
}



for(i in 1:nrow(nhl.clean)){
  if(nhl.clean$team_id[i] == 52){
    nhl.clean$team_id[i] = 31
  } else if(nhl.clean$team_id[i] == 53){
    nhl.clean$team_id[i] = 32
  } else if(nhl.clean$team_id[i] == 54){
    nhl.clean$team_id[i] = 33
  }
}

for(i in 1:nrow(nhl.clean)){
  if(nhl.clean$away_team_id[i] == 52){
    nhl.clean$away_team_id[i] = 31
  } else if(nhl.clean$away_team_id[i] == 53){
    nhl.clean$away_team_id[i] = 32
  } else if(nhl.clean$away_team_id[i] == 54){
    nhl.clean$away_team_id[i] = 33
  }
}

for(i in 1:nrow(nhl.clean)){
  if(nhl.clean$home_team_id[i] == 52){
    nhl.clean$home_team_id[i] = 31
  } else if(nhl.clean$home_team_id[i] == 53){
    nhl.clean$home_team_id[i] = 32
  } else if(nhl.clean$home_team_id[i] == 54){
    nhl.clean$home_team_id[i] = 33
  }
  if(nhl.clean$team_id[i] == nhl.clean$away_team_id[i]){
    nhl.clean$other.id[i] = nhl.clean$home_team_id[i]
  }else{
    nhl.clean$other.id[i] = nhl.clean$away_team_id[i]
  }
}

#Summarize Betting Variable by H vs A
nhl.clean %>%
  select(away_team_id,home_team_id,first_goal_win) %>%
  group_by(away_team_id,home_team_id) %>%
  summarize(rate=mean(first_goal_win)) %>%
  spread(away_team_id,rate)

HA.tile = nhl.clean %>%
  select(away_team_id,home_team_id,first_goal_win) %>%
  group_by(away_team_id,home_team_id) %>%
  summarize(rate=mean(first_goal_win))

ggplot(HA.tile, aes(away_team_id, home_team_id)) + geom_tile(aes(fill = rate))

#Summarize Betting Variable with no Regard for H vs A

total.tile = nhl.clean %>%
  select(team_id, other.id, first_goal_win) %>%
  group_by(team_id, other.id) %>%
  summarize(rate=mean(first_goal_win))

ggplot(total.tile, aes(team_id, other.id)) + geom_tile(aes(fill = rate))

#Average FGW over the course of the dataset

target = mean(nhl.clean$first_goal_win)

#Summarize Betting Variable within Divisions
division = filter(nhl.clean, game_type == 1)

tileplotclose.func = function(data){
  data = data %>% select(home_team_name, away_team_name, first_goal_win) %>%
    group_by(home_team_name, away_team_name) %>%
    summarize(rate=mean(first_goal_win))
  data$OU = NA
  
  for(i in 1:nrow(data)){
    if(data$rate[i] > .7){
      data$OU[i] = 2
    }else if(data$rate[i] < .66){
      data$OU[i] = 1
    }else if(data$rate[i] >= .66 & data$rate[i] <= .7) {
      data$OU[i] = 0
    }
  }
  
  ggplot(data, aes(home_team_name, away_team_name)) + geom_tile(aes(fill = OU))+ theme(axis.text.x=element_text(angle=90, hjust=1))
}


atlantic1 = filter(division, home_division == "Atlantic")
central1 = filter(division, home_division == "Central")
tileplotclose.func(central1)
Metropolitan1 = filter(division, home_division == "Metropolitan")
Northeast1 = filter(division, home_division == "Northeast")
Northwest1 = filter(division, home_division == "Northwest")
Pacific1 = filter(division, home_division == "Pacific")
Southeast1 = filter(division, home_division == "Southeast")

#Summarize Betting Variable within Conferences
conference = filter(nhl.clean, game_type == 2)

atlantic2 = filter(conference, home_division == "Atlantic")
central2 = filter(conference, home_division == "Central")
Metropolitan2 = filter(conference, home_division == "Metropolitan")
Northeast2 = filter(conference, home_division == "Northeast")
Northwest2 = filter(conference, home_division == "Northwest")
Pacific2 = filter(conference, home_division == "Pacific")
Southeast2 = filter(conference, home_division == "Southeast")

##Summarize Betting Variable in out of conference
ooc = filter(nhl.clean, game_type == 3)

atlantic3 = filter(ooc, home_division == "Atlantic")
central3 = filter(ooc, home_division == "Central")
Metropolitan3 = filter(ooc, home_division == "Metropolitan")
Northeast3 = filter(ooc, home_division == "Northeast")
Northwest3 = filter(ooc, home_division == "Northwest")
Pacific3 = filter(ooc, home_division == "Pacific")
Southeast3 = filter(ooc, home_division == "Southeast")


tileplotclose.func(atlantic1)
tileplotclose.func(central1)
tileplotclose.func(Metropolitan1)
tileplotclose.func(Northeast1)
tileplotclose.func(Northwest1)
tileplotclose.func(Pacific1)
tileplotclose.func(Southeast1)

tileplotclose.func(atlantic2)
tileplotclose.func(central2)
tileplotclose.func(Metropolitan2)
tileplotclose.func(Northeast2)
tileplotclose.func(Northwest2)
tileplotclose.func(Pacific2)
tileplotclose.func(Southeast2)

tileplotclose.func(atlantic3)
tileplotclose.func(central3)
tileplotclose.func(Metropolitan3)
tileplotclose.func(Northeast3)
tileplotclose.func(Northwest3)
tileplotclose.func(Pacific3)
tileplotclose.func(Southeast3)

count = nhl.clean %>%
    select(team_id, other.id) %>%
    count(team_id, other.id)

MOE.data = left_join(total.tile, count) %>%
    mutate(ME = 1.96*sqrt(rate/n), upper = rate+ME, lower = rate - ME)