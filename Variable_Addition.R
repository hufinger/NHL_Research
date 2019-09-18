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

full_data = read.csv("nhl-game-data/game_outcomes.csv")

reg_2010 = full_data %>% filter(season == '20102011') %>% filter(event != 'Game Scheduled' & event != 'Period Ready')
#write.csv(reg_2010, file = "nhl-game-data/2010season.csv")
reg_2011 = full_data %>% filter(season == '20112012') %>% filter(event != 'Game Scheduled' & event != 'Period Ready')
#write.csv(reg_2011, file = "nhl-game-data/2011season.csv")
reg_2012 = full_data %>% filter(season == '20122013') %>% filter(event != 'Game Scheduled' & event != 'Period Ready')
#write.csv(reg_2012, file = "nhl-game-data/2012season.csv")
reg_2013 = full_data %>% filter(season == '20132014') %>% filter(event != 'Game Scheduled' & event != 'Period Ready')
#write.csv(reg_2013, file = "nhl-game-data/2013season.csv")
reg_2014 = full_data %>% filter(season == '20142015') %>% filter(event != 'Game Scheduled' & event != 'Period Ready')
#write.csv(reg_2014, file = "nhl-game-data/2014season.csv")
reg_2015 = full_data %>% filter(season == '20152016') %>% filter(event != 'Game Scheduled' & event != 'Period Ready')
#write.csv(reg_2015, file = "nhl-game-data/2015season.csv")
reg_2016 = full_data %>% filter(season == '20162017') %>% filter(event != 'Game Scheduled' & event != 'Period Ready')
#write.csv(reg_2016, file = "nhl-game-data/2016season.csv")
reg_2017 = full_data %>% filter(season == '20172018') %>% filter(event != 'Game Scheduled' & event != 'Period Ready')
#write.csv(reg_2017, file = "nhl-game-data/2017season.csv")
reg_2018 = full_data %>% filter(season == '20182019') %>% filter(event != 'Game Scheduled' & event != 'Period Ready')
#write.csv(reg_2018, file = "nhl-game-data/2018season.csv")


season2010 = reg_2010
season2011 = reg_2011
season2012 = reg_2012
season2013 = reg_2013
season2014 = reg_2014
season2015 = reg_2015
season2016 = reg_2016
season2017 = reg_2017
season2018 = reg_2018

functiondata2010 = season2010 %>% filter(goals_home == 1 & goals_away == 0 & event == 'Goal' | goals_home == 0 & goals_away == 1 & event == 'Goal') %>% select(game_id, goals_home, goals_away, home_goals_final, away_goals_final)

functiondata2011 = season2011 %>% filter(goals_home == 1 & goals_away == 0 & event == 'Goal' | goals_home == 0 & goals_away == 1 & event == 'Goal') %>% select(game_id, goals_home, goals_away, home_goals_final, away_goals_final)

functiondata2012 = season2012 %>% filter(goals_home == 1 & goals_away == 0 & event == 'Goal' | goals_home == 0 & goals_away == 1 & event == 'Goal') %>% select(game_id, goals_home, goals_away, home_goals_final, away_goals_final)

functiondata2013 = season2013 %>% filter(goals_home == 1 & goals_away == 0 & event == 'Goal' | goals_home == 0 & goals_away == 1 & event == 'Goal') %>% select(game_id, goals_home, goals_away, home_goals_final, away_goals_final)

functiondata2014 = season2014 %>% filter(goals_home == 1 & goals_away == 0 & event == 'Goal' | goals_home == 0 & goals_away == 1 & event == 'Goal') %>% select(game_id, goals_home, goals_away, home_goals_final, away_goals_final)

functiondata2015 = season2015 %>% filter(goals_home == 1 & goals_away == 0 & event == 'Goal' | goals_home == 0 & goals_away == 1 & event == 'Goal') %>% select(game_id, goals_home, goals_away, home_goals_final, away_goals_final)

functiondata2016 = season2016 %>% filter(goals_home == 1 & goals_away == 0 & event == 'Goal' | goals_home == 0 & goals_away == 1 & event == 'Goal') %>% select(game_id, goals_home, goals_away, home_goals_final, away_goals_final)

functiondata2017 = season2017 %>% filter(goals_home == 1 & goals_away == 0 & event == 'Goal' | goals_home == 0 & goals_away == 1 & event == 'Goal') %>% select(game_id, goals_home, goals_away, home_goals_final, away_goals_final)

functiondata2018 = season2018 %>% filter(goals_home == 1 & goals_away == 0 & event == 'Goal' | goals_home == 0 & goals_away == 1 & event == 'Goal') %>% select(game_id, goals_home, goals_away, home_goals_final, away_goals_final)


firstgoalwin.func = function(goaldata){
  
  y=rep(NA,nrow(goaldata))
  
  for(i in 1:nrow(goaldata)){
    if(i %% 2 == 0){
      if(goaldata$goals_home[i] == 1 & goaldata$goals_away[i] == 0 & goaldata$home_goals_final[i]  > goaldata$away_goals_final[i]){
        
        y[i] = 1
        
      } else if(goaldata$goals_home[i]  == 0 & goaldata$goals_away[i]  == 1 & goaldata$home_goals_final[i]  < goaldata$away_goals_final[i]){
        
        y[i] = 1
        
      } else if(goaldata$goals_home[i]  == 0 & goaldata$goals_away[i]  == 1 & goaldata$home_goals_final[i]  > goaldata$away_goals_final[i]  | goaldata$goals_home[i]  == 1 & goaldata$goals_away[i]  == 0 & goaldata$home_goals_final[i]  < goaldata$away_goals_final[i]) {
        
        y[i] = 0
        
      }
      
    }
  }
  y.factor = factor(y)
  return(y.factor)
}
functiondata2010$first_goal_win <- firstgoalwin.func(functiondata2010)
functiondata2010 = functiondata2010 %>% filter(first_goal_win == 1 | first_goal_win == 0) %>% select(game_id, first_goal_win)
season2010 = right_join(season2010, functiondata2010)

functiondata2011$first_goal_win <- firstgoalwin.func(functiondata2011)
functiondata2011 = functiondata2011 %>% filter(first_goal_win == 1 | first_goal_win == 0) %>% select(game_id, first_goal_win)
season2011 = right_join(season2011, functiondata2011)

functiondata2012$first_goal_win <- firstgoalwin.func(functiondata2012)
functiondata2012 = functiondata2012 %>% filter(first_goal_win == 1 | first_goal_win == 0) %>% select(game_id, first_goal_win)
season2012 = right_join(season2012, functiondata2012)

functiondata2013$first_goal_win <- firstgoalwin.func(functiondata2013)
functiondata2013 = functiondata2013 %>% filter(first_goal_win == 1 | first_goal_win == 0) %>% select(game_id, first_goal_win)
season2013 = right_join(season2013, functiondata2013)

functiondata2014$first_goal_win <- firstgoalwin.func(functiondata2014)
functiondata2014 = functiondata2014 %>% filter(first_goal_win == 1 | first_goal_win == 0) %>% select(game_id, first_goal_win)
season2014 = right_join(season2014, functiondata2014)

functiondata2015$first_goal_win <- firstgoalwin.func(functiondata2015)
functiondata2015 = functiondata2015 %>% filter(first_goal_win == 1 | first_goal_win == 0) %>% select(game_id, first_goal_win)
season2015 = right_join(season2015, functiondata2015)

functiondata2016$first_goal_win <- firstgoalwin.func(functiondata2016)
functiondata2016 = functiondata2016 %>% filter(first_goal_win == 1 | first_goal_win == 0) %>% select(game_id, first_goal_win)
season2016 = right_join(season2016, functiondata2016)

functiondata2017$first_goal_win <- firstgoalwin.func(functiondata2017)
functiondata2017 = functiondata2017 %>% filter(first_goal_win == 1 | first_goal_win == 0) %>% select(game_id, first_goal_win)
season2017 = right_join(season2017, functiondata2017)

functiondata2018$first_goal_win <- firstgoalwin.func(functiondata2018)
functiondata2018 = functiondata2018 %>% filter(first_goal_win == 1 | first_goal_win == 0) %>% select(game_id, first_goal_win)
season2018 = right_join(season2018, functiondata2018)

season2010sortdate = season2010 %>% filter(play_num == 3) %>% arrange(game_id) %>% select(game_id)
season2011sortdate = season2011 %>% filter(period == 1 & periodTime == 0) %>%filter(play_num ==3) %>% arrange(game_id) %>% select(game_id)
season2012sortdate = season2012 %>% filter(period == 1 & periodTime == 0) %>%filter(play_num ==3) %>% arrange(game_id) %>% select(game_id)
season2013sortdate = season2013 %>% filter(period == 1 & periodTime == 0) %>%filter(play_num ==3) %>% arrange(game_id) %>% select(game_id)
season2014sortdate = season2014 %>% filter(period == 1 & periodTime == 0) %>%filter(play_num ==3) %>% arrange(game_id) %>% select(game_id)
season2015sortdate = season2015 %>% filter(period == 1 & periodTime == 0) %>%filter(play_num ==3) %>% arrange(game_id) %>% select(game_id)
season2016sortdate = season2016 %>% filter(period == 1 & periodTime == 0) %>%filter(play_num ==3) %>% arrange(game_id) %>% select(game_id)
season2017sortdate = season2017 %>% filter(period == 1 & periodTime == 0) %>%filter(play_num ==3) %>% arrange(game_id) %>% select(game_id)
season2018sortdate = season2018 %>% filter(period == 1 & periodTime == 0) %>%filter(play_num ==3) %>% arrange(game_id) %>% select(game_id)

gameinyear.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 1:nrow(data)){
    if(i %% 2 == 0){
      y[i] = i/2
    }
  }
  y.factor = factor(y)
  return(y.factor)
}

season2010sortdate$game_number = gameinyear.func(season2010sortdate)
season2010sortdate = season2010sortdate %>% filter(is.na(game_number)==F)
season2010 = right_join(season2010, season2010sortdate)

season2011sortdate$game_number = gameinyear.func(season2011sortdate)
season2011sortdate = season2011sortdate %>% filter(is.na(game_number)==F)
season2011 = right_join(season2011, season2011sortdate)

season2012sortdate$game_number = gameinyear.func(season2012sortdate)
season2012sortdate = season2012sortdate %>% filter(is.na(game_number)==F)
season2012 = right_join(season2012, season2012sortdate)

season2013sortdate$game_number = gameinyear.func(season2013sortdate)
season2013sortdate = season2013sortdate %>% filter(is.na(game_number)==F)
season2013 = right_join(season2013, season2013sortdate)

season2014sortdate$game_number = gameinyear.func(season2014sortdate)
season2014sortdate = season2014sortdate %>% filter(is.na(game_number)==F)
season2014 = right_join(season2014, season2014sortdate)

season2015sortdate$game_number = gameinyear.func(season2015sortdate)
season2015sortdate = season2015sortdate %>% filter(is.na(game_number)==F)
season2015 = right_join(season2015, season2015sortdate) 

season2016sortdate$game_number = gameinyear.func(season2016sortdate)
season2016sortdate = season2016sortdate %>% filter(is.na(game_number)==F)
season2016 = right_join(season2016, season2016sortdate)

season2017sortdate$game_number = gameinyear.func(season2017sortdate)
season2017sortdate = season2017sortdate %>% filter(is.na(game_number)==F)
season2017 = right_join(season2017, season2017sortdate)  

season2018sortdate$game_number = gameinyear.func(season2018sortdate)
season2018sortdate = season2018sortdate %>% filter(is.na(game_number)==F)
season2018 = right_join(season2018, season2018sortdate)

write.csv(season2010, file = 'nhl-game-data/2010season.csv')
write.csv(season2011, file = 'nhl-game-data/2011season.csv')
write.csv(season2012, file = 'nhl-game-data/2012season.csv')
write.csv(season2013, file = 'nhl-game-data/2013season.csv')
write.csv(season2014, file = 'nhl-game-data/2014season.csv')
write.csv(season2015, file = 'nhl-game-data/2015season.csv')
write.csv(season2016, file = 'nhl-game-data/2016season.csv')
write.csv(season2017, file = 'nhl-game-data/2017season.csv')
write.csv(season2018, file = 'nhl-game-data/2018season.csv')

season2010 = as_tibble(read.csv("nhl-game-data/2010season.csv"))
season2011 = as_tibble(read.csv("nhl-game-data/2011season.csv"))
season2012 = as_tibble(read.csv("nhl-game-data/2012season.csv"))
season2013 = as_tibble(read.csv("nhl-game-data/2013season.csv"))
season2014 = as_tibble(read.csv("nhl-game-data/2014season.csv"))
season2015 = as_tibble(read.csv("nhl-game-data/2015season.csv"))
season2016 = as_tibble(read.csv("nhl-game-data/2016season.csv"))
season2017 = as_tibble(read.csv("nhl-game-data/2017season.csv"))
season2018 = as_tibble(read.csv("nhl-game-data/2018season.csv"))

FGW_per.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 1:nrow(data)){
    if(i %% 2 == 0){
      y[i] = mean(data$first_goal_win)
    }
  }
  y.factor = factor(y)
  return(y.factor)
}
FGW2010 = season2010 %>% filter(play_num == 3)
FGW2010$FGWpercent = FGW_per.func(FGW2010)
FGW2010 = FGW2010 %>% filter(is.na(FGWpercent) == F) %>% select(game_id, FGWpercent)
season2010 = right_join(season2010, FGW2010)

FGW2011 = season2011 %>% filter(play_num == 3) 
FGW2011$FGWpercent = FGW_per.func(FGW2011)
FGW2011 = FGW2011 %>% filter(is.na(FGWpercent) == F) %>% select(game_id, FGWpercent)
season2011 = right_join(season2011, FGW2011)

FGW2012 = season2012 %>% filter(play_num == 3) 
FGW2012$FGWpercent = FGW_per.func(FGW2012)
FGW2012 = FGW2012 %>% filter(is.na(FGWpercent) == F) %>% select(game_id, FGWpercent)
season2012 = right_join(season2012, FGW2012)

FGW2013 = season2013 %>% filter(play_num == 3) 
FGW2013$FGWpercent = FGW_per.func(FGW2013)
FGW2013 = FGW2013 %>% filter(is.na(FGWpercent) == F)  %>% select(game_id, FGWpercent)
season2013 = right_join(season2013, FGW2013)

FGW2014 = season2014 %>% filter(play_num == 3) 
FGW2014$FGWpercent = FGW_per.func(FGW2014)
FGW2014 = FGW2014 %>% filter(is.na(FGWpercent) == F)  %>% select(game_id, FGWpercent)
season2014 = right_join(season2014, FGW2014)

FGW2015 = season2015 %>% filter(play_num == 3)
FGW2015$FGWpercent = FGW_per.func(FGW2015)
FGW2015 = FGW2015 %>% filter(is.na(FGWpercent) == F)  %>% select(game_id, FGWpercent)
season2015 = right_join(season2015, FGW2015)

FGW2016 = season2016 %>% filter(play_num == 3)
FGW2016$FGWpercent = FGW_per.func(FGW2016)
FGW2016 = FGW2016 %>% filter(is.na(FGWpercent) == F)  %>% select(game_id, FGWpercent)
season2016 = right_join(season2016, FGW2016)

FGW2017 = season2017 %>% filter(play_num == 3)
FGW2017$FGWpercent = FGW_per.func(FGW2017)
FGW2017 = FGW2017 %>% filter(is.na(FGWpercent) == F)  %>% select(game_id, FGWpercent)
season2017 = right_join(season2017, FGW2017)

FGW2018 = season2018 %>% filter(play_num == 3) 
FGW2018$FGWpercent = FGW_per.func(FGW2018)
FGW2018 = FGW2018 %>% filter(is.na(FGWpercent) == F)  %>% select(game_id, FGWpercent)
season2018 = right_join(season2018, FGW2018)


scorefirst2010 = season2010 %>% filter(goals_away == 1 & goals_home == 0 & event == 'Goal' | goals_away == 0 & goals_home == 1 & event == 'Goal') %>% select(game_id, team_id, HoA, team_id_for, goals_away, goals_home)
scorefirst2011 = season2011 %>% filter(goals_away == 1 & goals_home == 0 & event == 'Goal' | goals_away == 0 & goals_home == 1 & event == 'Goal') %>% select(game_id, team_id, HoA, team_id_for, goals_away, goals_home)
scorefirst2012 = season2012 %>% filter(goals_away == 1 & goals_home == 0 & event == 'Goal' | goals_away == 0 & goals_home == 1 & event == 'Goal') %>% select(game_id, team_id, HoA, team_id_for, goals_away, goals_home)
scorefirst2013 = season2013 %>% filter(goals_away == 1 & goals_home == 0 & event == 'Goal' | goals_away == 0 & goals_home == 1 & event == 'Goal') %>% select(game_id, team_id, HoA, team_id_for, goals_away, goals_home)
scorefirst2014 = season2014 %>% filter(goals_away == 1 & goals_home == 0 & event == 'Goal' | goals_away == 0 & goals_home == 1 & event == 'Goal') %>% select(game_id, team_id, HoA, team_id_for, goals_away, goals_home)
scorefirst2015 = season2015 %>% filter(goals_away == 1 & goals_home == 0 & event == 'Goal' | goals_away == 0 & goals_home == 1 & event == 'Goal') %>% select(game_id, team_id, HoA, team_id_for, goals_away, goals_home)
scorefirst2016 = season2016 %>% filter(goals_away == 1 & goals_home == 0 & event == 'Goal' | goals_away == 0 & goals_home == 1 & event == 'Goal') %>% select(game_id, team_id, HoA, team_id_for, goals_away, goals_home)
scorefirst2017 = season2017 %>% filter(goals_away == 1 & goals_home == 0 & event == 'Goal' | goals_away == 0 & goals_home == 1 & event == 'Goal') %>% select(game_id, team_id, HoA, team_id_for, goals_away, goals_home)
scorefirst2018 = season2018 %>% filter(goals_away == 1 & goals_home == 0 & event == 'Goal' | goals_away == 0 & goals_home == 1 & event == 'Goal') %>% select(game_id, team_id, HoA, team_id_for, goals_away, goals_home)

firstgoalscored.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 1:nrow(data)){
    if(data$goals_away[i] == 1 & data$team_id_for[i] == data$team_id[i]){
      y[i] = 1
    } else if(data$goals_home[i] == 1 & data$team_id_for[i] == data$team_id[i]){
      y[i] = 1
    } else {
      y[i] = 0
    }
  }
  y.factor = factor(y)
  return(y.factor)
}

scorefirst2010$scored_first = firstgoalscored.func(scorefirst2010)
scorefirst2010 = scorefirst2010 %>% select(game_id, team_id, scored_first)
season2010 = right_join(season2010, scorefirst2010)

scorefirst2011$scored_first = firstgoalscored.func(scorefirst2011)
scorefirst2011 = scorefirst2011 %>% select(game_id, team_id, scored_first)
season2011 = right_join(season2011, scorefirst2011)

scorefirst2012$scored_first = firstgoalscored.func(scorefirst2012)
scorefirst2012 = scorefirst2012 %>% select(game_id, team_id, scored_first)
season2012 = right_join(season2012, scorefirst2012)

scorefirst2013$scored_first = firstgoalscored.func(scorefirst2013)
scorefirst2013 = scorefirst2013 %>% select(game_id, team_id, scored_first)
season2013 = right_join(season2013, scorefirst2013)

scorefirst2014$scored_first = firstgoalscored.func(scorefirst2014)
scorefirst2014 = scorefirst2014 %>% select(game_id, team_id, scored_first)
season2014 = right_join(season2014, scorefirst2014)

scorefirst2015$scored_first = firstgoalscored.func(scorefirst2015)
scorefirst2015 = scorefirst2015 %>% select(game_id, team_id, scored_first)
season2015 = right_join(season2015, scorefirst2015)

scorefirst2016$scored_first = firstgoalscored.func(scorefirst2016)
scorefirst2016 = scorefirst2016 %>% select(game_id, team_id, scored_first)
season2016 = right_join(season2016, scorefirst2016)

scorefirst2017$scored_first = firstgoalscored.func(scorefirst2017)
scorefirst2017 = scorefirst2017 %>% select(game_id, team_id, scored_first)
season2017 = right_join(season2017, scorefirst2017)

scorefirst2018$scored_first = firstgoalscored.func(scorefirst2018)
scorefirst2018 = scorefirst2018 %>% select(game_id, team_id, scored_first)
season2018 = right_join(season2018, scorefirst2018)

home_conference.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 1:nrow(data)){
    if(i %% 2 == 0){
    if(data$season[i] < 20132014){
      if(data$away_team_id[i] < 16 | data$away_team_id[i] == 52){
        y[i] = 'East'
      } else {
        y[i] = 'West'
      }
    } else if(data$season[i] >= 20132014){
      if(data$away_team_id[i] < 18){
        y[i] = 'East'
      } else {
        y[i] = 'West'
      }
    }
    }
  }
  y.factor = factor(y)
  return(y.factor)
}

season2010conf = season2010 %>% select(game_id, home_team_id, away_team_id, season, event, period, periodTime,play_num) %>% arrange(game_id) %>% filter(play_num ==3 & periodTime == 0 & period == 1)
season2010conf$home_conference = home_conference.func(season2010conf)



season2011conf = season2011 %>% select(game_id, home_team_id, away_team_id, season, event, period, periodTime,play_num) %>% arrange(game_id) %>% filter(play_num ==3 & periodTime == 0 & period == 1)
season2011conf$home_conference = home_conference.func(season2011conf)


season2012conf = season2012 %>% select(game_id, home_team_id, away_team_id, season, event, period, periodTime,play_num) %>% arrange(game_id) %>% filter(play_num ==3 & periodTime == 0 & period == 1)
season2012conf$home_conference = home_conference.func(season2012conf)


season2013conf = season2013 %>% select(game_id, home_team_id, away_team_id, season, event, period, periodTime,play_num) %>% arrange(game_id) %>% filter(play_num ==3 & periodTime == 0 & period == 1)
season2013conf$home_conference = home_conference.func(season2013conf)


season2014conf = season2014 %>% select(game_id, home_team_id, away_team_id, season, event, period, periodTime,play_num) %>% arrange(game_id) %>% filter(play_num ==3 & periodTime == 0 & period == 1)
season2014conf$home_conference = home_conference.func(season2014conf)


season2015conf = season2015 %>% select(game_id, home_team_id, away_team_id, season, event, period, periodTime,play_num) %>% arrange(game_id) %>% filter(play_num ==3 & periodTime == 0 & period == 1)
season2015conf$home_conference = home_conference.func(season2015conf)


season2016conf = season2016 %>% select(game_id, home_team_id, away_team_id, season, event, period, periodTime,play_num) %>% arrange(game_id) %>% filter(play_num ==3 & periodTime == 0 & period == 1)
season2016conf$home_conference = home_conference.func(season2016conf)


season2017conf = season2017 %>% select(game_id, home_team_id, away_team_id, season, event, period, periodTime,play_num) %>% arrange(game_id) %>% filter(play_num == 3 & periodTime == 0 & period == 1)
season2017conf$home_conference = home_conference.func(season2017conf)


season2018conf = season2018 %>% select(game_id, home_team_id, away_team_id, season, event, period, periodTime,play_num) %>% arrange(game_id) %>% filter(play_num == 3 & periodTime == 0 & period == 1)
season2018conf$home_conference = home_conference.func(season2018conf)



away_conference.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 1:nrow(data)){
    if(i %% 2 == 0){
    if(data$season[i] < 20132014){
      if(data$home_team_id[i] < 16 | data$home_team_id[i] == 52){
        y[i] = 'East'
      } else {
        y[i] = 'West'
      }
    } else if(data$season[i] >= 20132014){
      if(data$home_team_id[i] < 18){
        y[i] = 'East'
      } else {
        y[i] = 'West'
      }
    }
    }
  }
  y.factor = factor(y)
  return(y.factor)
}

season2010conf$away_conference = away_conference.func(season2010conf)
season2010conf = season2010conf %>% select(game_id, home_conference, away_conference) %>% filter(is.na(away_conference) == F)
season2011conf$away_conference = away_conference.func(season2011conf)
season2011conf = season2011conf %>% select(game_id, home_conference, away_conference) %>% filter(is.na(away_conference) == F)
season2012conf$away_conference = away_conference.func(season2012conf)
season2012conf = season2012conf %>% select(game_id, home_conference, away_conference) %>% filter(is.na(away_conference) == F)
season2013conf$away_conference = away_conference.func(season2013conf)
season2013conf = season2013conf %>% select(game_id, home_conference, away_conference) %>% filter(is.na(away_conference) == F)
season2014conf$away_conference = away_conference.func(season2014conf)
season2014conf = season2014conf %>% select(game_id, home_conference, away_conference) %>% filter(is.na(away_conference) == F)
season2015conf$away_conference = away_conference.func(season2015conf)
season2015conf = season2015conf %>% select(game_id, home_conference, away_conference) %>% filter(is.na(away_conference) == F)
season2016conf$away_conference = away_conference.func(season2016conf)
season2016conf = season2016conf %>% select(game_id, home_conference, away_conference) %>% filter(is.na(away_conference) == F)
season2017conf$away_conference = away_conference.func(season2017conf)
season2017conf = season2017conf %>% select(game_id, home_conference, away_conference) %>% filter(is.na(away_conference) == F)
season2018conf$away_conference = away_conference.func(season2018conf)
season2018conf = season2018conf %>% select(game_id, home_conference, away_conference) %>% filter(is.na(away_conference) == F)

season2010 = right_join(season2010, season2010conf)
season2011 = right_join(season2011, season2011conf)
season2012 = right_join(season2012, season2012conf)
season2013 = right_join(season2013, season2013conf)
season2014 = right_join(season2014, season2014conf)
season2015 = right_join(season2015, season2015conf)
season2016 = right_join(season2016, season2016conf)
season2017 = right_join(season2017, season2017conf)
season2018 = right_join(season2018, season2018conf)

season2010div = season2010 %>% arrange(game_id) %>% select(game_id, away_team_id, home_team_id, event, periodTime, period, season, play_num) %>% filter(play_num == 3 & period == 1 & periodTime == 0)
season2011div = season2011 %>% arrange(game_id) %>% select(game_id, away_team_id, home_team_id, event, periodTime, period, season, play_num) %>% filter(play_num == 3 & period == 1 & periodTime == 0)
season2012div = season2012 %>% arrange(game_id) %>% select(game_id, away_team_id, home_team_id, event, periodTime, period, season, play_num) %>% filter(play_num == 3 & period == 1 & periodTime == 0)
season2013div = season2013 %>% arrange(game_id) %>% select(game_id, away_team_id, home_team_id, event, periodTime, period, season, play_num) %>% filter(play_num == 3 & period == 1 & periodTime == 0)
season2014div = season2014 %>% arrange(game_id) %>% select(game_id, away_team_id, home_team_id, event, periodTime, period, season, play_num) %>% filter(play_num == 3 & period == 1 & periodTime == 0)
season2015div = season2015 %>% arrange(game_id) %>% select(game_id, away_team_id, home_team_id, event, periodTime, period, season, play_num) %>% filter(play_num == 3 & period == 1 & periodTime == 0)
season2016div = season2016 %>% arrange(game_id) %>% select(game_id, away_team_id, home_team_id, event, periodTime, period, season, play_num) %>% filter(play_num == 3 & period == 1 & periodTime == 0)
season2017div = season2017 %>% arrange(game_id) %>% select(game_id, away_team_id, home_team_id, event, periodTime, period, season, play_num) %>% filter(play_num == 3 & period == 1 & periodTime == 0)
season2018div = season2018 %>% arrange(game_id) %>% select(game_id, away_team_id, home_team_id, event, periodTime, period, season, play_num) %>% filter(play_num == 3 & period == 1 & periodTime == 0)

home_div.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 1:nrow(data)){
     
      if(data$season[i] < 20132014){
        if(data$home_team_id[i] <= 5){
          y[i] = 'Atlantic'
        }else if(data$home_team_id[i] > 5 & data$home_team_id[i] < 11){
          y[i] = 'Northeast'
        } else if(data$home_team_id[i] > 10 & data$home_team_id[i] < 16 | data$home_team_id[i] == 52){
          y[i] = 'Southeast'
        } else if(data$home_team_id[i] > 15 & data$home_team_id[i] < 21){
          y[i] = 'Central'
        } else if(data$home_team_id[i] > 20 & data$home_team_id[i] < 26){
          y[i] = 'Northwest'
        } else if(data$home_team_id[i] > 25 & data$home_team_id[i] < 31){
          y[i] = 'Pacific'
        }
      } else if(data$season[i] >= 20132014){
        if(data$home_team_id[i] == 6 | data$home_team_id[i] == 7 | data$home_team_id[i] == 17 | data$home_team_id[i] == 13 | data$home_team_id[i] == 8 | data$home_team_id[i] == 9 | data$home_team_id[i] == 14 | data$home_team_id[i] == 10){
          y[i] = 'Atlantic'
        } else if(data$home_team_id[i] == 12 | data$home_team_id[i] == 29 | data$home_team_id[i] == 1 | data$home_team_id[i] == 2 | data$home_team_id[i] == 3 | data$home_team_id[i] == 4 | data$home_team_id[i] == 5 | data$home_team_id[i] == 15){
          y[i] = 'Metropolitan'
        } else if(data$home_team_id[i] == 16 | data$home_team_id[i] == 21 | data$home_team_id[i] == 25 | data$home_team_id[i] == 30 | data$home_team_id[i] == 18 | data$home_team_id[i] == 19 | data$home_team_id[i] == 52){
          y[i] = 'Central'
        } else if(data$home_team_id[i] == 24 | data$home_team_id[i] == 53 | data$home_team_id[i] == 20 | data$home_team_id[i] == 22 | data$home_team_id[i] == 26 | data$home_team_id[i] == 28 | data$home_team_id[i] == 23 | data$home_team_id[i] == 27 | data$home_team_id[i] == 54){
          y[i] = 'Pacific'
        }
      }
    }
  y.factor = factor(y)
  return(y.factor)
}

away_div.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 1:nrow(data)){
    if(i %% 2 == 0){
      if(data$season[i] < 20132014){
        if(data$away_team_id[i] <= 5){
          y[i] = 'Atlantic'
        }else if(data$away_team_id[i] > 5 & data$away_team_id[i] < 11){
          y[i] = 'Northeast'
        } else if(data$away_team_id[i] > 10 & data$away_team_id[i] < 16 | data$away_team_id[i] == 52){
          y[i] = 'Southeast'
        } else if(data$away_team_id[i] > 15 & data$away_team_id[i] < 21){
          y[i] = 'Central'
        } else if(data$away_team_id[i] > 20 & data$away_team_id[i] < 26){
          y[i] = 'Northwest'
        } else if(data$away_team_id[i] > 25 & data$away_team_id[i] < 31){
          y[i] = 'Pacific'
        }
      } else if(data$season[i] >= 20132014){
        if(data$away_team_id[i] == 6 | data$away_team_id[i] == 7 | data$away_team_id[i] == 17 | data$away_team_id[i] == 13 | data$away_team_id[i] == 8 | data$away_team_id[i] == 9 | data$away_team_id[i] == 14 | data$away_team_id[i] == 10){
          y[i] = 'Atlantic'
        } else if(data$away_team_id[i] == 12 | data$away_team_id[i] == 29 | data$away_team_id[i] == 1 | data$away_team_id[i] == 2 | data$away_team_id[i] == 3 | data$away_team_id[i] == 4 | data$away_team_id[i] == 5 | data$away_team_id[i] == 15){
          y[i] = 'Metropolitan'
        } else if(data$away_team_id[i] == 16 | data$away_team_id[i] == 21 | data$away_team_id[i] == 25 | data$away_team_id[i] == 30 | data$away_team_id[i] == 18 | data$away_team_id[i] == 19 | data$away_team_id[i] == 52){
          y[i] = 'Central'
        } else if(data$away_team_id[i] == 24 | data$away_team_id[i] == 53 | data$away_team_id[i] == 20 | data$away_team_id[i] == 22 | data$away_team_id[i] == 26 | data$away_team_id[i] == 28 | data$away_team_id[i] == 23 | data$away_team_id[i] == 27 | data$away_team_id[i] == 54){
          y[i] = 'Pacific'
        }
      }
    }
  }
  y.factor = factor(y)
  return(y.factor)
}

season2010div$home_division = home_div.func(season2010div)
season2010div$away_division = away_div.func(season2010div)
season2010div = season2010div %>% select(game_id, home_division, away_division)%>% filter(is.na(away_division ) ==F)
season2010 = right_join(season2010, season2010div)

season2011div$home_division = home_div.func(season2011div)
season2011div$away_division = away_div.func(season2011div)
season2011div = season2011div %>% select(game_id, home_division, away_division) %>% filter(is.na(away_division ) ==F)
season2011 = right_join(season2011, season2011div)

season2012div$home_division = home_div.func(season2012div)
season2012div$away_division = away_div.func(season2012div)
season2012div = season2012div %>% select(game_id, home_division, away_division)%>% filter(is.na(away_division ) ==F)
season2012 = right_join(season2012, season2012div)

season2013div$home_division = home_div.func(season2013div)
season2013div$away_division = away_div.func(season2013div)
season2013div = season2013div %>% select(game_id, home_division, away_division)%>% filter(is.na(away_division ) ==F)
season2013 = right_join(season2013, season2013div)

season2014div$home_division = home_div.func(season2014div)
season2014div$away_division = away_div.func(season2014div)
season2014div = season2014div %>% select(game_id, home_division, away_division)%>% filter(is.na(away_division ) ==F)
season2014 = right_join(season2014, season2014div)

season2015div$home_division = home_div.func(season2015div)
season2015div$away_division = away_div.func(season2015div)
season2015div = season2015div %>% select(game_id, home_division, away_division)%>% filter(is.na(away_division ) ==F)
season2015 = right_join(season2015, season2015div)

season2016div$home_division = home_div.func(season2016div)
season2016div$away_division = away_div.func(season2016div)
season2016div = season2016div %>% select(game_id, home_division, away_division)%>% filter(is.na(away_division ) ==F)
season2016 = right_join(season2016, season2016div)

season2017div$home_division = home_div.func(season2017div)
season2017div$away_division = away_div.func(season2017div)
season2017div = season2017div %>% select(game_id, home_division, away_division)%>% filter(is.na(away_division ) ==F)
season2017 = right_join(season2017, season2017div)

season2018div$home_division = home_div.func(season2018div)
season2018div$away_division = away_div.func(season2018div)
season2018div = season2018div %>% select(game_id, home_division, away_division)%>% filter(is.na(away_division ) ==F)
season2018 = right_join(season2018, season2018div)

game_type.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 1:nrow(data)){
    if(i%%2==0){
      if(data$home_division[i] == data$away_division[i]){
        y[i] = 1
      }else if(data$home_division[i] != data$away_division[i] & data$home_conference[i] == data$away_conference[i]){
        y[i] = 2
      } else {
        y[i] = 3
      }
    }
  }
  y.factor = factor(y)
  return(y.factor)
}
season2010type = select(season2010, game_id, home_division, away_division, home_conference, away_conference, play_num) %>% filter(play_num == 3) %>% arrange(game_id)
season2010type$game_type = game_type.func(season2010type)
season2010type = select(season2010type, game_id, game_type) %>% filter(is.na(game_type) ==F)
season2010 = right_join(season2010, season2010type)

season2011type = select(season2011, game_id, home_division, away_division, home_conference, away_conference, play_num) %>% filter(play_num == 3) %>% arrange(game_id)
season2011type$game_type = game_type.func(season2011type)
season2011type = select(season2011type, game_id, game_type) %>% filter(is.na(game_type) ==F)
season2011 = right_join(season2011, season2011type)

season2012type = select(season2012, game_id, home_division, away_division, home_conference, away_conference, play_num) %>% filter(play_num == 3) %>% arrange(game_id)
season2012type$game_type = game_type.func(season2012type)
season2012type = select(season2012type, game_id, game_type) %>% filter(is.na(game_type) ==F)
season2012 = right_join(season2012, season2012type)

season2013type = select(season2013, game_id, home_division, away_division, home_conference, away_conference, play_num) %>% filter(play_num == 3) %>% arrange(game_id)
season2013type$game_type = game_type.func(season2013type)
season2013type = select(season2013type, game_id, game_type) %>% filter(is.na(game_type) ==F)
season2013 = right_join(season2013, season2013type)

season2014type = select(season2014, game_id, home_division, away_division, home_conference, away_conference, play_num) %>% filter(play_num == 3) %>% arrange(game_id)
season2014type$game_type = game_type.func(season2014type)
season2014type = select(season2014type, game_id, game_type) %>% filter(is.na(game_type) ==F)
season2014 = right_join(season2014, season2014type)

season2015type = select(season2015, game_id, home_division, away_division, home_conference, away_conference, play_num) %>% filter(play_num == 3) %>% arrange(game_id)
season2015type$game_type = game_type.func(season2015type)
season2015type = select(season2015type, game_id, game_type) %>% filter(is.na(game_type) ==F)
season2015 = right_join(season2015, season2015type)

season2016type = select(season2016, game_id, home_division, away_division, home_conference, away_conference, play_num) %>% filter(play_num == 3) %>% arrange(game_id)
season2016type$game_type = game_type.func(season2016type)
season2016type = select(season2016type, game_id, game_type) %>% filter(is.na(game_type) ==F)
season2016 = right_join(season2016, season2016type)

season2017type = select(season2017, game_id, home_division, away_division, home_conference, away_conference, play_num) %>% filter(play_num == 3) %>% arrange(game_id)
season2017type$game_type = game_type.func(season2017type)
season2017type = select(season2017type, game_id, game_type) %>% filter(is.na(game_type) ==F)
season2017 = right_join(season2017, season2017type)

season2018type = select(season2018, game_id, home_division, away_division, home_conference, away_conference, play_num) %>% filter(play_num == 3) %>% arrange(game_id)
season2018type$game_type = game_type.func(season2018type)
season2018type = select(season2018type, game_id, game_type) %>% filter(is.na(game_type) ==F)
season2018 = right_join(season2018, season2018type)





alldata = full_join(season2010, season2011) %>% full_join(season2012) %>% full_join(season2013) %>% full_join(season2014) %>% full_join(season2015) %>% full_join(season2016) %>% full_join(season2017) %>% full_join(season2018)

write.csv(season2010, file = 'nhl-game-data/2010season.csv')
write.csv(season2011, file = 'nhl-game-data/2011season.csv')
write.csv(season2012, file = 'nhl-game-data/2012season.csv')
write.csv(season2013, file = 'nhl-game-data/2013season.csv')
write.csv(season2014, file = 'nhl-game-data/2014season.csv')
write.csv(season2015, file = 'nhl-game-data/2015season.csv')
write.csv(season2016, file = 'nhl-game-data/2016season.csv')
write.csv(season2017, file = 'nhl-game-data/2017season.csv')
write.csv(season2018, file = 'nhl-game-data/2018season.csv')
write.csv(alldata, file = 'nhl-game-data/allseasons.csv')


