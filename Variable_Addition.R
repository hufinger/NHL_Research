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

alldata$date_time=as.Date(alldata$date_time)
alldata = arrange(alldata, team_id, date_time) %>% filter(play_num == 3)

gamesinweek.func = function(data){
  y = rep(NA, nrow(data))
  z = rep(NA, nrow(data))
  for(i in 2:nrow(data)){
    if(data$season[i] == data$season[i-1] & data$team_id[i] == data$team_id[i-1]){
      y[i] = data$date_time[i] - data$date_time[i-1]
      h = i - 1
      startdate = data$date_time[i] - 7
      count = 0
      while(h > 0){
        if(data$date_time[h] >= startdate & data$team_id[h]==data$team_id[i]){
          count = count + 1
          h = h - 1
        } else {
          h = h-1
        }
      }
      z[i] = count
    }
  }
  list = list(y, z)
  list
  return(list)
}
list = gamesinweek.func(alldata)
rest = list[[1]]
giw = list[[2]]
alldata$rest_days= rest
alldata$game_in_week = giw

winstreak5.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 6:nrow(data)){
    if(data$season[i] == data$season[i-5] & data$team_id[i] == data$team_id[i-5]){
      h = 5
      count = 0
      while(h > 0){
        if(data$won[i-h] == "TRUE"){
          count = count + 1
          h= h-1
        }else{
          h = h-1
        }
      }
      y[i] = count
    }
  }
  return(factor(y))
}

winstreak10.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 11:nrow(data)){
    if(data$season[i] == data$season[i-10] & data$team_id[i] == data$team_id[i-10]){
      h = 10
      count = 0
      while(h > 0){
        if(data$won[i-h] == "TRUE"){
          count = count + 1
          h= h-1
        }else{
          h = h-1
        }
      }
      y[i] = count
    }
  }
  return(factor(y))
}

winstreak20.func = function(data){
  y = rep(NA, nrow(data))
  for(i in 21:nrow(data)){
    if(data$season[i] == data$season[i-20] & data$team_id[i] == data$team_id[i-20]){
      h = 20
      count = 0
      while(h > 0){
        if(data$won[i-h] == "TRUE"){
          count = count + 1
          h= h-1
        }else{
          h = h-1
        }
      }
      y[i] = count
    }
  }
  return(factor(y))
}
alldata$last5 = winstreak5.func(alldata)
alldata$last10 = winstreak10.func(alldata)
alldata$last20 =winstreak20.func(alldata)

winpercent.func = function(data){
  y = rep(NA, nrow(data))
  wins = 0
  gamenum = 0
  for(i in 2:nrow(data)){
    if(data$won[i-1]=="TRUE" & data$season[i] == data$season[i-1]& data$team_id[i]==data$team_id[i-1]){
      wins = wins + 1
      gamenum = gamenum + 1
    }else if(data$won[i-1]=="FALSE"& data$season[i] == data$season[i-1]& data$team_id[i]==data$team_id[i-1]){
      gamenum = gamenum + 1
    } else{
      wins = 0
      gamenum = 0
    }
    if(gamenum == 0){
      y[i] = 0
    }
    y[i] = (wins/gamenum)*100
  }
  return(factor(y))
}

alldata$win_percent = winpercent.func(alldata)

#Select the Variables that are useful before the game begins
slim_data = select(alldata, game_id, team_id, date_time, HoA, season, away_team_id, home_team_id, goals, shots, hits, pim, powerPlayOpportunities, powerPlayGoals, faceOffWinPercentage, giveaways, takeaways, shortName, teamName, game_number, first_goal_win, scored_first, home_conference, home_division, away_conference, away_division, game_type, play_num, rest_days, game_in_week, last5, last10, last20, win_percent)

#Make shortName and teamName be one variable (team)
slim_data$team = paste(slim_data$shortName, slim_data$teamName)
slim_data = select(slim_data, -c(shortName, teamName)) %>% filter(play_num == 3)

#gather game onto 1 line
home_data = filter(slim_data, HoA == "home" & play_num == 3)
away_data = filter(slim_data, HoA == "away" & play_num == 3)

single.line = full_join(by = "game_id", away_data, home_data) %>% arrange(game_id, team_id.x)

test1 = gather(single.line, Type, Team, c(team_id.x, team_id.y))


for(i in 1:nrow(test1)){
  if(test1$Type[i] == "team_id.x"){
    test1$Type[i] = "Away"
  }else{
    test1$Type[i] = "Home" 
  }
}

cleaned = select(test1, game_id, Type, Team, season.x, away_team_id.x, home_team_id.x, goals.x, shots.x, hits.x, pim.x, powerPlayOpportunities.x, powerPlayGoals.x, faceOffWinPercentage.x, giveaways.x, takeaways.x, game_number.x, first_goal_win.x, scored_first.x, home_conference.x, home_division.x, away_conference.x, away_division.x, game_type.x, team.x, goals.y, shots.y, hits.y, pim.y, powerPlayOpportunities.y, powerPlayGoals.y, faceOffWinPercentage.y, giveaways.y, takeaways.y, scored_first.y, team.y, rest_days.x, game_in_week.x, last5.x, last10.x, last20.x, win_percent.x, rest_days.y, game_in_week.y, last5.y, last10.y, last20.y, win_percent.y)

cleaned=cleaned[with(cleaned, order(Team, game_id)),]

colnames(cleaned) = c("game_id", "Type", "team_id", "Season", "away_team_id", "home_team_id", "away_goals", "away_shots", "away_hits", "away_pim", "away_powerPlayOpportunities", "away_powerPlayGoals", "away_faceOffWinPercentage", "away_giveaways", "away_takeaways", "game_number", "first_goal_win", "away_scored_first", "home_conference", "home_division", "away_conference", "away_division", "game_type", "away_team_name", "home_goals", "home_shots", "home_hits", "home_pim", "home_powerPlayOpportunities", "home_powerPlayGoals", "home_faceOffWinPercentage", "home_giveaways", "home_takeaways", "home_scored_first", "home_team_name", "away_rest", "away_game_in_week", "away_last5_win", "away_last10_win", "away_last20_win", "away_win_percent", "home_rest", "home_game_in_week", "home_last5_win", "home_last10_win", "home_last20_win", "home_win_percent")

#create varaibles that make running averages of hits, goals, etc for past 1,5,10,20 games
avgSeason.func = function(data, var1, var2){
  y = rep(NA, nrow(data))
  teamscore = 0
  gamenum = 0
  for(i in 2:nrow(data)){
    if(data$Type[i-1] == "Home" & data$Season[i] == data$Season[i-1]& data$team_id[i]==data$team_id[i-1]){
      teamscore = teamscore + var2[i-1]
      gamenum = gamenum + 1
    }else if(data$Type[i-1] != "Home"& data$Season[i] == data$Season[i-1]& data$team_id[i]==data$team_id[i-1]){
      teamscore = teamscore + var1[i-1]
      gamenum = gamenum + 1
    } else{
      teamscore = 0
      gamenum = 0
    }
    if(gamenum == 0){
      y[i] = 0
    }
    y[i] = teamscore/gamenum
  }
  y.factor = factor(y)
  return(y.factor)
}

cleaned$avg.goals = avgSeason.func(cleaned, cleaned$away_goals, cleaned$home_goals)
cleaned$avggoalsAgainst = avgSeason.func(cleaned, cleaned$home_goals, cleaned$away_goals)
cleaned$avgshots = avgSeason.func(cleaned, cleaned$away_shots, cleaned$home_shots)
cleaned$avgshotsAgainst = avgSeason.func(cleaned, cleaned$home_shots, cleaned$away_shots)
cleaned$avghits = avgSeason.func(cleaned, cleaned$away_hits, cleaned$home_hits)
cleaned$avghitsAgainst = avgSeason.func(cleaned, cleaned$home_hits, cleaned$away_hits)
cleaned$avgpim = avgSeason.func(cleaned, cleaned$away_pim, cleaned$home_pim)
cleaned$avgpimAgainst = avgSeason.func(cleaned, cleaned$home_pim, cleaned$away_pim)
cleaned$avgPPO = avgSeason.func(cleaned, cleaned$away_powerPlayOpportunities, cleaned$home_powerPlayOpportunities)
cleaned$avgPPOagainst = avgSeason.func(cleaned, cleaned$home_powerPlayOpportunities, cleaned$away_powerPlayOpportunities)
cleaned$avgPPG = avgSeason.func(cleaned, cleaned$away_powerPlayGoals, cleaned$home_powerPlayGoals)
cleaned$avgPPGagainst = avgSeason.func(cleaned, cleaned$home_powerPlayGoals, cleaned$away_powerPlayGoals)
cleaned$avggiveaways = avgSeason.func(cleaned, cleaned$away_giveaways, cleaned$home_giveaways)
cleaned$avgtakeaways = avgSeason.func(cleaned, cleaned$away_takeaways, cleaned$home_takeaways)
cleaned$avgoppGiveaway = avgSeason.func(cleaned, cleaned$home_giveaways, cleaned$away_giveaways)
cleaned$avgoppTakeaway = avgSeason.func(cleaned, cleaned$home_takeaways, cleaned$away_takeaways)

last5avg.func = function(data, var1, var2){
  y = rep(NA, nrow(data))
  avg = 0
  for(i in 6:nrow(data)){
    if(data$Type[i-5] == "Away" & data$Season[i] == data$Season[i-5]& data$team_id[i]==data$team_id[i-5]){
      avg = avg + var1[i-5]
      j = 4
      while(j > 0){
        if(data$Type[i-j] == "Away"){
          avg = avg + var1[i-j]
        }else{
          avg = avg + var2[i-j]
        }
        j = j-1
      }
      avg = avg/5
      y[i] = avg
      avg = 0
    } else if(data$Type[i-5] != "Away" & data$Season[i] == data$Season[i-5]& data$team_id[i]==data$team_id[i-5]){
      avg = avg + var2[i-5]
      j = 4
      while(j > 0){
        if(data$Type[i-j] == "Away"){
          avg = avg + var1[i-j]
        }else{
          avg = avg + var2[i-j]
        }
        j = j-1
      }
      avg = avg/5
      y[i] = avg
      avg = 0
    }
  }
  return(factor(y))
}

cleaned$avg5goals = last5avg.func(cleaned, cleaned$away_goals,cleaned$home_goals)
cleaned$avg5goalsAgainst = last5avg.func(cleaned, cleaned$home_goals, cleaned$away_goals)
cleaned$avg5shots = last5avg.func(cleaned, cleaned$away_shots, cleaned$home_shots)
cleaned$avg5shotsAgainst = last5avg.func(cleaned, cleaned$home_shots, cleaned$away_shots)
cleaned$avg5hits = last5avg.func(cleaned, cleaned$away_hits, cleaned$home_hits)
cleaned$avg5hitsAgainst = last5avg.func(cleaned, cleaned$home_hits, cleaned$away_hits)
cleaned$avg5pim = last5avg.func(cleaned, cleaned$away_pim, cleaned$home_pim)
cleaned$avg5pimAgainst = last5avg.func(cleaned, cleaned$home_pim, cleaned$away_pim)
cleaned$avg5PPO = last5avg.func(cleaned, cleaned$away_powerPlayOpportunities, cleaned$home_powerPlayOpportunities)
cleaned$avg5PPOagainst = last5avg.func(cleaned, cleaned$home_powerPlayOpportunities, cleaned$away_powerPlayOpportunities)
cleaned$avg5PPG = last5avg.func(cleaned, cleaned$away_powerPlayGoals, cleaned$home_powerPlayGoals)
cleaned$avg5PPGagainst = last5avg.func(cleaned, cleaned$home_powerPlayGoals, cleaned$away_powerPlayGoals)
cleaned$avg5giveaways = last5avg.func(cleaned, cleaned$away_giveaways, cleaned$home_giveaways)
cleaned$avg5takeaways = last5avg.func(cleaned, cleaned$away_takeaways, cleaned$home_takeaways)
cleaned$avg5oppGiveaway = last5avg.func(cleaned, cleaned$home_giveaways, cleaned$away_giveaways)
cleaned$avg5oppTakeaway = last5avg.func(cleaned, cleaned$home_takeaways, cleaned$away_takeaways)


last10avg.func = function(data, var1, var2){
  y = rep(NA, nrow(data))
  avg = 0
  for(i in 11:nrow(data)){
    if(data$Type[i-10] == "Away" & data$Season[i] == data$Season[i-10]& data$team_id[i]==data$team_id[i-10]){
      avg = avg + var1[i-10]
      j = 9
      while(j > 0){
        if(data$Type[i-j] == "Away"){
          avg = avg + var1[i-j]
        }else{
          avg = avg + var2[i-j]
        }
        j = j-1
      }
      avg = avg/10
      y[i] = avg
      avg = 0
    } else if(data$Type[i-10] != "Away" & data$Season[i] == data$Season[i-10]& data$team_id[i]==data$team_id[i-10]){
      avg = avg + var2[i-10]
      j = 9
      while(j > 0){
        if(data$Type[i-j] == "Away"){
          avg = avg + var1[i-j]
        }else{
          avg = avg + var2[i-j]
        }
        j = j-1
      }
      avg = avg/10
      y[i] = avg
      avg = 0
    }
  }
  return(factor(y))
}

last20avg.func = function(data, var1, var2){
  y = rep(NA, nrow(data))
  avg = 0
  for(i in 21:nrow(data)){
    if(data$Type[i-20] == "Away" & data$Season[i] == data$Season[i-20] & data$team_id[i]==data$team_id[i-20]){
      avg = avg + var1[i-20]
      j = 19
      while(j > 0){
        if(data$Type[i-j] == "Away"){
          avg = avg + var1[i-j]
        }else{
          avg = avg + var2[i-j]
        }
        j = j-1
      }
      avg = avg/20
      y[i] = avg
      avg = 0
    } else if(data$Type[i-20] != "Away" & data$Season[i] == data$Season[i-20]& data$team_id[i]==data$team_id[i-20]){
      avg = avg + var2[i-20]
      j = 19
      while(j > 0){
        if(data$Type[i-j] == "Away"){
          avg = avg + var1[i-j]
        }else{
          avg = avg + var2[i-j]
        }
        j = j-1
      }
      avg = avg/20
      y[i] = avg
      avg = 0
    }
  }
  return(factor(y))
}

cleaned$avg10goals = last10avg.func(cleaned, cleaned$away_goals,cleaned$home_goals)
cleaned$avg10goalsAgainst = last10avg.func(cleaned, cleaned$home_goals, cleaned$away_goals)
cleaned$avg10shots = last10avg.func(cleaned, cleaned$away_shots, cleaned$home_shots)
cleaned$avg10shotsAgainst = last10avg.func(cleaned, cleaned$home_shots, cleaned$away_shots)
cleaned$avg10hits = last10avg.func(cleaned, cleaned$away_hits, cleaned$home_hits)
cleaned$avg10hitsAgainst = last10avg.func(cleaned, cleaned$home_hits, cleaned$away_hits)
cleaned$avg10pim = last10avg.func(cleaned, cleaned$away_pim, cleaned$home_pim)
cleaned$avg10pimAgainst = last10avg.func(cleaned, cleaned$home_pim, cleaned$away_pim)
cleaned$avg10PPO = last10avg.func(cleaned, cleaned$away_powerPlayOpportunities, cleaned$home_powerPlayOpportunities)
cleaned$avg10PPOagainst = last10avg.func(cleaned, cleaned$home_powerPlayOpportunities, cleaned$away_powerPlayOpportunities)
cleaned$avg10PPG = last10avg.func(cleaned, cleaned$away_powerPlayGoals, cleaned$home_powerPlayGoals)
cleaned$avg10PPGagainst = last10avg.func(cleaned, cleaned$home_powerPlayGoals, cleaned$away_powerPlayGoals)
cleaned$avg10giveaways = last10avg.func(cleaned, cleaned$away_giveaways, cleaned$home_giveaways)
cleaned$avg10takeaways = last10avg.func(cleaned, cleaned$away_takeaways, cleaned$home_takeaways)
cleaned$avg10oppGiveaway = last10avg.func(cleaned, cleaned$home_giveaways, cleaned$away_giveaways)
cleaned$avg10oppTakeaway = last10avg.func(cleaned, cleaned$home_takeaways, cleaned$away_takeaways)

cleaned$avg20goals = last20avg.func(cleaned, cleaned$away_goals,cleaned$home_goals)
cleaned$avg20goalsAgainst = last20avg.func(cleaned, cleaned$home_goals, cleaned$away_goals)
cleaned$avg20shots = last20avg.func(cleaned, cleaned$away_shots, cleaned$home_shots)
cleaned$avg20shotsAgainst = last20avg.func(cleaned, cleaned$home_shots, cleaned$away_shots)
cleaned$avg20hits = last20avg.func(cleaned, cleaned$away_hits, cleaned$home_hits)
cleaned$avg20hitsAgainst = last20avg.func(cleaned, cleaned$home_hits, cleaned$away_hits)
cleaned$avg20pim = last20avg.func(cleaned, cleaned$away_pim, cleaned$home_pim)
cleaned$avg20pimAgainst = last20avg.func(cleaned, cleaned$home_pim, cleaned$away_pim)
cleaned$avg20PPO = last20avg.func(cleaned, cleaned$away_powerPlayOpportunities, cleaned$home_powerPlayOpportunities)
cleaned$avg20PPOagainst = last20avg.func(cleaned, cleaned$home_powerPlayOpportunities, cleaned$away_powerPlayOpportunities)
cleaned$avg20PPG = last20avg.func(cleaned, cleaned$away_powerPlayGoals, cleaned$home_powerPlayGoals)
cleaned$avg20PPGagainst = last20avg.func(cleaned, cleaned$home_powerPlayGoals, cleaned$away_powerPlayGoals)
cleaned$avg20giveaways = last20avg.func(cleaned, cleaned$away_giveaways, cleaned$home_giveaways)
cleaned$avg20takeaways = last20avg.func(cleaned, cleaned$away_takeaways, cleaned$home_takeaways)
cleaned$avg20oppGiveaway = last20avg.func(cleaned, cleaned$home_giveaways, cleaned$away_giveaways)
cleaned$avg20oppTakeaway = last20avg.func(cleaned, cleaned$home_takeaways, cleaned$away_takeaways)

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
write.csv(cleaned, file = 'nhl-game-data/cleaned_NHL.csv')
