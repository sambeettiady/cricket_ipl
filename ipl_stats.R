rm(list = ls())

#load packages
library(dplyr)
library(readr)
library(tidyr)

#Set working directory
setwd(dir = '/Users/sambeet/Data Science/cricket_ipl_analysis')

#Read dataset
all_data = read_csv(file = 'ipl_data.csv',col_names = T,progress = T)

#Player IPL Career Statistics - Slightly buggy
player = 'V Kohli'

#Batting Statistics
player_batting_stats = all_data %>% 
    filter((batsman == player | non_striker == player) & innings_no <= 2) %>% 
    group_by(match_number_total,innings_no) %>%
    mutate(run_out_as_non_striker = as.numeric(ifelse(sum(ifelse(
        non_striker_run_out == 1 & non_striker == player,1,0)) > 0,1,0)),
        mom = as.numeric(ifelse(man_of_the_match == player,1,0))) %>% 
    filter(batsman == player) %>% 
    mutate(innings_score = sum(runs_off_bat),innings_ball_number = rank(over + (ball/6)),
           thirty_or_more = ifelse(innings_score >= 30,1,0),
           fifty_or_more = ifelse(innings_score >= 50,1,0), 
           hundred_or_more = ifelse(innings_score >= 100,1,0)) %>% ungroup() %>% 
    group_by(batsman,season_year,ipl_edition,match_number_total,match,stadium,toss_winner,
             toss_decision,winner,innings_no) %>%
    summarise(mom = unique(mom),runs = unique(innings_score),
              balls_faced = n() - sum(wide),highest = max(innings_score),
              out = sum(caught) + sum(bowled) + sum(lbw) + sum(striker_run_out) + sum(stumped) +
                  sum(obstructing_the_field) + sum(hit_wicket) + unique(run_out_as_non_striker),
              not_out = ifelse(out == 0,1,0), thirty_or_more = unique(thirty_or_more),
              fifty_or_more = unique((fifty_or_more)), hundred_or_more = unique(hundred_or_more),
              fours = sum(fours),sixes = sum(sixes),dot_balls = sum(dot_ball)) %>% 
    ungroup() %>% group_by(batsman,stadium,innings_no) %>% 
    summarise(innings_played = sum(length(unique(match_number_total))),runs = sum(runs),
              balls_faced = sum(balls_faced),outs = sum(out),not_outs = sum(not_out),
              thirty_or_more = sum(thirty_or_more),fifty_or_more = sum(fifty_or_more),
              hundred_or_more = sum(hundred_or_more),MoMs = sum(mom),fours = sum(fours),
              sixes = sum(sixes),dot_balls = sum(dot_balls)) %>%
    mutate(average = as.numeric(ifelse(outs == 0,NA,runs/outs)),
           strike_rate = ifelse(balls_faced == 0,NA,100*runs/balls_faced),
           dot_ball_percentage = ifelse(balls_faced == 0,NA,100*dot_balls/balls_faced)) %>% 
    select(-outs,-dot_balls)

#Bowling Statistics
player_bowling_stats = all_data %>% filter(bowler == player & innings_no != 3) %>% 
    group_by(bowler,season_year,ipl_edition,match_number_total,match,stadium,toss_winner,
             toss_decision,winner,innings_no) %>% 
    summarise(balls_bowled = n() - sum(no_ball) - sum(wide), 
              runs_conceded = sum(runs_off_bat) + sum(ifelse(wide == 1 | no_ball == 1,extra_runs,0)),
              wickets = sum(type_of_dismissal != 'Not Out' & type_of_dismissal != 'run out'),
              fours = sum(fours),sixes = sum(sixes),dot_balls = sum(dot_ball)) %>% 
    ungroup() %>% mutate(max_wickets = max(wickets),
                         runs_max_wickets = min(ifelse(wickets == unique(max_wickets),
                                                       runs_conceded,NA),na.rm = T),
                         three_wickets_or_more = ifelse(wickets >= 3,1,0),
                         five_wickets_or_more = ifelse(wickets >= 5,1,0)) %>% 
    group_by(bowler,innings_no) %>% 
    summarise(innings_played = sum(length(unique(match_number_total))),
              balls_bowled = sum(balls_bowled),runs_conceded = sum(runs_conceded),
              wickets = sum(wickets),
              best_bowling = paste(unique(runs_max_wickets),'/',unique(max_wickets),sep = ''),
              three_wickets_or_more = sum(three_wickets_or_more),
              five_wickets_or_more = sum(five_wickets_or_more),
              fours = sum(fours),sixes = sum(sixes),dot_balls = sum(dot_balls)) %>%
    mutate(bowling_average = ifelse(wickets == 0,NA,runs_conceded/wickets),
           strike_rate = ifelse(balls_bowled == 0,NA,balls_bowled/wickets),
           dot_ball_percentage = ifelse(balls_bowled == 0,NA,100*dot_balls/balls_bowled),
           economy_rate = ifelse(balls_bowled == 0,NA,6*runs_conceded/balls_bowled)) %>%
    select(-dot_balls)
