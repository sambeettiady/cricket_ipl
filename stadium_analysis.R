rm(list = ls())

library(readr)
library(dplyr)

setwd(dir = 'Data Science/cricket_ipl_analysis/')

ipl_data = read_csv(file = 'ipl_data.csv')

stadium_data = ipl_data %>% filter(innings_no <= 2) %>% group_by(stadium,innings_no) %>% 
    summarise(total_innings = length(unique(match_number_total)),total_balls_bowled = n(),
              total_runs = sum(runs_off_bat), total_wickets = sum(bowled + caught + lbw + stumped)) %>%
    group_by(stadium) %>% 
    summarise(total_innings = sum(total_innings),total_balls_bowled = sum(total_balls_bowled),
              total_runs = sum(total_runs), total_wickets = sum(total_wickets)) %>%
    mutate(avg_runs_per_innings = total_runs/total_innings,avg_runs_per_wicket = total_runs/total_wickets,
           econ_rate = 6*(total_runs/total_balls_bowled))

stadium_data_batting_first = ipl_data %>% filter(innings_no == 1) %>% group_by(stadium,innings_no) %>% 
    summarise(total_innings = length(unique(match_number_total)),total_balls_bowled = n(),
              total_runs = sum(runs_off_bat), total_wickets = sum(bowled + caught + lbw + stumped)) %>%
    group_by(stadium) %>% 
    summarise(total_innings = sum(total_innings),total_balls_bowled = sum(total_balls_bowled),
              total_runs = sum(total_runs), total_wickets = sum(total_wickets)) %>%
    mutate(avg_runs_per_innings = total_runs/total_innings,avg_runs_per_wicket = total_runs/total_wickets,
           econ_rate = 6*(total_runs/total_balls_bowled))

stadium_data_batting_second = ipl_data %>% filter(innings_no == 2) %>% group_by(stadium,innings_no) %>% 
    summarise(total_innings = length(unique(match_number_total)),total_balls_bowled = n(),
              total_runs = sum(runs_off_bat), total_wickets = sum(bowled + caught + lbw + stumped)) %>%
    group_by(stadium) %>% 
    summarise(total_innings = sum(total_innings),total_balls_bowled = sum(total_balls_bowled),
              total_runs = sum(total_runs), total_wickets = sum(total_wickets)) %>%
    mutate(avg_runs_per_innings = total_runs/total_innings,avg_runs_per_wicket = total_runs/total_wickets,
           econ_rate = 6*(total_runs/total_balls_bowled))

stadium_batting_first_or_second = ipl_data %>% 
    mutate(won_batting_first = ifelse(winner_runs == 'Won By Wickets',0,1)) %>% 
    group_by(stadium,match_number_total) %>% summarise(won_batting_first = unique(won_batting_first),
                                                       tie = unique(tie)) %>%
    group_by(stadium) %>% summarise(total_matches = n(),matches_won_batting_first = sum(won_batting_first) - sum(tie),
                                    ties = sum(tie)) %>%
    mutate(pct_of_matches_won_batting_first = 100*matches_won_batting_first/total_matches,
           matches_won_batting_second = total_matches - ties - matches_won_batting_first) %>% 
    mutate(pct_of_matches_won_batting_second = 100*matches_won_batting_second/total_matches) %>%
    select(stadium,total_matches,pct_of_matches_won_batting_first,pct_of_matches_won_batting_second)

stadium_diff = stadium_data_batting_first[6:8] - stadium_data_batting_second[6:8]
stadium_diff$stadium = stadium_data_batting_first$stadium
stadium_diff$matches = stadium_data_batting_first$total_innings
