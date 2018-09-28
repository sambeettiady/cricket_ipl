rm(list = ls())

#load packages
library(dplyr)
library(readr)
library(tidyr)

#Set working directory
setwd(dir = '/Users/sambeet/Data Science/cricket_ipl_analysis')

#Read dataset
all_data = read_csv(file = 'ipl_data.csv',col_names = T,progress = T,trim_ws = T)

#Batting Statistics
get_player_batting_performance = function(player, overall = T, grouping_var_string, recent = F,
                                          cumulative_avg = F){
    
    if(recent){
        all_data = all_data %>% filter(season_year == 2015 | season_year == 2016)
    }
    
    player_grouped_batting_stats = all_data %>% 
        filter((batsman == player | non_striker == player) & innings_no <= 2) %>% 
        group_by_('match_number_total') %>% 
        mutate(run_out_as_non_striker = as.numeric(ifelse(sum(ifelse(
            non_striker_run_out == 1 & non_striker == player,1,0)) > 0,1,0)),
            mom = as.numeric(ifelse(man_of_the_match == player,1,0))) %>% 
        filter(batsman == player) %>% 
#        mutate(innings_ball_number = rank(over + (ball/6))) %>%
        group_by(batsman,season_year,ipl_edition,match_number_total,match,stadium,toss_winner,
                  toss_decision,winner,innings_no,bowling_team,bowling_hand,bowling_style,
                  batting_position,bowler_role,foreign_bowler,man_of_the_match,over_category,bowler) %>%
        summarise(runs = sum(runs_off_bat),balls_faced = n() - sum(wide),
                  outs = sum(caught) + sum(bowled) + sum(lbw) + sum(stumped) + 
                      sum(obstructing_the_field) + sum(hit_wicket), 
                      run_out = sum(striker_run_out) + unique(run_out_as_non_striker),
               fours = sum(fours),sixes = sum(sixes),dot_balls = sum(dot_ball)) %>% ungroup()

        player_batting_matchwise = player_grouped_batting_stats %>% 
            group_by(batsman,match_number_total) %>% 
            summarise(runs = sum(runs), balls_faced = sum(balls_faced), outs = sum(outs) + ifelse(sum(run_out) > 1,1,sum(run_out)),
                      fours = sum(fours), sixes = sum(sixes), dot_balls = sum(dot_balls),
                      mom = ifelse(unique(man_of_the_match) == player,1,0),
                      twenty_five_or_more = ifelse(runs >= 25,1,0),
                      fifty_or_more = ifelse(runs >= 50,1,0),
                      seventy_five_or_more = ifelse(runs >= 75,1,0),
                      hundred_or_more = ifelse(runs >= 100,1,0)) %>% ungroup() %>% 
            mutate(highest_runs = max(runs))
        
        if(overall){
            player_batting_overall_stats = player_batting_matchwise %>% group_by(batsman) %>%
            summarise(innings = sum(length(unique(match_number_total))), runs = sum(runs),
                      balls_faced = sum(balls_faced), outs = sum(outs), 
                      not_outs = innings - outs, fours = sum(fours), sixes = sum(sixes), 
                      dot_balls = sum(dot_balls), highest_runs = unique(highest_runs),MoMs = sum(mom), 
                      `25+` = sum(twenty_five_or_more),
                      `50+` = sum(fifty_or_more),
                      `75+` = sum(seventy_five_or_more),
                      `100+` = sum(hundred_or_more)) %>% ungroup() %>%
                mutate(batting_avg = round(as.numeric(ifelse(outs == 0,NA,runs/outs)),2), 
                       strike_rate = round(ifelse(balls_faced == 0,NA,100*runs/balls_faced),2), 
                       dot_ball_pct = round(ifelse(balls_faced == 0,NA,100*dot_balls/balls_faced),2)) %>% 
            select(-outs,-dot_balls,-batsman)
            return(player_batting_overall_stats)
        }else if(cumulative_avg){
            player_cumulative_batting_stats = player_batting_matchwise %>% 
                select(match_number_total,runs,balls_faced,outs,fours,sixes) %>%
                mutate(match_index = rank(match_number_total),cum_runs = cumsum(runs),
                       cum_balls_faced = cumsum(balls_faced),cum_outs = cumsum(outs),
                       cum_fours = cumsum(fours),cum_sixes = cumsum(sixes)) %>% 
                mutate(cum_batting_avg = round(as.numeric(ifelse(cum_outs == 0,NA,cum_runs/cum_outs)),2),
                       strike_rate = round(ifelse(cum_balls_faced == 0,NA,100*cum_runs/cum_balls_faced),2)) %>%
                select(-match_number_total:-sixes)
            return(player_cumulative_batting_stats)
        }else{
            player_batting_stats_grouped = player_grouped_batting_stats %>% 
                group_by_('batsman',grouping_var_string) %>%
                summarise(runs = sum(runs), balls_faced = sum(balls_faced), outs = sum(outs), 
                          fours = sum(fours), sixes = sum(sixes), dot_balls = sum(dot_balls)) %>% 
                ungroup() %>%
                mutate(batting_avg = round(as.numeric(ifelse(outs == 0,NA,runs/outs)),2), 
                       strike_rate = round(ifelse(balls_faced == 0,NA,100*runs/balls_faced),2), 
                       dot_ball_pct = round(ifelse(balls_faced == 0,NA,100*dot_balls/balls_faced),2)) %>%
                arrange(desc(batting_avg)) %>% select(-batsman)
            return(player_batting_stats_grouped)
        }
}

#Bowling Statistics
get_player_bowling_performance = function(player,overall = T,grouping_var_string,recent = F,
                                          cumulative_avg = F){
    
    if(recent){
        all_data = all_data %>% filter(season_year == 2014 | season_year == 2015)
    }
    
    player_grouped_bowling_stats = all_data %>% filter(bowler == player & innings_no <= 2) %>% 
        group_by(bowler, season_year, ipl_edition, match_number_total, match, stadium, toss_winner,
                 toss_decision, winner, innings_no, batting_team, batting_position, batting_hand,
                 batsman_role, foreign_batsman,over_category,batsman,man_of_the_match) %>% 
        summarise(balls_bowled = n() - sum(no_ball) - sum(wide), 
                  runs_conceded = sum(runs_off_bat) + sum(ifelse(wide == 1 | no_ball == 1,extra_runs,0)),
                  wickets = sum(type_of_dismissal != 'Not Out' & type_of_dismissal != 'run out'),
                  fours = sum(fours),sixes = sum(sixes),dot_balls = sum(dot_ball)) %>% ungroup() 
    
    player_bowling_matchwise = player_grouped_bowling_stats %>% group_by(bowler,match_number_total) %>%
        summarise(balls_bowled = sum(balls_bowled), runs_conceded = sum(runs_conceded),
                  wickets = sum(wickets), fours = sum(fours), sixes = sum(sixes), 
                  dot_balls = sum(dot_balls),mom = ifelse(unique(man_of_the_match) == player,1,0)) %>% 
        ungroup() %>% mutate(max_wickets = max(wickets),
               runs_max_wickets = min(ifelse(wickets == unique(max_wickets),runs_conceded,NA),na.rm = T),
               three_wickets_or_more = ifelse(wickets >= 3,1,0), 
               five_wickets_or_more = ifelse(wickets >= 5,1,0))
    
    if(overall){
        player_bowling_overall_stats = player_bowling_matchwise %>% group_by(bowler) %>% 
            summarise(innings = sum(length(unique(match_number_total))),
                      balls_bowled = sum(balls_bowled), runs_conceded = sum(runs_conceded),
                      wickets = sum(wickets), MoMs = sum(mom),
                      best_bowling = paste(unique(runs_max_wickets),'/',unique(max_wickets),sep = ''),
                      `3w+` = sum(three_wickets_or_more),
                      `5w+` = sum(five_wickets_or_more), fours = sum(fours),
                      sixes = sum(sixes),dot_balls = sum(dot_balls)) %>% ungroup() %>%
            mutate(bowling_avg = round(ifelse(wickets == 0,NA,runs_conceded/wickets),2),
                   strike_rate = round(ifelse(wickets == 0,NA,balls_bowled/wickets),2),
                   dot_ball_pct = round(ifelse(balls_bowled == 0,NA,100*dot_balls/balls_bowled),2),
                   economy_rate = round(ifelse(balls_bowled == 0,NA,6*runs_conceded/balls_bowled),2)) %>%
            select(-dot_balls,-bowler)
        return(player_bowling_overall_stats)
    }else if(cumulative_avg){
        player_cumulative_bowling_stats = player_bowling_matchwise %>% 
            select(match_number_total,runs_conceded,balls_bowled,wickets,dot_balls) %>%
            mutate(match_index = rank(match_number_total),cum_runs_conceded = cumsum(runs_conceded),
                   cum_balls_bowled = cumsum(balls_bowled),cum_wickets = cumsum(wickets),
                   cum_dot_balls = cumsum(dot_balls)) %>% 
            mutate(cum_bowling_avg = round(ifelse(cum_wickets == 0,NA,cum_runs_conceded/cum_wickets),2),
                   cum_strike_rate = round(ifelse(cum_wickets == 0,NA,cum_balls_bowled/cum_wickets),2),
                   cum_dot_ball_pct = round(ifelse(cum_balls_bowled == 0,NA,100*cum_dot_balls/cum_balls_bowled),2),
                   cum_economy_rate = round(ifelse(cum_balls_bowled == 0,NA,6*cum_runs_conceded/cum_balls_bowled),2)) %>%
            select(-match_number_total:-dot_balls,-cum_dot_balls)
        return(player_cumulative_bowling_stats)
    }else{
        player_bowling_stats_grouped = player_grouped_bowling_stats %>% 
            group_by_('bowler',grouping_var_string) %>%
            summarise(balls_bowled = sum(balls_bowled), runs_conceded = sum(runs_conceded),
                      wickets = sum(wickets), fours = sum(fours), sixes = sum(sixes), 
                      dot_balls = sum(dot_balls)) %>% ungroup() %>%
            mutate(bowling_avg = round(ifelse(wickets == 0,NA,runs_conceded/wickets),1),
                   strike_rate = round(ifelse(wickets == 0,NA,balls_bowled/wickets),1),
                   dot_ball_pct = round(ifelse(balls_bowled == 0,NA,100*dot_balls/balls_bowled),2),
                   economy_rate = round(ifelse(balls_bowled == 0,NA,6*runs_conceded/balls_bowled),2)) %>%
            arrange(bowling_avg) %>% select(-bowler)
        return(player_bowling_stats_grouped)
        
    }
}
