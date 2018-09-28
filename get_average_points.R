rm(list = ls())

setwd(dir = 'Data Science/cricket_ipl_analysis/')

library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
source(file = 'ipl_2016_fantasy.R')

all_data = read_csv('cricket_2017.csv')
players = read_csv(file = 'ipl_players.csv')
players$value = players$value/10000
#Create function to find players with highest Points
get_overall_points = function(players,recent = F){

#    players = players[players$team != 'Not Playing',]

    for(i in 1:length(players$player)){
        if(recent){
            batting = get_player_batting_performance(player = players$player[i], overall = F,grouping_var_string = 'match_number_total',recent = T)
            bowling = get_player_bowling_performance(player = players$player[i],overall = F,grouping_var_string = 'match_number_total',recent = T)
            batting_overall = get_player_batting_performance(player = players$player[i],recent = T)
            bowling_overall = get_player_bowling_performance(player = players$player[i],recent = T)
        }else{
            batting = get_player_batting_performance(player = players$player[i], overall = F,grouping_var_string = 'match_number_total')
            bowling = get_player_bowling_performance(player = players$player[i],overall = F,grouping_var_string = 'match_number_total')
            batting_overall = get_player_batting_performance(player = players$player[i])
            bowling_overall = get_player_bowling_performance(player = players$player[i])
        }
        moms = max(batting_overall$MoMs,batting_overall$MoMs,0,na.rm = T)
        calculate_batting_points = function(data_frame,points_system = 'ipl'){
            if(points_system == 'ipl'){
                points = ifelse(data_frame$runs == 0 & data_frame$outs == 1,-5,data_frame$runs) + 
                    data_frame$sixes*2 + 
                    ifelse(data_frame$runs >= 10,
                        ifelse(data_frame$strike_rate >= 200,15,
                                ifelse(data_frame$strike_rate >= 150,10,
                                        ifelse(data_frame$strike_rate >= 100,5,
                                                ifelse(data_frame$strike_rate >= 75,-10,-15)))),0) + 
                    10*as.integer(data_frame$runs/25)
            }else{
                points = ifelse(data_frame$runs == 0 & data_frame$outs == 1,-5,data_frame$runs) + 
                    data_frame$sixes*2 + data_frame$fours*1 +
                    data_frame$runs - data_frame$balls_faced + 
                    ifelse(data_frame$runs >= 100,50,
                           ifelse(data_frame$runs >= 75,30,
                                  ifelse(data_frame$runs >= 50,15,
                                         ifelse(data_frame$runs >= 25,5,0))))
            }
            return(points)
        }
        
        calculate_bowling_points = function(data_frame,points_system = 'ipl'){
            if(points_system == 'ipl'){
                points = data_frame$wickets*20 + 
                    ifelse(data_frame$economy_rate <= 5,15,
                        ifelse(data_frame$economy_rate <= 8,10,
                                ifelse(data_frame$economy_rate <= 10,5,
                                        ifelse(data_frame$economy_rate <= 12,-10,
                                                ifelse(data_frame$economy_rate > 12,-15,0))))) +
                    ifelse(data_frame$wickets >= 2,(data_frame$wickets - 1)*10,0) + data_frame$dot_balls +
                    ifelse(data_frame$economy_rate <= 6,as.integer(0.8*data_frame$dot_balls/6)*20,0)
            }else{
                points = data_frame$wickets*20 + 
                    ifelse(data_frame$balls_bowled*1.5 - data_frame$runs_conceded > 0,2*(data_frame$balls_bowled*1.5 - data_frame$runs_conceded),data_frame$balls_bowled*1.5 - data_frame$runs_conceded) +
                    ifelse(data_frame$wickets >= 5,50,
                           ifelse(data_frame$wickets == 4,30,
                                  ifelse(data_frame$wickets == 3,15,
                                         ifelse(data_frame$wickets ==2,5,0)))) + 
                    data_frame$dot_balls +
                    ifelse(data_frame$economy_rate <= 6,as.integer(0.8*data_frame$dot_balls/6)*25,0)
            }
            return(points)
        }
        
        players$bowling_points[i] = sum(calculate_bowling_points(data_frame = bowling),na.rm = T)
        players$batting_points[i] = sum(calculate_batting_points(data_frame = batting),na.rm = T)
        players$overall_points[i] = players$bowling_points[i] + players$batting_points[i] + moms*25
        players$bowling_points_fandromeda[i] = sum(calculate_bowling_points(data_frame = bowling,points_system = 'fandromeda'),na.rm = T)
        players$batting_points_fandromeda[i] = sum(calculate_batting_points(data_frame = batting,points_system = 'fandromeda'),na.rm = T)
        players$overall_points_fandromeda[i] = players$bowling_points_fandromeda[i] + players$batting_points_fandromeda[i] + moms*25
        
        batting_innings = nrow(batting)
        bowling_innings = nrow(bowling)

        players$innings[i] = max(batting_innings,bowling_innings)

        players$avg_bowling_points_overall[i] = players$bowling_points[i]/bowling_innings
        players$avg_batting_points_overall[i] = players$batting_points[i]/batting_innings
        players$avg_points_overall[i] = players$overall_points[i]/players$innings[i]
        players$avg_bowling_points_overall_fandromeda[i] = players$bowling_points_fandromeda[i]/bowling_innings
        players$avg_batting_points_overall_fandromeda[i] = players$batting_points_fandromeda[i]/batting_innings
        players$avg_points_overall_fandromeda[i] = players$overall_points_fandromeda[i]/players$innings[i]
        players$vfm_index[i] = players$avg_points_overall[i]/players$value[i]
    }
    players[is.na(players)] = 0
    players = players %>% select(-bowling_points,-batting_points,-overall_points)
    return(players)
}

avg_points = get_overall_points(players)
avg_points$bowling_speed = ifelse(avg_points$bowling_style %in% c("Medium Fast","Fast"),'Fast',ifelse(avg_points$bowling_style %in% c("Off Spin","Orthodox Spin","Leg Spin","Chinaman"),'Spin','Medium'))

x = avg_points[avg_points$innings >= 1,]
write.csv(x,file = '2017_points_data.csv',row.names = F)
g = ggplot(data = x,mapping = aes(text = paste('Name: ',player,', Team: ',team),
                                  x = value - mean(value),
                                  y = avg_points_overall - mean(avg_points_overall),
                                  alpha = 0.1*innings,size = 0.05*innings,col = factor(team))) + 
    geom_point() + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
    ggtitle('Performance Vs Price') + xlab('Price') + ylab('Performance') + geom_hline(yintercept = 15,size = 1.1,color = 'green')
ggplotly(g,tooltip = 'text')
