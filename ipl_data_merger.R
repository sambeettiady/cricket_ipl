rm(list = ls())

#load packages
library(dplyr)
library(readr)
library(tidyr)

#Set working directory
setwd(dir = '/Users/sambeet/Data Science/cricket_ipl_analysis')

#Read season file information
season_data = read_csv(file = 'season_data.csv',col_names = T,progress = T)
season_data$diff = season_data$final_match - season_data$first_match
player_info = read_csv(file = 'ipl_players.csv')

bowler_info = player_info %>% select(bowler = player, bowler_role = role, bowling_hand, 
                                     bowling_style, foreign_bowler = foreign_player)
batsman_info = player_info %>% select(batsman = player, batsman_role = role, batting_hand,
                                      foreign_batsman = foreign_player)

filenames = list.files(path = '/Users/sambeet/Data Science/cricket_ipl_analysis/ipl_csv_male/.',pattern = '.csv',all.files = F,full.names = F)
#Loop to read each csv and collate them into one
for(k in 1:length(filenames)){
    
    dir = '/Users/sambeet/Data Science/cricket_ipl_analysis/ipl_csv_male/'
    j = filenames[k]
    filename = paste(dir,j,sep = '')
    
    if(file.exists(filename)){
        temp <- read_csv(file = filename,col_names = F,skip = 1)
    }else{
        next()
    }
    
    temp_match = temp[which(temp$X1 == 'info'),]
    skip = nrow(temp_match) + 1
    
    temp = read_csv(file = filename,col_names = F,skip = skip)
    temp = temp[2:ncol(temp)]
    names(temp) <- c('innings_no','over_and_ball','batting_team','batsman','non_striker',
                     'bowler','runs_off_bat','extra_runs','type_of_dismissal','batsman_out')
    
    temp$home_team = as.character(temp_match[which(temp_match$X2 == 'team'),'X3']$X3[1])
    temp$away_team = as.character(temp_match[which(temp_match$X2 == 'team'),'X3']$X3[2])
    
    temp$season_year = as.character(temp_match[which(temp_match$X2 == 'season'),'X3']$X3)
    temp$match_date = temp_match[which(temp_match$X2 == 'date'),'X3']$X3[1]
    
    temp$venue = as.character(temp_match[which(temp_match$X2 == 'venue'),'X3']$X3)
    temp$city = as.character(temp_match[which(temp_match$X2 == 'city'),'X3']$X3)
    
    temp$toss_winner = as.character(temp_match[which(temp_match$X2 == 'toss_winner'),'X3']$X3)
    temp$toss_decision = temp_match[which(temp_match$X2 == 'toss_decision'),'X3']$X3
    temp$cricinfo_match_id = j
    if('player_of_match' %in% temp_match$X2){
        temp$man_of_the_match = as.character(temp_match[which(temp_match$X2 == 'player_of_match'),'X3']$X3)
    }else{
        temp$man_of_the_match = 'Not Awarded'
    }
    
    if('winner' %in% temp_match$X2){
        temp$winner = as.character(temp_match[which(temp_match$X2 == 'winner'),'X3']$X3)
        temp$tie = 0
    }else if('eliminator' %in% temp_match$X2){
        temp$winner = as.character(temp_match[which(temp_match$X2 == 'eliminator'),'X3']$X3)
        temp$tie = 1
    }else{
        temp$winner = 'No Result'
        temp$tie = 0
    }
    
    if('winner_runs' %in% temp_match$X2){
        temp$winner_runs = temp_match[which(temp_match$X2 == 'winner_runs'),'X3']$X3
        temp$winner_wickets = 'Won By Runs'
    }else if('winner_wickets' %in% temp_match$X2){
        temp$winner_runs = 'Won By Wickets'
        temp$winner_wickets = temp_match[which(temp_match$X2 == 'winner_wickets'),'X3']$X3
    }else{
        temp$winner_runs = 'No Result'
        temp$winner_wickets = 'No Result'
    }
    
    if('method' %in% temp_match$X2){
        temp$duckworth_lewis = 1
    }else{
        temp$duckworth_lewis = 0
    }
    
    temp$umpire_1 = temp_match[which(temp_match$X2 == 'umpire'),'X3']$X3[1]
    temp$umpire_2 = temp_match[which(temp_match$X2 == 'umpire'),'X3']$X3[2]
    
    if('reserve_umpire' %in% temp_match$X2){
        temp$reserve_umpire = temp_match[which(temp_match$X2 == 'reserve_umpire'),'X3']$X3
    }else{
        temp$reserve_umpire = 'Data Not Available'
    }
    
    if('tv_umpire' %in% temp_match$X2){
        temp$tv_umpire = temp_match[which(temp_match$X2 == 'tv_umpire'),'X3']$X3
    }else{
        temp$tv_umpire = 'Data Not Available'
    }
    temp$match_referee = temp_match[which(temp_match$X2 == 'match_referee'),'X3']$X3
    
    if(k == 1){
        all_data = temp
    }else{
        all_data = rbind(all_data,temp)
    }
}

all_data = separate(data = all_data,col = over_and_ball,into = c('over','ball'),
                    sep = '[[:punct:]]+',remove = T)

all_data$over = as.integer(all_data$over)
all_data$over = all_data$over + 1
all_data$ball = as.integer(all_data$ball)

all_data$type_of_dismissal[is.na(all_data$type_of_dismissal)] = 'Not Out'
all_data$batsman_out[is.na(all_data$batsman_out)] = 'Not Out'
for(i in 1:nrow(all_data)){
    all_data$bowling_team[i] = ifelse(all_data$batting_team[i] == all_data$home_team[i],
                                      all_data$away_team[i],all_data$home_team[i])
}

all_data$run_out = ifelse(test = all_data$type_of_dismissal == 'run out',1,0)
all_data$bowled = ifelse(test = all_data$type_of_dismissal == 'bowled',1,0)
all_data$caught_by_fielder = ifelse(test = all_data$type_of_dismissal == 'caught',1,0)
all_data$lbw = ifelse(test = all_data$type_of_dismissal == 'lbw',1,0)
all_data$retired_hurt = ifelse(test = all_data$type_of_dismissal == 'retired hurt',1,0)
all_data$stumped = ifelse(test = all_data$type_of_dismissal == 'stumped',1,0)
all_data$caught_and_bowled = ifelse(test = all_data$type_of_dismissal == 'caught and bowled',1,0)
all_data$hit_wicket = ifelse(test = all_data$type_of_dismissal == 'hit wicket',1,0)
all_data$obstructing_the_field = ifelse(test = all_data$type_of_dismissal == 'obstructing the field',
                                        1,0)
all_data$caught = ifelse(all_data$caught_and_bowled == 1 | all_data$caught_by_fielder == 1,1,0)
all_data$fours = ifelse(all_data$runs_off_bat == 4,1,0)
all_data$sixes = ifelse(all_data$runs_off_bat == 6,1,0)
all_data$dot_ball = ifelse(all_data$extra_runs == 0 & 
                               all_data$type_of_dismissal == 'Not Out' & 
                               all_data$runs_off_bat == 0,1,0)

all_data$season_year[which(all_data$season_year == '2007/08')] = '2008'
all_data$season_year[which(all_data$season_year == '2009/10')] = '2010'
all_data$season_year = as.numeric(all_data$season_year)

season_mapping = data.frame(season_year = unique(all_data$season_year))
season_mapping$ipl_edition = rank(season_mapping$season_year)
all_data = left_join(x = all_data,y = season_mapping,by = 'season_year')

all_data$match_date = as.Date(x = all_data$match_date,format = '%Y/%m/%d')
date_mapping = all_data %>% select(ipl_edition,match_date,home_team,away_team) %>% unique(.) %>%
    mutate(match_number_total = rank(match_date,ties.method = 'first')) %>% 
    group_by(ipl_edition) %>% 
    mutate(match_number_season = rank(match_date,ties.method = 'first'))

all_data = all_data %>% left_join(y = date_mapping,by = c('ipl_edition','match_date','home_team',
                                                          'away_team'))
all_data = all_data %>% group_by(ipl_edition) %>% 
    mutate(final = ifelse(match_number_season == max(match_number_season),1,0)) %>% ungroup()

all_data = unite(data = all_data,col = match,sep = ' Vs ',home_team,away_team,remove = F)
all_data$extra = ifelse(test = all_data$extra_runs > 0,1,0)
all_data = unite(data = all_data,col = stadium,sep = ', ',remove = T,venue,city)

all_data$winner_runs = ifelse(all_data$tie == 1,'Super Over',all_data$winner_runs)
all_data$winner_wickets = ifelse(all_data$tie == 1,'Super Over',all_data$winner_wickets)

all_data = all_data %>% group_by(match_number_total,innings_no) %>% 
    mutate(next_ball_batsman = lead(batsman,n = 1)) %>% ungroup() %>% 
    mutate(next_ball_batsman = replace(next_ball_batsman,is.na(next_ball_batsman),'-')) %>% 
    group_by(match_number_total,innings_no,over) %>% 
    mutate(strike_change = as.numeric(ifelse(ball == max(ball),
                                             ifelse(batsman == next_ball_batsman,1,0),
                                             ifelse(batsman == next_ball_batsman,0,1)))) %>% 
    ungroup() %>% 
    mutate(strike_change = replace(strike_change,is.na(strike_change),0)) %>%
    mutate(no_ball = ifelse(extra == 1 & runs_off_bat > 0,1,0)) %>% 
    group_by(match_number_total,innings_no,over) %>%
    mutate(wide = ifelse(extra == 1 & no_ball == 0,
                         ifelse(extra_runs %% 2 == 0 & strike_change == 1,1,
                                ifelse(extra_runs %% 2 == 1 & strike_change == 0,1,0)),0)) %>% 
    ungroup() %>% mutate(bye_or_legbye = ifelse(extra == 1 & no_ball == 0 & wide == 0,1,0)) %>%
    mutate(striker_run_out = ifelse(run_out == 0,0,ifelse(run_out == 1 & batsman == batsman_out,1,0))) %>%
    mutate(non_striker_run_out = 
               ifelse(run_out == 0,0,ifelse(run_out == 1 & non_striker == batsman_out,1,0)))

#Function to get batting position
batting_position = function(batsman,non_striker,innings_no,match_number_total){
    if((length(batsman) != 0) && (length(non_striker) != 0) && (unique(innings_no) <= 2)){
        player_order = data.frame(batsman = batsman,stringsAsFactors = F)
        batting_position = data.frame(batsman = unique(c(batsman,non_striker[1])),
                                      batting_position = 1:length(unique(c(batsman,non_striker[1]))),
                                      stringsAsFactors = F)
        if(batting_position$batting_position[batting_position$batsman == non_striker[1]] != 2){
            batting_position$batting_position[batting_position$batting_position == 2] = 3
            batting_position$batting_position[batting_position$batsman == non_striker[1]] = 2
        }
        player_order = player_order %>% left_join(y = batting_position,by = 'batsman')
        return(player_order$batting_position)
    }else{
        return(rep(NA,length(batsman)))
    }
}

all_data = all_data %>% arrange(match_number_total,innings_no,over,ball) %>% 
    group_by(match_number_total,innings_no) %>% 
    mutate(batting_position = as.numeric(batting_position(batsman,non_striker,innings_no,match_number_total)))

all_data$batsman[all_data$batsman == 'Shami Ahmed'] = 'Mohammed Shami'
all_data$bowler[all_data$bowler == 'Shami Ahmed'] = 'Mohammed Shami'
all_data$batsman[all_data$batsman == 'B Brainder Sran'] = 'BB Sran'
all_data$bowler[all_data$bowler == 'B Brainder Sran'] = 'BB Sran'
all_data$batsman[all_data$batsman == 'Sandeep sharma (1)'] = 'Sandeep sharma'
all_data$bowler[all_data$bowler == 'Sandeep sharma (1)'] = 'Sandeep sharma'

all_data = all_data %>% left_join(y = batsman_info,by = 'batsman') %>% 
    left_join(y = bowler_info,by = 'bowler')

all_data$over_category = ifelse(all_data$over <= 5,'Overs 1-5',ifelse(all_data$over <= 10,'Overs 6-10',
                                                                      ifelse(all_data$over <= 15,'Overs 11-15','Overs 16-20')))
#all_data = read_csv('ipl_data.csv')
wagon_data = read_csv('all_wagon_data_num.csv')
wagon_data = wagon_data[2:ncol(wagon_data)]

all_data$cricinfo_match_id = stringr::str_replace_all(string = all_data$cricinfo_match_id,pattern = '.csv',replacement = '')
all_data$cricinfo_match_id = as.integer(all_data$cricinfo_match_id)
all_data = all_data %>% left_join(y = wagon_data,by = c('cricinfo_match_id','innings_no','over','ball'))

write.csv(x = all_data,file = 'ipl_data.csv',row.names = F)
write.csv(date_mapping,file = 'date_mapping.csv',row.names = F)
write.csv(season_mapping,'season_mapping.csv',row.names = F)
write.csv(unique(c(all_data$batsman,all_data$non_striker,all_data$bowler)),'player_list.csv',row.names = F)
write.csv(unique(all_data$cricinfo_match_id),'match_ids.csv',row.names = F)
