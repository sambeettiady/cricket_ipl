rm(list =ls())
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(jpeg)
source('ipl_2016_fantasy.R')

player_data = read_csv(file = 'ipl_players.csv')
player_data = player_data[player_data$team != 'Not Playing',]
players = unique(player_data$player)

ui = fluidPage(fluidRow(column(width = 3,selectInput(inputId = 'player',label = 'Choose Player',choices = players)),
                        column(width = 3,selectInput(inputId = 'analysis_type',label = 'Choose Analysis Type',choices = c('Batting','Bowling')))),
               title = 'IPL Player Analysis',
               fluidRow(
                    column(width = 2,imageOutput("Player_Photo",width = "150px",height = "150px")),
                    column(width = 2,textOutput(outputId = 'team')),
                    column(width = 2,textOutput(outputId = 'role')),
                    column(width = 2,textOutput(outputId = 'foreign')),
                    column(width = 2,textOutput(outputId = 'batting_style')),
                    column(width = 2,textOutput(outputId = 'bowling_style'))
               ),
               navbarPage(
               title = 'Player Performance',
               tabPanel(title = 'Overall', DT::dataTableOutput('Overall_Performance')),
               tabPanel(title = 'Innings No.', DT::dataTableOutput('innings')),
               tabPanel('Vs Teams', DT::dataTableOutput('team_wise')),
               tabPanel('All Stadiums', DT::dataTableOutput('stadium_wise')),
               tabPanel('Batting/Bowling Type',DT::dataTableOutput('type')),
               tabPanel('Season/Year', DT::dataTableOutput('season')),
               tabPanel('Batting Position', DT::dataTableOutput('batting_position')),
               tabPanel('Over Category', DT::dataTableOutput('over_category')),
               tabPanel('Vs Batsman/Bowler', DT::dataTableOutput('vs_batsman_or_bowler')),
               tabPanel('Cumulative',  
                        fluidRow(column(width = 2,conditionalPanel(condition = "input.analysis_type == 'Batting'",
                                         radioButtons("batting", "Variable to Plot:",
                                                      c("Runs" = "cum_runs",
                                                        "Average" = "cum_batting_avg",
                                                        "Strike Rate" = "strike_rate",
                                                        "Fours" = "cum_fours",
                                                        "Sixes" = "cum_sixes"))),
                        conditionalPanel(condition = "input.analysis_type == 'Bowling'",
                                         radioButtons("bowling", "Variable to Plot:",
                                                      c("Wickets" = "cum_wickets",
                                                        "Average" = "cum_bowling_avg",
                                                        "Strike Rate" = "cum_strike_rate",
                                                        "Economy Rate" = "cum_economy_rate",
                                                        "Dot Ball Percentage" = "cum_dot_ball_pct")))),
                        column(width = 10,plotOutput('cumulative'))))
               ),
               fluidRow(textOutput(outputId = 'creator'))
)

server = function(input,output){
    output$Player_Photo = renderImage({
        filename = normalizePath(file.path('./player_photos',paste(input$player, '.jpg', sep='')))
        list(src = filename,width = 150,height = 150)
    }, deleteFile = FALSE)
    output$team = renderText(expr = paste('Team:',player_data$team[player_data$player == input$player]))
    output$role = renderText(paste('Player Role:',player_data$role[player_data$player == input$player]))
    output$batting_style = renderText(paste('Batting:',player_data$batting_hand[player_data$player == input$player]))
    output$bowling_style = renderText(paste('Bowling:',player_data$bowling_hand[player_data$player == input$player],player_data$bowling_style[player_data$player == input$player]))
    output$foreign = renderText(paste('Overseas Player:',ifelse(player_data$foreign_player[player_data$player == input$player] == 1,'Yes','No')))
    
    output$Overall_Performance <- DT::renderDataTable(expr = 
        if(input$analysis_type == 'Batting'){DT::datatable(get_player_batting_performance(player = input$player),options = list(pageLength = 1,searching = F,paging = F),rownames = F)
        }else if(input$analysis_type == 'Bowling'){DT::datatable(get_player_bowling_performance(player = input$player),options = list(pageLength = 1,searching = F,paging = F),rownames = F)}
    )
    output$innings <- DT::renderDataTable(
        if(input$analysis_type == 'Batting'){DT::datatable(get_player_batting_performance(player = input$player,overall = F,grouping_var_string = 'innings_no'),options = list(pageLength = 2,searching = F,paging = F),rownames = F)
        }else if(input$analysis_type == 'Bowling'){DT::datatable(get_player_bowling_performance(player = input$player,overall = F,grouping_var_string = 'innings_no'),options = list(pageLength = 2,searching = F,paging = F),rownames = F)}
    )
    output$team_wise <- DT::renderDataTable(
        if(input$analysis_type == 'Batting'){DT::datatable(get_player_batting_performance(player = input$player,overall = F,grouping_var_string = 'bowling_team'),options = list(pageLength = 15,searching = F,paging = F),rownames = F)
        }else if(input$analysis_type == 'Bowling'){DT::datatable(get_player_bowling_performance(player = input$player,overall = F,grouping_var_string = 'batting_team'),options = list(pageLength = 15,searching = F,paging = F),rownames = F)}
    )
    output$stadium_wise <- DT::renderDataTable(
        if(input$analysis_type == 'Batting'){DT::datatable(get_player_batting_performance(player = input$player,overall = F,grouping_var_string = 'stadium'),options = list(pageLength = 10),rownames = F)
        }else if(input$analysis_type == 'Bowling'){DT::datatable(get_player_bowling_performance(player = input$player,overall = F,grouping_var_string = 'stadium'),options = list(pageLength = 10),rownames = F)}
    )
    output$type <- DT::renderDataTable(
        if(input$analysis_type == 'Batting'){DT::datatable(get_player_batting_performance(player = input$player,overall = F,grouping_var_string = 'bowling_style'),options = list(pageLength = 10,searching = F,paging = F),rownames = F)
        }else if(input$analysis_type == 'Bowling'){DT::datatable(get_player_bowling_performance(player = input$player,overall = F,grouping_var_string = 'batting_hand'),options = list(pageLength = 2,searching = F,paging = F),rownames = F)}
    )
    output$season <- DT::renderDataTable(
        if(input$analysis_type == 'Batting'){DT::datatable(get_player_batting_performance(player = input$player,overall = F,grouping_var_string = 'season_year'),options = list(pageLength = 10),rownames = F)
        }else if(input$analysis_type == 'Bowling'){DT::datatable(get_player_bowling_performance(player = input$player,overall = F,grouping_var_string = 'season_year'),options = list(pageLength = 10),rownames = F)}
    )
    output$batting_position <- DT::renderDataTable(
        if(input$analysis_type == 'Batting'){DT::datatable(get_player_batting_performance(player = input$player,overall = F,grouping_var_string = 'batting_position'),options = list(pageLength = 11,searching = F,paging = F),rownames = F)
        }else if(input$analysis_type == 'Bowling'){DT::datatable(get_player_bowling_performance(player = input$player,overall = F,grouping_var_string = 'batting_position'),options = list(pageLength = 11,searching = F,paging = F),rownames = F)}
    )
    output$over_category <- DT::renderDataTable(
        if(input$analysis_type == 'Batting'){DT::datatable(get_player_batting_performance(player = input$player,overall = F,grouping_var_string = 'over_category'),options = list(pageLength = 4,searching = F,paging = F),rownames = F)
        }else if(input$analysis_type == 'Bowling'){DT::datatable(get_player_bowling_performance(player = input$player,overall = F,grouping_var_string = 'over_category'),options = list(pageLength = 4,searching = F,paging = F),rownames = F)}
    )
    output$vs_batsman_or_bowler <- DT::renderDataTable(
        if(input$analysis_type == 'Batting'){DT::datatable(get_player_batting_performance(player = input$player,overall = F,grouping_var_string = 'bowler'),options = list(pageLength = 10),rownames = F)
        }else if(input$analysis_type == 'Bowling'){DT::datatable(get_player_bowling_performance(player = input$player,overall = F,grouping_var_string = 'batsman'),options = list(pageLength = 10),rownames = F)}
    )
    output$cumulative <- renderPlot(expr = 
        if(input$analysis_type == 'Batting'){
            cum_plot = ggplot(data = get_player_batting_performance(player = input$player, overall = F, cumulative_avg = T))
            cum_plot = cum_plot + geom_point(mapping = aes_string(x = "match_index", y = input$batting)) + xlab('Innings No.')
            cum_plot
        }else if(input$analysis_type == 'Bowling'){
            cum_plot = ggplot(data = get_player_bowling_performance(player = input$player, overall = F, cumulative_avg = T))
            cum_plot = cum_plot + geom_point(mapping = aes_string(x = "match_index", y = input$bowling)) + xlab('Innings No.')
            cum_plot
        }
    )
    output$creator = renderText(expr = 'Created By: Sambeet Tiady, Email: sambeet.tiady@gmail.com')
}

shinyApp(ui = ui,server = server)