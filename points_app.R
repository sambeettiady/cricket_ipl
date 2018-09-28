rm(list = ls())
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(ggthemes)

#setwd(dir = '/Users/sambeet/Data Science/cricket_ipl_analysis/')
x = read_csv(file = '2017_points_data.csv')
x$player_type = ifelse(x$foreign_player == 1,'Overseas',ifelse(x$uncapped_player == 1,'Uncapped Indian','Indian'))

ui = fluidPage(
    title = "IPL 2017 Performance vs Price",
    fluidRow(
        column(3,selectInput(inputId = 'points_system',
                           label = 'Choose Points System:',
                           choices = c("IPL Fantasy League" = "ipl",
                                       "Fandromeda" = "fandromeda")),
               selectInput(inputId = 'color_factor',
                           label = 'Choose Grouping Variable:',
                           choices = c("Role" = "role",
                                       "Player Nationality" = "player_type",
                                       "Team" = "team",
                                       "Batting Hand" = "batting_hand",
                                       "Bowling Hand" = "bowling_hand",
                                       "Bowling Style" = "bowling_style")),
               checkboxGroupInput("filter_team", "Select Teams:",
                                  unique(x$team),selected = unique(x$team)),
               checkboxGroupInput("filter_role", "Select Roles:",
                                  unique(x$role),selected = unique(x$role)),
               checkboxGroupInput("filter_nationality", "Select Nationality:",
                                  unique(x$player_type),selected = unique(x$player_type))),
        column(width = 9,plotlyOutput('interactive_plot',width = "1000px",height = "700px"))),
    textOutput(outputId = 'creator')
)

server = function(input,output){
    output$interactive_plot <- renderPlotly(expr =
      {                                         y = x[x$team %in% input$filter_team,]
                                                y = y[y$role %in% input$filter_role,]
                                                y = y[y$player_type %in% input$filter_nationality,]
                                                if(input$points_system == 'ipl'){
                                                    g = ggplot(data = y,
                                                               mapping = aes(text = paste(paste('Name:',player),paste('Team:',team),paste('Role:',role),paste('Avg. Points:',round(avg_points_overall,0)),paste('Innings:',innings),sep = '<br>'),
                                                                  x = value - mean(x$value),
                                                                  y = avg_points_overall - mean(x$avg_points_overall),
                                                                  alpha = sqrt(innings),size = sqrt(innings))) + 
                                                        geom_point(mapping = aes_string(col = input$color_factor)) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + xlim(c(-25,30)) + ylim(c(-50,80)) + 
                                                        ggtitle('Performance Vs Price') + xlab('Price') + ylab('Performance') + geom_hline(yintercept = 20,size = 1,color = 'green') + guides(fill=guide_legend(title=NULL)) + theme_fivethirtyeight()
                                                    ggplotly(g,tooltip = 'text')
                                                }else{
                                                    g = ggplot(data = y,
                                                               mapping = aes(text = paste(paste('Name:',player),paste('Team:',team),paste('Role:',role),paste('Avg. Points:',round(avg_points_overall_fandromeda,0)),paste('Innings:',innings),sep = '<br>'),
                                                                             x = value - mean(x$value),
                                                                             y = avg_points_overall_fandromeda - mean(x$avg_points_overall_fandromeda),
                                                                             alpha = sqrt(innings),size = sqrt(innings))) + 
                                                        geom_point(mapping = aes_string(col = input$color_factor)) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + xlim(c(-25,30)) + ylim(c(-50,100)) + 
                                                        ggtitle('Performance Vs Price') + xlab('Price') + ylab('Performance') + geom_hline(yintercept = 30,size = 1,color = 'green') + guides(fill=guide_legend(title=NULL)) + theme_fivethirtyeight()
                                                    ggplotly(g,tooltip = 'text')
                                                }
                                                })
    output$creator = renderText(expr = '*Note: 1) Fielding points have not been included, 2) Updated till Match 41 - KKR vs RPSG')
}

shinyApp(ui = ui,server = server)
