rm(list = ls())

library(ggplot2)
library(png)
library(grid)
library(dplyr)
library(readr)
library(tidyr)

#Set working directory
setwd(dir = '/Users/sambeet/Data Science/cricket_ipl_analysis')

img <- readPNG("cricket_ground.png")
all_data = read_csv('ipl_data.csv')

plot_data = all_data[all_data$runs_off_bat != 0,][1:200,]

create_wagon_wheel_plot = function(plot_data){
    wagon_wheel_plot = ggplot(data = plot_data,aes(colour = factor(runs_off_bat))) + 
        ggtitle('Wagon Wheel') + 
        annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) + 
        theme_void() + geom_segment(data = plot_data,
                                    aes(x = wagon_x - 180,y = wagon_y - 180,xend = 0,yend = 0,
                                        size = I(0.25*runs_off_bat)),lineend = 'butt') + 
        theme(legend.key = element_rect(colour = "black",fill = 'white'),
              legend.text = element_text(size = 13)) + 
        scale_colour_manual(name = 'Run Type:',
                            values = c("1" = 'yellow2',"2" = 'orange3',"3" = 'antiquewhite3',
                                       "4" = 'steelblue3',"5" = 'slateblue4',"6" = 'tomato3'))
    return(wagon_wheel_plot)
}

create_wagon_wheel_plot(plot_data)

create_wagon_zone_plot = function(plot_data){
    plot_data$zone = factor(plot_data$wagon_zone,levels = c("1","2","3","4","5","6","7","8"),nmax = 8)
    wagon_zone_plot_data = plot_data %>% 
        group_by(zone) %>% summarise(runs = sum(runs_off_bat)) %>% ungroup() %>% 
        mutate(percentage_runs = round(100*runs/sum(runs),0)) %>% 
        complete(zone,fill = list(runs = 0,percentage_runs = 0)) %>% 
        mutate(labels_pct = paste(percentage_runs,'%',sep = ''),
               labels_runs = paste('Runs: ',runs,sep = ''))
    wagon_zone_plot = ggplot(data = wagon_zone_plot_data,mapping = aes(zone)) + 
        geom_bar(position = 'stack',fill = 'green4') + coord_polar() + theme_void() + 
        ggtitle('Wagon Zone') + geom_text(aes(label = labels_pct, y = 0.75),size = 5.7,col = 'whitesmoke') +
        geom_text(aes(label = labels_runs, y = 1.2),size = 3.5,col = 'black')
    return(wagon_zone_plot)
}

create_wagon_zone_plot(plot_data)
