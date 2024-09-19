## Figures for Ocean Acidification Risk

## **Laura Gruenburg, lagruenburg@gmail.com**

##   **LAST UPDATED: Jan 10, 2023**

#####load required functions
#  You will need to dfwnload the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
#library(mgcViz)
library(ggplot2)
library(dplyr)

#######Load the datasets
setwd("/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/")
df<-read.csv("Aragonite_2023.csv", header = TRUE)


###################


year = c(2018.875,2018.875, 2019.875,2019.875, 2020.875,2020.875, 2021.875,2021.875,2022.875, 2022.875, 2023.875, 2023.875)
yval = c(0, 47, 0, 47, 0, 47, 0, 47, 0, 47, 0, 47)

vertical_lines <- data.frame(year, yval)
ggplot() + 
  
  geom_rect(aes(xmin=2018.875, xmax=2019.125, ymin=0, ymax=47), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2019.125, xmax=2019.375, ymin=0, ymax=47), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2019.375, xmax=2019.625, ymin=0, ymax=47), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2019.625, xmax=2019.875, ymin=0, ymax=47), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2019.875, xmax=2020.125, ymin=0, ymax=47), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2020.125, xmax=2020.375, ymin=0, ymax=47), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2020.375, xmax=2020.625, ymin=0, ymax=47), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2020.625, xmax=2020.875, ymin=0, ymax=47), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2020.875, xmax=2021.125, ymin=0, ymax=47), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2021.125, xmax=2021.375, ymin=0, ymax=47), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2021.375, xmax=2021.625, ymin=0, ymax=47), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2021.625, xmax=2021.875, ymin=0, ymax=47), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2021.875, xmax=2022.125, ymin=0, ymax=47), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2022.125, xmax=2022.375, ymin=0, ymax=47), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2022.375, xmax=2022.625, ymin=0, ymax=47), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2022.625, xmax=2022.875, ymin=0, ymax=47), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2022.875, xmax=2023.125, ymin=0, ymax=47), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2023.125, xmax=2023.375, ymin=0, ymax=47), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2023.375, xmax=2023.625, ymin=0, ymax=47), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2023.625, xmax=2023.875, ymin=0, ymax=47), fill = 'orange', alpha = 0.5) +
  
  geom_line(data = vertical_lines[1:2,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[3:4,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[5:6,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[7:8,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[9:10,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[11:12,], aes(x = year, y = yval), color = 'black') +
  
  #geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +
  geom_point(data = df[1:8,], aes(x = year_frac, y = X..Area.under.1.7), color = 'black', size =3) +
  #geom_line(data = T7, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue',linetype = 'dashed') +
  geom_point(data = df[9:10,], aes(x = year_frac, y = X..Area.under.1.7),shape = 17, color = 'black', size =3) +
  
  #geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +
  geom_point(data = df[1:8,], aes(x = year_frac, y = X..Area.under.1.5), shape = 1, color = 'black', size =3) +
  #geom_line(data = T7, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue',linetype = 'dashed') +
  geom_point(data = df[9:10,], aes(x = year_frac, y = X..Area.under.1.5),shape = 2, color = 'black', size =3) +
  
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )+
  annotate("text", x = 2019.35, y = -3.7, label = "2019", size=5)+
  annotate("text", x = 2020.35, y = -3.7, label = "2020", size=5)+
  annotate("text", x = 2021.35, y = -3.7, label = "2021", size=5)+
  annotate("text", x = 2022.35, y = -3.7, label = "2022", size=5)+
  annotate("text", x = 2023.35, y = -3.7, label = "2023", size=5)+
  coord_cartesian(ylim = c(-0.25, 47),  clip = 'off') +
  labs (y = 'Percent Area', x = '', title = 'Regions of Ocean Acidification Concern') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

