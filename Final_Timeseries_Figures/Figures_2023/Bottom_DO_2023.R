
#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
#library(mgcViz)
library(ggplot2)
library(dplyr)

#######Load the datasets
setwd("/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/")
DO<-read.csv("Bottom_DO_2023.csv", header = TRUE)

quarter = c(2019.0, 2019.25, 2019.5, 2019.75, 2020.0, 2020.75, 2021.0, 2021.25, 2022.0, 2022.25, 2022.5, 2022.75, 2023.25, 2023.75)
DO$Quarter = quarter

first_column <- c(2018.875, 2023.875)
second_column <- c(3, 3)
hypoxia_risk <- data.frame(first_column, second_column)

second_column2 <- c(2, 2)
hypoxia<- data.frame(first_column, second_column)
annotation1 <- data.frame(
  x = c(2020.8),
  y = c(3.2),
  label = c("hypoxia risk")
)

annotation2 <- data.frame(
  x = c(2020.8),
  y = c(2.2),
  label = c("hypoxia")
)

year = c(2018.875,2018.875, 2019.875,2019.875, 2020.875,2020.875, 2021.875,2021.875,2022.875, 2022.875,2023.875, 2023.875)
yval = c(1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10)

vertical_lines <- data.frame(year, yval)

Val_plot <- ggplot() + 
  geom_rect(aes(xmin=2018.875, xmax=2019.125, ymin=1, ymax=10), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2019.125, xmax=2019.375, ymin=1, ymax=10), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2019.375, xmax=2019.625, ymin=1, ymax=10), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2019.625, xmax=2019.875, ymin=1, ymax=10), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2019.875, xmax=2020.125, ymin=1, ymax=10), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2020.125, xmax=2020.375, ymin=1, ymax=10), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2020.375, xmax=2020.625, ymin=1, ymax=10), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2020.625, xmax=2020.875, ymin=1, ymax=10), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2020.875, xmax=2021.125, ymin=1, ymax=10), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2021.125, xmax=2021.375, ymin=1, ymax=10), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2021.375, xmax=2021.625, ymin=1, ymax=10), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2021.625, xmax=2021.875, ymin=1, ymax=10), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2021.875, xmax=2022.125, ymin=1, ymax=10), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2022.125, xmax=2022.375, ymin=1, ymax=10), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2022.375, xmax=2022.625, ymin=1, ymax=10), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2022.625, xmax=2022.875, ymin=1, ymax=10), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2022.875, xmax=2023.125, ymin=1, ymax=10), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2023.125, xmax=2023.375, ymin=1, ymax=10), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2023.375, xmax=2023.625, ymin=1, ymax=10), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2023.625, xmax=2023.875, ymin=1, ymax=10), fill = 'orange', alpha = 0.5) +
  
  geom_line(data = vertical_lines[1:2,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[3:4,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[5:6,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[7:8,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[9:10,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[11:12,], aes(x = year, y = yval), color = 'black') +
  
  #geom_line(data = DO_meds, aes(x = Quarterly, y = Val), color = 'grey53') +
  geom_point(data = DO[1:12,], aes(x = Quarter, y = DO), color = 'black', size = 3) + 
  geom_point(data = DO[13:14,], aes(x = Quarter, y = DO), shape = 17, size =3) + 
  geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'black', linetype = 'dashed') +
  geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'black', linetype = 'dashed') +
  #geom_smooth(data = DO_meds, aes(Quarterly = Quarterly, y = Val), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(Quarterly = Quarterly, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, Quarterly = Quarterly), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, Quarterly = Quarterly), color = 'red', size = 1) + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )+
  annotate("text", x = 2019.35, y = 0.25, label = "2019", size=5)+
  annotate("text", x = 2020.35, y = 0.25, label = "2020", size=5)+
  annotate("text", x = 2021.35, y = 0.25, label = "2021", size=5)+
  annotate("text", x = 2022.35, y = 0.25, label = "2022", size=5)+
  annotate("text", x = 2023.35, y = 0.25, label = "2023", size=5)+
  coord_cartesian(ylim = c(1, 10),  clip = 'off') +
  geom_text(data=annotation1, aes( x=x, y=y, label=label),                 , 
            color="black", 
            size=5) +
  geom_text(data=annotation2, aes( x=x, y=y, label=label),                 , 
            color="black", 
            size=5) +
  labs (y = bquote("mg/L"), x = '', title = 'Bottom Dissolved Oxygen') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

