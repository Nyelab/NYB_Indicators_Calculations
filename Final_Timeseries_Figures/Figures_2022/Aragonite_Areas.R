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
setwd("/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/")
df<-read.csv("Aragonite2022.csv", header = TRUE)

T1 = df[df$Transect == 1,]
T2 = df[df$Transect == 2,]
T3 = df[df$Transect == 3,]
T4 = df[df$Transect == 4,]
T5 = df[df$Transect == 5,]
T6 = df[df$Transect == 6,]
T7 = df[df$Transect == 7,]

ggplot() + 
  geom_line(data = T1, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'red') +
  geom_point(data = T1, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'red') +
  
  geom_line(data = T2, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'orange') +
  geom_point(data = T2, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'orange') +
  
  geom_line(data = T3, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'goldenrod') +
  geom_point(data = T3, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'goldenrod') +
  
  geom_line(data = T4, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'green') +
  geom_point(data = T4, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'green') +
  
  geom_line(data = T5, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'cyan') +
  geom_point(data = T5, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'cyan') +
  
  geom_line(data = T6, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'blue') +
  geom_point(data = T6, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'blue') +
  
  geom_line(data = T7, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'purple') +
  geom_point(data = T7, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'purple') +
  
  #geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'orange') +
  #geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'red') +
  #geom_smooth(data = df, aes(X = X, y = Percent_Inhospitable), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(X = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, X = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, X = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Percent', x = 'Cruise', title = 'Area under 1.7') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


ggplot() + 
  geom_line(data = T6, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'blue') +
  geom_point(data = T6, aes(x = Cruise, y = X..Area.under.1.7, group = 1), color = 'blue') +

  geom_line(data = T6, aes(x = Cruise, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  geom_point(data = T6, aes(x = Cruise, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  
  geom_line(data = T6, aes(x = Cruise, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  geom_point(data = T6, aes(x = Cruise, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  
  #geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'orange') +
  #geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'red') +
  #geom_smooth(data = df, aes(X = X, y = Percent_Inhospitable), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(X = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, X = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, X = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Percent', x = 'Cruise', title = 'Area under Aragonite Threshholds') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

ggplot() + 
  geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +
  #geom_point(data = T6, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.00,], aes(x = year_frac, y = X..Area.under.1.7, group = 1), shape = 8, color = 'blue', size =3) +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.25,], aes(x = year_frac, y = X..Area.under.1.7, group = 1), shape = 21, color = 'blue', size =3) +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.50,], aes(x = year_frac, y = X..Area.under.1.7, group = 1), shape = 17, color = 'blue', size =3) +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.75,], aes(x = year_frac, y = X..Area.under.1.7, group = 1), shape = 9, color = 'blue', size =3) +
  
  geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  #geom_point(data = T6, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.00,], aes(x = year_frac, y = X..Area.under.1.5, group = 1), shape = 8, color = 'darkgreen', size =3) +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.25,], aes(x = year_frac, y = X..Area.under.1.5, group = 1), shape = 21, color = 'darkgreen', size =3) +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.50,], aes(x = year_frac, y = X..Area.under.1.5, group = 1), shape = 17, color = 'darkgreen', size =3) +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.75,], aes(x = year_frac, y = X..Area.under.1.5, group = 1), shape = 9, color = 'darkgreen', size =3) +
  
  geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  #geom_point(data = T6, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.00,], aes(x = year_frac, y = X..Area.under.1.1, group = 1), shape = 8, color = 'goldenrod', size =3) +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.25,], aes(x = year_frac, y = X..Area.under.1.1, group = 1), shape = 21, color = 'goldenrod', size =3) +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.50,], aes(x = year_frac, y = X..Area.under.1.1, group = 1), shape = 17, color = 'goldenrod', size =3) +
  geom_point(data = T6[T6$year_frac %% floor(T6$year_frac) == 0.75,], aes(x = year_frac, y = X..Area.under.1.1, group = 1), shape = 9, color = 'goldenrod', size =3) +
  
  #geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'orange') +
  #geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'red') +
  #geom_smooth(data = df, aes(X = X, y = Percent_Inhospitable), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(X = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, X = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, X = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Percent', x = 'Year', title = 'Aragonite Saturation State') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

ggplot() + 
  geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +
  geom_point(data = T6, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +
  geom_line(data = T7, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue',linetype = 'dashed') +
  geom_point(data = T7, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +

  geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  geom_point(data = T6, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  geom_line(data = T7, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen',linetype = 'dashed') +
  geom_point(data = T7, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  
  
  geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  geom_point(data = T6, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  geom_line(data = T7, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod',linetype = 'dashed') +
  geom_point(data = T7, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  
  #geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'orange') +
  #geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'red') +
  #geom_smooth(data = df, aes(X = X, y = Percent_Inhospitable), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(X = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, X = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, X = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Percent', x = 'Year', title = 'Transect 6 Area under Aragonite Threshholds') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


ggplot() + 
  geom_line(data = T1, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +
  geom_point(data = T1, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +
  
  geom_line(data = T1, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  geom_point(data = T1, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  
  geom_line(data = T1, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  geom_point(data = T1, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  
  #geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'orange') +
  #geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'red') +
  #geom_smooth(data = df, aes(X = X, y = Percent_Inhospitable), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(X = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, X = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, X = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Percent', x = 'Year', title = 'Transect 1 Area under Aragonite Threshholds') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

###################
first_column <- c(2019, 2022.75)
second_column <- c(3, 3)
hypoxia_risk <- data.frame(first_column, second_column)

second_column2 <- c(2, 2)
hypoxia<- data.frame(first_column, second_column)
annotation1 <- data.frame(
  x = c(2020.75),
  y = c(-1),
  label = c("hypoxia risk")
)

annotation2 <- data.frame(
  x = c(2020.75),
  y = c(2.2),
  label = c("hypoxia")
)

year = c(2018.875,2018.875, 2019.875,2019.875, 2020.875,2020.875, 2021.875,2021.875,2022.875, 2022.875)
yval = c(0, 47, 0, 47, 0, 47, 0, 47, 0, 47)

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
  
  geom_line(data = vertical_lines[1:2,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[3:4,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[5:6,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[7:8,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[9:10,], aes(x = year, y = yval), color = 'black') +

  #geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue') +
  geom_point(data = T6, aes(x = year_frac-0.05, y = X..Area.under.1.7, group = 1), color = 'black', size =3) +
  #geom_line(data = T7, aes(x = year_frac, y = X..Area.under.1.7, group = 1), color = 'blue',linetype = 'dashed') +
  geom_point(data = T7[c(5,6),], aes(x = year_frac-0.05, y = X..Area.under.1.7, group = 1), color = 'black', size =3) +
  
  #geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen') +
  #geom_point(data = T6, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'black', shape = 1, size =3) +
  #geom_line(data = T7, aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'darkgreen',linetype = 'dashed') +
  #geom_point(data = T7[c(5,6),], aes(x = year_frac, y = X..Area.under.1.5, group = 1), color = 'black', shape = 0, size = 3) +
  
  
  #geom_line(data = T6, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod') +
  geom_point(data = T6, aes(x = year_frac+0.05, y = X..Area.under.1.1, group = 1), color = 'black', shape = 0, size =3) +
  #geom_line(data = T7, aes(x = year_frac, y = X..Area.under.1.1, group = 1), color = 'goldenrod',linetype = 'dashed') +
  geom_point(data = T7[c(5,6),], aes(x = year_frac+0.05, y = X..Area.under.1.1, group = 1), color = 'black', shape = 0, size = 3) +
  
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )+
  annotate("text", x = 2019.35, y = -3.7, label = "2019", size=5)+
  annotate("text", x = 2020.35, y = -3.7, label = "2020", size=5)+
  annotate("text", x = 2021.35, y = -3.7, label = "2021", size=5)+
  annotate("text", x = 2022.35, y = -3.7, label = "2022", size=5)+
  coord_cartesian(ylim = c(-0.25, 47),  clip = 'off') +
  labs (y = 'Percent Area', x = '', title = 'Regions of Ocean Acidification Concern') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

