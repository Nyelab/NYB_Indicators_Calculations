
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
DO<-read.csv("centropages_2018_2022.csv", header = TRUE)

year = c(2017.875, 2017.875, 2018.875,2018.875, 2019.875,2019.875, 2020.875,2020.875, 2021.875,2021.875,2022.875, 2022.875)
yval = c(1, 1650, 1, 1650, 1, 1650, 1, 1650, 1, 1650, 1, 1650)

vertical_lines <- data.frame(year, yval)

Val_plot <- ggplot() + 
  geom_rect(aes(xmin=2017.875, xmax=2018.125, ymin=1, ymax=1650), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2018.125, xmax=2018.375, ymin=1, ymax=1650), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2018.375, xmax=2018.625, ymin=1, ymax=1650), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2018.625, xmax=2018.875, ymin=1, ymax=1650), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2018.875, xmax=2019.125, ymin=1, ymax=1650), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2019.125, xmax=2019.375, ymin=1, ymax=1650), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2019.375, xmax=2019.625, ymin=1, ymax=1650), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2019.625, xmax=2019.875, ymin=1, ymax=1650), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2019.875, xmax=2020.125, ymin=1, ymax=1650), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2020.125, xmax=2020.375, ymin=1, ymax=1650), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2020.375, xmax=2020.625, ymin=1, ymax=1650), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2020.625, xmax=2020.875, ymin=1, ymax=1650), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2020.875, xmax=2021.125, ymin=1, ymax=1650), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2021.125, xmax=2021.375, ymin=1, ymax=1650), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2021.375, xmax=2021.625, ymin=1, ymax=1650), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2021.625, xmax=2021.875, ymin=1, ymax=1650), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2021.875, xmax=2022.125, ymin=1, ymax=1650), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2022.125, xmax=2022.375, ymin=1, ymax=1650), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2022.375, xmax=2022.625, ymin=1, ymax=1650), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2022.625, xmax=2022.875, ymin=1, ymax=1650), fill = 'orange', alpha = 0.5) +

  geom_line(data = vertical_lines[1:2,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[3:4,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[5:6,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[7:8,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[9:10,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[11:12,], aes(x = year, y = yval), color = 'black') +
  
  #geom_line(data = DO_meds, aes(x = Quarterly, y = Val), color = 'grey53') +
  geom_point(data = DO, aes(x = year, y = meanDens), color = 'black', size = 3) + 
  #geom_point(data = DO[13:14,], aes(x = Quarter, y = DO), shape = 17, size =3) + 
  #geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'black', linetype = 'dashed') +
  #geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'black', linetype = 'dashed') +
  #geom_smooth(data = DO_meds, aes(Quarterly = Quarterly, y = Val), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(Quarterly = Quarterly, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, Quarterly = Quarterly), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, Quarterly = Quarterly), color = 'red', size = 1) + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )+
  annotate("text", x = 2018.35, y = -118, label = "2018", size=5)+
  annotate("text", x = 2019.35, y = -118, label = "2019", size=5)+
  annotate("text", x = 2020.35, y = -118, label = "2020", size=5)+
  annotate("text", x = 2021.35, y = -118, label = "2021", size=5)+
  annotate("text", x = 2022.35, y = -118, label = "2022", size=5)+
  
  coord_cartesian(ylim = c(0, 1650),  clip = 'off') +

  labs (y = bquote("Number of individuals per " ~m^3~ " "), x = '', title = 'Centropages typicus') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

### C. finmarchicus

cf<-read.csv("calfin_2018_2022.csv", header = TRUE)

year = c(2017.875, 2017.875, 2018.875,2018.875, 2019.875,2019.875, 2020.875,2020.875, 2021.875,2021.875,2022.875, 2022.875)
yval = c(1, 250, 1, 250, 1, 250, 1, 250, 1, 250, 1, 250)

vertical_lines <- data.frame(year, yval)

Val_plot <- ggplot() + 
  geom_rect(aes(xmin=2017.875, xmax=2018.125, ymin=1, ymax=250), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2018.125, xmax=2018.375, ymin=1, ymax=250), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2018.375, xmax=2018.625, ymin=1, ymax=250), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2018.625, xmax=2018.875, ymin=1, ymax=250), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2018.875, xmax=2019.125, ymin=1, ymax=250), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2019.125, xmax=2019.375, ymin=1, ymax=250), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2019.375, xmax=2019.625, ymin=1, ymax=250), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2019.625, xmax=2019.875, ymin=1, ymax=250), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2019.875, xmax=2020.125, ymin=1, ymax=250), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2020.125, xmax=2020.375, ymin=1, ymax=250), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2020.375, xmax=2020.625, ymin=1, ymax=250), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2020.625, xmax=2020.875, ymin=1, ymax=250), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2020.875, xmax=2021.125, ymin=1, ymax=250), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2021.125, xmax=2021.375, ymin=1, ymax=250), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2021.375, xmax=2021.625, ymin=1, ymax=250), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2021.625, xmax=2021.875, ymin=1, ymax=250), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2021.875, xmax=2022.125, ymin=1, ymax=250), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2022.125, xmax=2022.375, ymin=1, ymax=250), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2022.375, xmax=2022.625, ymin=1, ymax=250), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2022.625, xmax=2022.875, ymin=1, ymax=250), fill = 'orange', alpha = 0.5) +
  
  geom_line(data = vertical_lines[1:2,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[3:4,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[5:6,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[7:8,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[9:10,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[11:12,], aes(x = year, y = yval), color = 'black') +
  
  #geom_line(data = cf_meds, aes(x = Quarterly, y = Val), color = 'grey53') +
  geom_point(data = cf, aes(x = year, y = meanDens), color = 'black', size = 3) + 
  #geom_point(data = cf[13:14,], aes(x = Quarter, y = cf), shape = 17, size =3) + 
  #geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'black', linetype = 'dashed') +
  #geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'black', linetype = 'dashed') +
  #geom_smooth(data = cf_meds, aes(Quarterly = Quarterly, y = Val), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(Quarterly = Quarterly, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, Quarterly = Quarterly), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, Quarterly = Quarterly), color = 'red', size = 1) + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )+
  annotate("text", x = 2018.35, y = -18, label = "2018", size=5)+
  annotate("text", x = 2019.35, y = -18, label = "2019", size=5)+
  annotate("text", x = 2020.35, y = -18, label = "2020", size=5)+
  annotate("text", x = 2021.35, y = -18, label = "2021", size=5)+
  annotate("text", x = 2022.35, y = -18, label = "2022", size=5)+
  
  coord_cartesian(ylim = c(0, 250),  clip = 'off') +
  
  labs (y = bquote("Number of individuals per " ~m^3~ " "), x = '', title = 'Calanus finmarchicus') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

