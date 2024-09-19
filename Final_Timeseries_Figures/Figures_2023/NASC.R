
#####load required functions
#  You will need to NASCwnload the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
#library(mgcViz)
library(ggplot2)
library(dplyr)

#######Load the datasets
setwd("/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/")
NASC<-read.csv("strat_nasc_2018_2022.csv", header = TRUE)


year = c(2017.875, 2017.875,2018.875,2018.875, 2019.875,2019.875, 2020.875,2020.875, 2021.875,2021.875,2022.875, 2022.875)
yval_38 = c(0, 800, 0, 800, 0, 800, 0, 800, 0, 800, 0, 800)
yval_120 = c(0, 300, 0, 300, 0, 300, 0, 300, 0, 300, 0, 300)
yval_70 = c(0, 350, 0, 350, 0, 350, 0, 350, 0, 350, 0, 350)
yval_200 = c(0, 200, 0, 200, 0, 200, 0, 200, 0, 200, 0, 200)

vertical_lines38 <- data.frame(year, yval_38)
vertical_lines70 <- data.frame(year, yval_70)
vertical_lines120 <- data.frame(year, yval_120)
vertical_lines200 <- data.frame(year, yval_200)

annotation38 <- data.frame(
  x = c(2018.25),
  y = c(750),
  label = c("38kHz")
) 
  
annotation70 <- data.frame(
    x = c(2018.25),
    y = c(325),
    label = c("70kHz")
)

annotation120 <- data.frame(
  x = c(2018.25),
  y = c(275),
  label = c("120kHz")
)

annotation200 <- data.frame(
  x = c(2018.25),
  y = c(185),
  label = c("200kHz")
)

plot38 <- ggplot() + 
  geom_rect(aes(xmin=2017.875, xmax=2018.125, ymin=0, ymax=800), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2018.125, xmax=2018.375, ymin=0, ymax=800), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2018.375, xmax=2018.625, ymin=0, ymax=800), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2018.625, xmax=2018.875, ymin=0, ymax=800), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2018.875, xmax=2019.125, ymin=0, ymax=800), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2019.125, xmax=2019.375, ymin=0, ymax=800), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2019.375, xmax=2019.625, ymin=0, ymax=800), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2019.625, xmax=2019.875, ymin=0, ymax=800), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2019.875, xmax=2020.125, ymin=0, ymax=800), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2020.125, xmax=2020.375, ymin=0, ymax=800), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2020.375, xmax=2020.625, ymin=0, ymax=800), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2020.625, xmax=2020.875, ymin=0, ymax=800), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2020.875, xmax=2021.125, ymin=0, ymax=800), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2021.125, xmax=2021.375, ymin=0, ymax=800), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2021.375, xmax=2021.625, ymin=0, ymax=800), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2021.625, xmax=2021.875, ymin=0, ymax=800), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2021.875, xmax=2022.125, ymin=0, ymax=800), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2022.125, xmax=2022.375, ymin=0, ymax=800), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2022.375, xmax=2022.625, ymin=0, ymax=800), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2022.625, xmax=2022.875, ymin=0, ymax=800), fill = 'orange', alpha = 0.5) +
  
  geom_line(data = vertical_lines38[1:2,], aes(x = year, y = yval_38), color = 'black') +
  geom_line(data = vertical_lines38[3:4,], aes(x = year, y = yval_38), color = 'black') +
  geom_line(data = vertical_lines38[5:6,], aes(x = year, y = yval_38), color = 'black') +
  geom_line(data = vertical_lines38[7:8,], aes(x = year, y = yval_38), color = 'black') +
  geom_line(data = vertical_lines38[9:10,], aes(x = year, y = yval_38), color = 'black') +
  geom_line(data = vertical_lines38[11:12,], aes(x = year, y = yval_38), color = 'black') +
  
  geom_point(data = NASC[NASC$Frequency == "38kHz", ], aes(x = quarter, y = Survey_mean_NASC), color = 'black', size = 3) + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )+
  #annotate("text", x = 2028.35, y = -100, label = "2023", size=5)+
  #annotate("text", x = 2019.35, y = 0.25, label = "2019", size=5)+
  #annotate("text", x = 2020.35, y = 0.25, label = "2020", size=5)+
  #annotate("text", x = 2021.35, y = 0.25, label = "2021", size=5)+
  #annotate("text", x = 2022.35, y = 0.25, label = "2022", size=5)+
  coord_cartesian(ylim = c(0, 800),  clip = 'off') +
  geom_text(data=annotation38, aes( x=x, y=y, label=label),                 , 
            color="black", 
            size=5) +
  #geom_text(data=annotation2, aes( x=x, y=y, label=label),                 , 
  #          color="black", 
  #          size=5) +
  labs (y = bquote("NASC "~(m^2~" "~nmi^-2)~" "), x = '') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


plot70 <- ggplot() + 
  geom_rect(aes(xmin=2017.875, xmax=2018.125, ymin=0, ymax=350), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2018.125, xmax=2018.375, ymin=0, ymax=350), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2018.375, xmax=2018.625, ymin=0, ymax=350), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2018.625, xmax=2018.875, ymin=0, ymax=350), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2018.875, xmax=2019.125, ymin=0, ymax=350), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2019.125, xmax=2019.375, ymin=0, ymax=350), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2019.375, xmax=2019.625, ymin=0, ymax=350), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2019.625, xmax=2019.875, ymin=0, ymax=350), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2019.875, xmax=2020.125, ymin=0, ymax=350), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2020.125, xmax=2020.375, ymin=0, ymax=350), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2020.375, xmax=2020.625, ymin=0, ymax=350), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2020.625, xmax=2020.875, ymin=0, ymax=350), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2020.875, xmax=2021.125, ymin=0, ymax=350), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2021.125, xmax=2021.375, ymin=0, ymax=350), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2021.375, xmax=2021.625, ymin=0, ymax=350), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2021.625, xmax=2021.875, ymin=0, ymax=350), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2021.875, xmax=2022.125, ymin=0, ymax=350), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2022.125, xmax=2022.375, ymin=0, ymax=350), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2022.375, xmax=2022.625, ymin=0, ymax=350), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2022.625, xmax=2022.875, ymin=0, ymax=350), fill = 'orange', alpha = 0.5) +
  
  geom_line(data = vertical_lines70[1:2,], aes(x = year, y = yval_70), color = 'black') +
  geom_line(data = vertical_lines70[3:4,], aes(x = year, y = yval_70), color = 'black') +
  geom_line(data = vertical_lines70[5:6,], aes(x = year, y = yval_70), color = 'black') +
  geom_line(data = vertical_lines70[7:8,], aes(x = year, y = yval_70), color = 'black') +
  geom_line(data = vertical_lines70[9:10,], aes(x = year, y = yval_70), color = 'black') +
  geom_line(data = vertical_lines70[11:12,], aes(x = year, y = yval_70), color = 'black') +
  
  geom_point(data = NASC[NASC$Frequency == "70kHz", ], aes(x = quarter, y = Survey_mean_NASC), color = 'black', size = 3) + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )+
  #annotate("text", x = 2028.35, y = -100, label = "2023", size=5)+
  #annotate("text", x = 2019.35, y = 0.25, label = "2019", size=5)+
  #annotate("text", x = 2020.35, y = 0.25, label = "2020", size=5)+
  #annotate("text", x = 2021.35, y = 0.25, label = "2021", size=5)+
  #annotate("text", x = 2022.35, y = 0.25, label = "2022", size=5)+
  coord_cartesian(ylim = c(0, 350),  clip = 'off') +
  geom_text(data=annotation70, aes( x=x, y=y, label=label),                 , 
            color="black", 
            size=5) +
  #geom_text(data=annotation2, aes( x=x, y=y, label=label),                 , 
  #          color="black", 
  #          size=5) +
  labs (y = bquote(" "), x = '') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


plot120 <- ggplot() + 
  geom_rect(aes(xmin=2017.875, xmax=2018.125, ymin=0, ymax=300), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2018.125, xmax=2018.375, ymin=0, ymax=300), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2018.375, xmax=2018.625, ymin=0, ymax=300), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2018.625, xmax=2018.875, ymin=0, ymax=300), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2018.875, xmax=2019.125, ymin=0, ymax=300), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2019.125, xmax=2019.375, ymin=0, ymax=300), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2019.375, xmax=2019.625, ymin=0, ymax=300), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2019.625, xmax=2019.875, ymin=0, ymax=300), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2019.875, xmax=2020.125, ymin=0, ymax=300), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2020.125, xmax=2020.375, ymin=0, ymax=300), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2020.375, xmax=2020.625, ymin=0, ymax=300), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2020.625, xmax=2020.875, ymin=0, ymax=300), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2020.875, xmax=2021.125, ymin=0, ymax=300), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2021.125, xmax=2021.375, ymin=0, ymax=300), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2021.375, xmax=2021.625, ymin=0, ymax=300), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2021.625, xmax=2021.875, ymin=0, ymax=300), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2021.875, xmax=2022.125, ymin=0, ymax=300), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2022.125, xmax=2022.375, ymin=0, ymax=300), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2022.375, xmax=2022.625, ymin=0, ymax=300), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2022.625, xmax=2022.875, ymin=0, ymax=300), fill = 'orange', alpha = 0.5) +
  
  geom_line(data = vertical_lines120[1:2,], aes(x = year, y = yval_120), color = 'black') +
  geom_line(data = vertical_lines120[3:4,], aes(x = year, y = yval_120), color = 'black') +
  geom_line(data = vertical_lines120[5:6,], aes(x = year, y = yval_120), color = 'black') +
  geom_line(data = vertical_lines120[7:8,], aes(x = year, y = yval_120), color = 'black') +
  geom_line(data = vertical_lines120[9:10,], aes(x = year, y = yval_120), color = 'black') +
  geom_line(data = vertical_lines120[11:12,], aes(x = year, y = yval_120), color = 'black') +
  
  geom_point(data = NASC[NASC$Frequency == "120kHz", ], aes(x = quarter, y = Survey_mean_NASC), color = 'black', size = 3) + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )+
  annotate("text", x = 2018.35, y = -32, label = "2018", size=5)+
  annotate("text", x = 2019.35, y = -32, label = "2019", size=5)+
  annotate("text", x = 2020.35, y = -32, label = "2020", size=5)+
  annotate("text", x = 2021.35, y = -32, label = "2021", size=5)+
  annotate("text", x = 2022.35, y = -32, label = "2022", size=5)+
  coord_cartesian(ylim = c(0, 300),  clip = 'off') +
  geom_text(data=annotation120, aes( x=x, y=y, label=label),                 , 
            color="black", 
            size=5) +
  #geom_text(data=annotation2, aes( x=x, y=y, label=label),                 , 
  #          color="black", 
  #          size=5) +
  labs (y = bquote("NASC "~(m^2~" "~nmi^-2)~" "), x = '') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

plot200 <- ggplot() + 
  geom_rect(aes(xmin=2017.875, xmax=2018.125, ymin=0, ymax=200), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2018.125, xmax=2018.375, ymin=0, ymax=200), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2018.375, xmax=2018.625, ymin=0, ymax=200), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2018.625, xmax=2018.875, ymin=0, ymax=200), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2018.875, xmax=2019.125, ymin=0, ymax=200), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2019.125, xmax=2019.375, ymin=0, ymax=200), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2019.375, xmax=2019.625, ymin=0, ymax=200), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2019.625, xmax=2019.875, ymin=0, ymax=200), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2019.875, xmax=2020.125, ymin=0, ymax=200), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2020.125, xmax=2020.375, ymin=0, ymax=200), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2020.375, xmax=2020.625, ymin=0, ymax=200), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2020.625, xmax=2020.875, ymin=0, ymax=200), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2020.875, xmax=2021.125, ymin=0, ymax=200), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2021.125, xmax=2021.375, ymin=0, ymax=200), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2021.375, xmax=2021.625, ymin=0, ymax=200), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2021.625, xmax=2021.875, ymin=0, ymax=200), fill = 'orange', alpha = 0.5) +
  
  geom_rect(aes(xmin=2021.875, xmax=2022.125, ymin=0, ymax=200), fill = 'lightblue', alpha = 0.5) +
  geom_rect(aes(xmin=2022.125, xmax=2022.375, ymin=0, ymax=200), fill = 'violet', alpha = 0.5) +
  geom_rect(aes(xmin=2022.375, xmax=2022.625, ymin=0, ymax=200), fill = 'green', alpha = 0.5) +
  geom_rect(aes(xmin=2022.625, xmax=2022.875, ymin=0, ymax=200), fill = 'orange', alpha = 0.5) +
  
  geom_line(data = vertical_lines200[1:2,], aes(x = year, y = yval_200), color = 'black') +
  geom_line(data = vertical_lines200[3:4,], aes(x = year, y = yval_200), color = 'black') +
  geom_line(data = vertical_lines200[5:6,], aes(x = year, y = yval_200), color = 'black') +
  geom_line(data = vertical_lines200[7:8,], aes(x = year, y = yval_200), color = 'black') +
  geom_line(data = vertical_lines200[9:10,], aes(x = year, y = yval_200), color = 'black') +
  geom_line(data = vertical_lines200[11:12,], aes(x = year, y = yval_200), color = 'black') +
  
  geom_point(data = NASC[NASC$Frequency == "200kHz", ], aes(x = quarter, y = Survey_mean_NASC), color = 'black', size = 3) + 
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )+
  annotate("text", x = 2018.35, y = -20, label = "2018", size=5)+
  annotate("text", x = 2019.35, y = -20, label = "2019", size=5)+
  annotate("text", x = 2020.35, y = -20, label = "2020", size=5)+
  annotate("text", x = 2021.35, y = -20, label = "2021", size=5)+
  annotate("text", x = 2022.35, y = -20, label = "2022", size=5)+
  coord_cartesian(ylim = c(0, 200),  clip = 'off') +
  geom_text(data=annotation200, aes( x=x, y=y, label=label),                 , 
            color="black", 
            size=5) +
  #geom_text(data=annotation2, aes( x=x, y=y, label=label),                 , 
  #          color="black", 
  #          size=5) +
  labs (y = bquote(" "), x = '') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Now plot all 4 together
library(ggpubr)

ggarrange(plot38,plot70,plot120,plot200,nrow=2,ncol=2)

