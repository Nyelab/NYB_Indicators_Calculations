## Figures for Bottom Dissolved Oxygen

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: December 21, 2022**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
#library(mgcViz)
library(ggplot2)
library(dplyr)

#######Load the datasets
setwd("/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/")
DO<-read.csv("btm_do_2022_forplot.csv", header = TRUE)

DO_meds <- DO %>% group_by(Quarterly) %>% summarise(Val = mean(DO))

mod<- gam(Val ~ s(Quarterly, k=5), data = DO_meds)
summary(mod) #check out model
gam.check(mod)

pdata <- with(DO_meds, data.frame(Quarterly = Quarterly))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept =  7.077225 # look at p2_mod and eQuarterlytract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

Term = "Quarterly"
mod.d <- Deriv(mod, n=12) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Quarterly, data = DO_meds)
lines(Val ~ Quarterly, data = DO_meds)
lines(p2_mod+intercept ~ Quarterly, data = pdata, type = "n")
lines(p2_mod+intercept ~ Quarterly, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Quarterly, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Quarterly, data = pdata, col = "red", lwd = 3)

linearMod<- lm(Val ~ Quarterly, data=DO_meds)
summary(linearMod)

first_column <- c(2018.875, 2022.875)
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

year = c(2018.875,2018.875, 2019.875,2019.875, 2020.875,2020.875, 2021.875,2021.875,2022.875, 2022.875)
yval = c(1, 10, 1, 10, 1, 10, 1, 10, 1, 10)

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
  
  geom_line(data = vertical_lines[1:2,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[3:4,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[5:6,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[7:8,], aes(x = year, y = yval), color = 'black') +
  geom_line(data = vertical_lines[9:10,], aes(x = year, y = yval), color = 'black') +
  
  #geom_line(data = DO_meds, aes(x = Quarterly, y = Val), color = 'grey53') +
  geom_point(data = DO_meds[1:4,], aes(x = Quarterly, y = Val), color = 'black', size = 3) + 
  geom_point(data = DO_meds[5:12,], aes(x = Quarterly, y = Val), shape = 17, size =3) + 
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
  coord_cartesian(ylim = c(1, 10),  clip = 'off') +
  geom_text(data=annotation1, aes( x=x, y=y, label=label),                 , 
              color="black", 
              size=5) +
  geom_text(data=annotation2, aes( x=x, y=y, label=label),                 , 
            color="black", 
            size=5) +
  labs (y = bquote("mg/L"), x = '', title = 'Bottom Dissolved Oxygen') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

