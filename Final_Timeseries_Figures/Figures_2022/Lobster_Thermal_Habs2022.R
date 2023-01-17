## Figures for Lobster Thermal Habitat

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: December 16, 2022**

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
df<-read.csv("Lobster_Thermal_Hab_Dec_12_2022.csv", header = TRUE)

mod<- gam(Percent_Inhospitable ~ s(X, k=5), data = df)
summary(mod) #check out model
gam.check(mod)

pdata <- with(df, data.frame(X = X))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept =  0.07144636 # look at p2_mod and eXtract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

Term = "X"
mod.d <- Deriv(mod, n=28) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Percent_Inhospitable ~ X, data = df)
lines(Percent_Inhospitable ~ X, data = df)
lines(p2_mod+intercept ~ X, data = pdata, type = "n")
lines(p2_mod+intercept ~ X, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ X, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ X, data = pdata, col = "red", lwd = 3)

linearMod<- lm(Percent_Inhospitable ~ X, data=df)
summary(linearMod)


Percent_Inhospitable_plot <- ggplot() + 
  geom_line(data = df, aes(x = X, y = Percent_Inhospitable), color = 'grey53') +
  geom_point(data = df, aes(x = X, y = Percent_Inhospitable), color = 'gray53') + 
  geom_point(data = df[28,], aes(x = X, y = Percent_Inhospitable), shape = 17, size =3) + 
  #geom_line(data = hypoxia_risk, aes(x = first_column, y = second_column), color = 'orange') +
  #geom_line(data = hypoxia, aes(x = first_column, y = second_column2), color = 'red') +
  #geom_smooth(data = df, aes(X = X, y = Percent_Inhospitable), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(X = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, X = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, X = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Percent Inhospitable', x = 'Year', title = 'Lobster Thermal Habitat') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
