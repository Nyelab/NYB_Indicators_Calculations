## Figures for Long Island Population

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: December 26, 2022**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)
library(dplyr)

#######Load the datasets
setwd("/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022")
pop<-read.csv("Annual_Population_Estimates_for_New_York_State_and_Counties__Beginning_1970.csv", header = TRUE)
kings = pop[pop$Geography == 'Kings County',]
queens = pop[pop$Geography == 'Queens County',]
nassau = pop[pop$Geography == 'Nassau County',]
suffolk = pop[pop$Geography == 'Suffolk County',]

counties = rbind(kings, queens, nassau, suffolk)
counties <- counties %>%  filter(Program.Type != 'Census Base Population')
# If you dont remove the decadal census then it wayyy overcounts for those years when we do sum

population <- counties %>% group_by(Year)
c_pop <- population %>% summarise(
  pop = sum(Population))

# Your final time series is (hopefully) a dataframe with a column for the year 
# and a column for whatever the data variable is.  Here I give an example using 
# Hudson river mean flow data where one column is year and the other is flowrate

# Creat a GAM - adjust k and remember to check model
mod<- gam(pop ~ s(Year, k=15), data = c_pop)
summary(mod) #check out model
gam.check(mod)

pdata <- with(c_pop, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 7244863  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=52) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(pop ~ Year, data = c_pop)
lines(pop ~ Year, data = c_pop)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(pop ~ Year, data=c_pop)
summary(linearMod)

ggplot() + 
  geom_line(data = c_pop, aes(x = Year, y = pop), color = 'grey53') +
  geom_point(data = c_pop, aes(x = Year, y = pop), color = 'gray53') + 
  geom_point(data = c_pop[51:52,], aes(x = Year, y = pop), shape = 17, size =3) + 
  geom_smooth(data = c_pop, aes(x = Year, y = pop), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Population", x = 'Year', title = 'Population of Long Island') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
