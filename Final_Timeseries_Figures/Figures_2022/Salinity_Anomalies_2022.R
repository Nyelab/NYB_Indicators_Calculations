## Figures for Salinity

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: December 12, 2022**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/Final_timeseries")
surfSal<-read.csv("SurfSal_insitu_Dec_12_2022.csv", header = TRUE)
surfSal

# Creat a GAM - adjust k and remember to check model
mod<- gam(Val ~ s(Year, k=17), data = surfSal)
summary(mod) #check out model
gam.check(mod)

pdata <- with(surfSal, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 5.409273e-17  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=44) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = surfSal)
lines(Val ~ Year, data = surfSal)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(Val ~ Year, data=surfSal)
summary(linearMod)

ggplot() + 
  geom_line(data = surfSal, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = surfSal, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = surfSal[44,], aes(x = Year, y = Val), shape = 17, size = 3) +
  #geom_smooth(data = surfSal, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Salinity Anomaly (PSU)", x = 'Year', title = 'Surface Salinity Anomaly') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


#######Load the datasets
setwd("~/Desktop/NYB Indicators/Final_timeseries")
BotSal<-read.csv("BotSal_insitu_Dec_12_2022.csv", header = TRUE)
BotSal

# Your final time series is (hopefully) a dataframe with a column for the year 
# and a column for whatever the data variable is.  Here I give an example using 
# Hudson river mean flow data where one column is year and the other is flowrate

# Creat a GAM - adjust k and remember to check model
mod2<- gam(Val ~ s(Year, k=17), data = BotSal)
summary(mod2) #check out model
gam.check(mod2)

pdata2 <- with(BotSal, data.frame(Year = Year))
p2_mod2 <- predict(mod2, newdata = pdata2,  type = "terms", se.fit = TRUE)
intercept2 = 3.021998e-17 # look at p2_mod and extract the intercept
pdata2 <- transform(pdata2, p2_mod2 = p2_mod2$fit[,1], se2 = p2_mod2$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod2.d <- Deriv(mod2, n=44) # n is the number of years
mod2.dci <- confint(mod2.d, term = Term)
mod2.dsig <- signifD(pdata2$p2_mod2, d = mod2.d[[Term]]$deriv,
                    +                    mod2.dci[[Term]]$upper, mod2.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = BotSal)
lines(Val ~ Year, data = BotSal)
lines(p2_mod2+intercept2 ~ Year, data = pdata2, type = "n")
lines(p2_mod2+intercept2 ~ Year, data = pdata2)
lines(unlist(mod2.dsig$incr)+intercept2 ~ Year, data = pdata2, col = "blue", lwd = 3)
lines(unlist(mod2.dsig$decr)+intercept2 ~ Year, data = pdata2, col = "red", lwd = 3)

linearMod2<- lm(Val ~ Year, data=BotSal)
summary(linearMod2)

colors <- c("Surface" = 'grey53', "Bottom" = 'darkgoldenrod1')

ggplot() + 
  geom_line(data = surfSal, aes(x = Year, y = Val, color = 'Surface')) +
  geom_point(data = surfSal, aes(x = Year, y = Val), color = 'gray53') +
  geom_point(data = surfSal[44,], aes(x = Year, y = Val), shape = 17, size =3)+
  geom_line(data = BotSal, aes(x = Year, y = Val, color = 'Bottom')) +
  geom_point(data = BotSal, aes(x = Year, y = Val), color = 'darkgoldenrod1') + 
  geom_point(data = BotSal[44,], aes(x = Year, y = Val), shape = 17, size =3, color = 'darkgoldenrod1')+
  #geom_smooth(data = surfSal, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Salinity Anomaly (PSU)", x = 'Year', title = 'Salinity Anomaly',color = "") +
  scale_color_manual(values = colors) + 
  theme(legend.position = 'bottom', plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        legend.text=element_text(size=12))
