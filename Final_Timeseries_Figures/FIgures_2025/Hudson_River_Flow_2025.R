## **Laura Gruenburg, lagruenburg@gmail.com**
  
  #   **LAST UPDATED: December 14, 2023**
  
  #####load required functions
  #  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30
  
setwd("~/Desktop/NYB_Indicators_Calculations")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


####### Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025")
hudsondaily<-read.csv("Hudson_Green_Island_2025.csv", header = TRUE)

narrows_d <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/Hudson_at_Narrows2023daily.csv')
narrows_m <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/Hudson_at_Narrows2023monthly.csv')


#hms <- hudson[hudson$Variable == 'Hudson_hms',]
cfs_to_cms<-0.0283168
#hms$vol_transport <- hms$vol_transport*cfs_to_cms

hudsondaily$vol_transportue <- hudsondaily$vol_transportue*cfs_to_cms
hudsondaily$month <- month(hudsondaily$time)
hudsondaily$yday <- yday(hudsondaily$time)
hudsondaily$year <- year(hudsondaily$time)
hudsondaily$season <- 'winter'
hudsondaily[hudsondaily$month > 2 & hudsondaily$month < 6, ]$season <- 'spring'
hudsondaily[hudsondaily$month > 5 & hudsondaily$month < 9, ]$season <- 'summer'
hudsondaily[hudsondaily$month > 9 & hudsondaily$month < 12, ]$season <- 'fall'

#makesure the previous december is with the jan and feb of the next year for winter
hudsondaily[hudsondaily$month==12, ]$year <- hudsondaily[hudsondaily$month==12, ]$year +1

hms<-hudsondaily %>%
  group_by(year, season) %>%
  summarise(vol_transport = mean(vol_transportue))

#winter
qhms_winter = quantile(hms[hms$season == 'winter', ]$vol_transport, probs = c(.30, .70))
winter = hms[hms$season == 'winter', ]
# find the last 5 years mean
mn_hms5 = mean(winter$vol_transport[winter$year >= 2021])
# what quintile the data is in
mn_hms5 >qhms_winter


# Creat a GAM - adjust k and remember to check model
mod<- gam(vol_transport ~ s(year, k=5), data = winter)
summary(mod) #check out model
gam.check(mod)

pdata <- with(winter, data.frame(year = year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 482.9843  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod.d <- Deriv(mod, n=30) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(vol_transport ~ year, data = winter)
lines(vol_transport ~ year, data = winter)
lines(p2_mod+intercept ~ year, data = pdata, type = "n")
lines(p2_mod+intercept ~ year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(vol_transport ~ year, data=winter)
summary(linearMod)

#No sig GAM, no sig linear 2025 winter

#spring
qhms_spring = quantile(hms[hms$season == 'spring', ]$vol_transport, probs = c(.30, .70))
spring = hms[hms$season == 'spring', ]
# find the last 5 years mean
mn_hms5 = mean(spring$vol_transport[spring$year >= 2021])
# what quintile the data is in
mn_hms5 >qhms_spring


# Creat a GAM - adjust k and remember to check model
mod<- gam(vol_transport ~ s(year, k=5), data = spring)
summary(mod) #check out model
gam.check(mod)

pdata <- with(spring, data.frame(year = year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 716.7011  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod.d <- Deriv(mod, n=29) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(vol_transport ~ year, data = spring)
lines(vol_transport ~ year, data = spring)
lines(p2_mod+intercept ~ year, data = pdata, type = "n")
lines(p2_mod+intercept ~ year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(vol_transport ~ year, data=spring)
summary(linearMod)

# No sig GAM or linear trend 2025 summer

#summer
qhms_summer = quantile(hms[hms$season == 'summer', ]$vol_transport, probs = c(.30, .70))
summer = hms[hms$season == 'summer', ]
# find the last 5 years mean
mn_hms5 = mean(summer$vol_transport[summer$year >= 2021])
# what quintile the data is in
mn_hms5 >qhms_summer


# Creat a GAM - adjust k and remember to check model
mod<- gam(vol_transport ~ s(year, k=5), data = summer)
summary(mod) #check out model
gam.check(mod)

pdata <- with(summer, data.frame(year = year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 331.0147  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod.d <- Deriv(mod, n=28) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(vol_transport ~ year, data = summer)
lines(vol_transport ~ year, data = summer)
lines(p2_mod+intercept ~ year, data = pdata, type = "n")
lines(p2_mod+intercept ~ year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(vol_transport ~ year, data=summer)
summary(linearMod)


#No sig GAM or linear trend for summer 2025


#autumn
qhms_fall = quantile(hms[hms$season == 'fall', ]$vol_transport, probs = c(.30, .70))
fall = hms[hms$season == 'fall', ]
# find the last 5 years mean
mn_hms5 = mean(fall$vol_transport[fall$year >= 2021])
# what quintile the data is in
mn_hms5 >qhms_fall


# Creat a GAM - adjust k and remember to check model
mod<- gam(vol_transport ~ s(year, k=5), data = fall)
summary(mod) #check out model
gam.check(mod)

pdata <- with(fall, data.frame(year = year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 427.6803   # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod.d <- Deriv(mod, n=29) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(vol_transport ~ year, data = fall)
lines(vol_transport ~ year, data = fall)
lines(p2_mod+intercept ~ year, data = pdata, type = "n")
lines(p2_mod+intercept ~ year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(vol_transport ~ year, data=fall)
summary(linearMod)

#No sig GAM or linear trend for autumn 2025

hms$season_f = factor(hms$season, levels=c('winter','spring','summer','fall'))

p <- ggplot(hms, aes(year, vol_transport)) + geom_line(color = 'grey53') + geom_point()
p + facet_grid(rows = vars(season_f)) +
  theme_bw() +
  labs (y = bquote("Mean Flow "~m^3~"/s"), x = 'year', title = 'Hudson Mean Flow at Green Island') + 
  theme(strip.text = element_text(size = 16), plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))



## OLD from here on...
ggplot() + 
  geom_line(data = hms, aes(x = year, y = vol_transport), color = 'grey53') +
  geom_point(data = hms, aes(x = year, y = vol_transport), color = 'gray53') + 
  geom_point(data = hms[72,], aes(x = year, y = vol_transport), shape = 17, size = 3) + 
  geom_smooth(data = hms, aes(x = year, y = vol_transport), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Mean Flow "~m^3~"/s"), x = 'year', title = 'Hudson Mean Flow at Green Island') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))



linearMod2<- lm(vol_transport ~ month, data=hms)
summary(linearMod2)

linearMod3<- lm(vol_transport ~ month, data=narrows_m)
summary(linearMod3)


ggplot() + 
  geom_line(data = hms, aes(x = month, y = vol_transport), color = 'grey53') +
  geom_point(data = hms, aes(x = month, y = vol_transport), color = 'gray53', size =3) + 
  geom_line(data = narrows_m, aes(x = month, y = vol_transport), color = 'purple') +
  geom_point(data = narrows_m, aes(x = month, y = vol_transport), color = 'purple', shape = 17, size =3) + 
  theme_bw() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c("Jan","Feb", "Mar", "Apr","May","Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs (y = bquote("Mean Flow"~m^3~"/s"), x = 'Month', title = 'Hudson Mean Flow 2023') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

ggplot() + 
  geom_line(data = hms, aes(x = month, y = vol_transport-(mean(vol_transport))), color = 'grey53') +
  geom_point(data = hms, aes(x = month, y = vol_transport-(mean(vol_transport))), color = 'gray53', size =3) + 
  geom_line(data = narrows_m, aes(x = month, y = vol_transport-(mean(vol_transport))), color = 'purple') +
  geom_point(data = narrows_m, aes(x = month, y = vol_transport-(mean(vol_transport))), color = 'purple', shape = 17, size =3) + 
  theme_bw() +
  labs (y = bquote("Flow Anomaly"~m^3~"/s"), x = 'year', title = 'Hudson Mean Flow at Green Island') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

