## Figures for River Flow

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: December 14, 2023**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


####### Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
hudson<-read.csv("Riverflow_Dec_14_2023.csv", header = TRUE)
hudsondaily<-read.csv("Riverflow_Dec_14_2023_Daily2023.csv", header = TRUE)

narrows_d <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/Hudson_at_Narrows2023daily.csv')
narrows_m <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/Hudson_at_Narrows2023monthly.csv')


meanflow <- hudson[hudson$Variable == 'Hudson_meanflow',]
cfs_to_cms<-0.0283168
meanflow$Val <- meanflow$Val*cfs_to_cms

hudsondaily$val <- hudsondaily$val*cfs_to_cms
hudsondaily$month <- month(hudsondaily$dates)
hudsondaily$yday <- yday(hudsondaily$dates)

hms<-hudsondaily %>%
  group_by(month) %>%
  summarise(vol_transport = mean(val))


qmeanflow = quantile(meanflow$Val, probs = c(.30, .70))

# find the last 5 years mean
mn_meanflow5 = mean(meanflow$Val[meanflow$Year >= 2019])
# what quintile the data is in
mn_meanflow5 >qmeanflow


# Creat a GAM - adjust k and remember to check model
mod<- gam(Val ~ s(Year, k=15), data = meanflow)
summary(mod) #check out model
gam.check(mod)

pdata <- with(meanflow, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 412.4179  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=72) # n is the number of Years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = meanflow)
lines(Val ~ Year, data = meanflow)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(Val ~ Year, data=meanflow)
summary(linearMod)


ggplot() + 
  geom_line(data = meanflow, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = meanflow, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = meanflow[72,], aes(x = Year, y = Val), shape = 17, size = 3) + 
  geom_smooth(data = meanflow, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Mean Flow "~m^3~"/s"), x = 'Year', title = 'Hudson Mean Flow at Green Island') + 
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
  labs (y = bquote("Flow Anomaly"~m^3~"/s"), x = 'Year', title = 'Hudson Mean Flow at Green Island') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

