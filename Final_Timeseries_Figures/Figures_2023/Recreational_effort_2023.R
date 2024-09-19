#####load required functions
## Figures for Recreational Effort

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: January 9, 2023**

#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
RE<-read.csv("mrip_effort_series_2023.csv", header = TRUE)
RE
Ch_P_boat <- RE[RE$Fishing.Mode == 'PARTY/CHARTER BOAT',]
P_R_Boat <- RE[RE$Fishing.Mode == 'PRIVATE/RENTAL BOAT',]
Sh <- RE[RE$Fishing.Mode == 'SHORE',]
Effort <- aggregate(Angler.Trips ~ Year, data = RE, sum) # total number of angler trips

#determing the 30th and 70th percentiles for the short term column in the indicators at a glance
#table in the indicators report
qE = quantile(Effort$Angler.Trips, probs = c(.30, .70))

# find the last 5 years mean
mn_E5 = mean(Effort$Angler.Trips[Effort$Year >= 2019])
# what quintile the data is in
mn_E5 >qE

# GAM for total effort
# Creat a GAM - adjust k and remember to check model
mod<- gam(Angler.Trips ~ s(Year, k=10), data = Effort)
summary(mod) #check out model
gam.check(mod)

pdata <- with(Effort, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 2544793   # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=43) # n is the number of Years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                       +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Angler.Trips ~ Year, data = Effort)
lines(Angler.Trips ~ Year, data = Effort)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

linearmod<- lm(Angler.Trips ~ Year, data=Effort)
summary(linearmod)

ggplot() + 
  geom_line(data = RE, aes(x=Year, y = Angler.Trips, color = Fishing.Mode)) +
  geom_point(data = RE, aes(x=Year, y = Angler.Trips, color = Fishing.Mode)) +
  geom_line(data = Effort, aes(x = Year, y = Angler.Trips), color = 'grey53') +
  geom_point(data = Effort, aes(x = Year, y = Angler.Trips), color = 'gray53') + 
  geom_point(data = Effort[43,], aes(x= Year, y = Angler.Trips), shape = 17, size =3) +
  geom_smooth(data = Effort, aes(x = Year, y = Angler.Trips), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Trips", x = 'Year', title = 'Recreational Effort') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#

#ggplot() + 
#  geom_line(data = Ch_boat, aes(x=Year, y = Angler.Trips), color = 'deeppink1') +
#  geom_point(data = Ch_boat, aes(x=Year, y = Angler.Trips), color = 'deeppink1') +
#  geom_line(data = P_boat, aes(x=Year, y = Angler.Trips), color = 'darkred') +
#  geom_point(data = P_boat, aes(x=Year, y = Angler.Trips), color = 'darkred') +
#  geom_line(data = P_R_Boat, aes(x=Year, y = Angler.Trips), color = 'darkgoldenrod2') +
#  geom_point(data = P_R_Boat, aes(x=Year, y = Angler.Trips), color = 'darkgoldenrod2') +
#  geom_line(data = Ch_P_boat, aes(x=Year, y = Angler.Trips), color = 'darkorchid2') +
#  geom_point(data = Ch_P_boat, aes(x=Year, y = Angler.Trips), color = 'darkorchid2') +
#  geom_line(data = Sh, aes(x=Year, y = Angler.Trips), color = 'darkolivegreen3') +
#  geom_point(data = Sh, aes(x=Year, y = Angler.Trips), color = 'darkolivegreen3') +
#  geom_line(data = Effort, aes(x = Year, y = Angler.Trips), color = 'grey53') +
#  geom_point(data = Effort, aes(x = Year, y = Angler.Trips), color = 'gray53') + 
#  geom_point(data = Effort[42,], aes(x= Year, y = Angler.Trips), shape = 17, size =3) +
#  geom_smooth(data = Effort, aes(x = Year, y = Angler.Trips), method = lm, se = FALSE, color = 'black') + 
#  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
#  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
#  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
#  theme_bw() +
#  labs (y = "Number of Trips", x = 'Year', title = 'Recreational Effort') + 
#  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
##
