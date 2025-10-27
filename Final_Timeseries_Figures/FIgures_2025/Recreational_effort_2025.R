#####load required functions
## Figures for Recreational Effort

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: September 26, 2025**

#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB_Indicators_Calculations")
source("Deriv.R")
library(mgcv)
library(ggplot2)
library(dplyr)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025")
RE<-read.csv("Recreational_effort_2025.csv", header = TRUE)

RE2 <- read.csv('Recreational_effort_2025_byarea.csv', header = TRUE)

RE$Angler.Trips<-as.numeric(gsub(",","",RE$Angler.Trips))#remove commas, make numeric
RE <- RE[RE$Year < 2025, ] #2025 is still incomplete

RE2$Angler.Trips<-as.numeric(gsub(",","",RE2$Angler.Trips))#remove commas, make numeric
RE2 <- RE2[RE2$Year < 2025, ] #2025 is still incomplete
RE2_yes <- RE2[RE2$Does.Angler.Trips.Meet.MRIP.Standard == 'YES', ]

Ch_Boat <- RE[RE$Fishing.Mode == 'CHARTER BOAT',]
Pt_Boat <- RE[RE$Fishing.Mode == 'PARTY BOAT',]
P_R_Boat <- RE[RE$Fishing.Mode == 'PRIVATE/RENTAL BOAT',]
Sh <- RE[RE$Fishing.Mode == 'SHORE',]
RE <- RE[RE$Does.Angler.Trips.Meet.MRIP.Standard == 'YES', ]

Effort <- aggregate(Angler.Trips ~ Year, data = RE, sum) # total number of angler trips
Effort2 <- aggregate(Angler.Trips ~ Year, data = RE2, sum)
#determing the 30th and 70th percentiles for the short term column in the indicators at a glance
#table in the indicators report
qE = quantile(Effort$Angler.Trips, probs = c(.30, .70))

# find the last 5 years mean
mn_E5 = mean(Effort$Angler.Trips[Effort$Year >= 2020])
# what quintile the data is in
mn_E5 >qE

# GAM for total effort
# Creat a GAM - adjust k and remember to check model
mod<- gam(Angler.Trips ~ s(Year, k=10), data = Effort)
summary(mod) #check out model
gam.check(mod)

pdata <- with(Effort, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 11845288   # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=44) # n is the number of Years
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
  geom_point(data = Effort[44,], aes(x= Year, y = Angler.Trips), shape = 17, size =3) +
  geom_smooth(data = Effort, aes(x = Year, y = Angler.Trips), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Trips", x = 'Year', title = 'Recreational Effort') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#

ggplot() + 
  geom_line(data = RE2, aes(x=Year, y = Angler.Trips, color = Fishing.Mode)) +
  geom_point(data = RE2, aes(x=Year, y = Angler.Trips, color = Fishing.Mode)) +
  geom_line(data = Effort2, aes(x = Year, y = Angler.Trips), color = 'grey53') +
  geom_point(data = Effort2, aes(x = Year, y = Angler.Trips), color = 'gray53') + 
  geom_point(data = Effort[43,], aes(x= Year, y = Angler.Trips), shape = 17, size =3) +
  geom_smooth(data = Effort, aes(x = Year, y = Angler.Trips), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Trips", x = 'Year', title = 'Recreational Effort') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#
p <- ggplot(RE2_yes, aes(x = Year, y = Angler.Trips))+
  geom_line()
p + facet_grid(Fishing.Mode ~ Fishing.Area, scales ="free")

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

rec_summary %<% 