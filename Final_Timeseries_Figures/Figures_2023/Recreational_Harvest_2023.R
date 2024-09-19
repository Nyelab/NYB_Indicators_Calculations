## Figures for Recreational Fishing

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: January 9, 2023**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
RF<-read.csv("mrip_national_series_Harvest_2023.csv", header = TRUE)
RF[RF$Does.Landing..A.B1..Meet.MRIP.Standard == 'NO', 7] = NA #ONe year harvest does nto meet data standard

# Determined the 30th and 70th percentiles for the short term column of
#the Indicators at a glance table in the indicator report.

RF = RF[-20, ] #remove that one row with not significant data (double check your data FIRST)

qRF = quantile(RF$Total.Harvest..A.B1., probs = c(.30, .70))

# find the last 5 years mean
mn_RF5 = mean(RF$Total.Harvest..A.B1.[RF$Year >= 2019])
# what quintile the data is in
mn_RF5 >qRF


# Creat a GAM - adjust k and remember to check model
mod_cr<- gam(Total.Harvest..A.B1. ~ s(Year, k=10), data = RF)
summary(mod_cr) #check out model
gam.check(mod_cr)

pdata_cr <- with(RF, data.frame(Year = Year))
p2_mod_cr <- predict(mod_cr, newdata = pdata_cr,  type = "terms", se.fit = TRUE)
intercept_cr = 18688122  # look at p2_mod and extract the intercept
pdata_cr <- transform(pdata_cr, p2_mod_cr = p2_mod_cr$fit[,1], se2 = p2_mod_cr$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_cr.d <- Deriv(mod_cr, n=43) # n is the number of years
mod_cr.dci <- confint(mod_cr.d, term = Term)
mod_cr.dsig <- signifD(pdata_cr$p2_mod_cr, d = mod_cr.d[[Term]]$deriv,
                       +                    mod_cr.dci[[Term]]$upper, mod_cr.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Total.Harvest..A.B1. ~ Year, data = RF)
lines(Total.Harvest..A.B1. ~ Year, data = RF)
lines(p2_mod_cr+intercept_cr ~ Year, data = pdata_cr, type = "n")
lines(p2_mod_cr+intercept_cr ~ Year, data = pdata_cr)
lines(unlist(mod_cr.dsig$incr)+intercept_cr ~ Year, data = pdata_cr, col = "blue", lwd = 3)
lines(unlist(mod_cr.dsig$decr)+intercept_cr ~ Year, data = pdata_cr, col = "red", lwd = 3)

linearmod_cr<- lm(Total.Harvest..A.B1. ~ Year, data=RF)
summary(linearmod_cr)

Harvest <- ggplot() + 
  geom_line(data = RF, aes(x = Year, y = Total.Harvest..A.B1.), color = 'grey53') +
  geom_point(data = RF, aes(x = Year, y = Total.Harvest..A.B1.), color = 'gray53') + 
  geom_point(data = RF[43,], aes(x= Year, y = Total.Harvest..A.B1.), shape = 17, size =3) +
  geom_smooth(data = RF, aes(x = Year, y = Total.Harvest..A.B1.), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_cr, aes(x = Year, y = p2_mod_cr+intercept_cr), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_cr, aes(y = unlist(mod_cr.dsig$incr)+intercept_cr, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_cr, aes(y = unlist(mod_cr.dsig$decr)+intercept_cr, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Fish", x = '', title = 'Recreational Harvest') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#


qRFr = quantile(RF$Released..B2., probs = c(.30, .70))

# find the last 5 years mean
mn_RF5r = mean(RF$Released..B2.[RF$Year >= 2019])
# what quintile the data is in
mn_RF5r >qRFr

## Released
mod_ct<- gam(Released..B2. ~ s(Year, k=10), data = RF)
summary(mod_ct) #check out model
gam.check(mod_ct)

pdata_ct <- with(RF, data.frame(Year = Year))
p2_mod_ct <- predict(mod_ct, newdata = pdata_ct,  type = "terms", se.fit = TRUE)
intercept_ct =  32736798    # look at p2_mod and extract the intercept
pdata_ct <- transform(pdata_ct, p2_mod_ct = p2_mod_ct$fit[,1], se2 = p2_mod_ct$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_ct.d <- Deriv(mod_ct, n=43) # n is the number of years
mod_ct.dci <- confint(mod_ct.d, term = Term)
mod_ct.dsig <- signifD(pdata_ct$p2_mod_ct, d = mod_ct.d[[Term]]$deriv,
                       +                    mod_ct.dci[[Term]]$upper, mod_ct.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Released..B2. ~ Year, data = RF)
lines(Released..B2. ~ Year, data = RF)
lines(p2_mod_ct+intercept_ct ~ Year, data = pdata_ct, type = "n")
lines(p2_mod_ct+intercept_ct ~ Year, data = pdata_ct)
lines(unlist(mod_ct.dsig$incr)+intercept_ct ~ Year, data = pdata_ct, col = "blue", lwd = 3)
lines(unlist(mod_ct.dsig$decr)+intercept_ct ~ Year, data = pdata_ct, col = "red", lwd = 3)

linearmod_ct<- lm(Released..B2. ~ Year, data=RF)
summary(linearmod_ct)

Released <- ggplot() + 
  geom_line(data = RF, aes(x = Year, y = Released..B2.), color = 'grey53') +
  geom_point(data = RF, aes(x = Year, y = Released..B2.), color = 'gray53') + 
  geom_point(data = RF[43,], aes(x = Year, y = Released..B2.), shape = 17, size = 3) +
  geom_smooth(data = RF, aes(x = Year, y = Released..B2.), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_ct, aes(x = Year, y = p2_mod_ct+intercept_ct), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_ct, aes(y = unlist(mod_ct.dsig$incr)+intercept_ct, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_ct, aes(y = unlist(mod_ct.dsig$decr)+intercept_ct, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Fish", x = '', title = 'Released') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


#####

library(ggpubr)

ggarrange(Harvest, Released, 
          ncol = 1, nrow = 2)