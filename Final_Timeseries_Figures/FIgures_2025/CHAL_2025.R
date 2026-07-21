## Figures for Surface Chlorophyll

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: December 15, 2025**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB_Indicators_Calculations")
source("Deriv.R")
library(mgcv)
library(ggplot2)
library(lubridate)
library(dplyr)

# https://coastwatch.pfeg.noaa.gov/erddap/info/pmlEsaCCI50OceanColorMonthly/indeyear.html

#######Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025")
ds<-read.csv("chla_monthly_2025.csv", header = TRUE)
ds$month <- month(ds$date)
ds$year <- year(ds$date)

jan<-ds %>% select('X','Jan')
feb<-ds %>% select('X','Feb')
mar<-ds %>% select('X','Mar')
apr<-ds %>% select('X','Apr')
may<-ds %>% select('X','May')
jun<-ds %>% select('X','Jun')
jul<-ds %>% select('X','Jul')
aug<-ds %>% select('X','Aug')
sep<-ds %>% select('X','Sep')
oct<-ds %>% select('X','Oct')
nov<-ds %>% select('X','Nov')
dec<-ds %>% select('X','Dec')

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qjan = quantile(jan$Jan, probs = c(.30, .70), na.rm = 'TRUE')

# find the last 5 years mean
mn_jan5 = mean(jan$Jan[jan$X >= 2021])
# what quintile the data is in
mn_jan5 >qjan


# Creat a GAM - adjust k and remember to check model
mod_jan<- gam(Jan ~ s(X, k=10), data = jan)
summary(mod_jan) #check out model
gam.check(mod_jan)

pdata_jan <- with(jan, data.frame(X = X))
p2_mod_jan <- predict(mod_jan, newdata = pdata_jan,  type = "terms", se.fit = TRUE)
intercept_jan = 1.292813  # look at p2_mod and eyeartract the intercept
pdata_jan <- transform(pdata_jan, p2_mod_jan = p2_mod_jan$fit[,1], se2_jan = p2_mod_jan$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_jan.d <- Deriv(mod_jan, n=29) # n is the number of years
mod_jan.dci <- confint(mod_jan.d, term = Term)
mod_jan.dsig <- signifD(pdata_jan$p2_mod_jan, d = mod_jan.d[[Term]]$deriv,
                        +                    mod_jan.dci[[Term]]$upper, mod_jan.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Jan ~ X, data = jan )
lines(Jan ~ X, data = jan)
lines(p2_mod_jan+intercept_jan ~ X, data = pdata_jan, type = "n")
lines(p2_mod_jan+intercept_jan ~ X, data = pdata_jan)
lines(unlist(mod_jan.dsig$incr)+intercept_jan ~ X, data = pdata_jan, col = "blue", lwd = 3)
lines(unlist(mod_jan.dsig$decr)+intercept_jan ~ X, data = pdata_jan, col = "red", lwd = 3)

linearMod_jan<- lm(Jan ~ X, data=jan)
summary(linearMod_jan)

##########################FEB

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qfeb = quantile(feb$Feb, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_feb5 = mean(feb$Feb[feb$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_feb5 >qfeb

mod_feb<- gam(Feb ~ s(X, k=10), data = feb)
summary(mod_feb) #check out model
gam.check(mod_feb)

pdata_feb <- with(feb, data.frame(X = X))
p2_mod_feb <- predict(mod_feb, newdata = pdata_feb,  type = "terms", se.fit = TRUE)
intercept_feb = 1.167802  # look at p2_mod and eyeartract the intercept
pdata_feb <- transform(pdata_feb , p2_mod_feb  = p2_mod_feb$fit[,1], se2_feb = p2_mod_feb$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_feb.d <- Deriv(mod_feb, n=29) # n is the number of years
mod_feb.dci <- confint(mod_feb.d, term = Term)
mod_feb.dsig <- signifD(pdata_feb$p2_mod_feb, d = mod_feb.d[[Term]]$deriv,
                        +                    mod_feb.dci[[Term]]$upper, mod_feb.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Feb ~ X, data = feb)
lines(Feb ~ X, data = feb)
lines(p2_mod_feb+intercept_feb ~ X, data = pdata_feb, type = "n")
lines(p2_mod_feb+intercept_feb ~ X, data = pdata_feb)
lines(unlist(mod_feb.dsig$incr)+intercept_feb ~ X, data = pdata_feb, col = "blue", lwd = 3)
lines(unlist(mod_feb.dsig$decr)+intercept_feb ~ X, data = pdata_feb, col = "red", lwd = 3)

linearMod_feb<- lm(Feb ~ X, data=feb)
summary(linearMod_feb)
##########################MAR

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qmar = quantile(mar$Mar, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_mar5 = mean(mar$Mar[mar$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_mar5 >qmar

mod_mar<- gam(Mar ~ s(X, k=10), data = mar)
summary(mod_mar) #check out model
gam.check(mod_mar)

pdata_mar <- with(mar, data.frame(X = X))
p2_mod_mar <- predict(mod_mar, newdata = pdata_mar,  type = "terms", se.fit = TRUE)
intercept_mar = 1.391874  # look at p2_mod and eXtract the intercept
pdata_mar <- transform(pdata_mar , p2_mod_mar  = p2_mod_mar$fit[,1], se2_mar = p2_mod_mar$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_mar.d <- Deriv(mod_mar, n=29) # n is the number of years
mod_mar.dci <- confint(mod_mar.d, term = Term)
mod_mar.dsig <- signifD(pdata_mar$p2_mod_mar, d = mod_mar.d[[Term]]$deriv,
                        +                    mod_mar.dci[[Term]]$upper, mod_mar.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Mar ~ X, data = mar)
lines(Mar ~ X, data = mar)
lines(p2_mod_mar+intercept_mar ~ X, data = pdata_mar, type = "n")
lines(p2_mod_mar+intercept_mar ~ X, data = pdata_mar)
lines(unlist(mod_mar.dsig$incr)+intercept_mar ~ X, data = pdata_mar, col = "blue", lwd = 3)
lines(unlist(mod_mar.dsig$decr)+intercept_mar ~ X, data = pdata_mar, col = "red", lwd = 3)

linearMod_mar<- lm(Mar ~ X, data=mar)
summary(linearMod_mar)


##########################APR

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qapr = quantile(apr$Apr, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_apr5 = mean(apr$Apr[apr$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_apr5 >qapr

mod_apr<- gam(Apr ~ s(X, k=10), data = apr)
summary(mod_apr) #check out model
gam.check(mod_apr)

pdata_apr <- with(apr, data.frame(X = X))
p2_mod_apr <- predict(mod_apr, newdata = pdata_apr,  type = "terms", se.fit = TRUE)
intercept_apr = 1.611312  # look at p2_mod and eXtract the intercept
pdata_apr <- transform(pdata_apr , p2_mod_apr  = p2_mod_apr$fit[,1], se2_apr = p2_mod_apr$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_apr.d <- Deriv(mod_apr, n=29) # n is the number of years
mod_apr.dci <- confint(mod_apr.d, term = Term)
mod_apr.dsig <- signifD(pdata_apr$p2_mod_apr, d = mod_apr.d[[Term]]$deriv,
                        +                    mod_apr.dci[[Term]]$upper, mod_apr.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Apr ~ X, data = apr)
lines(Apr ~ X, data = apr)
lines(p2_mod_apr+intercept_apr ~ X, data = pdata_apr, type = "n")
lines(p2_mod_apr+intercept_apr ~ X, data = pdata_apr)
lines(unlist(mod_apr.dsig$incr)+intercept_apr ~ X, data = pdata_apr, col = "blue", lwd = 3)
lines(unlist(mod_apr.dsig$decr)+intercept_apr ~ X, data = pdata_apr, col = "red", lwd = 3)

linearMod_apr<- lm(Apr ~ X, data=apr)
summary(linearMod_apr)
# April had a statistically significant part of the GAM

##########################MAY

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qmay = quantile(may$May, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_may5 = mean(may$May[may$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_may5 >qmay

mod_may<- gam(May ~ s(X, k=10), data = may)
summary(mod_may) #check out model
gam.check(mod_may)

pdata_may <- with(may, data.frame(X = X))
p2_mod_may <- predict(mod_may, newdata = pdata_may,  type = "terms", se.fit = TRUE)
intercept_may = 1.18702  # look at p2_mod and eXtract the intercept
pdata_may <- transform(pdata_may , p2_mod_may  = p2_mod_may$fit[,1], se2_may = p2_mod_may$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_may.d <- Deriv(mod_may, n=29) # n is the number of years
mod_may.dci <- confint(mod_may.d, term = Term)
mod_may.dsig <- signifD(pdata_may$p2_mod_may, d = mod_may.d[[Term]]$deriv,
                        +                    mod_may.dci[[Term]]$upper, mod_may.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(May ~ X, data = may)
lines(May ~ X, data = may)
lines(p2_mod_may+intercept_may ~ X, data = pdata_may, type = "n")
lines(p2_mod_may+intercept_may ~ X, data = pdata_may)
lines(unlist(mod_may.dsig$incr)+intercept_may ~ X, data = pdata_may, col = "blue", lwd = 3)
lines(unlist(mod_may.dsig$decr)+intercept_may ~ X, data = pdata_may, col = "red", lwd = 3)

linearMod_may<- lm(May ~ X, data=may)
summary(linearMod_may)

##########################JUN - one significant increase

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qjun = quantile(jun$Jun, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_jun5 = mean(jun$Jun[jun$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_jun5 >qjun


mod_jun<- gam(Jun ~ s(X, k=10), data = jun)
summary(mod_jun) #check out model
gam.check(mod_jun)

pdata_jun <- with(jun, data.frame(X = X))
p2_mod_jun <- predict(mod_jun, newdata = pdata_jun,  type = "terms", se.fit = TRUE)
intercept_jun = 0.964111  # look at p2_mod and eXtract the intercept
pdata_jun <- transform(pdata_jun , p2_mod_jun  = p2_mod_jun$fit[,1], se2_jun = p2_mod_jun$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_jun.d <- Deriv(mod_jun, n=29) # n is the number of years
mod_jun.dci <- confint(mod_jun.d, term = Term)
mod_jun.dsig <- signifD(pdata_jun$p2_mod_jun, d = mod_jun.d[[Term]]$deriv,
                        +                    mod_jun.dci[[Term]]$upper, mod_jun.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( Jun~ X, data = jun)
lines(Jun ~ X, data = jun)
lines(p2_mod_jun+intercept_jun ~ X, data = pdata_jun, type = "n")
lines(p2_mod_jun+intercept_jun ~ X, data = pdata_jun)
lines(unlist(mod_jun.dsig$incr)+intercept_jun ~ X, data = pdata_jun, col = "blue", lwd = 3)
lines(unlist(mod_jun.dsig$decr)+intercept_jun ~ X, data = pdata_jun, col = "red", lwd = 3)

linearMod_jun<- lm(Jun ~ X, data=jun)
summary(linearMod_jun)

##########################JUL

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qjul = quantile(jul$Jul, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_jul5 = mean(jul$Jul[jul$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_jul5 >qjul

mod_jul<- gam(Jul ~ s(X, k=10), data = jul)
summary(mod_jul) #check out model
gam.check(mod_jul)

pdata_jul <- with(jul, data.frame(X = X))
p2_mod_jul <- predict(mod_jul, newdata = pdata_jul,  type = "terms", se.fit = TRUE)
intercept_jul = 1.010732 # look at p2_mod and eXtract the intercept
pdata_jul <- transform(pdata_jul , p2_mod_jul  = p2_mod_jul$fit[,1], se2_jul = p2_mod_jul$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_jul.d <- Deriv(mod_jul, n=29) # n is the number of years
mod_jul.dci <- confint(mod_jul.d, term = Term)
mod_jul.dsig <- signifD(pdata_jul$p2_mod_jul, d = mod_jul.d[[Term]]$deriv,
                        +                    mod_jul.dci[[Term]]$upper, mod_jul.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( Jul~ X, data = jul)
lines(Jul ~ X, data = jul)
lines(p2_mod_jul+intercept_jul ~ X, data = pdata_jul, type = "n")
lines(p2_mod_jul+intercept_jul ~ X, data = pdata_jul)
lines(unlist(mod_jul.dsig$incr)+intercept_jul ~ X, data = pdata_jul, col = "blue", lwd = 3)
lines(unlist(mod_jul.dsig$decr)+intercept_jul ~ X, data = pdata_jul, col = "red", lwd = 3)

linearMod_jul<- lm(Jul ~ X, data=jul)
summary(linearMod_jul)

##########################AUG

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qaug = quantile(aug$Aug, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_aug5 = mean(aug$Aug[aug$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_aug5 >qaug

mod_aug<- gam(Aug ~ s(X, k=10), data = aug)
summary(mod_aug) #check out model
gam.check(mod_aug)

pdata_aug <- with(aug, data.frame(X = X))
p2_mod_aug <- predict(mod_aug, newdata = pdata_aug,  type = "terms", se.fit = TRUE)
intercept_aug = 1.052248  # look at p2_mod and eXtract the intercept
pdata_aug <- transform(pdata_aug , p2_mod_aug  = p2_mod_aug$fit[,1], se2_aug = p2_mod_aug$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_aug.d <- Deriv(mod_aug, n=29) # n is the number of years
mod_aug.dci <- confint(mod_aug.d, term = Term)
mod_aug.dsig <- signifD(pdata_aug$p2_mod_aug, d = mod_aug.d[[Term]]$deriv,
                        +                    mod_aug.dci[[Term]]$upper, mod_aug.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( Aug~ X, data = aug)
lines(Aug ~ X, data = aug)
lines(p2_mod_aug+intercept_aug ~ X, data = pdata_aug, type = "n")
lines(p2_mod_aug+intercept_aug ~ X, data = pdata_aug)
lines(unlist(mod_aug.dsig$incr)+intercept_aug ~ X, data = pdata_aug, col = "blue", lwd = 3)
lines(unlist(mod_aug.dsig$decr)+intercept_aug ~ X, data = pdata_aug, col = "red", lwd = 3)

linearMod_aug<- lm(Aug ~ X, data=aug)
summary(linearMod_aug)

########################## SEP

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qsep = quantile(sep$Sep, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_sep5 = mean(sep$Sep[sep$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_sep5 >qsep

mod_sep<- gam(Sep ~ s(X, k=10), data = sep)
summary(mod_sep) #check out model
gam.check(mod_sep)

pdata_sep <- with(sep, data.frame(X = X))
p2_mod_sep <- predict(mod_sep, newdata = pdata_sep,  type = "terms", se.fit = TRUE)
intercept_sep = 0.7959707  # look at p2_mod and eXtract the intercept
pdata_sep <- transform(pdata_sep , p2_mod_sep  = p2_mod_sep$fit[,1], se2_sep = p2_mod_sep$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_sep.d <- Deriv(mod_sep, n=29) # n is the number of years
mod_sep.dci <- confint(mod_sep.d, term = Term)
mod_sep.dsig <- signifD(pdata_sep$p2_mod_sep, d = mod_sep.d[[Term]]$deriv,
                        +                    mod_sep.dci[[Term]]$upper, mod_sep.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( Sep~ X, data = sep)
lines(Sep ~ X, data = sep)
lines(p2_mod_sep+intercept_sep ~ X, data = pdata_sep, type = "n")
lines(p2_mod_sep+intercept_sep ~ X, data = pdata_sep)
lines(unlist(mod_sep.dsig$incr)+intercept_sep ~ X, data = pdata_sep, col = "blue", lwd = 3)
lines(unlist(mod_sep.dsig$decr)+intercept_sep ~ X, data = pdata_sep, col = "red", lwd = 3)

linearMod_sep<- lm(Sep ~ X, data=sep)
summary(linearMod_sep)

########################## OCT

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qoct = quantile(oct$Oct, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_oct5 = mean(oct$Oct[oct$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_oct5 >qoct


mod_oct<- gam(Oct ~ s(X, k=10), data = oct)
summary(mod_oct) #check out model
gam.check(mod_oct)

pdata_oct <- with(oct, data.frame(X = X))
p2_mod_oct <- predict(mod_oct, newdata = pdata_oct,  type = "terms", se.fit = TRUE)
intercept_oct = 1.120443 # look at p2_mod and eXtract the intercept
pdata_oct <- transform(pdata_oct , p2_mod_oct  = p2_mod_oct$fit[,1], se2_oct = p2_mod_oct$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_oct.d <- Deriv(mod_oct, n=29) # n is the number of years
mod_oct.dci <- confint(mod_oct.d, term = Term)
mod_oct.dsig <- signifD(pdata_oct$p2_mod_oct, d = mod_oct.d[[Term]]$deriv,
                        +                    mod_oct.dci[[Term]]$upper, mod_oct.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( Oct~ X, data = oct)
lines(Oct ~ X, data = oct)
lines(p2_mod_oct+intercept_oct ~ X, data = pdata_oct, type = "n")
lines(p2_mod_oct+intercept_oct ~ X, data = pdata_oct)
lines(unlist(mod_oct.dsig$incr)+intercept_oct ~ X, data = pdata_oct, col = "blue", lwd = 3)
lines(unlist(mod_oct.dsig$decr)+intercept_oct ~ X, data = pdata_oct, col = "red", lwd = 3)

linearMod_oct<- lm(Oct ~ X, data=oct)
summary(linearMod_oct)

########################## NOV

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qnov = quantile(nov$Nov, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_nov5 = mean(nov$Nov[nov$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_nov5 >qnov

mod_nov<- gam(Nov ~ s(X, k=10), data = nov)
summary(mod_nov) #check out model
gam.check(mod_nov)

pdata_nov <- with(nov, data.frame(X = X))
p2_mod_nov <- predict(mod_nov, newdata = pdata_nov,  type = "terms", se.fit = TRUE)
intercept_nov = 1.529819  # look at p2_mod and eXtract the intercept
pdata_nov <- transform(pdata_nov , p2_mod_nov  = p2_mod_nov$fit[,1], se2_nov = p2_mod_nov$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_nov.d <- Deriv(mod_nov, n=29) # n is the number of years
mod_nov.dci <- confint(mod_nov.d, term = Term)
mod_nov.dsig <- signifD(pdata_nov$p2_mod_nov, d = mod_nov.d[[Term]]$deriv,
                        +                    mod_nov.dci[[Term]]$upper, mod_nov.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( Nov~ X, data = nov)
lines(Nov ~ X, data = nov)
lines(p2_mod_nov+intercept_nov ~ X, data = pdata_nov, type = "n")
lines(p2_mod_nov+intercept_nov ~ X, data = pdata_nov)
lines(unlist(mod_nov.dsig$incr)+intercept_nov ~ X, data = pdata_nov, col = "blue", lwd = 3)
lines(unlist(mod_nov.dsig$decr)+intercept_nov ~ X, data = pdata_nov, col = "red", lwd = 3)

linearMod_nov<- lm(Nov ~ X, data=nov)
summary(linearMod_nov)

########################## DEC

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qdec = quantile(dec$Dec, probs = c(.30, .70), na.rm = TRUE)

# find the last 5 years mean
mn_dec5 = mean(dec$Dec[dec$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_dec5 >qdec

mod_dec<- gam(Dec ~ s(X, k=10), data = dec)
summary(mod_dec) #check out model
gam.check(mod_dec)

pdata_dec <- with(dec, data.frame(X = X))
p2_mod_dec <- predict(mod_dec, newdata = pdata_dec,  type = "terms", se.fit = TRUE)
intercept_dec = 1.229311  # look at p2_mod and eXtract the intercept
pdata_dec <- transform(pdata_dec , p2_mod_dec  = p2_mod_dec$fit[,1], se2_dec = p2_mod_dec$se.fit[,1])

#  Now that we have the model prediction, the neXt step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_dec.d <- Deriv(mod_dec, n=29) # n is the number of years
mod_dec.dci <- confint(mod_dec.d, term = Term)
mod_dec.dsig <- signifD(pdata_dec$p2_mod_dec, d = mod_dec.d[[Term]]$deriv,
                        +                    mod_dec.dci[[Term]]$upper, mod_dec.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( Dec~ X, data = dec)
lines(Dec ~ X, data = dec)
lines(p2_mod_dec+intercept_dec ~ X, data = pdata_dec, type = "n")
lines(p2_mod_dec+intercept_dec ~ X, data = pdata_dec)
lines(unlist(mod_dec.dsig$incr)+intercept_dec ~ X, data = pdata_dec, col = "blue", lwd = 3)
lines(unlist(mod_dec.dsig$decr)+intercept_dec ~ X, data = pdata_dec, col = "red", lwd = 3)

linearMod_dec<- lm(Dec ~ X, data=dec)
summary(linearMod_dec)


JAN <- ggplot() + 
  #geom_line(data = ds, aes(year = year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(year = year, y = dec), color = 'gray') +   
  geom_line(data = jan, aes(x =X, y = Jan), color = 'grey53') +
  geom_point(data = jan, aes(x =X, y = Jan), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_line(data = jan, aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = jan, aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Jan') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        #axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

FEB <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = feb, aes(x =X, y = Feb), color = 'grey53') +
  geom_point(data = feb, aes(x =X, y = Feb), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = feb, aes(year= year, y = feb), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Feb') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

MAR <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = mar, aes(x =X, y = Mar), color = 'grey53') +
  geom_point(data = mar, aes(x =X, y = Mar), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = mar), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Mar') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

APR <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = apr, aes(x =X, y = Apr), color = 'grey53') +
  geom_point(data = apr, aes(x =X, y = Apr), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = apr), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_apr, aes(y = unlist(mod_apr.dsig$decr)+intercept_apr, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Apr') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

MAY <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = may, aes(x =X, y = May), color = 'grey53') +
  geom_point(data = may, aes(x =X, y = May), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = may), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'May') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

JUN <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = jun, aes(x =X, y = Jun), color = 'grey53') +
  geom_point(data = jun, aes(x =X, y = Jun), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = jun), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Jun') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

JUL <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = jul, aes(x =X, y = Jul), color = 'grey53') +
  geom_point(data = jul, aes(x =X, y = Jul), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = jul), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Jul') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

AUG <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = aug, aes(x =X, y = Aug), color = 'grey53') +
  geom_point(data = aug, aes(x =X, y = Aug), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = aug), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Aug') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

SEP <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = sep, aes(x =X, y = Sep), color = 'grey53') +
  geom_point(data = sep, aes(x =X, y = Sep), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = sep), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Sep') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

OCT <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = oct, aes(x =X, y = Oct), color = 'grey53') +
  geom_point(data = oct, aes(x =X, y = Oct), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = oct), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Oct') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #  axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

NOV <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = nov, aes(x =X, y = Nov), color = 'grey53') +
  geom_point(data = nov, aes(x =X, y = Nov), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = nov), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Nov') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

DEC <- ggplot() + 
  #geom_line(data = ds, aes(x =year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x =year, y = dec), color = 'gray') +   
  geom_line(data = dec, aes(x =X, y = Dec), color = 'grey53') +
  geom_point(data = dec, aes(x =X, y = Dec), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = dec), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1997,2025), labels = c(1997,2025))+
  labs (y = '', x ='', title = 'Dec') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x =element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))


# Now plot all 12 together
library(ggpubr)

ggarrange(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC,nrow=1,ncol=12)