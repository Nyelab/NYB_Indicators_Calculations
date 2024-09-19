## Figures for Surface Chlorophyll

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: June 5, 2024**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
library(lubridate)
#library(mgcViz)

# https://coastwatch.pfeg.noaa.gov/erddap/info/pmlEsaCCI50OceanColorMonthly/indeyear.html

#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
ds<-read.csv("chla_2023.csv", header = TRUE)
ds$month <- month(ds$date)
ds$year <- year(ds$date)

jan<-ds[ds$month == 1, ]
feb<-ds[ds$month == 2, ]
mar<-ds[ds$month == 3, ]
apr<-ds[ds$month == 4, ]
may<-ds[ds$month == 5, ]
jun<-ds[ds$month == 6, ]
jul<-ds[ds$month == 7, ]
aug<-ds[ds$month == 8, ]
sep<-ds[ds$month == 9, ]
oct<-ds[ds$month == 10, ]
nov<-ds[ds$month == 11, ]
dec<-ds[ds$month == 12, ]

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qjan = quantile(jan$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_jan5 = mean(jan$chla[jan$year >= 2019])
# what quintile the data is in
mn_jan5 >qjan


# Creat a GAM - adjust k and remember to check model
mod_jan<- gam(chla ~ s(year, k=10), data = jan)
summary(mod_jan) #check out model
gam.check(mod_jan)

pdata_jan <- with(jan, data.frame(year = year))
p2_mod_jan <- predict(mod_jan, newdata = pdata_jan,  type = "terms", se.fit = TRUE)
intercept_jan = 1.232035  # look at p2_mod and eyeartract the intercept
pdata_jan <- transform(pdata_jan, p2_mod_jan = p2_mod_jan$fit[,1], se2_jan = p2_mod_jan$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_jan.d <- Deriv(mod_jan, n=23) # n is the number of years
mod_jan.dci <- confint(mod_jan.d, term = Term)
mod_jan.dsig <- signifD(pdata_jan$p2_mod_jan, d = mod_jan.d[[Term]]$deriv,
                        +                    mod_jan.dci[[Term]]$upper, mod_jan.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(chla ~ year, data = jan )
lines(chla ~ year, data = jan)
lines(p2_mod_jan+intercept_jan ~ year, data = pdata_jan, type = "n")
lines(p2_mod_jan+intercept_jan ~ year, data = pdata_jan)
lines(unlist(mod_jan.dsig$incr)+intercept_jan ~ year, data = pdata_jan, col = "blue", lwd = 3)
lines(unlist(mod_jan.dsig$decr)+intercept_jan ~ year, data = pdata_jan, col = "red", lwd = 3)

linearMod_jan<- lm(chla ~ year, data=jan)
summary(linearMod_jan)

##########################FEB

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qfeb = quantile(feb$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_feb5 = mean(feb$chla[feb$year >= 2019])
# what quintile the data is in
mn_feb5 >qfeb

mod_feb<- gam(chla ~ s(year, k=10), data = feb)
summary(mod_feb) #check out model
gam.check(mod_feb)

pdata_feb <- with(feb, ], data.frame(year = year))
p2_mod_feb <- predict(mod_feb, newdata = pdata_feb,  type = "terms", se.fit = TRUE)
intercept_feb = 1.123051  # look at p2_mod and eyeartract the intercept
pdata_feb <- transform(pdata_feb , p2_mod_feb  = p2_mod_feb$fit[,1], se2_feb = p2_mod_feb$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_feb.d <- Deriv(mod_feb, n=24) # n is the number of years
mod_feb.dci <- confint(mod_feb.d, term = Term)
mod_feb.dsig <- signifD(pdata_feb$p2_mod_feb, d = mod_feb.d[[Term]]$deriv,
                        +                    mod_feb.dci[[Term]]$upper, mod_feb.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(feb ~ year, data = feb, ])
lines(feb ~ year, data = feb, ])
lines(p2_mod_feb+intercept_feb ~ year, data = pdata_feb, type = "n")
lines(p2_mod_feb+intercept_feb ~ year, data = pdata_feb)
lines(unlist(mod_feb.dsig$incr)+intercept_feb ~ year, data = pdata_feb, col = "blue", lwd = 3)
lines(unlist(mod_feb.dsig$decr)+intercept_feb ~ year, data = pdata_feb, col = "red", lwd = 3)

linearMod_feb<- lm(feb ~ year, data=feb, ])
summary(linearMod_feb)
##########################MAR

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qmar = quantile(mar$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_mar5 = mean(mar$chla[mar$year >= 2019])
# what quintile the data is in
mn_mar5 >qmar

mod_mar<- gam(mar ~ s(year, k=10), data = ds[2:25,])
summary(mod_mar) #check out model
gam.check(mod_mar)

pdata_mar <- with(ds, data.frame(year = year))
p2_mod_mar <- predict(mod_mar, newdata = pdata_mar,  type = "terms", se.fit = TRUE)
intercept_mar = 1.210976  # look at p2_mod and eyeartract the intercept
pdata_mar <- transform(pdata_mar , p2_mod_mar  = p2_mod_mar$fit[,1], se2_mar = p2_mod_mar$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_mar.d <- Deriv(mod_mar, n=24) # n is the number of years
mod_mar.dci <- confint(mod_mar.d, term = Term)
mod_mar.dsig <- signifD(pdata_mar$p2_mod_mar, d = mod_mar.d[[Term]]$deriv,
                        +                    mod_mar.dci[[Term]]$upper, mod_mar.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(mar ~ year, data = ds[2:25, ])
lines(mar ~ year, data = ds[2:25, ])
lines(p2_mod_mar+intercept_mar ~ year, data = pdata_mar, type = "n")
lines(p2_mod_mar+intercept_mar ~ year, data = pdata_mar)
lines(unlist(mod_mar.dsig$incr)+intercept_mar ~ year, data = pdata_mar, col = "blue", lwd = 3)
lines(unlist(mod_mar.dsig$decr)+intercept_mar ~ year, data = pdata_mar, col = "red", lwd = 3)

linearMod_mar<- lm(mar ~ year, data=ds[2:25, ])
summary(linearMod_mar)


##########################APR

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qapr = quantile(apr$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_apr5 = mean(apr$chla[apr$year >= 2019])
# what quintile the data is in
mn_apr5 >qapr

mod_apr<- gam(apr ~ s(year, k=10), data = ds[2:25, ])
summary(mod_apr) #check out model
gam.check(mod_apr)

pdata_apr <- with(ds[2:25, ], data.frame(year = year))
p2_mod_apr <- predict(mod_apr, newdata = pdata_apr,  type = "terms", se.fit = TRUE)
intercept_apr = 1.244894  # look at p2_mod and eyeartract the intercept
pdata_apr <- transform(pdata_apr , p2_mod_apr  = p2_mod_apr$fit[,1], se2_apr = p2_mod_apr$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_apr.d <- Deriv(mod_apr, n=24) # n is the number of years
mod_apr.dci <- confint(mod_apr.d, term = Term)
mod_apr.dsig <- signifD(pdata_apr$p2_mod_apr, d = mod_apr.d[[Term]]$deriv,
                        +                    mod_apr.dci[[Term]]$upper, mod_apr.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(apr ~ year, data = ds[2:25, ])
lines(apr ~ year, data = ds[2:25, ])
lines(p2_mod_apr+intercept_apr ~ year, data = pdata_apr, type = "n")
lines(p2_mod_apr+intercept_apr ~ year, data = pdata_apr)
lines(unlist(mod_apr.dsig$incr)+intercept_apr ~ year, data = pdata_apr, col = "blue", lwd = 3)
lines(unlist(mod_apr.dsig$decr)+intercept_apr ~ year, data = pdata_apr, col = "red", lwd = 3)

linearMod_apr<- lm(apr ~ year, data=ds[2:25, ])
summary(linearMod_apr)
# April had a statistically significant part of the GAM

##########################MAY

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qmay = quantile(may$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_may5 = mean(may$chla[may$year >= 2019])
# what quintile the data is in
mn_may5 >qmay

mod_may<- gam(may ~ s(year, k=10), data = ds[2:25, ])
summary(mod_may) #check out model
gam.check(mod_may)

pdata_may <- with(ds[2:25, ], data.frame(year = year))
p2_mod_may <- predict(mod_may, newdata = pdata_may,  type = "terms", se.fit = TRUE)
intercept_may = 1.039975  # look at p2_mod and eyeartract the intercept
pdata_may <- transform(pdata_may , p2_mod_may  = p2_mod_may$fit[,1], se2_may = p2_mod_may$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_may.d <- Deriv(mod_may, n=24) # n is the number of years
mod_may.dci <- confint(mod_may.d, term = Term)
mod_may.dsig <- signifD(pdata_may$p2_mod_may, d = mod_may.d[[Term]]$deriv,
                        +                    mod_may.dci[[Term]]$upper, mod_may.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(may ~ year, data = ds[2:25, ])
lines(may ~ year, data = ds[2:25, ])
lines(p2_mod_may+intercept_may ~ year, data = pdata_may, type = "n")
lines(p2_mod_may+intercept_may ~ year, data = pdata_may)
lines(unlist(mod_may.dsig$incr)+intercept_may ~ year, data = pdata_may, col = "blue", lwd = 3)
lines(unlist(mod_may.dsig$decr)+intercept_may ~ year, data = pdata_may, col = "red", lwd = 3)

linearMod_may<- lm(may ~ year, data=ds[2:25, ])
summary(linearMod_may)

##########################JUN - one significant increase

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qjun = quantile(jun$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_jun5 = mean(jun$chla[jun$year >= 2019])
# what quintile the data is in
mn_jun5 >qjun


mod_jun<- gam(jun ~ s(year, k=10), data = ds[2:25, ])
summary(mod_jun) #check out model
gam.check(mod_jun)

pdata_jun <- with(ds[2:25, ], data.frame(year = year))
p2_mod_jun <- predict(mod_jun, newdata = pdata_jun,  type = "terms", se.fit = TRUE)
intercept_jun = 0.9287668  # look at p2_mod and eyeartract the intercept
pdata_jun <- transform(pdata_jun , p2_mod_jun  = p2_mod_jun$fit[,1], se2_jun = p2_mod_jun$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_jun.d <- Deriv(mod_jun, n=24) # n is the number of years
mod_jun.dci <- confint(mod_jun.d, term = Term)
mod_jun.dsig <- signifD(pdata_jun$p2_mod_jun, d = mod_jun.d[[Term]]$deriv,
                        +                    mod_jun.dci[[Term]]$upper, mod_jun.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( jun~ year, data = ds[2:25, ])
lines(jun ~ year, data = ds[2:25, ])
lines(p2_mod_jun+intercept_jun ~ year, data = pdata_jun, type = "n")
lines(p2_mod_jun+intercept_jun ~ year, data = pdata_jun)
lines(unlist(mod_jun.dsig$incr)+intercept_jun ~ year, data = pdata_jun, col = "blue", lwd = 3)
lines(unlist(mod_jun.dsig$decr)+intercept_jun ~ year, data = pdata_jun, col = "red", lwd = 3)

linearMod_jun<- lm(jun ~ year, data=ds[2:25, ])
summary(linearMod_jun)

##########################JUL

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qjul = quantile(jul$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_jul5 = mean(jul$chla[jul$year >= 2019])
# what quintile the data is in
mn_jul5 >qjul

mod_jul<- gam(jul ~ s(year, k=10), data = ds[2:25, ])
summary(mod_jul) #check out model
gam.check(mod_jul)

pdata_jul <- with(ds[2:25, ], data.frame(year = year))
p2_mod_jul <- predict(mod_jul, newdata = pdata_jul,  type = "terms", se.fit = TRUE)
intercept_jul = 1.009002 # look at p2_mod and eyeartract the intercept
pdata_jul <- transform(pdata_jul , p2_mod_jul  = p2_mod_jul$fit[,1], se2_jul = p2_mod_jul$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_jul.d <- Deriv(mod_jul, n=24) # n is the number of years
mod_jul.dci <- confint(mod_jul.d, term = Term)
mod_jul.dsig <- signifD(pdata_jul$p2_mod_jul, d = mod_jul.d[[Term]]$deriv,
                        +                    mod_jul.dci[[Term]]$upper, mod_jul.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( jul~ year, data = ds[2:25, ])
lines(jul ~ year, data = ds[2:25, ])
lines(p2_mod_jul+intercept_jul ~ year, data = pdata_jul, type = "n")
lines(p2_mod_jul+intercept_jul ~ year, data = pdata_jul)
lines(unlist(mod_jul.dsig$incr)+intercept_jul ~ year, data = pdata_jul, col = "blue", lwd = 3)
lines(unlist(mod_jul.dsig$decr)+intercept_jul ~ year, data = pdata_jul, col = "red", lwd = 3)

linearMod_jul<- lm(jul ~ year, data=ds[2:25, ])
summary(linearMod_jul)

##########################AUG

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qaug = quantile(aug$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_aug5 = mean(aug$chla[aug$year >= 2019])
# what quintile the data is in
mn_aug5 >qaug

mod_aug<- gam(aug ~ s(year, k=10), data = ds[2:25, ])
summary(mod_aug) #check out model
gam.check(mod_aug)

pdata_aug <- with(ds[2:25, ], data.frame(year = year))
p2_mod_aug <- predict(mod_aug, newdata = pdata_aug,  type = "terms", se.fit = TRUE)
intercept_aug = 1.062954  # look at p2_mod and eyeartract the intercept
pdata_aug <- transform(pdata_aug , p2_mod_aug  = p2_mod_aug$fit[,1], se2_aug = p2_mod_aug$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_aug.d <- Deriv(mod_aug, n=24) # n is the number of years
mod_aug.dci <- confint(mod_aug.d, term = Term)
mod_aug.dsig <- signifD(pdata_aug$p2_mod_aug, d = mod_aug.d[[Term]]$deriv,
                        +                    mod_aug.dci[[Term]]$upper, mod_aug.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( aug~ year, data = ds[2:25, ])
lines(aug ~ year, data = ds[2:25, ])
lines(p2_mod_aug+intercept_aug ~ year, data = pdata_aug, type = "n")
lines(p2_mod_aug+intercept_aug ~ year, data = pdata_aug)
lines(unlist(mod_aug.dsig$incr)+intercept_aug ~ year, data = pdata_aug, col = "blue", lwd = 3)
lines(unlist(mod_aug.dsig$decr)+intercept_aug ~ year, data = pdata_aug, col = "red", lwd = 3)

linearMod_aug<- lm(aug ~ year, data=ds[2:25, ])
summary(linearMod_aug)

########################## SEP

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qsep = quantile(sep$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_sep5 = mean(sep$chla[sep$year >= 2019])
# what quintile the data is in
mn_sep5 >qsep

mod_sep<- gam(sep ~ s(year, k=10), data = ds[2:25, ])
summary(mod_sep) #check out model
gam.check(mod_sep)

pdata_sep <- with(ds[2:25, ], data.frame(year = year))
p2_mod_sep <- predict(mod_sep, newdata = pdata_sep,  type = "terms", se.fit = TRUE)
intercept_sep = 0.8920338  # look at p2_mod and eyeartract the intercept
pdata_sep <- transform(pdata_sep , p2_mod_sep  = p2_mod_sep$fit[,1], se2_sep = p2_mod_sep$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_sep.d <- Deriv(mod_sep, n=24) # n is the number of years
mod_sep.dci <- confint(mod_sep.d, term = Term)
mod_sep.dsig <- signifD(pdata_sep$p2_mod_sep, d = mod_sep.d[[Term]]$deriv,
                        +                    mod_sep.dci[[Term]]$upper, mod_sep.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( sep~ year, data = ds[2:25, ])
lines(sep ~ year, data = ds[2:25, ])
lines(p2_mod_sep+intercept_sep ~ year, data = pdata_sep, type = "n")
lines(p2_mod_sep+intercept_sep ~ year, data = pdata_sep)
lines(unlist(mod_sep.dsig$incr)+intercept_sep ~ year, data = pdata_sep, col = "blue", lwd = 3)
lines(unlist(mod_sep.dsig$decr)+intercept_sep ~ year, data = pdata_sep, col = "red", lwd = 3)

linearMod_sep<- lm(sep ~ year, data=ds[2:25, ])
summary(linearMod_sep)

########################## OCT

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qoct = quantile(oct$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_oct5 = mean(oct$chla[oct$year >= 2019])
# what quintile the data is in
mn_oct5 >qoct


mod_oct<- gam(oct ~ s(year, k=10), data = ds[2:25, ])
summary(mod_oct) #check out model
gam.check(mod_oct)

pdata_oct <- with(ds[2:25, ], data.frame(year = year))
p2_mod_oct <- predict(mod_oct, newdata = pdata_oct,  type = "terms", se.fit = TRUE)
intercept_oct = 1.079448  # look at p2_mod and eyeartract the intercept
pdata_oct <- transform(pdata_oct , p2_mod_oct  = p2_mod_oct$fit[,1], se2_oct = p2_mod_oct$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_oct.d <- Deriv(mod_oct, n=24) # n is the number of years
mod_oct.dci <- confint(mod_oct.d, term = Term)
mod_oct.dsig <- signifD(pdata_oct$p2_mod_oct, d = mod_oct.d[[Term]]$deriv,
                        +                    mod_oct.dci[[Term]]$upper, mod_oct.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( oct~ year, data = ds[2:25, ])
lines(oct ~ year, data = ds[2:25, ])
lines(p2_mod_oct+intercept_oct ~ year, data = pdata_oct, type = "n")
lines(p2_mod_oct+intercept_oct ~ year, data = pdata_oct)
lines(unlist(mod_oct.dsig$incr)+intercept_oct ~ year, data = pdata_oct, col = "blue", lwd = 3)
lines(unlist(mod_oct.dsig$decr)+intercept_oct ~ year, data = pdata_oct, col = "red", lwd = 3)

linearMod_oct<- lm(oct ~ year, data=ds[2:25, ])
summary(linearMod_oct)

########################## NOV

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qnov = quantile(nov$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_nov5 = mean(nov$chla[nov$year >= 2019])
# what quintile the data is in
mn_nov5 >qnov

mod_nov<- gam(nov ~ s(year, k=10), data = ds[2:25, ])
summary(mod_nov) #check out model
gam.check(mod_nov)

pdata_nov <- with(ds[2:25, ], data.frame(year = year))
p2_mod_nov <- predict(mod_nov, newdata = pdata_nov,  type = "terms", se.fit = TRUE)
intercept_nov = 1.453728  # look at p2_mod and eyeartract the intercept
pdata_nov <- transform(pdata_nov , p2_mod_nov  = p2_mod_nov$fit[,1], se2_nov = p2_mod_nov$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_nov.d <- Deriv(mod_nov, n=24) # n is the number of years
mod_nov.dci <- confint(mod_nov.d, term = Term)
mod_nov.dsig <- signifD(pdata_nov$p2_mod_nov, d = mod_nov.d[[Term]]$deriv,
                        +                    mod_nov.dci[[Term]]$upper, mod_nov.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( nov~ year, data = ds[2:25, ])
lines(nov ~ year, data = ds[2:25, ])
lines(p2_mod_nov+intercept_nov ~ year, data = pdata_nov, type = "n")
lines(p2_mod_nov+intercept_nov ~ year, data = pdata_nov)
lines(unlist(mod_nov.dsig$incr)+intercept_nov ~ year, data = pdata_nov, col = "blue", lwd = 3)
lines(unlist(mod_nov.dsig$decr)+intercept_nov ~ year, data = pdata_nov, col = "red", lwd = 3)

linearMod_nov<- lm(nov ~ year, data=ds[2:25, ])
summary(linearMod_nov)

########################## DEC

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qdec = quantile(dec$chla, probs = c(.30, .70))

# find the last 5 years mean
mn_dec5 = mean(dec$chla[dec$year >= 2019])
# what quintile the data is in
mn_dec5 >qdec

mod_dec<- gam(dec ~ s(year, k=10), data = ds[2:25, ])
summary(mod_dec) #check out model
gam.check(mod_dec)

pdata_dec <- with(ds[2:25, ], data.frame(year = year))
p2_mod_dec <- predict(mod_dec, newdata = pdata_dec,  type = "terms", se.fit = TRUE)
intercept_dec = 1.348864  # look at p2_mod and eyeartract the intercept
pdata_dec <- transform(pdata_dec , p2_mod_dec  = p2_mod_dec$fit[,1], se2_dec = p2_mod_dec$se.fit[,1])

#  Now that we have the model prediction, the neyeart step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_dec.d <- Deriv(mod_dec, n=24) # n is the number of years
mod_dec.dci <- confint(mod_dec.d, term = Term)
mod_dec.dsig <- signifD(pdata_dec$p2_mod_dec, d = mod_dec.d[[Term]]$deriv,
                        +                    mod_dec.dci[[Term]]$upper, mod_dec.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( dec~ year, data = ds[2:25, ])
lines(dec ~ year, data = ds[2:25, ])
lines(p2_mod_dec+intercept_dec ~ year, data = pdata_dec, type = "n")
lines(p2_mod_dec+intercept_dec ~ year, data = pdata_dec)
lines(unlist(mod_dec.dsig$incr)+intercept_dec ~ year, data = pdata_dec, col = "blue", lwd = 3)
lines(unlist(mod_dec.dsig$decr)+intercept_dec ~ year, data = pdata_dec, col = "red", lwd = 3)

linearMod_dec<- lm(dec ~ year, data=ds[2:25, ])
summary(linearMod_dec)


JAN <- ggplot() + 
  #geom_line(data = ds, aes(year = year, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(year = year, y = dec), color = 'gray') +   
  geom_line(data = jan, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = jan, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_line(data = jan, aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = jan, aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = feb, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = feb, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = feb, aes(year= year, y = feb), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = mar, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = mar, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = mar), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = apr, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = apr, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = apr), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_apr, aes(y = unlist(mod_apr.dsig$decr)+intercept_apr, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = may, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = may, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = may), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = jun, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = jun, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = jun), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = jul, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = jul, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = jul), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = aug, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = aug, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = aug), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = sep, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = sep, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = sep), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = oct, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = oct, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = oct), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = nov, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = nov, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = nov), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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
  geom_line(data = dec, aes(x =year, y = chla), color = 'grey53') +
  geom_point(data = dec, aes(x =year, y = chla), color = 'gray53') + 
  ylim(0.62, 2.2) +
  #geom_point(data = ds[25,], aes(year= year, y = dec), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x =year, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x =year, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x =year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x =year, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x =year), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x =year), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(2000,2023), labels = c(2000,2023))+
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