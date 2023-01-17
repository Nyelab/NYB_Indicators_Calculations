## Figures for Surface Chlorophyll

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: November 14, 2022**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022")
ds<-read.csv("chla_nyb_12_14_2022.csv", header = TRUE)

# Creat a GAM - adjust k and remember to check model
mod_jan<- gam(jan ~ s(X, k=10), data = ds[2:25, ])
summary(mod_jan) #check out model
gam.check(mod_jan)

pdata_jan <- with(ds[2:25, ], data.frame(X = X))
p2_mod_jan <- predict(mod_jan, newdata = pdata_jan,  type = "terms", se.fit = TRUE)
intercept_jan = 1.270188 # look at p2_mod and extract the intercept
pdata_jan <- transform(pdata_jan, p2_mod_jan = p2_mod_jan$fit[,1], se2_jan = p2_mod_jan$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_jan.d <- Deriv(mod_jan, n=24) # n is the number of years
mod_jan.dci <- confint(mod_jan.d, term = Term)
mod_jan.dsig <- signifD(pdata_jan$p2_mod_jan, d = mod_jan.d[[Term]]$deriv,
                    +                    mod_jan.dci[[Term]]$upper, mod_jan.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(jan ~ X, data = ds[2:25, ])
lines(jan ~ X, data = ds[2:25, ])
lines(p2_mod_jan+intercept_jan ~ X, data = pdata_jan, type = "n")
lines(p2_mod_jan+intercept_jan ~ X, data = pdata_jan)
lines(unlist(mod_jan.dsig$incr)+intercept_jan ~ X, data = pdata_jan, col = "blue", lwd = 3)
lines(unlist(mod_jan.dsig$decr)+intercept_jan ~ X, data = pdata_jan, col = "red", lwd = 3)

linearMod_jan<- lm(jan ~ X, data=ds[2:25, ])
summary(linearMod_jan)
##########################FEB
mod_feb<- gam(feb ~ s(X, k=10), data = ds[2:25, ])
summary(mod_feb) #check out model
gam.check(mod_feb)

pdata_feb <- with(ds[2:25, ], data.frame(X = X))
p2_mod_feb <- predict(mod_feb, newdata = pdata_feb,  type = "terms", se.fit = TRUE)
intercept_feb = 1.123051  # look at p2_mod and extract the intercept
pdata_feb <- transform(pdata_feb , p2_mod_feb  = p2_mod_feb$fit[,1], se2_feb = p2_mod_feb$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_feb.d <- Deriv(mod_feb, n=24) # n is the number of years
mod_feb.dci <- confint(mod_feb.d, term = Term)
mod_feb.dsig <- signifD(pdata_feb$p2_mod_feb, d = mod_feb.d[[Term]]$deriv,
                        +                    mod_feb.dci[[Term]]$upper, mod_feb.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(feb ~ X, data = ds[2:25, ])
lines(feb ~ X, data = ds[2:25, ])
lines(p2_mod_feb+intercept_feb ~ X, data = pdata_feb, type = "n")
lines(p2_mod_feb+intercept_feb ~ X, data = pdata_feb)
lines(unlist(mod_feb.dsig$incr)+intercept_feb ~ X, data = pdata_feb, col = "blue", lwd = 3)
lines(unlist(mod_feb.dsig$decr)+intercept_feb ~ X, data = pdata_feb, col = "red", lwd = 3)

linearMod_feb<- lm(feb ~ X, data=ds[2:25, ])
summary(linearMod_feb)
##########################MAR
mod_mar<- gam(mar ~ s(X, k=10), data = ds[2:25,])
summary(mod_mar) #check out model
gam.check(mod_mar)

pdata_mar <- with(ds, data.frame(X = X))
p2_mod_mar <- predict(mod_mar, newdata = pdata_mar,  type = "terms", se.fit = TRUE)
intercept_mar = 1.210976  # look at p2_mod and extract the intercept
pdata_mar <- transform(pdata_mar , p2_mod_mar  = p2_mod_mar$fit[,1], se2_mar = p2_mod_mar$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_mar.d <- Deriv(mod_mar, n=24) # n is the number of years
mod_mar.dci <- confint(mod_mar.d, term = Term)
mod_mar.dsig <- signifD(pdata_mar$p2_mod_mar, d = mod_mar.d[[Term]]$deriv,
                        +                    mod_mar.dci[[Term]]$upper, mod_mar.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(mar ~ X, data = ds[2:25, ])
lines(mar ~ X, data = ds[2:25, ])
lines(p2_mod_mar+intercept_mar ~ X, data = pdata_mar, type = "n")
lines(p2_mod_mar+intercept_mar ~ X, data = pdata_mar)
lines(unlist(mod_mar.dsig$incr)+intercept_mar ~ X, data = pdata_mar, col = "blue", lwd = 3)
lines(unlist(mod_mar.dsig$decr)+intercept_mar ~ X, data = pdata_mar, col = "red", lwd = 3)

linearMod_mar<- lm(mar ~ X, data=ds[2:25, ])
summary(linearMod_mar)


##########################APR
mod_apr<- gam(apr ~ s(X, k=10), data = ds[2:25, ])
summary(mod_apr) #check out model
gam.check(mod_apr)

pdata_apr <- with(ds[2:25, ], data.frame(X = X))
p2_mod_apr <- predict(mod_apr, newdata = pdata_apr,  type = "terms", se.fit = TRUE)
intercept_apr = 1.244894  # look at p2_mod and extract the intercept
pdata_apr <- transform(pdata_apr , p2_mod_apr  = p2_mod_apr$fit[,1], se2_apr = p2_mod_apr$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_apr.d <- Deriv(mod_apr, n=24) # n is the number of years
mod_apr.dci <- confint(mod_apr.d, term = Term)
mod_apr.dsig <- signifD(pdata_apr$p2_mod_apr, d = mod_apr.d[[Term]]$deriv,
                        +                    mod_apr.dci[[Term]]$upper, mod_apr.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(apr ~ X, data = ds[2:25, ])
lines(apr ~ X, data = ds[2:25, ])
lines(p2_mod_apr+intercept_apr ~ X, data = pdata_apr, type = "n")
lines(p2_mod_apr+intercept_apr ~ X, data = pdata_apr)
lines(unlist(mod_apr.dsig$incr)+intercept_apr ~ X, data = pdata_apr, col = "blue", lwd = 3)
lines(unlist(mod_apr.dsig$decr)+intercept_apr ~ X, data = pdata_apr, col = "red", lwd = 3)

linearMod_apr<- lm(apr ~ X, data=ds[2:25, ])
summary(linearMod_apr)
# April had a statistically significant part of the GAM

##########################MAY
mod_may<- gam(may ~ s(X, k=10), data = ds[2:25, ])
summary(mod_may) #check out model
gam.check(mod_may)

pdata_may <- with(ds[2:25, ], data.frame(X = X))
p2_mod_may <- predict(mod_may, newdata = pdata_may,  type = "terms", se.fit = TRUE)
intercept_may = 1.039975  # look at p2_mod and extract the intercept
pdata_may <- transform(pdata_may , p2_mod_may  = p2_mod_may$fit[,1], se2_may = p2_mod_may$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_may.d <- Deriv(mod_may, n=24) # n is the number of years
mod_may.dci <- confint(mod_may.d, term = Term)
mod_may.dsig <- signifD(pdata_may$p2_mod_may, d = mod_may.d[[Term]]$deriv,
                        +                    mod_may.dci[[Term]]$upper, mod_may.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(may ~ X, data = ds[2:25, ])
lines(may ~ X, data = ds[2:25, ])
lines(p2_mod_may+intercept_may ~ X, data = pdata_may, type = "n")
lines(p2_mod_may+intercept_may ~ X, data = pdata_may)
lines(unlist(mod_may.dsig$incr)+intercept_may ~ X, data = pdata_may, col = "blue", lwd = 3)
lines(unlist(mod_may.dsig$decr)+intercept_may ~ X, data = pdata_may, col = "red", lwd = 3)

linearMod_may<- lm(may ~ X, data=ds[2:25, ])
summary(linearMod_may)

##########################JUN - one significant increase
mod_jun<- gam(jun ~ s(X, k=10), data = ds[2:25, ])
summary(mod_jun) #check out model
gam.check(mod_jun)

pdata_jun <- with(ds[2:25, ], data.frame(X = X))
p2_mod_jun <- predict(mod_jun, newdata = pdata_jun,  type = "terms", se.fit = TRUE)
intercept_jun = 0.9287668  # look at p2_mod and extract the intercept
pdata_jun <- transform(pdata_jun , p2_mod_jun  = p2_mod_jun$fit[,1], se2_jun = p2_mod_jun$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_jun.d <- Deriv(mod_jun, n=24) # n is the number of years
mod_jun.dci <- confint(mod_jun.d, term = Term)
mod_jun.dsig <- signifD(pdata_jun$p2_mod_jun, d = mod_jun.d[[Term]]$deriv,
                        +                    mod_jun.dci[[Term]]$upper, mod_jun.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( jun~ X, data = ds[2:25, ])
lines(jun ~ X, data = ds[2:25, ])
lines(p2_mod_jun+intercept_jun ~ X, data = pdata_jun, type = "n")
lines(p2_mod_jun+intercept_jun ~ X, data = pdata_jun)
lines(unlist(mod_jun.dsig$incr)+intercept_jun ~ X, data = pdata_jun, col = "blue", lwd = 3)
lines(unlist(mod_jun.dsig$decr)+intercept_jun ~ X, data = pdata_jun, col = "red", lwd = 3)

linearMod_jun<- lm(jun ~ X, data=ds[2:25, ])
summary(linearMod_jun)

##########################JUL
mod_jul<- gam(jul ~ s(X, k=10), data = ds[2:25, ])
summary(mod_jul) #check out model
gam.check(mod_jul)

pdata_jul <- with(ds[2:25, ], data.frame(X = X))
p2_mod_jul <- predict(mod_jul, newdata = pdata_jul,  type = "terms", se.fit = TRUE)
intercept_jul = 1.009002 # look at p2_mod and extract the intercept
pdata_jul <- transform(pdata_jul , p2_mod_jul  = p2_mod_jul$fit[,1], se2_jul = p2_mod_jul$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_jul.d <- Deriv(mod_jul, n=24) # n is the number of years
mod_jul.dci <- confint(mod_jul.d, term = Term)
mod_jul.dsig <- signifD(pdata_jul$p2_mod_jul, d = mod_jul.d[[Term]]$deriv,
                        +                    mod_jul.dci[[Term]]$upper, mod_jul.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( jul~ X, data = ds[2:25, ])
lines(jul ~ X, data = ds[2:25, ])
lines(p2_mod_jul+intercept_jul ~ X, data = pdata_jul, type = "n")
lines(p2_mod_jul+intercept_jul ~ X, data = pdata_jul)
lines(unlist(mod_jul.dsig$incr)+intercept_jul ~ X, data = pdata_jul, col = "blue", lwd = 3)
lines(unlist(mod_jul.dsig$decr)+intercept_jul ~ X, data = pdata_jul, col = "red", lwd = 3)

linearMod_jul<- lm(jul ~ X, data=ds[2:25, ])
summary(linearMod_jul)

##########################AUG
mod_aug<- gam(aug ~ s(X, k=10), data = ds[2:25, ])
summary(mod_aug) #check out model
gam.check(mod_aug)

pdata_aug <- with(ds[2:25, ], data.frame(X = X))
p2_mod_aug <- predict(mod_aug, newdata = pdata_aug,  type = "terms", se.fit = TRUE)
intercept_aug = 1.062954  # look at p2_mod and extract the intercept
pdata_aug <- transform(pdata_aug , p2_mod_aug  = p2_mod_aug$fit[,1], se2_aug = p2_mod_aug$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_aug.d <- Deriv(mod_aug, n=24) # n is the number of years
mod_aug.dci <- confint(mod_aug.d, term = Term)
mod_aug.dsig <- signifD(pdata_aug$p2_mod_aug, d = mod_aug.d[[Term]]$deriv,
                        +                    mod_aug.dci[[Term]]$upper, mod_aug.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( aug~ X, data = ds[2:25, ])
lines(aug ~ X, data = ds[2:25, ])
lines(p2_mod_aug+intercept_aug ~ X, data = pdata_aug, type = "n")
lines(p2_mod_aug+intercept_aug ~ X, data = pdata_aug)
lines(unlist(mod_aug.dsig$incr)+intercept_aug ~ X, data = pdata_aug, col = "blue", lwd = 3)
lines(unlist(mod_aug.dsig$decr)+intercept_aug ~ X, data = pdata_aug, col = "red", lwd = 3)

linearMod_aug<- lm(aug ~ X, data=ds[2:25, ])
summary(linearMod_aug)

########################## SEP
mod_sep<- gam(sep ~ s(X, k=10), data = ds[2:25, ])
summary(mod_sep) #check out model
gam.check(mod_sep)

pdata_sep <- with(ds[2:25, ], data.frame(X = X))
p2_mod_sep <- predict(mod_sep, newdata = pdata_sep,  type = "terms", se.fit = TRUE)
intercept_sep = 0.8920338  # look at p2_mod and extract the intercept
pdata_sep <- transform(pdata_sep , p2_mod_sep  = p2_mod_sep$fit[,1], se2_sep = p2_mod_sep$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_sep.d <- Deriv(mod_sep, n=24) # n is the number of years
mod_sep.dci <- confint(mod_sep.d, term = Term)
mod_sep.dsig <- signifD(pdata_sep$p2_mod_sep, d = mod_sep.d[[Term]]$deriv,
                        +                    mod_sep.dci[[Term]]$upper, mod_sep.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( sep~ X, data = ds[2:25, ])
lines(sep ~ X, data = ds[2:25, ])
lines(p2_mod_sep+intercept_sep ~ X, data = pdata_sep, type = "n")
lines(p2_mod_sep+intercept_sep ~ X, data = pdata_sep)
lines(unlist(mod_sep.dsig$incr)+intercept_sep ~ X, data = pdata_sep, col = "blue", lwd = 3)
lines(unlist(mod_sep.dsig$decr)+intercept_sep ~ X, data = pdata_sep, col = "red", lwd = 3)

linearMod_sep<- lm(sep ~ X, data=ds[2:25, ])
summary(linearMod_sep)

########################## OCT
mod_oct<- gam(oct ~ s(X, k=10), data = ds[2:25, ])
summary(mod_oct) #check out model
gam.check(mod_oct)

pdata_oct <- with(ds[2:25, ], data.frame(X = X))
p2_mod_oct <- predict(mod_oct, newdata = pdata_oct,  type = "terms", se.fit = TRUE)
intercept_oct = 1.079448  # look at p2_mod and extract the intercept
pdata_oct <- transform(pdata_oct , p2_mod_oct  = p2_mod_oct$fit[,1], se2_oct = p2_mod_oct$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_oct.d <- Deriv(mod_oct, n=24) # n is the number of years
mod_oct.dci <- confint(mod_oct.d, term = Term)
mod_oct.dsig <- signifD(pdata_oct$p2_mod_oct, d = mod_oct.d[[Term]]$deriv,
                        +                    mod_oct.dci[[Term]]$upper, mod_oct.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( oct~ X, data = ds[2:25, ])
lines(oct ~ X, data = ds[2:25, ])
lines(p2_mod_oct+intercept_oct ~ X, data = pdata_oct, type = "n")
lines(p2_mod_oct+intercept_oct ~ X, data = pdata_oct)
lines(unlist(mod_oct.dsig$incr)+intercept_oct ~ X, data = pdata_oct, col = "blue", lwd = 3)
lines(unlist(mod_oct.dsig$decr)+intercept_oct ~ X, data = pdata_oct, col = "red", lwd = 3)

linearMod_oct<- lm(oct ~ X, data=ds[2:25, ])
summary(linearMod_oct)

########################## NOV
mod_nov<- gam(nov ~ s(X, k=10), data = ds[2:25, ])
summary(mod_nov) #check out model
gam.check(mod_nov)

pdata_nov <- with(ds[2:25, ], data.frame(X = X))
p2_mod_nov <- predict(mod_nov, newdata = pdata_nov,  type = "terms", se.fit = TRUE)
intercept_nov = 1.453728  # look at p2_mod and extract the intercept
pdata_nov <- transform(pdata_nov , p2_mod_nov  = p2_mod_nov$fit[,1], se2_nov = p2_mod_nov$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_nov.d <- Deriv(mod_nov, n=24) # n is the number of years
mod_nov.dci <- confint(mod_nov.d, term = Term)
mod_nov.dsig <- signifD(pdata_nov$p2_mod_nov, d = mod_nov.d[[Term]]$deriv,
                        +                    mod_nov.dci[[Term]]$upper, mod_nov.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( nov~ X, data = ds[2:25, ])
lines(nov ~ X, data = ds[2:25, ])
lines(p2_mod_nov+intercept_nov ~ X, data = pdata_nov, type = "n")
lines(p2_mod_nov+intercept_nov ~ X, data = pdata_nov)
lines(unlist(mod_nov.dsig$incr)+intercept_nov ~ X, data = pdata_nov, col = "blue", lwd = 3)
lines(unlist(mod_nov.dsig$decr)+intercept_nov ~ X, data = pdata_nov, col = "red", lwd = 3)

linearMod_nov<- lm(nov ~ X, data=ds[2:25, ])
summary(linearMod_nov)

########################## DEC
mod_dec<- gam(dec ~ s(X, k=10), data = ds[2:25, ])
summary(mod_dec) #check out model
gam.check(mod_dec)

pdata_dec <- with(ds[2:25, ], data.frame(X = X))
p2_mod_dec <- predict(mod_dec, newdata = pdata_dec,  type = "terms", se.fit = TRUE)
intercept_dec = 1.348864  # look at p2_mod and extract the intercept
pdata_dec <- transform(pdata_dec , p2_mod_dec  = p2_mod_dec$fit[,1], se2_dec = p2_mod_dec$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_dec.d <- Deriv(mod_dec, n=24) # n is the number of years
mod_dec.dci <- confint(mod_dec.d, term = Term)
mod_dec.dsig <- signifD(pdata_dec$p2_mod_dec, d = mod_dec.d[[Term]]$deriv,
                        +                    mod_dec.dci[[Term]]$upper, mod_dec.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot( dec~ X, data = ds[2:25, ])
lines(dec ~ X, data = ds[2:25, ])
lines(p2_mod_dec+intercept_dec ~ X, data = pdata_dec, type = "n")
lines(p2_mod_dec+intercept_dec ~ X, data = pdata_dec)
lines(unlist(mod_dec.dsig$incr)+intercept_dec ~ X, data = pdata_dec, col = "blue", lwd = 3)
lines(unlist(mod_dec.dsig$decr)+intercept_dec ~ X, data = pdata_dec, col = "red", lwd = 3)

linearMod_dec<- lm(dec ~ X, data=ds[2:25, ])
summary(linearMod_dec)


JAN <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds[2:25, ], aes(x = X, y = jan), color = 'grey53') +
  geom_point(data = ds[2:25, ], aes(x = X, y = jan), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = jan), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Jan') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank(),
  #axis.ticks.x = element_blank(),
  axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
  plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

FEB <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey53') +
  geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = feb), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Feb') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

MAR <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds[2:25, ], aes(x = X, y = mar), color = 'grey53') +
  geom_point(data = ds[2:25, ], aes(x = X, y = mar), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = mar), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Mar') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
       axis.text.y = element_blank(),
       #axis.ticks.x = element_blank(),
       axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
       plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

APR <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds[2:25, ], aes(x = X, y = apr), color = 'grey53') +
  geom_point(data = ds[2:25, ], aes(x = X, y = apr), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = apr), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  geom_line(data = pdata_apr, aes(y = unlist(mod_apr.dsig$decr)+intercept_apr, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Apr') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

MAY <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds[2:25, ], aes(x = X, y = may), color = 'grey53') +
  geom_point(data = ds[2:25, ], aes(x = X, y = may), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = may), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'May') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

JUN <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds[2:25, ], aes(x = X, y = jun), color = 'grey53') +
  geom_point(data = ds[2:25, ], aes(x = X, y = jun), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = jun), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Jun') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

JUL <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds[2:25, ], aes(x = X, y = jul), color = 'grey53') +
  geom_point(data = ds[2:25, ], aes(x = X, y = jul), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = jul), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Jul') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
       # axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

AUG <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds, aes(x = X, y = aug), color = 'grey53') +
  geom_point(data = ds, aes(x = X, y = aug), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = aug), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Aug') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
       # axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

SEP <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds, aes(x = X, y = sep), color = 'grey53') +
  geom_point(data = ds, aes(x = X, y = sep), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = sep), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Sep') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
       # axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

OCT <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds, aes(x = X, y = oct), color = 'grey53') +
  geom_point(data = ds, aes(x = X, y = oct), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = oct), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Oct') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
      #  axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

NOV <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds, aes(x = X, y = nov), color = 'grey53') +
  geom_point(data = ds, aes(x = X, y = nov), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = nov), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Nov') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))

DEC <- ggplot() + 
  #geom_line(data = ds, aes(x = X, y = dec), color = 'grey') +
  #geom_point(data = ds, aes(x = X, y = dec), color = 'gray') +   
  geom_line(data = ds, aes(x = X, y = dec), color = 'grey53') +
  geom_point(data = ds, aes(x = X, y = dec), color = 'gray53') + 
  ylim(0.62, 2.2) +
  geom_point(data = ds[25,], aes(x= X, y = dec), shape = 17, size =3) +
  #geom_line(data = ds[2:25, ], aes(x = X, y = feb), color = 'grey') +
  #geom_point(data = ds[2:25, ], aes(x = X, y = feb), color = 'gray') +   
  #geom_smooth(data = ds, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_jan, aes(x = X, y = p2_mod_jan+intercept_jan), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$incr)+intercept_jan, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_jan, aes(y = unlist(mod_jan.dsig$decr)+intercept_jan, x = X), color = 'red', size = 1) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1998,2021), labels = c(1998,2021))+
  labs (y = '', x = '', title = 'Dec') + 
  theme(plot.title=element_text(size = 12,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 10, angle =90, vjust = 0.5, hjust=0.5),
        plot.margin = margin(.25, 0.2, 0.01, 0.01, "cm"))


# Now plot all 12 together
library(ggpubr)

ggarrange(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC,nrow=1,ncol=12)
