## Figures for Sea Surface Salinity Seasonally

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: October 27, 2025**

setwd("~/Desktop/NYB_Indicators_Calculations/")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025")
cp<-read.csv("COld_Pool_2025.csv", header = TRUE)
cp$volume <- cp$volume/(1000*1000*1000)

linearMod_april<- lm(volume ~ year, data=cp[cp$month == 4,])
summary(linearMod_april) #not significant

linearMod_may<- lm(volume ~ year, data=cp[cp$month == 5,])
summary(linearMod_may) #not significant

linearMod_june<- lm(volume ~ year, data=cp[cp$month == 6,])
summary(linearMod_june) #not significant

linearMod_july<- lm(volume ~ year, data=cp[cp$month == 7,])
summary(linearMod_july) # not significant

linearMod_aug<- lm(volume ~ year, data=cp[cp$month == 8,])
summary(linearMod_aug) #significant 95%

linearMod_sep<- lm(volume ~ year, data=cp[cp$month == 9,])
summary(linearMod_sep) #significant 95%

linearMod_oct<- lm(volume ~ year, data=cp[cp$month == 10,])
summary(linearMod_oct) #significant 95%


# JUNE

qjune = quantile(cp[cp$month == 6,]$volume, probs = c(.30,.70))

# find the last 5 years mean
mn_june5 = mean(cp[cp$month == 6 & cp$year >= 2021,]$volume)
# what quintile the data is in
mn_june5 > qjune

# Creat a GAM - adjust k and remember to check model
mod_june<- gam(volume ~ s(year, k=5), data = cp[cp$month == 6,])
summary(mod_june) #check out model
gam.check(mod_june)

pdata_w <- with(cp[cp$month == 6,], data.frame(year = year))
p2_mod_june <- predict(mod_june, newdata = pdata_w,  type = "terms", se.fit = TRUE)
intercept_w =  618.3764   # look at p2_mod and extract the intercept
pdata_w <- transform(pdata_w, p2_mod_june = p2_mod_june$fit[,1], se2_w = p2_mod_june$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_june.d <- Deriv(mod_june, n=33) # n is the number of Xs
mod_june.dci <- confint(mod_june.d, term = Term)
mod_june.dsig <- signifD(pdata_w$p2_mod_june, d = mod_june.d[[Term]]$deriv,
                         +                    mod_june.dci[[Term]]$upper, mod_june.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(volume ~ year, data = cp[cp$month == 6,])
lines(volume ~ year, data = cp[cp$month == 6,])
lines(p2_mod_june+intercept_w ~ year, data = pdata_w, type = "n")
lines(p2_mod_june+intercept_w ~ year, data = pdata_w)
lines(unlist(mod_june.dsig$incr)+intercept_w ~ year, data = pdata_w, col = "blue", lwd = 3)
lines(unlist(mod_june.dsig$decr)+intercept_w ~ year, data = pdata_w, col = "red", lwd = 3)

linearmod_june<- lm(volume ~ year, data=cp[cp$month == 6,])
summary(linearmod_june)

june_plot <- ggplot() + 
  geom_line(data = cp[cp$month == 6,], aes(x = year, y = volume), color = 'grey52') +
  geom_point(data = cp[cp$month == 6,], aes(x = year, y = volume), color = 'grey52') + 
  geom_point(data = cp[cp$month == 6,][33,], aes(x= year, y = volume), shape = 17, size =3) +
  #geom_smooth(data = cp[cp$month == 6,], aes(x = year, y = volume), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_w, aes(x = year, y = p2_mod_june+intercept_w), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_june.dsig$incr)+intercept_w, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_june.dsig$decr)+intercept_w, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~(km^3)~" "), x =' ',title = 'June') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# JULY

qJuly = quantile(cp[cp$month == 7,]$volume, probs = c(.30, .70))

# find the last 5 years mean
mn_July5 = mean(cp[cp$month == 7 & cp$year >= 2021,]$volume)
# what quintile the data is in
mn_July5 > qJuly

#the last cp Julyue is not averaging all the cp months so it looks anomalously low
#cp = cp[cp$X<77,]
# Creat a GAM - adjust k and remember to check model
mod_july<- gam(volume ~ s(year, k=5), data = cp[cp$month == 7,])
summary(mod_july) #check out model
gam.check(mod_july)

pdata_july <- with(cp[cp$month == 7,], data.frame(year = year))
p2_mod_july <- predict(mod_july, newdata = pdata_july,  type = "terms", se.fit = TRUE)
intercept_july = 365.6449     # look at p2_mod and extract the intercept
pdata_july <- transform(pdata_july, p2_mod_july = p2_mod_july$fit[,1], se2_july = p2_mod_july$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_july.d <- Deriv(mod_july, n=33) # n is the number of Xs
mod_july.dci <- confint(mod_july.d, term = Term)
mod_july.dsig <- signifD(pdata_july$p2_mod_july, d = mod_july.d[[Term]]$deriv,
                         +                    mod_july.dci[[Term]]$upper, mod_july.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(volume ~ year, data = cp[cp$month == 7,])
lines(volume ~ year, data = cp[cp$month == 7,])
lines(p2_mod_july+intercept_july ~ year, data = pdata_july, type = "n")
lines(p2_mod_july+intercept_july ~ year, data = pdata_july)
lines(unlist(mod_july.dsig$incr)+intercept_july ~ year, data = pdata_july, col = "blue", lwd = 3)
lines(unlist(mod_july.dsig$decr)+intercept_july ~ year, data = pdata_july, col = "red", lwd = 3)

linearMod_july<- lm(volume ~ year, data=cp[cp$month == 7,])
summary(linearMod_july)

july_plot <- ggplot() + 
  geom_line(data = cp[cp$month == 7,], aes(x = year, y = volume), color = 'grey52') +
  geom_point(data = cp[cp$month == 7,], aes(x = year, y = volume), color = 'grey52') + 
  geom_point(data = cp[cp$month == 7,][33,], aes(x= year, y = volume), shape = 17, size =3) +
  #geom_smooth(data = cp[cp$month == 7,], aes(x = year, y = volume), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_july, aes(x = year, y = p2_mod_july+intercept_july), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_july, aes(y = unlist(mod_july.dsig$incr)+intercept_july, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata_july, aes(y = unlist(mod_july.dsig$decr)+intercept_july, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = ' ', title = 'July') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# August


qAugust = quantile(cp[cp$month == 8,]$volume, probs = c(.30, .70))

# find the last 5 years mean
mn_August5 = mean(cp[cp$month == 8 & cp$year >= 2020,]$volume)
# what quintile the data is in
mn_August5 > qAugust

# Creat a GAM - adjust k and remember to check model
mod_aug<- gam(volume ~ s(year, k=5), data = cp[cp$month == 8,])
summary(mod_aug) #check out model
gam.check(mod_aug)

pdata_aug <- with(cp[cp$month == 8,], data.frame(year = year))
p2_mod_aug <- predict(mod_aug, newdata = pdata_aug,  type = "terms", se.fit = TRUE)
intercept_aug = 237.7354  # look at p2_mod and extract the intercept
pdata_aug <- transform(pdata_aug, p2_mod_aug = p2_mod_aug$fit[,1], se2_aug = p2_mod_aug$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_aug.d <- Deriv(mod_aug, n=32) # n is the number of Xs
mod_aug.dci <- confint(mod_aug.d, term = Term)
mod_aug.dsig <- signifD(pdata_aug$p2_mod_aug, d = mod_aug.d[[Term]]$deriv,
                        +                    mod_aug.dci[[Term]]$upper, mod_aug.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(volume ~ year, data = cp[cp$month == 8,])
lines(volume ~ year, data = cp[cp$month == 8,])
lines(p2_mod_aug+intercept_aug ~ year, data = pdata_aug, type = "n")
lines(p2_mod_aug+intercept_aug ~ year, data = pdata_aug)
lines(unlist(mod_aug.dsig$incr)+intercept_aug ~ year, data = pdata_aug, col = "blue", lwd = 3)
lines(unlist(mod_aug.dsig$decr)+intercept_aug ~ year, data = pdata_aug, col = "red", lwd = 3)

linearMod_aug<- lm(volume ~ year, data=cp[cp$month == 8,])
summary(linearMod_aug)

aug_plot <- ggplot() + 
  geom_line(data = cp[cp$month == 8,], aes(x = year, y = volume), color = 'grey52') +
  geom_point(data = cp[cp$month == 8,], aes(x = year, y = volume), color = 'grey52') + 
  #geom_point(data = cp[cp$month == 8,][42,], aes(x= year, y = volume), shape = 17, size =3) +
  geom_smooth(data = cp[cp$month == 8,], aes(x = year, y = volume), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_aug, aes(x = year, y = p2_mod_aug+intercept_aug), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_aug, aes(y = unlist(mod_aug.dsig$incr)+intercept_aug, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata_aug, aes(y = unlist(mod_aug.dsig$decr)+intercept_aug, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~(km^3)~" "), x = ' ', title = 'August') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# September

qSeptember = quantile(cp[cp$month == 9,]$volume, probs = c(.30, .70))

# find the last 5 years mean
mn_September5 = mean(cp[cp$month == 9 & cp$year >= 2020,]$volume)
# what quintile the data is in
mn_September5 > qSeptember

mn_September = mean(cp[cp$month == 9,]$volume)
#standard deviation
sd_September = sd(cp[cp$month == 9,]$volume)
# find the last 5 years mean
mn_September5 = mean(cp[cp$month == 9,]$volume[cp$year >= 2019])
# see if the average of the last 5 years is greater than 1sd from the long term mean
mn_September - sd_September > mn_September5 # if this is TRUE then the short term trend is above average in the indicators at a glance table.


# 
mod_sep<- gam(volume ~ s(year, k=5), data = cp[cp$month == 9, ])
summary(mod_sep) #check out model
gam.check(mod_sep)

pdata_sep <- with(cp[cp$month == 9, ], data.frame(year = year))
p2_mod_sep <- predict(mod_sep, newdata = pdata_sep,  type = "terms", se.fit = TRUE)
intercept_sep =  112.4906    # look at p2_mod and extract the intercept
pdata_sep <- transform(pdata_sep, p2_mod_sep = p2_mod_sep$fit[,1], se2_sep = p2_mod_sep$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_sep.d <- Deriv(mod_sep, n=32) # n is the number of Xs
mod_sep.dci <- confint(mod_sep.d, term = Term)
mod_sep.dsig <- signifD(pdata_sep$p2_mod_sep, d = mod_sep.d[[Term]]$deriv,
                        +                    mod_sep.dci[[Term]]$upper, mod_sep.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(volume ~ year, data = cp[cp$month == 9, ])
lines(volume ~ year, data = cp[cp$month == 9, ])
lines(p2_mod_sep+intercept_sep ~ year, data = pdata_sep, type = "n")
lines(p2_mod_sep+intercept_sep ~ year, data = pdata_sep)
lines(unlist(mod_sep.dsig$incr)+intercept_sep ~ year, data = pdata_sep, col = "blue", lwd = 3)
lines(unlist(mod_sep.dsig$decr)+intercept_sep ~ year, data = pdata_sep, col = "red", lwd = 3)

linearMod_sep<- lm(volume ~ year, data=cp[cp$month == 9, ])
summary(linearMod_sep)

sep_plot <- ggplot() + 
  geom_line(data = cp[cp$month == 9, ], aes(x = year, y = volume), color = 'grey52') +
  geom_point(data = cp[cp$month == 9, ], aes(x = year, y = volume), color = 'grey52') + 
  #geom_point(data = cp[cp$month == 9, ][41,], aes(x= year, y = volume), shape = 17, size =3) +
  geom_smooth(data = cp[cp$month == 9, ], aes(x = year, y = volume), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data = pdata_sep, aes(x = year, y = p2_mod_sep+intercept_sep), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_sep, aes(y = unlist(mod_sep.dsig$incr)+intercept_sep, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata_sep, aes(y = unlist(mod_sep.dsig$decr)+intercept_sep, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = '', title = 'September') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Now plot all 4 together
library(ggpubr)

ggarrange(june_plot,july_plot,aug_plot,sep_plot,nrow=2,ncol=2)

