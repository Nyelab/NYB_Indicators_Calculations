## Figures for Sea Surface Salinity Seasonally

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: January 8, 2024**

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
cp<-read.csv("CP_volume_Jan_8_2024.csv", header = TRUE)

linearMod_april<- lm(April ~ X, data=cp)
summary(linearMod_april) #significant 95%

linearMod_may<- lm(May ~ X, data=cp)
summary(linearMod_may) #significant 99%

linearMod_june<- lm(June ~ X, data=cp)
summary(linearMod_june) #significant 99%

linearMod_july<- lm(July ~ X, data=cp)
summary(linearMod_july) #significant 99%

linearMod_aug<- lm(August ~ X, data=cp)
summary(linearMod_aug) #significant 99%

linearMod_sep<- lm(September ~ X, data=cp)
summary(linearMod_sep) #significant 99%

linearMod_oct<- lm(October ~ X, data=cp)
summary(linearMod_oct) #significant 95%


# JUNE
qjune = quantile(cp$June, probs = c(.30,.70))

# find the last 5 years mean
mn_june5 = mean(cp$June[cp$X >= 2019])
# what quintile the data is in
mn_june5 < qjune

# Creat a GAM - adjust k and remember to check model
mod_june<- gam(June ~ s(X, k=5), data = cp)
summary(mod_june) #check out model
gam.check(mod_june)

pdata_w <- with(cp, data.frame(X = X))
p2_mod_june <- predict(mod_june, newdata = pdata_w,  type = "terms", se.fit = TRUE)
intercept_w = 576.1016   # look at p2_mod and extract the intercept
pdata_w <- transform(pdata_w, p2_mod_june = p2_mod_june$fit[,1], se2_w = p2_mod_june$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_june.d <- Deriv(mod_june, n=31) # n is the number of Xs
mod_june.dci <- confint(mod_june.d, term = Term)
mod_june.dsig <- signifD(pdata_w$p2_mod_june, d = mod_june.d[[Term]]$deriv,
                      +                    mod_june.dci[[Term]]$upper, mod_june.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(June ~ X, data = cp)
lines(June ~ X, data = cp)
lines(p2_mod_june+intercept_w ~ X, data = pdata_w, type = "n")
lines(p2_mod_june+intercept_w ~ X, data = pdata_w)
lines(unlist(mod_june.dsig$incr)+intercept_w ~ X, data = pdata_w, col = "blue", lwd = 3)
lines(unlist(mod_june.dsig$decr)+intercept_w ~ X, data = pdata_w, col = "red", lwd = 3)

linearmod_june<- lm(June ~ X, data=cp)
summary(linearmod_june)

june_plot <- ggplot() + 
  geom_line(data = cp, aes(x = X, y = June), color = 'grey52') +
  geom_point(data = cp, aes(x = X, y = June), color = 'grey52') + 
  #geom_point(data = cp[42,], aes(x= X, y = June), shape = 17, size =3) +
  geom_smooth(data = cp, aes(x = X, y = June), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_w, aes(x = X, y = p2_mod_june+intercept_w), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_june.dsig$incr)+intercept_w, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_june.dsig$decr)+intercept_w, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~(km^3)~" "), x =' ',title = 'June') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# JULY

qJuly = quantile(cp$July, probs = c(.30, .70))

# find the last 5 years mean
mn_July5 = mean(cp$July[cp$X >= 2019])
# what quintile the data is in
mn_July5 < qJuly

#the last cp Julyue is not averaging all the cp months so it looks anomalously low
#cp = cp[cp$X<77,]
# Creat a GAM - adjust k and remember to check model
mod_july<- gam(July ~ s(X, k=5), data = cp)
summary(mod_july) #check out model
gam.check(mod_july)

pdata_july <- with(cp, data.frame(X = X))
p2_mod_july <- predict(mod_july, newdata = pdata_july,  type = "terms", se.fit = TRUE)
intercept_july = 349.2075    # look at p2_mod and extract the intercept
pdata_july <- transform(pdata_july, p2_mod_july = p2_mod_july$fit[,1], se2_july = p2_mod_july$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_july.d <- Deriv(mod_july, n=31) # n is the number of Xs
mod_july.dci <- confint(mod_july.d, term = Term)
mod_july.dsig <- signifD(pdata_july$p2_mod_july, d = mod_july.d[[Term]]$deriv,
                       +                    mod_july.dci[[Term]]$upper, mod_july.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(July ~ X, data = cp)
lines(July ~ X, data = cp)
lines(p2_mod_july+intercept_july ~ X, data = pdata_july, type = "n")
lines(p2_mod_july+intercept_july ~ X, data = pdata_july)
lines(unlist(mod_july.dsig$incr)+intercept_july ~ X, data = pdata_july, col = "blue", lwd = 3)
lines(unlist(mod_july.dsig$decr)+intercept_july ~ X, data = pdata_july, col = "red", lwd = 3)

linearMod_july<- lm(July ~ X, data=cp)
summary(linearMod_july)

july_plot <- ggplot() + 
  geom_line(data = cp, aes(x = X, y = July), color = 'grey52') +
  geom_point(data = cp, aes(x = X, y = July), color = 'grey52') + 
  #geom_point(data = cp[42,], aes(x= X, y = July), shape = 17, size =3) +
  geom_smooth(data = cp, aes(x = X, y = July), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_july, aes(x = X, y = p2_mod_july+intercept_july), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_july, aes(y = unlist(mod_july.dsig$incr)+intercept_july, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_july, aes(y = unlist(mod_july.dsig$decr)+intercept_july, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = ' ', title = 'July') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# August


qAugust = quantile(cp$August, probs = c(.30, .70))

# find the last 5 years mean
mn_August5 = mean(cp$August[cp$X >= 2019])
# what quintile the data is in
mn_August5 < qAugust

# Creat a GAM - adjust k and remember to check model
mod_aug<- gam(August ~ s(X, k=5), data = cp)
summary(mod_aug) #check out model
gam.check(mod_aug)

pdata_aug <- with(cp, data.frame(X = X))
p2_mod_aug <- predict(mod_aug, newdata = pdata_aug,  type = "terms", se.fit = TRUE)
intercept_aug = 227.7404  # look at p2_mod and extract the intercept
pdata_aug <- transform(pdata_aug, p2_mod_aug = p2_mod_aug$fit[,1], se2_aug = p2_mod_aug$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_aug.d <- Deriv(mod_aug, n=31) # n is the number of Xs
mod_aug.dci <- confint(mod_aug.d, term = Term)
mod_aug.dsig <- signifD(pdata_aug$p2_mod_aug, d = mod_aug.d[[Term]]$deriv,
                       +                    mod_aug.dci[[Term]]$upper, mod_aug.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(August ~ X, data = cp)
lines(August ~ X, data = cp)
lines(p2_mod_aug+intercept_aug ~ X, data = pdata_aug, type = "n")
lines(p2_mod_aug+intercept_aug ~ X, data = pdata_aug)
lines(unlist(mod_aug.dsig$incr)+intercept_aug ~ X, data = pdata_aug, col = "blue", lwd = 3)
lines(unlist(mod_aug.dsig$decr)+intercept_aug ~ X, data = pdata_aug, col = "red", lwd = 3)

linearMod_aug<- lm(August ~ X, data=cp)
summary(linearMod_aug)

aug_plot <- ggplot() + 
  geom_line(data = cp, aes(x = X, y = August), color = 'grey52') +
  geom_point(data = cp, aes(x = X, y = August), color = 'grey52') + 
  #geom_point(data = cp[42,], aes(x= X, y = August), shape = 17, size =3) +
  geom_smooth(data = cp, aes(x = X, y = August), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_aug, aes(x = X, y = p2_mod_aug+intercept_aug), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_aug, aes(y = unlist(mod_aug.dsig$incr)+intercept_aug, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_aug, aes(y = unlist(mod_aug.dsig$decr)+intercept_aug, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~(km^3)~" "), x = ' ', title = 'August') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# September

qSeptember = quantile(cp$September, probs = c(.30, .70))

# find the last 5 years mean
mn_September5 = mean(cp$September[cp$X >= 2019])
# what quintile the data is in
mn_September5 < qSeptember

mn_September = mean(cp$September)
#standard deviation
sd_September = sd(cp$September)
# find the last 5 years mean
mn_September5 = mean(cp$September[cp$X >= 2019])
# see if the average of the last 5 years is greater than 1sd from the long term mean
mn_September - sd_September > mn_September5 # if this is TRUE then the short term trend is above average in the indicators at a glance table.


# 
mod_sep<- gam(September ~ s(X, k=5), data = cp)
summary(mod_sep) #check out model
gam.check(mod_sep)

pdata_sep <- with(cp, data.frame(X = X))
p2_mod_sep <- predict(mod_sep, newdata = pdata_sep,  type = "terms", se.fit = TRUE)
intercept_sep =  107.9713   # look at p2_mod and extract the intercept
pdata_sep <- transform(pdata_sep, p2_mod_sep = p2_mod_sep$fit[,1], se2_sep = p2_mod_sep$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_sep.d <- Deriv(mod_sep, n=31) # n is the number of Xs
mod_sep.dci <- confint(mod_sep.d, term = Term)
mod_sep.dsig <- signifD(pdata_sep$p2_mod_sep, d = mod_sep.d[[Term]]$deriv,
                       +                    mod_sep.dci[[Term]]$upper, mod_sep.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(September ~ X, data = cp)
lines(September ~ X, data = cp)
lines(p2_mod_sep+intercept_sep ~ X, data = pdata_sep, type = "n")
lines(p2_mod_sep+intercept_sep ~ X, data = pdata_sep)
lines(unlist(mod_sep.dsig$incr)+intercept_sep ~ X, data = pdata_sep, col = "blue", lwd = 3)
lines(unlist(mod_sep.dsig$decr)+intercept_sep ~ X, data = pdata_sep, col = "red", lwd = 3)

linearMod_sep<- lm(September ~ X, data=cp)
summary(linearMod_sep)

sep_plot <- ggplot() + 
  geom_line(data = cp, aes(x = X, y = September), color = 'grey52') +
  geom_point(data = cp, aes(x = X, y = September), color = 'grey52') + 
  #geom_point(data = cp[41,], aes(x= X, y = September), shape = 17, size =3) +
  geom_smooth(data = cp, aes(x = X, y = September), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data = pdata_sep, aes(x = X, y = p2_mod_sep+intercept_sep), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_sep, aes(y = unlist(mod_sep.dsig$incr)+intercept_sep, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_sep, aes(y = unlist(mod_sep.dsig$decr)+intercept_sep, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = '', title = 'September') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Now plot all 4 together
library(ggpubr)

ggarrange(june_plot,july_plot,aug_plot,sep_plot,nrow=2,ncol=2)

