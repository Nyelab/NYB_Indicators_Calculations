## Figures for Sea Surface Temp Seasonally

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: October 7, 2025**

# Import libraries
setwd("~/Desktop/NYB_Indicators_Calculations")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025")
sst<-read.csv("SST_2025.csv", header = TRUE)

# WINTER

# Setting up Quintiles for short term above below average
qwinter = quantile(sst$winter, probs = c(.30,.70)) #In discussion with the DEC on 5/24/2024
# we chose to go with low = <30th percentile, avg = beween 30th and 70th and high above 70th.

# find the last 5 years mean
mn_winter5 = mean(sst$winter[sst$X >= 2021])
# what quintile the data is in
mn_winter5 >qwinter

# Creat a GAM - adjust k and remember to check model
mod_w<- gam(winter ~ s(X, k=5), data = sst)
summary(mod_w) #check out model
gam.check(mod_w)

pdata_w <- with(sst, data.frame(X = X))
p2_mod_w <- predict(mod_w, newdata = pdata_w,  type = "terms", se.fit = TRUE)
intercept_w = 9.288887 # look at p2_mod and extract the intercept
pdata_w <- transform(pdata_w, p2_mod_w = p2_mod_w$fit[,1], se2_w = p2_mod_w$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_w.d <- Deriv(mod_w, n=45) # n is the number of years
mod_w.dci <- confint(mod_w.d, term = Term)
mod_w.dsig <- signifD(pdata_w$p2_mod_w, d = mod_w.d[[Term]]$deriv,
                      +                    mod_w.dci[[Term]]$upper, mod_w.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(winter ~ X, data = sst)
lines(winter ~ X, data = sst)
lines(p2_mod_w+intercept_w ~ X, data = pdata_w, type = "n")
lines(p2_mod_w+intercept_w ~ X, data = pdata_w)
lines(unlist(mod_w.dsig$incr)+intercept_w ~ X, data = pdata_w, col = "blue", lwd = 3)
lines(unlist(mod_w.dsig$decr)+intercept_w ~ X, data = pdata_w, col = "red", lwd = 3)

linearMod_w<- lm(winter ~ X, data=sst)
summary(linearMod_w) # See if the linear trend is significant

wint_plot <- ggplot() + 
  geom_line(data = sst, aes(x = X, y = winter), color = 'grey52') +
  geom_point(data = sst, aes(x = X, y = winter), color = 'grey52') + 
  geom_point(data = sst[45,], aes(x= X, y = winter), shape = 17, size =3) +
  #geom_smooth(data = sst, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_w, aes(x = X, y = p2_mod_w+intercept_w), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$incr)+intercept_w, x = X), color = "blue", size = 1) + 
  geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$decr)+intercept_w, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Temp (\u00B0C)"), x =' ',title = 'Winter') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# SPRING

qspring = quantile(sst$spring, probs = c(.30,.70), na.rm = TRUE)

# find the last 5 years mean
mn_spring5 = mean(sst$spring[sst$X >= 2021])
# what quintile the data is in
mn_spring5 >qspring

#the last spring value is not averaging all the spring months so it looks anomalously low
#spring = spring[spring$X<77,]
# Creat a GAM - adjust k and remember to check model
mod_sp<- gam(spring ~ s(X, k=5), data = sst)
summary(mod_sp) #check out model
gam.check(mod_sp)

pdata_sp <- with(sst, data.frame(X = X))
p2_mod_sp <- predict(mod_sp, newdata = pdata_sp,  type = "terms", se.fit = TRUE)
intercept_sp = 9.199429  # look at p2_mod and extract the intercept
pdata_sp <- transform(pdata_sp, p2_mod_sp = p2_mod_sp$fit[,1], se2_sp = p2_mod_sp$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_sp.d <- Deriv(mod_sp, n=45) # n is the number of years
mod_sp.dci <- confint(mod_sp.d, term = Term)
mod_sp.dsig <- signifD(pdata_sp$p2_mod_sp, d = mod_sp.d[[Term]]$deriv,
                       +                    mod_sp.dci[[Term]]$upper, mod_sp.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(spring ~ X, data = sst)
lines(spring ~ X, data = sst)
lines(p2_mod_sp+intercept_sp ~ X, data = pdata_sp, type = "n")
lines(p2_mod_sp+intercept_sp ~ X, data = pdata_sp)
lines(unlist(mod_sp.dsig$incr)+intercept_sp ~ X, data = pdata_sp, col = "blue", lwd = 3)
lines(unlist(mod_sp.dsig$decr)+intercept_sp ~ X, data = pdata_sp, col = "red", lwd = 3)

linearMod_sp<- lm(spring ~ X, data=sst)
summary(linearMod_sp)

spr_plot <- ggplot() + 
  geom_line(data = sst, aes(x = X, y = spring), color = 'grey52') +
  geom_point(data = sst, aes(x = X, y = spring), color = 'grey52') + 
  geom_point(data = sst[45,], aes(x= X, y = spring), shape = 17, size =3) +
  #geom_smooth(data = sst, aes(x = X, y = spring), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_sp, aes(x = X, y = p2_mod_sp+intercept_sp), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$incr)+intercept_sp, x = X), color = "blue", size = 1) + 
  geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$decr)+intercept_sp, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = 'Year', title = 'Spring') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# SUMMER

qsummer = quantile(sst$summer, probs = c(.30,.70), na.rm = TRUE)

# find the last 5 years mean
mn_summer5 = mean(sst$summer[sst$X >= 2021])
# what quintile the data is in
mn_summer5 >qsummer

# Creat a GAM - adjust k and remember to check model
mod_su<- gam(summer ~ s(X, k=7), data = sst)
summary(mod_su) #check out model
gam.check(mod_su)

pdata_su <- with(sst, data.frame(X = X))
p2_mod_su <- predict(mod_su, newdata = pdata_su,  type = "terms", se.fit = TRUE)
intercept_su = 21.11867  # look at p2_mod and extract the intercept
pdata_su <- transform(pdata_su, p2_mod_su = p2_mod_su$fit[,1], se2_su = p2_mod_su$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_su.d <- Deriv(mod_su, n=45) # n is the number of years
mod_su.dci <- confint(mod_su.d, term = Term)
mod_su.dsig <- signifD(pdata_su$p2_mod_su, d = mod_su.d[[Term]]$deriv,
                       +                    mod_su.dci[[Term]]$upper, mod_su.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(summer ~ X, data = sst)
lines(summer ~ X, data = sst)
lines(p2_mod_su+intercept_su ~ X, data = pdata_su, type = "n")
lines(p2_mod_su+intercept_su ~ X, data = pdata_su)
lines(unlist(mod_su.dsig$incr)+intercept_su ~ X, data = pdata_su, col = "blue", lwd = 3)
lines(unlist(mod_su.dsig$decr)+intercept_su ~ X, data = pdata_su, col = "red", lwd = 3)

linearMod_su<- lm(summer ~ X, data=sst)
summary(linearMod_su)

sum_plot <- ggplot() + 
  geom_line(data = sst, aes(x = X, y = summer), color = 'grey52') +
  geom_point(data = sst, aes(x = X, y = summer), color = 'grey52') + 
  geom_point(data = sst[45,], aes(x= X, y = summer), shape = 17, size =3) +
  geom_smooth(data = sst, aes(x = X, y = summer), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_su, aes(x = X, y = p2_mod_su+intercept_su), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_su, aes(y = unlist(mod_su.dsig$incr)+intercept_su, x = X), color = "blue", size = 1) + 
  geom_line(data = pdata_su, aes(y = unlist(mod_su.dsig$decr)+intercept_su, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Temp (\u00B0C)"), x = 'Year', title = 'Summer') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# FALL

qfall = quantile(sst$fall, probs = c(.30,.70), na.rm = TRUE)

# find the last 5 years mean
mn_fall5 = mean(sst$fall[sst$X >= 2020], na.rm = TRUE)
# what quintile the data is in
mn_fall5 >qfall

# Creat a GAM - adjust k and remember to check model
#fall 2025 is incomplete, we only have data through early Nov.  Fall 2025 will be included next year
mod_fa<- gam(fall ~ s(X, k=7), data = sst)
summary(mod_fa) #check out model
gam.check(mod_fa)

pdata_fa <- with(sst, data.frame(X = X))
p2_mod_fa <- predict(mod_fa, newdata = pdata_fa,  type = "terms", se.fit = TRUE)
intercept_fa = 17.99264 # look at p2_mod and extract the intercept
pdata_fa <- transform(pdata_fa, p2_mod_fa = p2_mod_fa$fit[,1], se2_fa = p2_mod_fa$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_fa.d <- Deriv(mod_fa, n=45) # n is the number of years
mod_fa.dci <- confint(mod_fa.d, term = Term)
mod_fa.dsig <- signifD(pdata_fa$p2_mod_fa, d = mod_fa.d[[Term]]$deriv,
                       +                    mod_fa.dci[[Term]]$upper, mod_fa.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(fall ~ X, data = sst)
lines(fall ~ X, data = sst)
lines(p2_mod_fa+intercept_fa ~ X, data = pdata_fa, type = "n")
lines(p2_mod_fa+intercept_fa ~ X, data = pdata_fa)
lines(unlist(mod_fa.dsig$incr)+intercept_fa ~ X, data = pdata_fa, col = "blue", lwd = 3)
lines(unlist(mod_fa.dsig$decr)+intercept_fa ~ X, data = pdata_fa, col = "red", lwd = 3)

linearMod_fa<- lm(fall ~ X, data=sst)
summary(linearMod_fa)

fall_plot <- ggplot() + 
  geom_line(data = sst, aes(x = X, y = fall), color = 'grey52') +
  geom_point(data = sst, aes(x = X, y = fall), color = 'grey52') + 
  geom_point(data = sst[44,], aes(x= X, y = fall), shape = 17, size =3) +
  geom_smooth(data = sst, aes(x = X, y = fall), method = lm, se = FALSE, color = 'black') + 
  geom_line(data = pdata_fa, aes(x = X, y = p2_mod_fa+intercept_fa), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$incr)+intercept_fa, x = X), color = "blue", size = 1) + 
  geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$decr)+intercept_fa, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = 'Year', title = 'Autumn') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Now plot all 4 together
library(ggpubr)

ggarrange(wint_plot,spr_plot,sum_plot,fall_plot,nrow=2,ncol=2)

