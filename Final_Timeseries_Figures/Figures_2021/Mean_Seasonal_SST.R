setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/Final_timeseries")
sst<-read.csv("OISST_Means_NYB_NOV_23_2021.csv", header = TRUE)

winter = sst[sst$Variable == 'winter_OISST',]
spring = sst[sst$Variable == 'spring_OISST',]
summer = sst[sst$Variable == 'summer_OISST',]
fall = sst[sst$Variable == 'fall_OISST',]

# WINTER
# Creat a GAM - adjust k and remember to check model
mod_w<- gam(Val ~ s(Year, k=5), data = winter)
summary(mod_w) #check out model
gam.check(mod_w)

pdata_w <- with(winter, data.frame(Year = Year))
p2_mod_w <- predict(mod_w, newdata = pdata_w,  type = "terms", se.fit = TRUE)
intercept_w = 7.641804 # look at p2_mod and extract the intercept
pdata_w <- transform(pdata_w, p2_mod_w = p2_mod_w$fit[,1], se2_w = p2_mod_w$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_w.d <- Deriv(mod_w, n=39) # n is the number of years
mod_w.dci <- confint(mod_w.d, term = Term)
mod_w.dsig <- signifD(pdata_w$p2_mod_w, d = mod_w.d[[Term]]$deriv,
                    +                    mod_w.dci[[Term]]$upper, mod_w.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = winter)
lines(Val ~ Year, data = winter)
lines(p2_mod_w+intercept_w ~ Year, data = pdata_w, type = "n")
lines(p2_mod_w+intercept_w ~ Year, data = pdata_w)
lines(unlist(mod_w.dsig$incr)+intercept_w ~ Year, data = pdata_w, col = "blue", lwd = 3)
lines(unlist(mod_w.dsig$decr)+intercept_w ~ Year, data = pdata_w, col = "red", lwd = 3)

linearMod_w<- lm(Val ~ Year, data=winter)
summary(linearMod_w)

wint_plot <- ggplot() + 
  geom_line(data = winter, aes(x = Year, y = Val), color = 'grey52') +
  geom_point(data = winter, aes(x = Year, y = Val), color = 'grey52') + 
  geom_smooth(data = winter, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_w, aes(x = Year, y = p2_mod_w+intercept_w), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$incr)+intercept_w, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$decr)+intercept_w, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Temp (\u00B0C)"), x =' ',title = 'Winter') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# SPRING

#the last spring value is not averaging all the spring months so it looks anomalously low
spring = spring[spring$X<77,]
# Creat a GAM - adjust k and remember to check model
mod_sp<- gam(Val ~ s(Year, k=5), data = spring)
summary(mod_sp) #check out model
gam.check(mod_sp)

pdata_sp <- with(spring, data.frame(Year = Year))
p2_mod_sp <- predict(mod_sp, newdata = pdata_sp,  type = "terms", se.fit = TRUE)
intercept_sp = 12.58782 # look at p2_mod and extract the intercept
pdata_sp <- transform(pdata_sp, p2_mod_sp = p2_mod_sp$fit[,1], se2_sp = p2_mod_sp$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_sp.d <- Deriv(mod_sp, n=38) # n is the number of years
mod_sp.dci <- confint(mod_sp.d, term = Term)
mod_sp.dsig <- signifD(pdata_sp$p2_mod_sp, d = mod_sp.d[[Term]]$deriv,
                    +                    mod_sp.dci[[Term]]$upper, mod_sp.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = spring)
lines(Val ~ Year, data = spring)
lines(p2_mod_sp+intercept_sp ~ Year, data = pdata_sp, type = "n")
lines(p2_mod_sp+intercept_sp ~ Year, data = pdata_sp)
lines(unlist(mod_sp.dsig$incr)+intercept_sp ~ Year, data = pdata_sp, col = "blue", lwd = 3)
lines(unlist(mod_sp.dsig$decr)+intercept_sp ~ Year, data = pdata_sp, col = "red", lwd = 3)

linearMod_sp<- lm(Val ~ Year, data=spring)
summary(linearMod_sp)

spr_plot <- ggplot() + 
  geom_line(data = spring, aes(x = Year, y = Val), color = 'grey52') +
  geom_point(data = spring, aes(x = Year, y = Val), color = 'grey52') + 
  geom_smooth(data = spring, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_sp, aes(x = Year, y = p2_mod_sp+intercept_sp), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$incr)+intercept_sp, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$decr)+intercept_sp, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = ' ', title = 'Spring') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# SUMMER

# Creat a GAM - adjust k and remember to check model
mod_su<- gam(Val ~ s(Year, k=7), data = summer)
summary(mod_su) #check out model
gam.check(mod_su)
          
pdata_su <- with(summer, data.frame(Year = Year))
p2_mod_su <- predict(mod_su, newdata = pdata_su,  type = "terms", se.fit = TRUE)
intercept_su = 22.30544 # look at p2_mod and extract the intercept
pdata_su <- transform(pdata_su, p2_mod_su = p2_mod_su$fit[,1], se2_su = p2_mod_su$se.fit[,1])
          
#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_su.d <- Deriv(mod_su, n=38) # n is the number of years
mod_su.dci <- confint(mod_su.d, term = Term)
mod_su.dsig <- signifD(pdata_su$p2_mod_su, d = mod_su.d[[Term]]$deriv,
                              +                    mod_su.dci[[Term]]$upper, mod_su.dci[[Term]]$lower)
          
# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = summer)
lines(Val ~ Year, data = summer)
lines(p2_mod_su+intercept_su ~ Year, data = pdata_su, type = "n")
lines(p2_mod_su+intercept_su ~ Year, data = pdata_su)
lines(unlist(mod_su.dsig$incr)+intercept_su ~ Year, data = pdata_su, col = "blue", lwd = 3)
lines(unlist(mod_su.dsig$decr)+intercept_su ~ Year, data = pdata_su, col = "red", lwd = 3)
          
linearMod_su<- lm(Val ~ Year, data=summer)
summary(linearMod_su)
          
sum_plot <- ggplot() + 
geom_line(data = summer, aes(x = Year, y = Val), color = 'grey52') +
geom_point(data = summer, aes(x = Year, y = Val), color = 'grey52') + 
geom_smooth(data = summer, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
geom_line(data=pdata_su, aes(x = Year, y = p2_mod_su+intercept_su), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
geom_line(data = pdata_su, aes(y = unlist(mod_su.dsig$incr)+intercept_su, x = Year), color = "blue", size = 1) + 
geom_line(data = pdata_su, aes(y = unlist(mod_su.dsig$decr)+intercept_su, x = Year), color = 'red', size = 1) + 
theme_bw() +
labs (y = bquote("Temp (\u00B0C)"), x = 'Year', title = 'Summer') + 
theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
          
# FALL

# Creat a GAM - adjust k and remember to check model
mod_fa<- gam(Val ~ s(Year, k=7), data = fall)
summary(mod_fa) #check out model
gam.check(mod_fa)

pdata_fa <- with(fall, data.frame(Year = Year))
p2_mod_fa <- predict(mod_fa, newdata = pdata_fa,  type = "terms", se.fit = TRUE)
intercept_fa = 14.72835 # look at p2_mod and extract the intercept
pdata_fa <- transform(pdata_fa, p2_mod_fa = p2_mod_fa$fit[,1], se2_fa = p2_mod_fa$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_fa.d <- Deriv(mod_fa, n=38) # n is the number of years
mod_fa.dci <- confint(mod_fa.d, term = Term)
mod_fa.dsig <- signifD(pdata_fa$p2_mod_fa, d = mod_fa.d[[Term]]$deriv,
                    +                    mod_fa.dci[[Term]]$upper, mod_fa.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = fall)
lines(Val ~ Year, data = fall)
lines(p2_mod_fa+intercept_fa ~ Year, data = pdata_fa, type = "n")
lines(p2_mod_fa+intercept_fa ~ Year, data = pdata_fa)
lines(unlist(mod_fa.dsig$incr)+intercept_fa ~ Year, data = pdata_fa, col = "blue", lwd = 3)
lines(unlist(mod_fa.dsig$decr)+intercept_fa ~ Year, data = pdata_fa, col = "red", lwd = 3)

linearMod_fa<- lm(Val ~ Year, data=fall)
summary(linearMod_fa)

fall_plot <- ggplot() + 
  geom_line(data = fall, aes(x = Year, y = Val), color = 'grey52') +
  geom_point(data = fall, aes(x = Year, y = Val), color = 'grey52') + 
  geom_smooth(data = fall, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data = pdata_fa, aes(x = Year, y = p2_mod_fa+intercept_fa), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$incr)+intercept_fa, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$decr)+intercept_fa, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = 'Year', title = 'Autumn') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Now plot all 4 together
library(ggpubr)

ggarrange(wint_plot,spr_plot,sum_plot,fall_plot,nrow=2,ncol=2)

