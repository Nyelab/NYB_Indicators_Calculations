#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/Final_timeseries")
ws<-read.csv("Mean_seasonal_wind_stress_JAN_04_2022.csv", header = TRUE)

wsDJF = ws[ws$seas == 'DJF', ]
wsMAM = ws[ws$seas == 'MAM', ]
wsJJA = ws[ws$seas == 'JJA', ]
wsSON = ws[ws$seas == 'SON', ]


# Creat a GAM - adjust k and remember to check model
mod<- gam(wind_stress ~ s(year, k=5), data = wsDJF)
summary(mod) #check out model
gam.check(mod)

pdata <- with(wsDJF, data.frame(year = year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 0.1504964  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod.d <- Deriv(mod, n=36) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(wind_stress ~ year, data = wsDJF)
lines(wind_stress ~ year, data = wsDJF)
lines(p2_mod+intercept ~ year, data = pdata, type = "n")
lines(p2_mod+intercept ~ year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(wind_stress ~ year, data=wsDJF)
summary(linearMod)

wint_plot <- ggplot() + 
  geom_line(data = wsDJF, aes(x = year, y = wind_stress), color = 'grey53') +
  geom_point(data = wsDJF, aes(x = year, y = wind_stress), color = 'gray53') + 
  #geom_smooth(data = meanflow, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Wind Stress N"~m^-2~" "), x = "", title = 'Winter') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Creat a GAM - adjust k and remember to check model
mods<- gam(wind_stress ~ s(year, k=15), data = wsMAM)
summary(mods) #check out model
gam.check(mods)

pdatas <- with(wsMAM, data.frame(year = year))
p2_mods <- predict(mods, newdata = pdatas,  type = "terms", se.fit = TRUE)
intercepts = 0.0918002  # look at p2_mod and extract the intercept
pdatas <- transform(pdatas, p2_mods = p2_mods$fit[,1], se2s = p2_mods$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mods.d <- Deriv(mods, n=34) # n is the number of years
mods.dci <- confint(mods.d, term = Term)
mods.dsig <- signifD(pdatas$p2_mods, d = mods.d[[Term]]$deriv,
                    +                    mods.dci[[Term]]$upper, mods.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(wind_stress ~ year, data = wsMAM)
lines(wind_stress ~ year, data = wsMAM)
lines(p2_mods+intercepts ~ year, data = pdatas, type = "n")
lines(p2_mods+intercepts ~ year, data = pdatas)
lines(unlist(mods.dsig$incr)+intercepts ~ year, data = pdatas, col = "blue", lwd = 3)
lines(unlist(mods.dsig$decr)+intercepts ~ year, data = pdatas, col = "red", lwd = 3)

linearMods<- lm(wind_stress ~ year, data=wsMAM)
summary(linearMods)

spr_plot <- ggplot() + 
  geom_line(data = wsMAM, aes(x = year, y = wind_stress), color = 'grey53') +
  geom_point(data = wsMAM, aes(x = year, y = wind_stress), color = 'gray53') + 
  geom_smooth(data = wsMAM, aes(x = year, y = wind_stress), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y= "", x= "", title = 'Spring') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Creat a GAM - adjust k and remember to check model
modsu<- gam(wind_stress ~ s(year, k=5), data = wsJJA)
summary(modsu) #check out model
gam.check(modsu)

pdatasu <- with(wsJJA, data.frame(year = year))
p2_modsu <- predict(modsu, newdata = pdatasu,  type = "terms", se.fit = TRUE)
interceptsu = 0.05866281  # look at p2_mod and extract the intercept
pdatasu <- transform(pdatasu, p2_modsu = p2_modsu$fit[,1], se2su = p2_modsu$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
modsu.d <- Deriv(modsu, n=34) # n is the number of years
modsu.dci <- confint(modsu.d, term = Term)
modsu.dsig <- signifD(pdatasu$p2_modsu, d = modsu.d[[Term]]$deriv,
                     +                    modsu.dci[[Term]]$upper, modsu.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(wind_stress ~ year, data = wsJJA)
lines(wind_stress ~ year, data = wsJJA)
lines(p2_modsu+interceptsu ~ year, data = pdatasu, type = "n")
lines(p2_modsu+interceptsu ~ year, data = pdatasu)
lines(unlist(modsu.dsig$incr)+interceptsu ~ year, data = pdatasu, col = "blue", lwd = 3)
lines(unlist(modsu.dsig$decr)+interceptsu ~ year, data = pdatasu, col = "red", lwd = 3)

linearModsu<- lm(wind_stress ~ year, data=wsJJA)
summary(linearModsu)

sum_plot <- ggplot() + 
  geom_line(data = wsJJA, aes(x = year, y = wind_stress), color = 'grey53') +
  geom_point(data = wsJJA, aes(x = year, y = wind_stress), color = 'gray53') + 
  #geom_smooth(data = wsMAM, aes(x = year, y = wind_stress), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Wind Stress N"~m^-2~" "), x = 'Year', title = 'Summer') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Creat a GAM - adjust k and remember to check model
wsSON2 = wsSON[wsSON$X>23,]
modf<- gam(wind_stress ~ s(year, k=15), data = wsSON2)
summary(modf) #check out model
gam.check(modf)

pdataf <- with(wsSON2, data.frame(year = year))
p2_modf <- predict(modf, newdata = pdataf,  type = "terms", se.fit = TRUE)
interceptf = 0.1113182   # look at p2_mod and extract the intercept
pdataf <- transform(pdataf, p2_modf = p2_modf$fit[,1], se2f = p2_modf$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
modf.d <- Deriv(modf, n=28) # n is the number of years
modf.dci <- confint(modf.d, term = Term)
modf.dsig <- signifD(pdataf$p2_modf, d = modf.d[[Term]]$deriv,
                      +                    modf.dci[[Term]]$upper, modf.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(wind_stress ~ year, data = wsSON2)
lines(wind_stress ~ year, data = wsSON2)
lines(p2_modf+interceptf ~ year, data = pdataf, type = "n")
lines(p2_modf+interceptf ~ year, data = pdataf)
lines(unlist(modf.dsig$incr)+interceptf ~ year, data = pdataf, col = "blue", lwd = 3)
lines(unlist(modf.dsig$decr)+interceptf ~ year, data = pdataf, col = "red", lwd = 3)

linearModf<- lm(wind_stress ~ year, data=wsSON2)
summary(linearModf)

fall_plot <- ggplot() + 
  geom_line(data = wsSON, aes(x = year, y = wind_stress), color = 'grey53') +
  geom_point(data = wsSON, aes(x = year, y = wind_stress), color = 'gray53') + 
  geom_smooth(data = wsSON2, aes(x = year, y = wind_stress), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdataf, aes(x = year, y = p2_modf+interceptf), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdataf, aes(y = unlist(modf.dsig$incr)+interceptf, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdataf, aes(y = unlist(modf.dsig$decr)+interceptf, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "", x = 'Year', title = 'Autumn') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

library(ggpubr)

ggarrange(wint_plot,spr_plot,sum_plot,fall_plot,nrow=2,ncol=2)
