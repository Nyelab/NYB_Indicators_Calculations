#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_timeseries_Figures/Timeseries_Files_2022")
ws1<-read.csv("Mean_seasonal_wind_stress_TT_Nov_15_2022.csv", header = TRUE)
ws2<-read.csv("Mean_seasonal_wind_stress_LI_Nov_15_2022.csv", header = TRUE)
ws3<-read.csv("Mean_seasonal_wind_stress_NY_Nov_15_2022.csv", header = TRUE)

ws1DJF = ws1[ws1$seas == 'DJF', ]
ws1MAM = ws1[ws1$seas == 'MAM', ]
ws1JJA = ws1[ws1$seas == 'JJA', ]
ws1SON = ws1[ws1$seas == 'SON', ]

ws2DJF = ws2[ws2$seas == 'DJF', ]
ws2MAM = ws2[ws2$seas == 'MAM', ]
ws2JJA = ws2[ws2$seas == 'JJA', ]
ws2SON = ws2[ws2$seas == 'SON', ]


ws3DJF = ws3[ws3$seas == 'DJF', ]
ws3MAM = ws3[ws3$seas == 'MAM', ]
ws3JJA = ws3[ws3$seas == 'JJA', ]
ws3SON = ws3[ws3$seas == 'SON', ]

# WINTER
# Creat a GAM - adjust k and remember to check model
modw1<- gam(wind_spd ~ s(year, k=5), data = ws1DJF)
summary(modw1) #check out model
gam.check(modw1)

modw2<- gam(wind_spd ~ s(year, k=5), data = ws2DJF)
summary(modw2) #check out model
gam.check(modw2)

modw3<- gam(wind_spd ~ s(year, k=5), data = ws3DJF)
summary(modw3) #check out model
gam.check(modw3)


pdataw1 <- with(ws1DJF, data.frame(year = year))
p2_modw1 <- predict(modw1, newdata = pdataw1,  type = "terms", se.fit = TRUE)
interceptw1 = 8.011928  # look at p2_mod and extract the intercept
pdataw1 <- transform(pdataw1, p2_modw1 = p2_modw1$fit[,1], se2w1 = p2_modw1$se.fit[,1])

pdataw2 <- with(ws2DJF, data.frame(year = year))
p2_modw2 <- predict(modw2, newdata = pdataw2,  type = "terms", se.fit = TRUE)
interceptw2 = 8.487086  # look at p2_mod and extract the intercept
pdataw2 <- transform(pdataw2, p2_modw2 = p2_modw2$fit[,1], se2w2 = p2_modw2$se.fit[,1])

pdataw3 <- with(ws3DJF, data.frame(year = year))
p2_modw3 <- predict(modw3, newdata = pdataw3,  type = "terms", se.fit = TRUE)
interceptw3 = 7.698009  # look at p2_mod and extract the intercept
pdataw3 <- transform(pdataw3, p2_modw3 = p2_modw3$fit[,1], se2w3 = p2_modw3$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
modw1.d <- Deriv(modw1, n=38) # n is the number of years
modw1.dci <- confint(modw1.d, term = Term)
modw1.dsig <- signifD(pdataw1$p2_modw1, d = modw1.d[[Term]]$deriv,
                    +                    modw1.dci[[Term]]$upper, modw1.dci[[Term]]$lower)

Term = "year"
modw2.d <- Deriv(modw2, n=14) # n is the number of years
modw2.dci <- confint(modw2.d, term = Term)
modw2.dsig <- signifD(pdataw2$p2_modw2, d = modw2.d[[Term]]$deriv,
                     +                    modw2.dci[[Term]]$upper, modw2.dci[[Term]]$lower)

Term = "year"
modw3.d <- Deriv(modw3, n=15) # n is the number of years
modw3.dci <- confint(modw3.d, term = Term)
modw3.dsig <- signifD(pdataw3$p2_modw3, d = modw3.d[[Term]]$deriv,
                     +                    modw3.dci[[Term]]$upper, modw3.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(wind_spd ~ year, data = ws1DJF)
lines(wind_spd ~ year, data = ws1DJF)
lines(p2_modw1+interceptw1 ~ year, data = pdataw1, type = "n")
lines(p2_modw1+interceptw1 ~ year, data = pdataw1)
lines(unlist(modw1.dsig$incr)+interceptw1 ~ year, data = pdataw1, col = "blue", lwd = 3)
lines(unlist(modw1.dsig$decr)+interceptw1 ~ year, data = pdataw1, col = "red", lwd = 3)

linearModw1<- lm(wind_spd ~ year, data=ws1DJF)
summary(linearModw1)

plot(wind_spd ~ year, data = ws2DJF)
lines(wind_spd ~ year, data = ws2DJF)
lines(p2_modw2+interceptw2 ~ year, data = pdataw2, type = "n")
lines(p2_modw2+interceptw2 ~ year, data = pdataw2)
lines(unlist(modw2.dsig$incr)+interceptw2 ~ year, data = pdataw2, col = "blue", lwd = 3)
lines(unlist(modw2.dsig$decr)+interceptw2 ~ year, data = pdataw2, col = "red", lwd = 3)

linearModw2<- lm(wind_spd ~ year, data=ws2DJF)
summary(linearModw2)

plot(wind_spd ~ year, data = ws3DJF)
lines(wind_spd ~ year, data = ws3DJF)
lines(p2_modw3+interceptw3 ~ year, data = pdataw3, type = "n")
lines(p2_modw3+interceptw3 ~ year, data = pdataw3)
lines(unlist(modw3.dsig$incr)+interceptw3 ~ year, data = pdataw3, col = "blue", lwd = 3)
lines(unlist(modw3.dsig$decr)+interceptw3 ~ year, data = pdataw3, col = "red", lwd = 3)

linearModw3<- lm(wind_spd ~ year, data=ws3DJF)
summary(linearModw3)

wint_plot_TT <- ggplot() + 
  geom_line(data = ws1DJF, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws1DJF, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws1DJF[37:38,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  ylim(0.5, 11) + 
  xlim(1975, 2022.25) +
  theme_bw() +
  labs ( y = "", x = "", title = 'Texas Tower') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

wint_plot_LI <- ggplot()+
  geom_line(data = ws2DJF, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws2DJF, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws2DJF[13:14,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  geom_smooth(data = ws2DJF, aes(x = year, y = wind_spd), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdataw2, aes(x = year, y = p2_modw2+interceptw2), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdataw2, aes(y = unlist(modw2.dsig$incr)+interceptw2, x = year), color = "blue", size = 1) + 
  geom_line(data = pdataw2, aes(y = unlist(modw2.dsig$decr)+interceptw2, x = year), color = 'red', size = 1) + 
  ylim(0.5, 11) +
  xlim(2009, 2022.25) +
  theme_bw() +
  labs ( y = "", x = "", title = 'Long Island') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

wint_plot_NY <- ggplot()+
  geom_line(data = ws3DJF, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws3DJF, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws3DJF[14:15,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  ylim(0.5, 11) +
  xlim(2007.75, 2022.25) +
  #geom_smooth(data = meanflow, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Winter (m/s)"), x = "", title = 'NY Harbor') + 
  theme(axis.title.y=element_text(size=12),plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

library(ggpubr)

winterplt<- ggarrange(wint_plot_NY,wint_plot_LI,wint_plot_TT,nrow=1,ncol=3)

# SPRING
# Creat a GAM - adjust k and remember to check model

mods1<- gam(wind_spd ~ s(year, k=5), data = ws1MAM)
summary(mods1) #check out model
gam.check(mods1)

pdatas1 <- with(ws1MAM, data.frame(year = year))
p2_mods1 <- predict(mods1, newdata = pdatas1,  type = "terms", se.fit = TRUE)
intercepts1 = 6.178322  # look at p2_mod and extract the intercept
pdatas1 <- transform(pdatas1, p2_mods1 = p2_mods1$fit[,1], se2s1 = p2_mods1$se.fit[,1])

Term = "year"
mods1.d <- Deriv(mods1, n=36) # n is the number of years
mods1.dci <- confint(mods1.d, term = Term)
mods1.dsig <- signifD(pdatas1$p2_mods1, d = mods1.d[[Term]]$deriv,
                      +                    mods1.dci[[Term]]$upper, mods1.dci[[Term]]$lower)

plot(wind_spd ~ year, data = ws1MAM)
lines(wind_spd ~ year, data = ws1MAM)
lines(p2_mods1+intercepts1 ~ year, data = pdatas1, type = "n")
lines(p2_mods1+intercepts1 ~ year, data = pdatas1)
lines(unlist(mods1.dsig$incr)+intercepts1 ~ year, data = pdatas1, col = "blue", lwd = 3)
lines(unlist(mods1.dsig$decr)+intercepts1 ~ year, data = pdatas1, col = "red", lwd = 3)

linearMods1<- lm(wind_spd ~ year, data=ws1MAM)#[6:36,])
summary(linearMods1)#significant

##

mods2<- gam(wind_spd ~ s(year, k=5), data = ws2MAM)
summary(mods2) #check out model
gam.check(mods2)

pdatas2 <- with(ws2MAM, data.frame(year = year))
p2_mods2 <- predict(mods2, newdata = pdatas2,  type = "terms", se.fit = TRUE)
intercepts2 = 7.536037  # look at p2_mod and extract the intercept
pdatas2 <- transform(pdatas2, p2_mods2 = p2_mods2$fit[,1], se2s2 = p2_mods2$se.fit[,1])

Term = "year"
mods2.d <- Deriv(mods2, n=8) # n is the number of years
mods2.dci <- confint(mods2.d, term = Term)
mods2.dsig <- signifD(pdatas2$p2_mods2, d = mods2.d[[Term]]$deriv,
                      +                    mods2.dci[[Term]]$upper, mods2.dci[[Term]]$lower)

plot(wind_spd ~ year, data = ws2MAM)
lines(wind_spd ~ year, data = ws2MAM)
lines(p2_mods2+intercepts2 ~ year, data = pdatas2, type = "n")
lines(p2_mods2+intercepts2 ~ year, data = pdatas2)
lines(unlist(mods2.dsig$incr)+intercepts2 ~ year, data = pdatas2, col = "blue", lwd = 3)
lines(unlist(mods2.dsig$decr)+intercepts2 ~ year, data = pdatas2, col = "red", lwd = 3)

linearMods2<- lm(wind_spd ~ year, data=ws2MAM)
summary(linearMods2)
#none significant
##
mods3<- gam(wind_spd ~ s(year, k=5), data = ws3MAM)
summary(mods3) #check out model
gam.check(mods3)

pdatas3 <- with(ws3MAM, data.frame(year = year))
p2_mods3 <- predict(mods3, newdata = pdatas3,  type = "terms", se.fit = TRUE)
intercepts3 = 6.123456  # look at p2_mod and extract the intercept
pdatas3 <- transform(pdatas3, p2_mods3 = p2_mods3$fit[,1], se2s3 = p2_mods3$se.fit[,1])

Term = "year"
mods3.d <- Deriv(mods3, n=14) # n is the number of years
mods3.dci <- confint(mods3.d, term = Term)
mods3.dsig <- signifD(pdatas3$p2_mods3, d = mods3.d[[Term]]$deriv,
                      +                    mods3.dci[[Term]]$upper, mods3.dci[[Term]]$lower)

plot(wind_spd ~ year, data = ws3MAM)
lines(wind_spd ~ year, data = ws3MAM)
lines(p2_mods3+intercepts3 ~ year, data = pdatas3, type = "n")
lines(p2_mods3+intercepts3 ~ year, data = pdatas3)
lines(unlist(mods3.dsig$incr)+intercepts3 ~ year, data = pdatas3, col = "blue", lwd = 3)
lines(unlist(mods3.dsig$decr)+intercepts3 ~ year, data = pdatas3, col = "red", lwd = 3)

linearMods3<- lm(wind_spd ~ year, data=ws3MAM)
summary(linearMods3)
## no sig

spr_plot_TT <- ggplot() + 
  geom_line(data = ws1MAM, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws1MAM, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws1MAM[35:36,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  geom_smooth(data = ws1MAM, aes(x = year, y = wind_spd), method = lm, se = FALSE, color = 'black') + 
  ylim(4,12) +
  xlim(1975, 2022.25) +
  theme_bw() +
  labs ( y = "", x = "") + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

spr_plot_LI <- ggplot()+
  geom_line(data = ws2MAM, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws2MAM, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws2MAM[8,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  #geom_smooth(data = ws2MAM, aes(x = year, y = wind_spd), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdataw2, aes(x = year, y = p2_modw2+interceptw2), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdataw2, aes(y = unlist(modw2.dsig$incr)+interceptw2, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdataw2, aes(y = unlist(modw2.dsig$decr)+interceptw2, x = year), color = 'red', size = 1) + 
  ylim(4, 12.5) +
  xlim(2009, 2022.25) +
  theme_bw() +
  labs ( y = "", x = "") + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

spr_plot_NY <- ggplot()+
  geom_line(data = ws3MAM, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws3MAM, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws3MAM[13:14,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  ylim(4, 12.5) +
  xlim(2007.75, 2022.25) +
  #geom_smooth(data = meanflow, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Spring (m/s)"), x = "") + 
  theme(axis.title.y=element_text(size=12),plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

library(ggpubr)

springplt<- ggarrange(spr_plot_NY,spr_plot_LI,spr_plot_TT,nrow=1,ncol=3)
winter_spring_plt<- ggarrange(wint_plot_NY,wint_plot_LI,wint_plot_TT,
                              spr_plot_NY,spr_plot_LI,spr_plot_TT,
                              nrow=2,ncol=3)
#annotate_figure(winter_spring_plt,
#                left = text_grob("Wind Speed (m/s)", rot = 90))
##SUMMER
# Creat a GAM - adjust k and remember to check model

modu1<- gam(wind_spd ~ s(year, k=5), data = ws1JJA)
summary(modu1) #check out model
gam.check(modu1)

pdatau1 <- with(ws1JJA, data.frame(year = year))
p2_modu1 <- predict(modu1, newdata = pdatau1,  type = "terms", se.fit = TRUE)
interceptu1 = 5.166417  # look at p2_mod and extract the intercept
pdatau1 <- transform(pdatau1, p2_modu1 = p2_modu1$fit[,1], se2u1 = p2_modu1$se.fit[,1])

Term = "year"
modu1.d <- Deriv(modu1, n=36) # n is the number of years
modu1.dci <- confint(modu1.d, term = Term)
modu1.dsig <- signifD(pdatau1$p2_modu1, d = modu1.d[[Term]]$deriv,
                      +                    modu1.dci[[Term]]$upper, modu1.dci[[Term]]$lower)

plot(wind_spd ~ year, data = ws1JJA)
lines(wind_spd ~ year, data = ws1JJA)
lines(p2_modu1+interceptu1 ~ year, data = pdatau1, type = "n")
lines(p2_modu1+interceptu1 ~ year, data = pdatau1)
lines(unlist(modu1.dsig$incr)+interceptu1 ~ year, data = pdatau1, col = "blue", lwd = 3)
lines(unlist(modu1.dsig$decr)+interceptu1 ~ year, data = pdatau1, col = "red", lwd = 3)

linearModu1<- lm(wind_spd ~ year, data=ws1JJA[6:36,])
summary(linearModu1)# NO significant

##

modu2<- gam(wind_spd ~ s(year, k=5), data = ws2JJA)
summary(modu2) #check out model
gam.check(modu2)

pdatau2 <- with(ws2JJA, data.frame(year = year))
p2_modu2 <- predict(modu2, newdata = pdatau2,  type = "terms", se.fit = TRUE)
interceptu2 = 5.205361   # look at p2_mod and extract the intercept
pdatau2 <- transform(pdatau2, p2_modu2 = p2_modu2$fit[,1], se2u2 = p2_modu2$se.fit[,1])

Term = "year"
modu2.d <- Deriv(modu2, n=9) # n is the number of years
modu2.dci <- confint(modu2.d, term = Term)
modu2.dsig <- signifD(pdatau2$p2_modu2, d = modu2.d[[Term]]$deriv,
                      +                    modu2.dci[[Term]]$upper, modu2.dci[[Term]]$lower)

plot(wind_spd ~ year, data = ws2JJA)
lines(wind_spd ~ year, data = ws2JJA)
lines(p2_modu2+interceptu2 ~ year, data = pdatau2, type = "n")
lines(p2_modu2+interceptu2 ~ year, data = pdatau2)
lines(unlist(modu2.dsig$incr)+interceptu2 ~ year, data = pdatau2, col = "blue", lwd = 3)
lines(unlist(modu2.dsig$decr)+interceptu2 ~ year, data = pdatau2, col = "red", lwd = 3)

linearModu2<- lm(wind_spd ~ year, data=ws2JJA)
summary(linearModu2)
#none significant
##
modu3<- gam(wind_spd ~ s(year, k=5), data = ws3JJA)
summary(modu3) #check out model
gam.check(modu3)

pdatau3 <- with(ws3JJA, data.frame(year = year))
p2_modu3 <- predict(modu3, newdata = pdatau3,  type = "terms", se.fit = TRUE)
interceptu3 = 4.96411  # look at p2_mod and extract the intercept
pdatau3 <- transform(pdatau3, p2_modu3 = p2_modu3$fit[,1], se2u3 = p2_modu3$se.fit[,1])

Term = "year"
modu3.d <- Deriv(modu3, n=13) # n is the number of years
modu3.dci <- confint(modu3.d, term = Term)
modu3.dsig <- signifD(pdatau3$p2_modu3, d = modu3.d[[Term]]$deriv,
                      +                    modu3.dci[[Term]]$upper, modu3.dci[[Term]]$lower)

plot(wind_spd ~ year, data = ws3JJA)
lines(wind_spd ~ year, data = ws3JJA)
lines(p2_modu3+interceptu3 ~ year, data = pdatau3, type = "n")
lines(p2_modu3+interceptu3 ~ year, data = pdatau3)
lines(unlist(modu3.dsig$incr)+interceptu3 ~ year, data = pdatau3, col = "blue", lwd = 3)
lines(unlist(modu3.dsig$decr)+interceptu3 ~ year, data = pdatau3, col = "red", lwd = 3)

linearModu3<- lm(wind_spd ~ year, data=ws3JJA)
summary(linearModu3)
##  sig in the gam

sum_plot_TT <- ggplot() + 
  geom_line(data = ws1JJA, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws1JJA, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws1JJA[35:36,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  #geom_smooth(data = ws1JJA, aes(x = year, y = wind_spd), method = lm, se = FALSE, color = 'black') + 
  ylim(4.5,8.5) +
  xlim(1975, 2022.25) +
  theme_bw() +
  labs ( y = "", x = "") + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

sum_plot_LI <- ggplot()+
  geom_line(data = ws2JJA, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws2JJA, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws2JJA[8:9,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  #geom_smooth(data = ws2JJA, aes(x = year, y = wind_spd), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdataw2, aes(x = year, y = p2_modw2+interceptw2), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdataw2, aes(y = unlist(modw2.dsig$incr)+interceptw2, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdataw2, aes(y = unlist(modw2.dsig$decr)+interceptw2, x = year), color = 'red', size = 1) + 
  ylim(4.5, 8.5) +
  xlim(2009, 2022.25) +
  theme_bw() +
  labs ( y = "", x = "") + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

sum_plot_NY <- ggplot()+
  geom_line(data = ws3JJA, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws3JJA, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws3JJA[12:13,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  ylim(4.5, 8.5) +
  xlim(2007.75, 2022.25) +
  #geom_smooth(data = meanflow, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdatau3, aes(x = year, y = p2_modu3+interceptu3), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdatau3, aes(y = unlist(modu3.dsig$incr)+interceptu3, x = year), color = "blue", size = 1) + 
  geom_line(data = pdatau3, aes(y = unlist(modu3.dsig$decr)+interceptu3, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Summer (m/s)"), x = "") + 
  theme(axis.title.y=element_text(size=12),plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


summerplt<- ggarrange(sum_plot_NY,sum_plot_LI,sum_plot_TT,nrow=1,ncol=3)
winter_spring_summer_plt<- ggarrange(wint_plot_NY,wint_plot_LI,wint_plot_TT,
                              spr_plot_NY,spr_plot_LI,spr_plot_TT,
                              sum_plot_NY,sum_plot_LI,sum_plot_TT,
                              nrow=3,ncol=3)
### Autumn
# Creat a GAM - adjust k and remember to check model

modf1<- gam(wind_spd ~ s(year, k=5), data = ws1SON)
summary(modf1) #check out model
gam.check(modf1)

pdataf1 <- with(ws1SON, data.frame(year = year))
p2_modf1 <- predict(modf1, newdata = pdataf1,  type = "terms", se.fit = TRUE)
interceptf1 = 6.959987   # look at p2_mod and extract the intercept
pdataf1 <- transform(pdataf1, p2_modf1 = p2_modf1$fit[,1], se2f1 = p2_modf1$se.fit[,1])

Term = "year"
modf1.d <- Deriv(modf1, n=37) # n is the number of years
modf1.dci <- confint(modf1.d, term = Term)
modf1.dsig <- signifD(pdataf1$p2_modf1, d = modf1.d[[Term]]$deriv,
                      +                    modf1.dci[[Term]]$upper, modf1.dci[[Term]]$lower)

plot(wind_spd ~ year, data = ws1SON)
lines(wind_spd ~ year, data = ws1SON)
lines(p2_modf1+interceptf1 ~ year, data = pdataf1, type = "n")
lines(p2_modf1+interceptf1 ~ year, data = pdataf1)
lines(unlist(modf1.dsig$incr)+interceptf1 ~ year, data = pdataf1, col = "blue", lwd = 3)
lines(unlist(modf1.dsig$decr)+interceptf1 ~ year, data = pdataf1, col = "red", lwd = 3)

linearModf1<- lm(wind_spd ~ year, data=ws1SON[7:37,])
summary(linearModf1)# significant both

##

modf2<- gam(wind_spd ~ s(year, k=5), data = ws2SON)
summary(modf2) #check out model
gam.check(modf2)

pdataf2 <- with(ws2SON, data.frame(year = year))
p2_modf2 <- predict(modf2, newdata = pdataf2,  type = "terms", se.fit = TRUE)
interceptf2 = 7.373496   # look at p2_mod and extract the intercept
pdataf2 <- transform(pdataf2, p2_modf2 = p2_modf2$fit[,1], se2f2 = p2_modf2$se.fit[,1])

Term = "year"
modf2.d <- Deriv(modf2, n=11) # n is the number of years
modf2.dci <- confint(modf2.d, term = Term)
modf2.dsig <- signifD(pdataf2$p2_modf2, d = modf2.d[[Term]]$deriv,
                      +                    modf2.dci[[Term]]$upper, modf2.dci[[Term]]$lower)

plot(wind_spd ~ year, data = ws2SON)
lines(wind_spd ~ year, data = ws2SON)
lines(p2_modf2+interceptf2 ~ year, data = pdataf2, type = "n")
lines(p2_modf2+interceptf2 ~ year, data = pdataf2)
lines(unlist(modf2.dsig$incr)+interceptf2 ~ year, data = pdataf2, col = "blue", lwd = 3)
lines(unlist(modf2.dsig$decr)+interceptf2 ~ year, data = pdataf2, col = "red", lwd = 3)

linearModf2<- lm(wind_spd ~ year, data=ws2SON)
summary(linearModf2)

#linear sig

##
modf3<- gam(wind_spd ~ s(year, k=5), data = ws3SON)
summary(modf3) #check out model
gam.check(modf3)

pdataf3 <- with(ws3SON, data.frame(year = year))
p2_modf3 <- predict(modf3, newdata = pdataf3,  type = "terms", se.fit = TRUE)
interceptf3 = 6.89814  # look at p2_mod and extract the intercept
pdataf3 <- transform(pdataf3, p2_modf3 = p2_modf3$fit[,1], se2f3 = p2_modf3$se.fit[,1])

Term = "year"
modf3.d <- Deriv(modf3, n=15) # n is the number of years
modf3.dci <- confint(modf3.d, term = Term)
modf3.dsig <- signifD(pdataf3$p2_modf3, d = modf3.d[[Term]]$deriv,
                      +                    modf3.dci[[Term]]$upper, modf3.dci[[Term]]$lower)

plot(wind_spd ~ year, data = ws3SON)
lines(wind_spd ~ year, data = ws3SON)
lines(p2_modf3+interceptf3 ~ year, data = pdataf3, type = "n")
lines(p2_modf3+interceptf3 ~ year, data = pdataf3)
lines(unlist(modf3.dsig$incr)+interceptf3 ~ year, data = pdataf3, col = "blue", lwd = 3)
lines(unlist(modf3.dsig$decr)+interceptf3 ~ year, data = pdataf3, col = "red", lwd = 3)

linearModf3<- lm(wind_spd ~ year, data=ws3SON)
summary(linearModf3)
##  sig in the linear

aut_plot_TT <- ggplot() + 
  geom_line(data = ws1SON, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws1SON, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws1SON[36:37,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  geom_smooth(data = ws1SON[7:37,], aes(x = year, y = wind_spd), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdataf1, aes(x = year, y = p2_modf1+interceptf1), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdataf1, aes(y = unlist(modf1.dsig$incr)+interceptf1, x = year), color = "blue", size = 1) + 
  geom_line(data = pdataf1, aes(y = unlist(modf1.dsig$decr)+interceptf1, x = year), color = 'red', size = 1) + 
  ylim(5.25,8.75) +
  xlim(1975, 2022.25) +
  theme_bw() +
  labs ( y = "", x = "") + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

aut_plot_LI <- ggplot()+
  geom_line(data = ws2SON, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws2SON, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws2SON[10:11,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  geom_smooth(data = ws2SON, aes(x = year, y = wind_spd), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdataw2, aes(x = year, y = p2_modw2+interceptw2), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdataw2, aes(y = unlist(modw2.dsig$incr)+interceptw2, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdataw2, aes(y = unlist(modw2.dsig$decr)+interceptw2, x = year), color = 'red', size = 1) + 
  ylim(5.25,8.75) +
  xlim(2009, 2022.25) +
  theme_bw() +
  labs ( y = "", x = "") + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

aut_plot_NY <- ggplot()+
  geom_line(data = ws3SON, aes(x = year, y = wind_spd), color = 'grey58') +
  geom_point(data = ws3SON, aes(x = year, y = wind_spd), color = 'grey58') + 
  geom_point(data = ws3SON[14:15,], aes(x = year, y = wind_spd), shape = 17, size = 3) +
  ylim(5.25, 8.75) +
  xlim(2007.75, 2022.25) +
  geom_smooth(data = ws3SON, aes(x = year, y = wind_spd), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdataf3, aes(x = year, y = p2_modf3+interceptf3), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdataf3, aes(y = unlist(modf3.dsig$incr)+interceptf3, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdataf3, aes(y = unlist(modf3.dsig$decr)+interceptf3, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Autmn (m/s)"), x = "") + 
  theme(axis.title.y=element_text(size=12),plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


autumnplt<- ggarrange(aut_plot_NY,aut_plot_LI,aut_plot_TT,nrow=1,ncol=3)
winter_spring_summer_autumnplt<- ggarrange(wint_plot_NY,wint_plot_LI,wint_plot_TT,
                                     spr_plot_NY,spr_plot_LI,spr_plot_TT,
                                     sum_plot_NY,sum_plot_LI,sum_plot_TT,
                                     aut_plot_NY,aut_plot_LI,aut_plot_TT,
                                     nrow=4,ncol=3)