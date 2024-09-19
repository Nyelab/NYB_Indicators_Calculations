#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/")
mhw<-read.csv("MarineHeatwaves_Nov_30_2023.csv", header = TRUE)

mhwdays = mhw[mhw$Variable == "OISST_HW_total_days", ]
mhwdays = mhwdays[mhwdays$Loc == "NYB", ]

qmhwdays = quantile(mhwdays$Val, probs = c(0.30, 0.70))

# find the last 5 years mean
mn_mhwdays5 = mean(mhwdays$Val[mhwdays$Year >= 2019])
# what quintile the data is in
mn_mhwdays5 >qmhwdays


# Creat a GAM - adjust k and remember to check model
mod_s<- gam(Val ~ s(Year, k=7), data = mhwdays)
summary(mod_s) #check out model
gam.check(mod_s)

pdata_s <- with(mhwdays, data.frame(Year = Year))
p2_mod_s <- predict(mod_s, newdata = pdata_s,  type = "terms", se.fit = TRUE)
intercept_s = 59.88095  # look at p2_mod and extract the intercept_s
pdata_s <- transform(pdata_s, p2_mod_s = p2_mod_s$fit[,1], se2 = p2_mod_s$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_s.d <- Deriv(mod_s, n=42) # n is the number of years
mod_s.dci <- confint(mod_s.d, term = Term)
mod_s.dsig <- signifD(pdata_s$p2_mod_s, d = mod_s.d[[Term]]$deriv,
                    +                    mod_s.dci[[Term]]$upper, mod_s.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = mhwdays)
lines(Val ~ Year, data = mhwdays)
lines(p2_mod_s+intercept_s ~ Year, data = pdata_s, type = "n")
lines(p2_mod_s+intercept_s ~ Year, data = pdata_s)
lines(unlist(mod_s.dsig$incr)+intercept_s ~ Year, data = pdata_s, col = "blue", lwd = 3)
lines(unlist(mod_s.dsig$decr)+intercept_s ~ Year, data = pdata_s, col = "red", lwd = 3)

linearmod_s<- lm(Val ~ Year, data=mhwdays)
summary(linearmod_s)

surf_plot <- ggplot() + 
  geom_line(data = mhwdays, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = mhwdays, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = mhwdays[42,], aes(x = Year, y = Val), shape = 17, size = 3) +
  geom_smooth(data = mhwdays, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_s, aes(x = Year, y = p2_mod_s+intercept_s), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_s, aes(y = unlist(mod_s.dsig$incr)+intercept_s, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_s, aes(y = unlist(mod_s.dsig$decr)+intercept_s, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Days", x = '', title = 'Surface Marine Heatwave Days') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


##### Bottom

mhwdays2 = mhw[mhw$Variable == "GLORYS12_total_days", ]
mhwdays2 = mhwdays2[mhwdays2$Loc == "NYB", ]

qmhwdays2 = quantile(mhwdays2$Val, probs = c(.30,.70))

# find the last 5 years mean
mn_mhwdays25 = mean(mhwdays2$Val[mhwdays2$Year >= 2019])
# what quintile the data is in
mn_mhwdays25 >qmhwdays2


# Creat a GAM - adjust k and remember to check model
mod2<- gam(Val ~ s(Year, k=12), data = mhwdays2)
summary(mod2) #check out model
gam.check(mod2)

pdata2 <- with(mhwdays2, data.frame(Year = Year))
p2_mod2 <- predict(mod2, newdata = pdata2,  type = "terms", se.fit = TRUE)
intercept2 = 71.74194   # look at p2_mod and extract the intercept
pdata2 <- transform(pdata2, p2_mod2 = p2_mod2$fit[,1], se2 = p2_mod2$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod2.d <- Deriv(mod2, n=31) # n is the number of years
mod2.dci <- confint(mod2.d, term = Term)
mod2.dsig <- signifD(pdata2$p2_mod2, d = mod2.d[[Term]]$deriv,
                     +                    mod2.dci[[Term]]$upper, mod2.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = mhwdays2)
lines(Val ~ Year, data = mhwdays2)
lines(p2_mod2+intercept2 ~ Year, data = pdata2, type = "n")
lines(p2_mod2+intercept2 ~ Year, data = pdata2)
lines(unlist(mod2.dsig$incr)+intercept2 ~ Year, data = pdata2, col = "blue", lwd = 3)
lines(unlist(mod2.dsig$decr)+intercept2 ~ Year, data = pdata2, col = "red", lwd = 3)

linearmod2<- lm(Val ~ Year, data=mhwdays2)
summary(linearmod2)

bottom_plot<- ggplot() + 
  geom_line(data = mhwdays2, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = mhwdays2, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = mhwdays2[29:31,], aes(x = Year, y = Val), shape = 17, size = 3) +
  geom_smooth(data = mhwdays2, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata2, aes(x = Year, y = p2_mod2+intercept2), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata2, aes(y = unlist(mod2.dsig$incr)+intercept2, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata2, aes(y = unlist(mod2.dsig$decr)+intercept2, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Days", x = 'Year', title = 'Bottom Marine Heatwave Days') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


ggplot() + 
  geom_line(data = mhwdays2, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = mhwdays2, aes(x = Year, y = Val), color = 'gray53') + 
  geom_smooth(data = mhwdays2, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') +
  geom_line(data = mhwdays, aes(x = Year, y = Val), color = 'pink') +
  geom_point(data = mhwdays, aes(x = Year, y = Val), color = 'pink') + 
  geom_smooth(data = mhwdays, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'red')+
  theme_bw() +
  labs (y = "Number of Days", x = 'Year', title = 'Marine Heatwave Days') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

library(ggpubr)

ggarrange(surf_plot,bottom_plot,nrow=2,ncol=1)

#####
count <- mhw[mhw$Variable == 'OISST_HW_count',]

ggplot() + 
  geom_line(data =count, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = count, aes(x = Year, y = Val), color = 'gray53') + 
  geom_smooth(data = mhwdays2, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') +
  geom_line(data = mhwdays, aes(x = Year, y = Val), color = 'pink') +
  geom_point(data = mhwdays, aes(x = Year, y = Val), color = 'pink') + 
  geom_smooth(data = mhwdays, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'red')+
  theme_bw() +
  labs (y = "Number of Days", x = 'Year', title = 'Marine Heatwave Days') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


surf_mov<-mhw[mhw$Variable == 'OISST_HW_moving_total_days', ]
bot_mov<-mhw[mhw$Variable == 'GLORYS12_moving_total_days', ]
surf_fixed <- mhwdays[mhwdays$Year > 2011, ]
bot_fixed <- mhwdays2[mhwdays2$Year > 2012, ]

surf_plot2 <- ggplot() + 
  geom_line(data = surf_fixed, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = surf_fixed, aes(x = Year, y = Val), color = 'gray53') + 
  geom_line(data = surf_mov, aes(x = Year, y = Val), color = 'orange') +
  geom_point(data = surf_mov, aes(x = Year, y = Val), color = 'orange') + 
  theme_bw() +
  annotate('text', x = 2016.25, y = 175, label = 'Fixed', size = 5, color = 'grey53', fontface = "bold") +
  annotate('text', x = 2016, y = 80, label = 'Moving', size = 5, color = 'orange',fontface = "bold") +
  labs (y = "Number of Days", x = ' ', title = 'Surface Marine Heatwave Days') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


bottom_plot2 <- ggplot() + 
  geom_line(data = bot_fixed, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = bot_fixed, aes(x = Year, y = Val), color = 'gray53') + 
  geom_line(data = bot_mov, aes(x = Year, y = Val), color = 'orange') +
  geom_point(data = bot_mov, aes(x = Year, y = Val), color = 'orange') + 
  theme_bw() +
  annotate('text', x = 2016.5, y = 130, label = 'Fixed', size = 5, color = 'grey53', fontface = "bold") +
  annotate('text', x = 2015.25, y = 50, label = 'Moving', size = 5, color = 'orange',fontface = "bold") +
  labs (y = "Number of Days", x = 'Year', title = 'Bottom Marine Heatwave Days') + 
  scale_x_continuous(breaks =c(2013,2016,2019,2022), labels =c(2013,2016,2019,2022)) +
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

ggarrange(surf_plot2, bottom_plot2, nrow = 2, ncol = 1)

