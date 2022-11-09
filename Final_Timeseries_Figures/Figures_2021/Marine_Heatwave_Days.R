#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/")
mhw<-read.csv("MarineHeatwaves_Sep_30_2022.csv", header = TRUE)

mhwdays = mhw[mhw$Variable == "OISST_HW_total_days", ]
mhwdays = mhwdays[mhwdays$Loc == "NYB", ]

# Creat a GAM - adjust k and remember to check model
mod<- gam(Val ~ s(Year, k=12), data = mhwdays)
summary(mod) #check out model
gam.check(mod)

pdata <- with(mhwdays, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 55.85366  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=41) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = mhwdays)
lines(Val ~ Year, data = mhwdays)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(Val ~ Year, data=mhwdays)
summary(linearMod)

ggplot() + 
  geom_line(data = mhwdays, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = mhwdays, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = mhwdays[40:41,], aes(x = Year, y = Val), shape = 17, size = 3) +
  geom_smooth(data = mhwdays, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Days", x = 'Year', title = 'Surface Marine Heatwave Days') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


##### Bottom
mhw2<-read.csv("Bottom_MarineHeatwaves_Sep_30_2022.csv", header = TRUE)

mhwdays2 = mhw2[mhw2$Variable == "GLORYS12_total_days", ]
mhwdays2 = mhwdays2[mhwdays2$Loc == "NYB", ]

# Creat a GAM - adjust k and remember to check model
mod2<- gam(Val ~ s(Year, k=12), data = mhwdays2)
summary(mod2) #check out model
gam.check(mod2)

pdata2 <- with(mhwdays2, data.frame(Year = Year))
p2_mod2 <- predict(mod2, newdata = pdata2,  type = "terms", se.fit = TRUE)
intercept2 = 60.71429  # look at p2_mod and extract the intercept
pdata2 <- transform(pdata2, p2_mod2 = p2_mod2$fit[,1], se2 = p2_mod2$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod2.d <- Deriv(mod2, n=28) # n is the number of years
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

ggplot() + 
  geom_line(data = mhwdays2, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = mhwdays2, aes(x = Year, y = Val), color = 'gray53') + 
  geom_smooth(data = mhwdays2, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata2, aes(x = Year, y = p2_mod2+intercept2), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata2, aes(y = unlist(mod2.dsig$incr)+intercept2, x = Year), color = "blue", size = 1) + 
  #geom_line(data = pdata2, aes(y = unlist(mod2.dsig$decr)+intercept2, x = Year), color = 'red', size = 1) + 
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

