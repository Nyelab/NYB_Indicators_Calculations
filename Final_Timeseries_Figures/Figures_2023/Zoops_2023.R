## Figures for Zooplankton

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: September 16, 2022**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(mgcViz)
library(ggplot2)

#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
zoops<-read.csv("Zoops_2023.csv", header = TRUE)

cf = zoops[zoops$Variable == 'calfin_100m3',]
cf_nyb = cf[cf$Loc == 'NYB',]

# Creat a GAM - adjust k and remember to check model
mod<- gam(Val ~ s(Year, k=15), data = cf_nyb)
summary(mod) #check out model
gam.check(mod)

pdata <- with(cf_nyb, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = -0.1720754  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=42) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                     +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = cf_nyb)
lines(Val ~ Year, data = cf_nyb)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

ggplot() + 
  geom_line(data = cf_nyb, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = cf_nyb, aes(x = Year, y = Val), color = 'grey53') + 
  #geom_smooth(data = cf_nyb, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_point(data = cf_nyb[41:42,], aes(x = Year, y = Val), shape = 17, size = 3) + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Abundance Anomaly"), x = 'Year', title = 'Calanus finmarchicus') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


sl = zoops[zoops$Variable == 'sm_lg',]
sl_nyb = sl[sl$Loc == 'NYB',]

# Creat a GAM - adjust k and remember to check model
mod<- gam(Val ~ s(Year, k=15), data = sl_nyb)
summary(mod) #check out model
gam.check(mod)

pdata <- with(sl_nyb, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 0.1829262  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=42) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = sl_nyb)
lines(Val ~ Year, data = sl_nyb)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

ggplot() + 
  geom_line(data = sl_nyb, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = sl_nyb, aes(x = Year, y = Val), color = 'grey53') + 
  #geom_smooth(data = sl_nyb, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_point(data = sl_nyb[41:42,], aes(x = Year, y = Val), shape = 17, size = 3) + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Abundance Anomaly"), x = 'Year', title = 'Small/Large Copepod Ratio') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

