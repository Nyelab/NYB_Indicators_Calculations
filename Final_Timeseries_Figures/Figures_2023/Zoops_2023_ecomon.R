## Figures for Zooplankton

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: June 6, 2024**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(mgcViz)
library(ggplot2)

#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
zoops<-read.csv("Zoops_2023nyb.csv", header = TRUE)

###### C. Finmarchicus

cf = zoops[zoops$Variable == 'calfin_100m3',]
cf_nyb = cf[cf$Loc == 'NYB',]
zoops

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qcf_nyb = quantile(cf_nyb$Val, probs = c(.30, .70))

# find the last 5 years mean
mn_cf_nyb5 = mean(cf_nyb$Val[cf_nyb$Year >= 2019])
# what quintile the data is in
mn_cf_nyb5 >qcf_nyb


# Creat a GAM - adjust k and remember to check model
mod<- gam(Val ~ s(Year, k=15), data = cf_nyb)
summary(mod) #check out model
gam.check(mod)

pdata <- with(cf_nyb, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = -0.1476779   # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=44) # n is the number of years
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
  geom_point(data = cf_nyb[43:44,], aes(x = Year, y = Val), shape = 17, size = 3) + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Abundance Anomaly"), x = 'Year', title = 'Calanus finmarchicus') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

###### C. Typicus

ct = zoops[zoops$Variable == 'ctyp_100m3',]
ct_nyb = ct[ct$Loc == 'NYB',]
zoops

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qct_nyb = quantile(ct_nyb$Val, probs = c(.30, .70))

# find the last 5 years mean
mn_ct_nyb5 = mean(ct_nyb$Val[ct_nyb$Year >= 2019])
# what quintile the data is in
mn_ct_nyb5 >qct_nyb


# Creat a GAM - adjust k and remember to check model
mod<- gam(Val ~ s(Year, k=15), data = ct_nyb)
summary(mod) #check out model
gam.check(mod)

pdata <- with(ct_nyb, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 0.1152717   # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=44) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = ct_nyb)
lines(Val ~ Year, data = ct_nyb)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

ggplot() + 
  geom_line(data = ct_nyb, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = ct_nyb, aes(x = Year, y = Val), color = 'grey53') + 
  #geom_smooth(data = cf_nyb, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_point(data = ct_nyb[43:44,], aes(x = Year, y = Val), shape = 17, size = 3) + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Abundance Anomaly"), x = 'Year', title = 'Centropages typicus') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))



##### Small to Large Ratio

sl = zoops[zoops$Variable == 'sm_lg',]
sl_nyb = sl[sl$Loc == 'NYB',]

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qsl_nyb = quantile(sl_nyb$Val, probs = c(.30, .70))

# find the last 5 years mean
mn_sl_nyb5 = mean(sl_nyb$Val[sl_nyb$Year >= 2019])
# what quintile the data is in
mn_sl_nyb5 >qsl_nyb

# Creat a GAM - adjust k and remember to check model
mod<- gam(Val ~ s(Year, k=15), data = sl_nyb)
summary(mod) #check out model
gam.check(mod)

pdata <- with(sl_nyb, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 0.1478699  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=44) # n is the number of years
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
  geom_point(data = sl_nyb[43:44,], aes(x = Year, y = Val), shape = 17, size = 3) + 
  geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Abundance Anomaly"), x = 'Year', title = 'Small/Large Copepod Ratio') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

