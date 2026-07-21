## Figures for Sea Level Rise

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: October 29, 2025**

setwd("~/Desktop/NYB_Indicators_Calculations")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)
library(tidyr)
library(dplyr)

#######Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025")
montauk<-read.csv("Montauk_SeaLevel.csv", header = FALSE, skip = 5)
montauk$V7 = NULL

montauk <- montauk %>%
  rename(Year = V1,
         Month = V2,
         Monthly_MSL = V3,
         Linear_Trend = V4,
         High_Conf = V5,
         Low_Conf = V6)

montauk <- montauk[-1, ]

sandyhook<-read.csv("SandyHook_SeaLevel.csv", header = FALSE, skip = 5)

sandyhook$V7 = NULL

sandyhook <- sandyhook %>%
  rename(Year = V1,
         Month = V2,
         Monthly_MSL = V3,
         Linear_Trend = V4,
         High_Conf = V5,
         Low_Conf = V6)

sandyhook <- sandyhook[-1, ]

montauk_yearly <- montauk %>% group_by(Year) %>%
  summarise(SL = mean(as.numeric(Monthly_MSL)))

sandyhook_yearly <- sandyhook %>% group_by(Year) %>%
  summarise(SL = mean(as.numeric(Monthly_MSL)))

montauk_yearly$Year <- as.numeric(montauk_yearly$Year)
sandyhook_yearly$Year <- as.numeric(sandyhook_yearly$Year)
# Detemining the 30th and 70th percentiles for the short term trends column in the 
# Indicators at a glance table for the indicators report

qsl = quantile(montauk_yearly[montauk_yearly$Year > 1949, ]$SL, probs = c(.30, .70))

# find the last 5 years mean
mn_sl5 = mean(montauk_yearly[montauk_yearly$Year > 1949, ]$SL[montauk_yearly[montauk_yearly$Year > 1949, ]$Year >= 2021])
# what percentile the data is in
mn_sl5 >qsl

qsl_s = quantile(sandyhook_yearly[sandyhook_yearly$Year > 1949, ]$SL, probs = c(.30, .70))

# find the last 5 years mean
mn_sl_s5 = mean(sandyhook_yearly[sandyhook_yearly$Year > 1949, ]$SL[sandyhook_yearly[sandyhook_yearly$Year > 1949, ]$Year >= 2021])
# what percentile the data is in
mn_sl_s5 >qsl_s


linearmod_montauk<- lm(Year ~ SL, data=montauk_yearly[montauk_yearly$Year > 1949, ])
summary(linearmod_montauk)

montauk_plot <- ggplot() + 
  geom_line(data = montauk_yearly[montauk_yearly$Year > 1947, ], aes(x = Year, y = SL), color = 'grey52') +
  geom_point(data = montauk_yearly[montauk_yearly$Year > 1947, ], aes(x = Year, y = SL), color = 'grey52') + 
  geom_point(data = montauk_yearly[79,], aes(x= Year, y = SL), shape = 17, size =3) +
  geom_smooth(data = montauk_yearly[montauk_yearly$Year > 1947, ], aes(x = Year, y = SL), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_w, aes(x = X, y = p2_mod_june+intercept_w), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_june.dsig$incr)+intercept_w, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_june.dsig$decr)+intercept_w, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("meters"), x =' ',title = 'Montauk') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

linearmod_sandyhook<- lm(Year ~ SL, data=sandyhook_yearly[sandyhook_yearly$Year > 1932, ])
summary(linearmod_sandyhook)

sandyhook_plot <- ggplot() + 
  geom_line(data = sandyhook_yearly[sandyhook_yearly$Year > 1932, ], aes(x = Year, y = SL), color = 'grey52') +
  geom_point(data = sandyhook_yearly[sandyhook_yearly$Year > 1932, ], aes(x = Year, y = SL), color = 'grey52') + 
  geom_point(data = sandyhook_yearly[94,], aes(x= Year, y = SL), shape = 17, size =3) +
  geom_smooth(data = sandyhook_yearly[sandyhook_yearly$Year > 1932, ], aes(x = Year, y = SL), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_w, aes(x = X, y = p2_mod_june+intercept_w), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_june.dsig$incr)+intercept_w, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_june.dsig$decr)+intercept_w, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("meters"), x =' ',title = 'Sandy Hook') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


# Now plot all 4 together
library(ggpubr)

ggarrange(montauk_plot,sandyhook_plot,nrow=2,ncol=1)

