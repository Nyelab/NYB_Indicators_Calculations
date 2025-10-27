## Figures for Atmospheric CO2

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: October 7, 2024**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB_Indicators_Calculations")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)

## Data can be downloaded at https://scrippsco2.ucsd.edu/data/atmospheric_co2/icecore_merged_products.html

#######Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025")
atm_co2<-read.csv("merged_ice_core_sample_dately_2025.csv", header = TRUE)
atm_co2 <- atm_co2[atm_co2$sample_date >= 1700, ]
qatm_co2 = quantile(atm_co2$co2, probs = c(.30, .70))

# find the last 5 sample_dates mean
mn_atm_co25 = mean(atm_co2$co2[atm_co2$sample_date >= 2019])
# what quintile the data is in
mn_atm_co25 >qatm_co2

# Creat a GAM - adjust k and remember to check model
mod<- gam(co2 ~ s(sample_date, k=15), data = atm_co2)
summary(mod) #check out model
gam.check(mod)

pdata <- with(atm_co2, data.frame(sample_date = sample_date))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 311.8323  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "sample_date"
mod.d <- Deriv(mod, n=270) # n is the number of sample_dates
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(co2 ~ sample_date, data = atm_co2)
lines(co2 ~ sample_date, data = atm_co2)
lines(p2_mod+intercept ~ sample_date, data = pdata, type = "n")
lines(p2_mod+intercept ~ sample_date, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ sample_date, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ sample_date, data = pdata, col = "red", lwd = 3)


ggplot() + 
  geom_line(data = atm_co2, aes(x = sample_date, y = co2), color = 'grey53') +
  geom_point(data = atm_co2, aes(x = sample_date, y = co2), color = 'gray53') + 
  geom_point(data = atm_co2[270, ], aes(x=sample_date, y =co2), color = 'black', shape = 17, size = 3) +
  #geom_smooth(data = atm_co2, aes(x = sample_date, y = co2), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = sample_date, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = sample_date), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = sample_date), color = 'red', size = 1) + 
  theme_bw() +
  xlim(1700, 2024) +
  labs (y = 'ppm' , x = 'Year', title = 'Atmospheric CO2 Concentration') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
  
  