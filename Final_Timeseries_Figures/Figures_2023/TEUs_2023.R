## Figures for TEUs of cargo

## **Laura Gruenburg, lagruenburg@gmail.com**

##   **LAST UPDATED: 2023 Report**


#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)
library(ggpubr)

#######Load the datasets
setwd("/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
ds<-read.csv("TEUs_2023.csv", header = TRUE)

qTEUs = quantile(ds[ds$Year > 1949, ]$TEUs, probs = c(.30, .70))

# find the last 5 years mean
mn_TEUs5 = mean(ds[ds$Year > 1949, ]$TEUs[ds[ds$Year > 1949, ]$Year >= 2019])
# what percentile the data is in
mn_TEUs5 >qTEUs

# Creat a GAM - adjust k and remember to check model
mod<- gam(TEUs ~ s(Year, k=5), data = ds)
summary(mod) #check out model
gam.check(mod)

pdata <- with(ds, data.frame(Year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 5961455   # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=18) # n is the number of Years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(TEUs ~ Year, data = ds)
lines(TEUs ~ Year, data = ds)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ Year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ Year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(TEUs ~ Year, data=ds)
summary(linearMod)

p1 = ggplot() + 
  geom_line(data = ds, aes(x = Year, y = TEUs), color = 'grey') +
  geom_point(data = ds, aes(x = Year, y = TEUs), color = 'gray') + 
  #geom_point(data = ds[54:55, ], aes(x = Year, y = TEUs), shape = 17, size = 3) + 
  geom_smooth(data = ds, aes(x = Year, y = TEUs), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = Year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = Year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("TEUs"), x = '', title = 'Cargo in Port of NY/NJ') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
