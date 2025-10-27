## Figures for total_teus of cargo

## **Laura Gruenburg, lagruenburg@gmail.com**

##   **LAST UPDATED: 2025 Report**


#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB_Indicators_Calculations")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)
library(ggpubr)

#######Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025")
ds<-read.csv("Vessel_Density2025.csv", header = TRUE)

ds$total_total_teus<-as.numeric(gsub(",","",ds$total_total_teus))#remove commas, make numeric
ds$year<-as.numeric(gsub(",","",ds$year))#remove commas, make numeric

qtotal_teus = quantile(ds$total_total_teus, probs = c(.30, .70))

# find the last 5 years mean
mn_total_teus5 = mean(ds$total_total_teus[ds$year >= 2020])
# what percentile the data is in
mn_total_teus5 >qtotal_teus

# Creat a GAM - adjust k and remember to check model
mod<- gam(total_teus ~ s(year, k=5), data = ds)
summary(mod) #check out model
gam.check(mod)

pdata <- with(ds, data.frame(year = year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 6172791   # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod.d <- Deriv(mod, n=19) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(total_teus ~ year, data = ds)
lines(total_teus ~ year, data = ds)
lines(p2_mod+intercept ~ year, data = pdata, type = "n")
lines(p2_mod+intercept ~ year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(total_teus ~ year, data=ds)
summary(linearMod)

p1 = ggplot() + 
  geom_line(data = ds, aes(x = year, y = total_teus), color = 'grey') +
  geom_point(data = ds, aes(x = year, y = total_teus), color = 'gray') + 
  geom_point(data = ds[1, ], aes(x = year, y = total_teus), shape = 17, size = 3) + 
  geom_smooth(data = ds, aes(x = year, y = total_teus), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Total TEUs"), x = '', title = 'Cargo in Port of NY/NJ') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


ggplot() + 
  geom_line(data = ds, aes(x = year, y = medium_boats_0_4999), color = 'pink') +
  geom_point(data = ds, aes(x = year, y = medium_boats_0_4999), color = 'pink') +
  geom_line(data = ds, aes(x = year, y = big_boats_5000_7999), color = 'red') +
  geom_point(data = ds, aes(x = year, y = big_boats_5000_7999), color = 'red') +
  geom_line(data = ds, aes(x = year, y = very_very_big_boats_8000_16999), color = 'orange') + 
  geom_point(data = ds, aes(x = year, y = very_very_big_boats_8000_16999), color = 'orange') + 
  
  geom_point(data = ds[1, ], aes(x = year, y = total_teus), shape = 17, size = 3) + 
  geom_smooth(data = ds, aes(x = year, y = total_teus), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Total TEUs"), x = '', title = 'Cargo in Port of NY/NJ') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
