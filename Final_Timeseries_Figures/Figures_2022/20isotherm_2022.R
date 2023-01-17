## Figures for 20C Isotherm

## **Laura Gruenburg, lagruenburg@gmail.com**
  
##   **LAST UPDATED: 8/29/2022**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/")
isotherm<-read.csv("Iso20_Aug_26_2022.csv", header = TRUE)

names(isotherm)[names(isotherm) == "X"] <- "Year"

# NO GAM on JJA_lon
# Creat a GAM - adjust k and remember to check model
mod<- gam(JJA_lon ~ s(X, k=10), data = isotherm)
summary(mod) #check out model
gam.check(mod)

pdata <- with(isotherm, data.frame(year = Year))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 324.3931  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod.d <- Deriv(mod, n=41) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(flowrate ~ Year, data = isotherm)
lines(flowrate ~ Year, data = isotherm)
lines(p2_mod+intercept ~ Year, data = pdata, type = "n")
lines(p2_mod+intercept ~ Year, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ year, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ year, data = pdata, col = "red", lwd = 3)

linearMod<- lm(SON_lat ~ X, data=isotherm)
summary(linearMod)

ggplot() + 
  geom_line(data = isotherm, aes(x = Year, y = SON_lat), color = 'grey53') +
  geom_point(data = isotherm, aes(x = Year, y = SON_lat), color = 'gray53') + 
  geom_point(data = isotherm[40,], aes(x = Year, y = SON_lat), shape = 17, size = 3) +
  #geom_smooth(data = isotherm, aes(x = year, y = flowrate), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Latitude"), x = 'Year', title = 'SON Northernmost Latitude of 20\u00B0C Isotherm') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

##
linearMod<- lm(JJA_lon ~ Year, data=isotherm)
summary(linearMod)

ggplot() + 
  geom_line(data = isotherm, aes(x = Year, y = JJA_lon), color = 'grey53') +
  geom_point(data = isotherm, aes(x = Year, y = JJA_lon), color = 'gray53') + 
  geom_point(data = isotherm[40:41,], aes(x = Year, y = JJA_lon), shape = 17, size =3) + 
  geom_smooth(data = isotherm, aes(x = Year, y = JJA_lon), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Longitude"), x = 'Year', title = 'JJA Easternmost Longitude of 20\u00B0C Isotherm') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

ggplot() + 
  #geom_line(data = isotherm, aes(x = JJA_lon, y = JJA_lat), color = 'grey53') +
  geom_point(data = isotherm, aes(x = JJA_lon, y = JJA_lat, color = Year), size = 5) +
  scale_color_gradient2(midpoint = 2001, low="blue", mid = 'white', high="red") +
  geom_point(data = isotherm[40:41,], aes(x = JJA_lon, y = JJA_lat), shape = 17, size = 3) + 
  #geom_smooth(data = isotherm, aes(x = X, y = JJA_lon), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Latitude"), x = 'Longitude', title = 'JJA Easternmost Lon and Lat of 20\u00B0C Isotherm') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), legend.text=element_text(size=11), axis.text= element_text(color = 'black', size = 12))
