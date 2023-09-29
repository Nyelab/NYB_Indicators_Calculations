## Figures for Summer Flounder

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: September 28, 2023**

###load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)
library(ggpubr)

#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
ds<-read.csv("summerflounder_spring_2023.csv", header = TRUE)


# Your final time series is (hopefully) a dataframe with a column for the YEAR 
# and a column for whatever the data variable is.  Here I give an example using 
# Hudson river mean flow data where one column is YEAR and the other is flowrate

# Creat a GAM - adjust k and remember to check model
mod<- gam(strat.biomass ~ s(YEAR, k=10), data = ds)
summary(mod) #check out model
gam.check(mod)

pdata <- with(ds, data.frame(YEAR = YEAR))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 1.25431     # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod.d <- Deriv(mod, n=53) # n is the number of YEARs
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = ds)
lines(strat.biomass ~ YEAR, data = ds)
lines(p2_mod+intercept ~ YEAR, data = pdata, type = "n")
lines(p2_mod+intercept ~ YEAR, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ YEAR, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ YEAR, data = pdata, col = "red", lwd = 3)

linearMod<- lm(strat.biomass ~ YEAR, data=ds)
summary(linearMod)

p1 = ggplot() + 
  geom_line(data = ds, aes(x = YEAR, y = strat.biomass), color = 'grey') +
  geom_point(data = ds, aes(x = YEAR, y = strat.biomass), color = 'gray') + 
  #geom_point(data = ds[52:53, ], aes(x = YEAR, y = strat.biomass), shape = 17, size = 3) + 
  geom_smooth(data = ds, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = YEAR, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Biomass"), x = '', title = 'Summer Flounder') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


ds2<-read.csv("summerflounder_fall_2023.csv", header = TRUE)


# Your final time series is (hopefully) a dataframe with a column for the YEAR 
# and a column for whatever the data variable is.  Here I give an example using 
# Hudson river mean flow data where one column is YEAR and the other is flowrate

# Creat a GAM - adjust k and remember to check model
mod2<- gam(strat.biomass ~ s(YEAR, k=10), data = ds2)
summary(mod2) #check out model
gam.check(mod2)

pdata2 <- with(ds2, data.frame(YEAR = YEAR))
p2_mod2 <- predict(mod2, newdata = pdata2,  type = "terms", se.fit = TRUE)
intercept2 = 0.9988367     # look at p2_mod and extract the intercept
pdata2 <- transform(pdata2, p2_mod2 = p2_mod2$fit[,1], se2 = p2_mod2$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod2.d <- Deriv(mod2, n=55) # n is the number of YEARs
mod2.dci <- confint(mod2.d, term = Term)
mod2.dsig <- signifD(pdata2$p2_mod2, d = mod2.d[[Term]]$deriv,
                     +                    mod2.dci[[Term]]$upper, mod2.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = ds2)
lines(strat.biomass ~ YEAR, data = ds2)
lines(p2_mod2+intercept2 ~ YEAR, data = pdata2, type = "n")
lines(p2_mod2+intercept2 ~ YEAR, data = pdata2)
lines(unlist(mod2.dsig$incr)+intercept2 ~ YEAR, data = pdata2, col = "blue", lwd = 3)
lines(unlist(mod2.dsig$decr)+intercept2 ~ YEAR, data = pdata2, col = "red", lwd = 3)

linearmod2<- lm(strat.biomass ~ YEAR, data=ds2)
summary(linearmod2)

p2 = ggplot() + 
  geom_line(data = ds2, aes(x = YEAR, y = strat.biomass), color = 'grey') +
  geom_point(data = ds2, aes(x = YEAR, y = strat.biomass), color = 'gray') + 
  geom_point(data = ds2[55, ], aes(x = YEAR, y = strat.biomass), shape = 17, size = 3) + 
  geom_smooth(data = ds2, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata2, aes(x = YEAR, y = p2_mod2+intercept2), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata2, aes(y = unlist(mod2.dsig$incr)+intercept2, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata2, aes(y = unlist(mod2.dsig$decr)+intercept2, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Biomass"), x = 'Year', title = '') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


ggarrange(p1, p2, 
          labels = c("Spring", " Fall"),
          label.x = c(.05,.05),
          label.y = c(0.88,0.88),
          font.label = (face = 'Italic'),
          ncol = 1, nrow = 2)