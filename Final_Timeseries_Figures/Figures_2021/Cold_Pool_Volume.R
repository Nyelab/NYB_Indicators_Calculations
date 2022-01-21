#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
#library(mgcViz)
library(ggplot2)

#######Load the datasets
setwd("~/Desktop/NYB Indicators/Final_timeseries")
CP<-read.csv("CP_volume_Dec_6_2021.csv", header = TRUE)


# Your final time series is (hopefully) a dataframe with a column for the year 
# and a column for whatever the data variable is.  Here I give an example using 
# Hudson river mean flow data where one column is year and the other is flowrate

# Creat a GAM - adjust k and remember to check model
mod<- gam(April ~ s(X, k=5), data = CP)
summary(mod) #check out model
gam.check(mod)

pdata <- with(CP, data.frame(X = X))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 6.183867e+12 # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod.d <- Deriv(mod, n=27) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(April ~ X, data = CP)
lines(April ~ X, data = CP)
lines(p2_mod+intercept ~ X, data = pdata, type = "n")
lines(p2_mod+intercept ~ X, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ X, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ X, data = pdata, col = "red", lwd = 3)

linearMod<- lm(April ~ X, data=CP)
summary(linearMod)

April_plot <- ggplot() + 
  geom_line(data = CP, aes(x = X, y = April), color = 'grey53') +
  geom_point(data = CP, aes(x = X, y = April), color = 'gray53') + 
  #geom_smooth(data = CP, aes(x = X, y = April), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~m^3~""), x = 'Year', title = 'April') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# JULY Creat a GAM - adjust k and remember to check model
mod<- gam(July ~ s(X, k=5), data = CP)
summary(mod) #check out model
gam.check(mod)

pdata <- with(CP, data.frame(X = X))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 828264073739  # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod.d <- Deriv(mod, n=27) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(July ~ X, data = CP)
lines(July ~ X, data = CP)
lines(p2_mod+intercept ~ X, data = pdata, type = "n")
lines(p2_mod+intercept ~ X, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ X, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ X, data = pdata, col = "red", lwd = 3)

linearMod<- lm(July ~ X, data=CP)
summary(linearMod)

July_plot<- ggplot() + 
  geom_line(data = CP, aes(x = X, y = July), color = 'grey53') +
  geom_point(data = CP, aes(x = X, y = July), color = 'gray53') + 
  #geom_smooth(data = CP, aes(x = X, y = April), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~m^3~ ""), x = 'Year', title = 'July') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# October Creat a GAM - adjust k and remember to check model
mod<- gam(October ~ s(X, k=5), data = CP)
summary(mod) #check out model
gam.check(mod)

pdata <- with(CP, data.frame(X = X))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 79364088943   # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod.d <- Deriv(mod, n=27) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(October ~ X, data = CP)
lines(October ~ X, data = CP)
lines(p2_mod+intercept ~ X, data = pdata, type = "n")
lines(p2_mod+intercept ~ X, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ X, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ X, data = pdata, col = "red", lwd = 3)

linearMod<- lm(October ~ X, data=CP)
summary(linearMod)

Oct_plot<-ggplot() + 
  geom_line(data = CP, aes(x = X, y = October), color = 'grey53') +
  geom_point(data = CP, aes(x = X, y = October), color = 'gray53') + 
  #geom_smooth(data = CP, aes(x = X, y = April), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~m^3~ ""), x = 'Year', title = 'October') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Now plot all 3 together
library(ggpubr)
ggarrange(April_plot,July_plot,Oct_plot,nrow=3,ncol=1)

