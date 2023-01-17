## Figures for Cold Pool Volume

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: December 23, 2022**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
#library(mgcViz)
library(ggplot2)

#######Load the datasets
setwd("~/Desktop/NYB Indicators/Final_timeseries")
CP<-read.csv("CP_volume_Dec_12_2022.csv", header = TRUE)

# Creat a GAM - adjust k and remember to check model
mod<- gam(April ~ s(X, k=5), data = CP)
summary(mod) #check out model
gam.check(mod)

pdata <- with(CP, data.frame(X = X))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 1.627296e+12 # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod.d <- Deriv(mod, n=28) # n is the number of years
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
  geom_point(data = CP[28,], aes(x = X, y = April), shape = 17, size =3) + 
  #geom_smooth(data = CP, aes(x = X, y = April), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = X, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~m^3~""), x = '', title = 'April') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# JULY Creat a GAM - adjust k and remember to check model
mod_j<- gam(July ~ s(X, k=5), data = CP)
summary(mod_j) #check out model
gam.check(mod_j)

pdata_j <- with(CP, data.frame(X = X))
p2_mod_j <- predict(mod_j, newdata = pdata_j,  type = "terms", se.fit = TRUE)
intercept_j = 373570478934   # look at p2_mod and extract the intercept
pdata_j <- transform(pdata_j, p2_mod_j = p2_mod_j$fit[,1], se2 = p2_mod_j$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_j.d <- Deriv(mod_j, n=28) # n is the number of years
mod_j.dci <- confint(mod_j.d, term = Term)
mod_j.dsig <- signifD(pdata$p2_mod_j, d = mod_j.d[[Term]]$deriv,
                    +                    mod_j.dci[[Term]]$upper, mod_j.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(July ~ X, data = CP)
lines(July ~ X, data = CP)
lines(p2_mod_j+intercept_j ~ X, data = pdata_j, type = "n")
lines(p2_mod_j+intercept_j ~ X, data = pdata_j)
lines(unlist(mod_j.dsig$incr)+intercept_j ~ X, data = pdata_j, col = "blue", lwd = 3)
lines(unlist(mod_j.dsig$decr)+intercept_j ~ X, data = pdata_j, col = "red", lwd = 3)

linearmod_j<- lm(July ~ X, data=CP)
summary(linearmod_j)

July_plot<- ggplot() + 
  geom_line(data = CP, aes(x = X, y = July), color = 'grey53') +
  geom_point(data = CP, aes(x = X, y = July), color = 'gray53') + 
  geom_point(data = CP[28,], aes(x = X, y = July), shape = 17, size =3) + 
  #geom_smooth(data = CP, aes(x = X, y = July), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = X, y = p2_mod_j+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod_j.dsig$incr)+intercept, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod_j.dsig$decr)+intercept, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~m^3~ ""), x = '', title = 'July') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# October Creat a GAM - adjust k and remember to check model
mod_o<- gam(October ~ s(X, k=5), data = CP)
summary(mod_o) #check out mod_oel
gam.check(mod_o)

pdata_o <- with(CP, data.frame(X = X))
p2_mod_o <- predict(mod_o, newdata = pdata_o,  type = "terms", se.fit = TRUE)
intercept_o = 34604146640    # look at p2_mod_o and extract the intercept_o
pdata_o <- transform(pdata_o, p2_mod_o = p2_mod_o$fit[,1], se2 = p2_mod_o$se.fit[,1])

#  Now that we have the mod_oel prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "X"
mod_o.d <- Deriv(mod_o, n=28) # n is the number of years
mod_o.dci <- confint(mod_o.d, term = Term)
mod_o.dsig <- signifD(pdata_o$p2_mod_o, d = mod_o.d[[Term]]$deriv,
                    +                    mod_o.dci[[Term]]$upper, mod_o.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(October ~ X, data = CP)
lines(October ~ X, data = CP)
lines(p2_mod_o+intercept_o ~ X, data = pdata_o, type = "n")
lines(p2_mod_o+intercept_o ~ X, data = pdata_o)
lines(unlist(mod_o.dsig$incr)+intercept_o ~ X, data = pdata_o, col = "blue", lwd = 3)
lines(unlist(mod_o.dsig$decr)+intercept_o ~ X, data = pdata_o, col = "red", lwd = 3)

linearmod_o<- lm(October ~ X, data=CP)
summary(linearmod_o)

Oct_plot<-ggplot() + 
  geom_line(data = CP, aes(x = X, y = October), color = 'grey53') +
  geom_point(data = CP, aes(x = X, y = October), color = 'gray53') + 
  geom_point(data = CP[28,], aes(x = X, y = October), shape = 17, size = 3) + 
  #geom_smooth(data = CP, aes(x = X, y = October), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_o, aes(x = X, y = p2_mod_o+intercept_o), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_o, aes(y = unlist(mod_o.dsig$incr)+intercept_o, x = X), color = "blue", size = 1) + 
  #geom_line(data = pdata_o, aes(y = unlist(mod_o.dsig$decr)+intercept_o, x = X), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Volume "~m^3~ ""), x = 'Year', title = 'October') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Now plot all 3 together
library(ggpubr)
ggarrange(April_plot,July_plot,Oct_plot,nrow=3,ncol=1)

