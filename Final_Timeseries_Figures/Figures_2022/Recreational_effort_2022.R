#####load required functions
## Figures for Recreational Effort

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: January 9, 2023**

#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022")
RE<-read.csv("Recreational_effort_2022.csv", header = TRUE)
RE
Ch_boat <- RE[RE$MODE == 'CHARTER BOAT', ]
P_boat <- RE[RE$MODE == 'PARTY BOAT', ]
Ch_P_boat <- RE[RE$MODE == 'PARTY/CHARTER BOAT',]
P_R_Boat <- RE[RE$MODE == 'PRIVATE/RENTAL BOAT',]
Sh <- RE[RE$MODE == 'SHORE',]
Effort <- aggregate(RE['ESTRIPS'], by = RE['YEAR'], sum) # total number of angler trips

# GAM for total effort
# Creat a GAM - adjust k and remember to check model
mod<- gam(ESTRIPS ~ s(YEAR, k=10), data = Effort)
summary(mod) #check out model
gam.check(mod)

pdata <- with(Effort, data.frame(YEAR = YEAR))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept = 11720285   # look at p2_mod and extract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod.d <- Deriv(mod, n=42) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                       +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(ESTRIPS ~ YEAR, data = Effort)
lines(ESTRIPS ~ YEAR, data = Effort)
lines(p2_mod+intercept ~ YEAR, data = pdata, type = "n")
lines(p2_mod+intercept ~ YEAR, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ YEAR, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ YEAR, data = pdata, col = "red", lwd = 3)

linearmod<- lm(ESTRIPS ~ YEAR, data=Effort)
summary(linearmod)

ggplot() + 
  geom_line(data = RE, aes(x=YEAR, y = ESTRIPS, color = MODE)) +
  geom_point(data = RE, aes(x=YEAR, y = ESTRIPS, color = MODE)) +
  geom_line(data = Effort, aes(x = YEAR, y = ESTRIPS), color = 'grey53') +
  geom_point(data = Effort, aes(x = YEAR, y = ESTRIPS), color = 'gray53') + 
  geom_point(data = Effort[42,], aes(x= YEAR, y = ESTRIPS), shape = 17, size =3) +
  geom_smooth(data = Effort, aes(x = YEAR, y = ESTRIPS), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata, aes(x = YEAR, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Trips", x = 'Year', title = 'Recreational Effort') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#

#ggplot() + 
#  geom_line(data = Ch_boat, aes(x=YEAR, y = ESTRIPS), color = 'deeppink1') +
#  geom_point(data = Ch_boat, aes(x=YEAR, y = ESTRIPS), color = 'deeppink1') +
#  geom_line(data = P_boat, aes(x=YEAR, y = ESTRIPS), color = 'darkred') +
#  geom_point(data = P_boat, aes(x=YEAR, y = ESTRIPS), color = 'darkred') +
#  geom_line(data = P_R_Boat, aes(x=YEAR, y = ESTRIPS), color = 'darkgoldenrod2') +
#  geom_point(data = P_R_Boat, aes(x=YEAR, y = ESTRIPS), color = 'darkgoldenrod2') +
#  geom_line(data = Ch_P_boat, aes(x=YEAR, y = ESTRIPS), color = 'darkorchid2') +
#  geom_point(data = Ch_P_boat, aes(x=YEAR, y = ESTRIPS), color = 'darkorchid2') +
#  geom_line(data = Sh, aes(x=YEAR, y = ESTRIPS), color = 'darkolivegreen3') +
#  geom_point(data = Sh, aes(x=YEAR, y = ESTRIPS), color = 'darkolivegreen3') +
#  geom_line(data = Effort, aes(x = YEAR, y = ESTRIPS), color = 'grey53') +
#  geom_point(data = Effort, aes(x = YEAR, y = ESTRIPS), color = 'gray53') + 
#  geom_point(data = Effort[42,], aes(x= YEAR, y = ESTRIPS), shape = 17, size =3) +
#  geom_smooth(data = Effort, aes(x = YEAR, y = ESTRIPS), method = lm, se = FALSE, color = 'black') + 
#  geom_line(data=pdata, aes(x = YEAR, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
#  geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = YEAR), color = "blue", size = 1) + 
#  geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = YEAR), color = 'red', size = 1) + 
#  theme_bw() +
#  labs (y = "Number of Trips", x = 'Year', title = 'Recreational Effort') + 
#  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
##
