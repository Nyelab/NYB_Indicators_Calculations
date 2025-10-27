## Figures for Lobster Thermal Habitat

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: October 24, 2025**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB_Indicators_Calculations")
source("Deriv.R")
library(mgcv)
#library(mgcViz)
library(ggplot2)
library(dplyr)

#######Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025/")
df<-read.csv("Lobster_Thermal_Habitat_annual2025.csv", header = TRUE)

qdf = quantile(df$val, probs = c(.3, 0.70))

# find the last 5 years mean
mn_df5 = mean(df$val[df$time>= 2021])
# what quintile the data is in
mn_df5 >qdf

mod<- gam(val ~ s(time, k=5), data = df)
summary(mod) #check out model
gam.check(mod)

pdata <- with(df, data.frame(time = time))
p2_mod <- predict(mod, newdata = pdata,  type = "terms", se.fit = TRUE)
intercept =   2.443644 # look at p2_mod and etimetract the intercept
pdata <- transform(pdata, p2_mod = p2_mod$fit[,1], se2 = p2_mod$se.fit[,1])

Term = "time"
mod.d <- Deriv(mod, n=33) # n is the number of years
mod.dci <- confint(mod.d, term = Term)
mod.dsig <- signifD(pdata$p2_mod, d = mod.d[[Term]]$deriv,
                    +                    mod.dci[[Term]]$upper, mod.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(val ~ time, data = df)
lines(val ~ time, data = df)
lines(p2_mod+intercept ~ time, data = pdata, type = "n")
lines(p2_mod+intercept ~ time, data = pdata)
lines(unlist(mod.dsig$incr)+intercept ~ time, data = pdata, col = "blue", lwd = 3)
lines(unlist(mod.dsig$decr)+intercept ~ time, data = pdata, col = "red", lwd = 3)

linearMod<- lm(val ~ time, data=df)
summary(linearMod)


val_plot <- ggplot() + 
  geom_line(data = df, aes(x = time, y = val), color = 'grey53') +
  geom_point(data = df, aes(x = time, y = val), color = 'gray53') + 
  geom_point(data = df[33,], aes(x = time, y = val), shape = 17, size =3) + 
  #geom_line(data = hypotimeia_risk, aes(x = first_column, y = second_column), color = 'orange') +
  #geom_line(data = hypotimeia, aes(x = first_column, y = second_column2), color = 'red') +
  geom_smooth(data = df, aes(x = time, y = val), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = time, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = time), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = time), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Percent Area Lethal', x = 'Year', title = 'Lobster Thermal Habitat') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
