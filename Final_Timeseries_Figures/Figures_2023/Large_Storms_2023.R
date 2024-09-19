## Figures for Large Storms

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: October 27, 2023**

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
Lstorms<-read.csv("Cyclones_2023.csv", header = TRUE)

AI <- Lstorms %>% group_by(year) %>% 
  summarise(number = length(unique(id)),
            mean_intensity = mean(DsqP),
            activity_index = number*mean_intensity)
LS <- Lstorms %>% group_by(year, id) %>% 
  summarise(strength = mean(strength)) %>% 
  group_by(year) %>%
  count(strength > 1)

# Activity Index

qAI = quantile(AI$activity_index, probs = c(.30, .70))

# find the last 5 years mean
mn_AI5 = mean(AI$activity_index[AI$year >= 2019])
# what quintile the data is in
mn_AI5 >qAI

# Creat a GAM - adjust k and remember to check model


mod_w<- gam(activity_index ~ s(year, k=7), data = AI)
summary(mod_w) #check out model
gam.check(mod_w)

pdata_w <- with(AI, data.frame(year = year))
p2_mod_w <- predict(mod_w, newdata = pdata_w,  type = "terms", se.fit = TRUE)
intercept_w = 132.1625 # look at p2_mod and extract the intercept
pdata_w <- transform(pdata_w, p2_mod_w = p2_mod_w$fit[,1], se2_w = p2_mod_w$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_w.d <- Deriv(mod_w, n=73) # n is the number of years
mod_w.dci <- confint(mod_w.d, term = Term)
mod_w.dsig <- signifD(pdata_w$p2_mod_w, d = mod_w.d[[Term]]$deriv,
                      +                    mod_w.dci[[Term]]$upper, mod_w.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(activity_index ~ year, data = AI)
lines(activity_index ~ year, data = AI)
lines(p2_mod_w+intercept_w ~ year, data = pdata_w, type = "n")
lines(p2_mod_w+intercept_w ~ year, data = pdata_w)
lines(unlist(mod_w.dsig$incr)+intercept_w ~ year, data = pdata_w, col = "blue", lwd = 3)
lines(unlist(mod_w.dsig$decr)+intercept_w ~ year, data = pdata_w, col = "red", lwd = 3)

linearMod_w<- lm(activity_index ~ year, data=AI)
summary(linearMod_w)

cia_plot <- ggplot() + 
  geom_line(data = AI, aes(x = year, y = activity_index), color = 'grey52') +
  geom_point(data = AI, aes(x = year, y = activity_index), color = 'grey52') + 
  #geom_point(data = winter[40:41,], aes(x= Year, y = Val), shape = 17, size =3) +
  #geom_smooth(data = winter, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_w, aes(x = year, y = p2_mod_w+intercept_w), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$incr)+intercept_w, x = year), color = "blue", size = 1) + 
  geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$decr)+intercept_w, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Activity Index"), x =' ',title = 'Cyclone Activity Index') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# Large


Large = LS[LS$`strength > 1` == TRUE, ]

qLarge = quantile(Large$n, probs = c(.30, .70))

# find the last 5 years mean
mn_Large5 = mean(Large$n[Large$year >= 2019])
# what quintile the data is in
mn_Large5 >qLarge

# Creat a GAM - adjust k and remember to check model
mod_sp<- gam(n ~ s(year, k=7), data = Large)
summary(mod_sp) #check out model
gam.check(mod_sp)

pdata_sp <- with(Large, data.frame(year = year))
p2_mod_sp <- predict(mod_sp, newdata = pdata_sp,  type = "terms", se.fit = TRUE)
intercept_sp = 8.506849 # look at p2_mod and extract the intercept
pdata_sp <- transform(pdata_sp, p2_mod_sp = p2_mod_sp$fit[,1], se2_sp = p2_mod_sp$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "year"
mod_sp.d <- Deriv(mod_sp, n=73) # n is the number of years
mod_sp.dci <- confint(mod_sp.d, term = Term)
mod_sp.dsig <- signifD(pdata_sp$p2_mod_sp, d = mod_sp.d[[Term]]$deriv,
                       +                    mod_sp.dci[[Term]]$upper, mod_sp.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(n ~ year, data = Large)
lines(n ~ year, data = Large)
lines(p2_mod_sp+intercept_sp ~ year, data = pdata_sp, type = "n")
lines(p2_mod_sp+intercept_sp ~ year, data = pdata_sp)
lines(unlist(mod_sp.dsig$incr)+intercept_sp ~ year, data = pdata_sp, col = "blue", lwd = 3)
lines(unlist(mod_sp.dsig$decr)+intercept_sp ~ year, data = pdata_sp, col = "red", lwd = 3)

linearMod_sp<- lm(n ~ year, data=Large)
summary(linearMod_sp)

Str_plot <- ggplot() + 
  geom_line(data = Large, aes(x = year, y = n), color = 'grey52') +
  geom_point(data = Large, aes(x = year, y = n), color = 'grey52') + 
 #geom_point(data = Large[39:41,], aes(x= year, y = n), shape = 17, size =3) +
  #geom_smooth(data = Large, aes(x = year, y = n), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_sp, aes(x = year, y = p2_mod_sp+intercept_sp), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$incr)+intercept_sp, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$decr)+intercept_sp, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Strong Storms per Year"), x = ' ', title = 'Number of Strong Storms') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


# Now plot 2 together
library(ggpubr)

ggarrange(cia_plot,Str_plot,nrow=1,ncol=2)
