## Figures for Bottom Oxygen Seasonally

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: November 19, 2025**

setwd("~/Desktop/NYB_Indicators_Calculations")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2025")
Ox<-read.csv("Oxygen2025_profn.csv", header = TRUE)

winter = Ox[Ox$season == 'winter',]
spring = Ox[Ox$season == 'spring',]
summer = Ox[Ox$season == 'summer',]
fall = Ox[Ox$season == 'autumn',]


# WINTER

linearMod_w<- lm(btm_oxygen ~ season_season_year, data=winter)
summary(linearMod_w)

wint_plot <- ggplot() + 
  geom_line(data = winter, aes(x = season_year, y = btm_oxygen), color = 'grey52') +
  geom_point(data = winter, aes(x = season_year, y = btm_oxygen), color = 'grey52') + 
  #geom_point(data = winter[42,], aes(x= season_year, y = btm_oxygen), shape = 17, size =3) +
  #geom_smooth(data = winter[19:81, ], aes(x = season_year, y = btm_oxygen), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_w, aes(x = season_year, y = p2_mod_w+intercept_w), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$incr)+intercept_w, x = season_year), color = "blue", size = 1) + 
  #geom_line(data = pdata_w, aes(y = unlist(mod_w.dsig$decr)+intercept_w, x = season_year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Salinity"), x =' ',title = 'Winter') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# SPRING


linearMod_sp<- lm(btm_oxygen ~ season_season_year, data=spring)
summary(linearMod_sp)

spr_plot <- ggplot() + 
  geom_line(data = spring, aes(x = season_year, y = btm_oxygen), color = 'grey52') +
  geom_point(data = spring, aes(x = season_year, y = btm_oxygen), color = 'grey52') + 
  #geom_point(data = spring[42,], aes(x= season_year, y = btm_oxygen), shape = 17, size =3) +
  geom_smooth(data = spring[spring$season_year > 1959, ], aes(x = season_year, y = btm_oxygen), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_sp, aes(x = season_year, y = p2_mod_sp+intercept_sp), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$incr)+intercept_sp, x = season_year), color = "blue", size = 1) + 
  #geom_line(data = pdata_sp, aes(y = unlist(mod_sp.dsig$decr)+intercept_sp, x = season_year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = ' ', title = 'Spring') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# SUMMER

linearMod_su<- lm(btm_oxygen ~ season_year, data=summer)
summary(linearMod_su)

sum_plot <- ggplot() + 
  geom_line(data = summer, aes(x = season_year, y = btm_oxygen), color = 'grey52') +
  geom_point(data = summer, aes(x = season_year, y = btm_oxygen), color = 'grey52') + 
  #geom_point(data = summer[42,], aes(x= season_year, y = btm_oxygen), shape = 17, size =3) +
  geom_smooth(data = summer, aes(x = season_year, y = btm_oxygen), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata_su, aes(x = season_year, y = p2_mod_su+intercept_su), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_su, aes(y = unlist(mod_su.dsig$incr)+intercept_su, x = season_year), color = "blue", size = 1) + 
  #geom_line(data = pdata_su, aes(y = unlist(mod_su.dsig$decr)+intercept_su, x = season_year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Salinity"), x = 'season_year', title = 'Summer') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

# FALL
linearMod_fa<- lm(btm_oxygen ~ season_year, data=fall)
summary(linearMod_fa)

fall_plot <- ggplot() + 
  geom_line(data = fall, aes(x = season_year, y = btm_oxygen), color = 'grey52') +
  geom_point(data = fall, aes(x = season_year, y = btm_oxygen), color = 'grey52') + 
  #geom_point(data = fall[43,], aes(x= season_year, y = btm_oxygen), shape = 17, size =3) +
  geom_smooth(data = fall[fall$season_year > 1959, ], aes(x = season_year, y = btm_oxygen), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data = pdata_fa, aes(x = season_year, y = p2_mod_fa+intercept_fa), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$incr)+intercept_fa, x = season_year), color = "blue", size = 1) + 
  #geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$decr)+intercept_fa, x = season_year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote(" "), x = 'season_year', title = 'Autumn') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


ggplot() + 
  geom_line(data = Ox, aes(x=season_year, y = btm_oxygen, color = season)) +
  geom_point(data = Ox, aes(x=season_year, y = btm_oxygen, color = season)) +
  #geom_point(data = fall[43,], aes(x= season_year, y = btm_oxygen), shape = 17, size =3) +
  #geom_smooth(data = fall[fall$season_year > 1959, ], aes(x = season_year, y = btm_oxygen), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data = pdata_fa, aes(x = season_year, y = p2_mod_fa+intercept_fa), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$incr)+intercept_fa, x = season_year), color = "blue", size = 1) + 
  #geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$decr)+intercept_fa, x = season_year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = bquote("Oxygen mg/L"), x = 'Year', title = 'Bottom Dissolved Oxygen') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

ggplot() + 
  geom_line(data = Ox, aes(x=season_year, y = btm_oxygen, color = season)) +
  geom_point(data = Ox, aes(x=season_year, y = btm_oxygen, color = season)) +
  #geom_point(data = fall[43,], aes(x= season_year, y = btm_oxygen), shape = 17, size =3) +
  #geom_smooth(data = fall[fall$season_year > 1959, ], aes(x = season_year, y = btm_oxygen), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data = pdata_fa, aes(x = season_year, y = p2_mod_fa+intercept_fa), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$incr)+intercept_fa, x = season_year), color = "blue", size = 1) + 
  #geom_line(data = pdata_fa, aes(y = unlist(mod_fa.dsig$decr)+intercept_fa, x = season_year), color = 'red', size = 1) + 
  geom_col(data = Ox, aes(x = season_year, y = nprof/30)) +
  theme_bw() +
  labs (y = bquote("Oxygen mg/L"), x = 'Year', title = 'Bottom Dissolved Oxygen') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

