## Figures for Feeding Groups

## **Laura Gruenburg, lagruenburg@gmail.com**

#   **LAST UPDATED: September 29, 2023**

#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023")
Benthos<-read.csv("benthos_spring_2023.csv", header = TRUE)
Benthos

#calculate the 30th a and 70th percentiles for the short term column of the indicators report
qBenthos = quantile(Benthos$strat.biomass, probs = c(.30, .70))

# find the last 5 years mean
mn_Benthos5 = mean(Benthos$strat.biomass[Benthos$YEAR >= 2019])
# what quintile the data is in
mn_Benthos5 >qBenthos

# Creat a GAM - adjust k and remember to check model
mod_cr<- gam(strat.biomass ~ s(YEAR, k=10), data = Benthos)
summary(mod_cr) #check out model
gam.check(mod_cr)

pdata_cr <- with(Benthos, data.frame(YEAR = YEAR))
p2_mod_cr <- predict(mod_cr, newdata = pdata_cr,  type = "terms", se.fit = TRUE)
intercept_cr = 1.199539   # look at p2_mod and extract the intercept
pdata_cr <- transform(pdata_cr, p2_mod_cr = p2_mod_cr$fit[,1], se2 = p2_mod_cr$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod_cr.d <- Deriv(mod_cr, n=55) # n is the number of YEARs
mod_cr.dci <- confint(mod_cr.d, term = Term)
mod_cr.dsig <- signifD(pdata_cr$p2_mod_cr, d = mod_cr.d[[Term]]$deriv,
                       +                    mod_cr.dci[[Term]]$upper, mod_cr.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = Benthos)
lines(strat.biomass ~ YEAR, data = Benthos)
lines(p2_mod_cr+intercept_cr ~ YEAR, data = pdata_cr, type = "n")
lines(p2_mod_cr+intercept_cr ~ YEAR, data = pdata_cr)
lines(unlist(mod_cr.dsig$incr)+intercept_cr ~ YEAR, data = pdata_cr, col = "blue", lwd = 3)
lines(unlist(mod_cr.dsig$decr)+intercept_cr ~ YEAR, data = pdata_cr, col = "red", lwd = 3)

linearmod_cr<- lm(strat.biomass ~ YEAR, data=Benthos)
summary(linearmod_cr)

Bt <- ggplot() + 
  geom_line(data = Benthos, aes(x = YEAR, y = strat.biomass), color = 'grey53') +
  geom_point(data = Benthos, aes(x = YEAR, y = strat.biomass), color = 'gray53') + 
  #geom_point(data = Benthos[54:55,], aes(x= YEAR, y = strat.biomass), shape = 17, size =3) +
  #geom_smooth(data = Benthos, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_cr, aes(x = YEAR, y = p2_mod_cr+intercept_cr), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_cr, aes(y = unlist(mod_cr.dsig$incr)+intercept_cr, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata_cr, aes(y = unlist(mod_cr.dsig$decr)+intercept_cr, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Biomass", x = '', title = 'Spring') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

### Benthos Fall
Benthos_f<-read.csv("benthos_fall_2023.csv", header = TRUE)
Benthos_f

qBenthos_f = quantile(Benthos_f$strat.biomass, probs = c(.30, .70))

# find the last 5 years mean
mn_Benthos_f5 = mean(Benthos_f$strat.biomass[Benthos_f$YEAR >= 2019])
# what quintile the data is in
mn_Benthos_f5 >qBenthos_f

# Creat a GAM - adjust k and remember to check model
mod_bf<- gam(strat.biomass ~ s(YEAR, k=10), data = Benthos_f)
summary(mod_bf) #check out model
gam.check(mod_bf)

pdata_bf <- with(Benthos_f, data.frame(YEAR = YEAR))
p2_mod_bf <- predict(mod_bf, newdata = pdata_bf,  type = "terms", se.fit = TRUE)
intercept_bf = 3.08281   # look at p2_mod and extract the intercept
pdata_bf <- transform(pdata_bf, p2_mod_bf = p2_mod_bf$fit[,1], se2 = p2_mod_bf$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod_bf.d <- Deriv(mod_bf, n=58) # n is the number of YEARs
mod_bf.dci <- confint(mod_bf.d, term = Term)
mod_bf.dsig <- signifD(pdata_bf$p2_mod_bf, d = mod_bf.d[[Term]]$deriv,
                       +                    mod_bf.dci[[Term]]$upper, mod_bf.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = Benthos_f)
lines(strat.biomass ~ YEAR, data = Benthos_f)
lines(p2_mod_bf+intercept_bf ~ YEAR, data = pdata_bf, type = "n")
lines(p2_mod_bf+intercept_bf ~ YEAR, data = pdata_bf)
lines(unlist(mod_bf.dsig$incr)+intercept_bf ~ YEAR, data = pdata_bf, col = "blue", lwd = 3)
lines(unlist(mod_bf.dsig$decr)+intercept_bf ~ YEAR, data = pdata_bf, col = "red", lwd = 3)

linearmod_bf<- lm(strat.biomass ~ YEAR, data=Benthos_f)
summary(linearmod_bf)

Bt_f <- ggplot() + 
  geom_line(data = Benthos_f, aes(x = YEAR, y = strat.biomass), color = 'grey53') +
  geom_point(data = Benthos_f, aes(x = YEAR, y = strat.biomass), color = 'gray53') + 
  geom_point(data = Benthos_f[58,], aes(x= YEAR, y = strat.biomass), shape = 17, size =3) +
  geom_smooth(data = Benthos_f, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_bf, aes(x = YEAR, y = p2_mod_bf+intercept_bf), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_bf, aes(y = unlist(mod_bf.dsig$incr)+intercept_bf, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata_bf, aes(y = unlist(mod_bf.dsig$decr)+intercept_bf, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Biomass", x = '', title = 'Fall') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#


library(ggpubr)

plot_Bt <- ggarrange(Bt,Bt_f, 
                     #labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 1)

plot_Benthos <- annotate_figure(plot_Bt, top = text_grob("Benthos", 
                                                         color = "black", face = "bold", size = 16))



######### BENTHIVORE

Benthivore<-read.csv("benthivore_spring_2023.csv", header = TRUE)
Benthivore

qBenthivore = quantile(Benthivore$strat.biomass, probs = c(.30, .70))

# find the last 5 years mean
mn_Benthivore5 = mean(Benthivore$strat.biomass[Benthivore$YEAR >= 2019])
# what quintile the data is in
mn_Benthivore5 >qBenthivore

# Creat a GAM - adjust k and remember to check model
mod_bvs<- gam(strat.biomass ~ s(YEAR, k=10), data = Benthivore)
summary(mod_bvs) #check out model
gam.check(mod_bvs)

pdata_bvs <- with(Benthivore, data.frame(YEAR = YEAR))
p2_mod_bvs <- predict(mod_bvs, newdata = pdata_bvs,  type = "terms", se.fit = TRUE)
intercept_bvs = 13.74268   # look at p2_mod and extract the intercept
pdata_bvs <- transform(pdata_bvs, p2_mod_bvs = p2_mod_bvs$fit[,1], se2 = p2_mod_bvs$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod_bvs.d <- Deriv(mod_bvs, n=55) # n is the number of YEARs
mod_bvs.dci <- confint(mod_bvs.d, term = Term)
mod_bvs.dsig <- signifD(pdata_bvs$p2_mod_bvs, d = mod_bvs.d[[Term]]$deriv,
                        +                    mod_bvs.dci[[Term]]$upper, mod_bvs.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = Benthivore)
lines(strat.biomass ~ YEAR, data = Benthivore)
lines(p2_mod_bvs+intercept_bvs ~ YEAR, data = pdata_bvs, type = "n")
lines(p2_mod_bvs+intercept_bvs ~ YEAR, data = pdata_bvs)
lines(unlist(mod_bvs.dsig$incr)+intercept_bvs ~ YEAR, data = pdata_bvs, col = "blue", lwd = 3)
lines(unlist(mod_bvs.dsig$decr)+intercept_bvs ~ YEAR, data = pdata_bvs, col = "red", lwd = 3)

linearmod_bvs<- lm(strat.biomass ~ YEAR, data=Benthivore)
summary(linearmod_bvs)

Bvs <- ggplot() + 
  geom_line(data = Benthivore, aes(x = YEAR, y = strat.biomass), color = 'grey53') +
  geom_point(data = Benthivore, aes(x = YEAR, y = strat.biomass), color = 'gray53') + 
  #geom_point(data = Benthivore[54:55,], aes(x= YEAR, y = strat.biomass), shape = 17, size =3) +
  #geom_smooth(data = Benthivore, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_bvs, aes(x = YEAR, y = p2_mod_bvs+intercept_bvs), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_bvs, aes(y = unlist(mod_bvs.dsig$incr)+intercept_bvs, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata_bvs, aes(y = unlist(mod_bvs.dsig$decr)+intercept_bvs, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Biomass", x = '', title = 'Spring') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

### Benthivore Fall
Benthivore_f<-read.csv("Benthivore_fall_2023.csv", header = TRUE)
Benthivore_f

qBenthivore_f = quantile(Benthivore_f$strat.biomass, probs = c(.30, .70))

# find the last 5 years mean
mn_Benthivore_f5 = mean(Benthivore_f$strat.biomass[Benthivore_f$YEAR >= 2019])
# what quintile the data is in
mn_Benthivore_f5 >qBenthivore_f

# Creat a GAM - adjust k and remember to check model
mod_bvf<- gam(strat.biomass ~ s(YEAR, k=10), data = Benthivore_f)
summary(mod_bvf) #check out model
gam.check(mod_bvf)

pdata_bvf <- with(Benthivore_f, data.frame(YEAR = YEAR))
p2_mod_bvf <- predict(mod_bvf, newdata = pdata_bvf,  type = "terms", se.fit = TRUE)
intercept_bvf = 19.54726   # look at p2_mod and extract the intercept
pdata_bvf <- transform(pdata_bvf, p2_mod_bvf = p2_mod_bvf$fit[,1], se2 = p2_mod_bvf$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod_bvf.d <- Deriv(mod_bvf, n=58) # n is the number of YEARs
mod_bvf.dci <- confint(mod_bvf.d, term = Term)
mod_bvf.dsig <- signifD(pdata_bvf$p2_mod_bvf, d = mod_bvf.d[[Term]]$deriv,
                        +                    mod_bvf.dci[[Term]]$upper, mod_bvf.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = Benthivore_f)
lines(strat.biomass ~ YEAR, data = Benthivore_f)
lines(p2_mod_bvf+intercept_bvf ~ YEAR, data = pdata_bvf, type = "n")
lines(p2_mod_bvf+intercept_bvf ~ YEAR, data = pdata_bvf)
lines(unlist(mod_bvf.dsig$incr)+intercept_bvf ~ YEAR, data = pdata_bvf, col = "blue", lwd = 3)
lines(unlist(mod_bvf.dsig$decr)+intercept_bvf ~ YEAR, data = pdata_bvf, col = "red", lwd = 3)

linearmod_bvf<- lm(strat.biomass ~ YEAR, data=Benthivore_f)
summary(linearmod_bvf)

Bv_f <- ggplot() + 
  geom_line(data = Benthivore_f, aes(x = YEAR, y = strat.biomass), color = 'grey53') +
  geom_point(data = Benthivore_f, aes(x = YEAR, y = strat.biomass), color = 'gray53') + 
  geom_point(data = Benthivore_f[58,], aes(x= YEAR, y = strat.biomass), shape = 17, size =3) +
  #geom_smooth(data = Benthivore_f, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_bvf, aes(x = YEAR, y = p2_mod_bvf+intercept_bvf), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_bvf, aes(y = unlist(mod_bvf.dsig$incr)+intercept_bvf, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata_bvf, aes(y = unlist(mod_bvf.dsig$decr)+intercept_bvf, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Biomass", x = '', title = 'Fall') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#


library(ggpubr)

plot_Bv <- ggarrange(Bvs,Bv_f, 
                     #labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 1)

plot_Benthivore <- annotate_figure(plot_Bv, top = text_grob("Benthivore", 
                                                            color = "black", face = "bold", size = 16))

######### PLANKTIVORE

Planktivore<-read.csv("Planktivore_spring_2023.csv", header = TRUE)
Planktivore

qPlanktivore = quantile(Planktivore$strat.biomass, probs = c(.30, .70))

# find the last 5 years mean
mn_Planktivore5 = mean(Planktivore$strat.biomass[Planktivore$YEAR >= 2019])
# what quintile the data is in
mn_Planktivore5 >qPlanktivore


# Creat a GAM - adjust k and remember to check model
mod_pvs<- gam(strat.biomass ~ s(YEAR, k=10), data = Planktivore)
summary(mod_pvs) #check out model
gam.check(mod_pvs)

pdata_pvs <- with(Planktivore, data.frame(YEAR = YEAR))
p2_mod_pvs <- predict(mod_pvs, newdata = pdata_pvs,  type = "terms", se.fit = TRUE)
intercept_pvs = 25.6243   # look at p2_mod and extract the intercept
pdata_pvs <- transform(pdata_pvs, p2_mod_pvs = p2_mod_pvs$fit[,1], se2 = p2_mod_pvs$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod_pvs.d <- Deriv(mod_pvs, n=55) # n is the number of YEARs
mod_pvs.dci <- confint(mod_pvs.d, term = Term)
mod_pvs.dsig <- signifD(pdata_pvs$p2_mod_pvs, d = mod_pvs.d[[Term]]$deriv,
                        +                    mod_pvs.dci[[Term]]$upper, mod_pvs.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = Planktivore)
lines(strat.biomass ~ YEAR, data = Planktivore)
lines(p2_mod_pvs+intercept_pvs ~ YEAR, data = pdata_pvs, type = "n")
lines(p2_mod_pvs+intercept_pvs ~ YEAR, data = pdata_pvs)
lines(unlist(mod_pvs.dsig$incr)+intercept_pvs ~ YEAR, data = pdata_pvs, col = "blue", lwd = 3)
lines(unlist(mod_pvs.dsig$decr)+intercept_pvs ~ YEAR, data = pdata_pvs, col = "red", lwd = 3)

linearmod_pvs<- lm(strat.biomass ~ YEAR, data=Planktivore)
summary(linearmod_pvs)

Pvs <- ggplot() + 
  geom_line(data = Planktivore, aes(x = YEAR, y = strat.biomass), color = 'grey53') +
  geom_point(data = Planktivore, aes(x = YEAR, y = strat.biomass), color = 'gray53') + 
  #geom_point(data = Planktivore[54:55,], aes(x= YEAR, y = strat.biomass), shape = 17, size =3) +
  #geom_smooth(data = Planktivore, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_pvs, aes(x = YEAR, y = p2_mod_pvs+intercept_pvs), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_pvs, aes(y = unlist(mod_pvs.dsig$incr)+intercept_pvs, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata_pvs, aes(y = unlist(mod_pvs.dsig$decr)+intercept_pvs, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Biomass", x = '', title = 'Spring') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

### Planktivore Fall
Planktivore_f<-read.csv("Planktivore_fall_2023.csv", header = TRUE)
Planktivore_f

qPlanktivore_f = quantile(Planktivore_f$strat.biomass, probs = c(.30, .70))

# find the last 5 years mean
mn_Planktivore_f5 = mean(Planktivore_f$strat.biomass[Planktivore_f$YEAR >= 2019])
# what quintile the data is in
mn_Planktivore_f5 >qPlanktivore_f

# Creat a GAM - adjust k and remember to check model
mod_pvf<- gam(strat.biomass ~ s(YEAR, k=10), data = Planktivore_f)
summary(mod_pvf) #check out model
gam.check(mod_pvf)

pdata_pvf <- with(Planktivore_f, data.frame(YEAR = YEAR))
p2_mod_pvf <- predict(mod_pvf, newdata = pdata_pvf,  type = "terms", se.fit = TRUE)
intercept_pvf = 26.40263   # look at p2_mod and extract the intercept
pdata_pvf <- transform(pdata_pvf, p2_mod_pvf = p2_mod_pvf$fit[,1], se2 = p2_mod_pvf$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod_pvf.d <- Deriv(mod_pvf, n=58) # n is the number of YEARs
mod_pvf.dci <- confint(mod_pvf.d, term = Term)
mod_pvf.dsig <- signifD(pdata_pvf$p2_mod_pvf, d = mod_pvf.d[[Term]]$deriv,
                        +                    mod_pvf.dci[[Term]]$upper, mod_pvf.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = Planktivore_f)
lines(strat.biomass ~ YEAR, data = Planktivore_f)
lines(p2_mod_pvf+intercept_pvf ~ YEAR, data = pdata_pvf, type = "n")
lines(p2_mod_pvf+intercept_pvf ~ YEAR, data = pdata_pvf)
lines(unlist(mod_pvf.dsig$incr)+intercept_pvf ~ YEAR, data = pdata_pvf, col = "blue", lwd = 3)
lines(unlist(mod_pvf.dsig$decr)+intercept_pvf ~ YEAR, data = pdata_pvf, col = "red", lwd = 3)

linearmod_pvf<- lm(strat.biomass ~ YEAR, data=Planktivore_f)
summary(linearmod_pvf)

Pv_f <- ggplot() + 
  geom_line(data = Planktivore_f, aes(x = YEAR, y = strat.biomass), color = 'grey53') +
  geom_point(data = Planktivore_f, aes(x = YEAR, y = strat.biomass), color = 'gray53') + 
  geom_point(data = Planktivore_f[58,], aes(x= YEAR, y = strat.biomass), shape = 17, size =3) +
  geom_smooth(data = Planktivore_f, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_pvf, aes(x = YEAR, y = p2_mod_pvf+intercept_pvf), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_pvf, aes(y = unlist(mod_pvf.dsig$incr)+intercept_pvf, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata_pvf, aes(y = unlist(mod_pvf.dsig$decr)+intercept_pvf, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Biomass", x = '', title = 'Fall') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#


library(ggpubr)

plot_Pv <- ggarrange(Pvs,Pv_f, 
                     #labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 1)

plot_Planktivore <- annotate_figure(plot_Pv, top = text_grob("Planktivore", 
                                                             color = "black", face = "bold", size = 16))

######### PISCIVORE

Piscivore<-read.csv("Piscivore_spring_2023.csv", header = TRUE)
Piscivore

qPiscivore = quantile(Piscivore$strat.biomass, probs = c(.30, .70))

# find the last 5 years mean
mn_Piscivore5 = mean(Piscivore$strat.biomass[Piscivore$YEAR >= 2019])
# what quintile the data is in
mn_Piscivore5 >qPiscivore


# Creat a GAM - adjust k and remember to check model
mod_pcs<- gam(strat.biomass ~ s(YEAR, k=10), data = Piscivore)
summary(mod_pcs) #check out model
gam.check(mod_pcs)

pdata_pcs <- with(Piscivore, data.frame(YEAR = YEAR))
p2_mod_pcs <- predict(mod_pcs, newdata = pdata_pcs,  type = "terms", se.fit = TRUE)
intercept_pcs = 148.2062   # look at p2_mod and extract the intercept
pdata_pcs <- transform(pdata_pcs, p2_mod_pcs = p2_mod_pcs$fit[,1], se2 = p2_mod_pcs$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod_pcs.d <- Deriv(mod_pcs, n=55) # n is the number of YEARs
mod_pcs.dci <- confint(mod_pcs.d, term = Term)
mod_pcs.dsig <- signifD(pdata_pcs$p2_mod_pcs, d = mod_pcs.d[[Term]]$deriv,
                        +                    mod_pcs.dci[[Term]]$upper, mod_pcs.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = Piscivore)
lines(strat.biomass ~ YEAR, data = Piscivore)
lines(p2_mod_pcs+intercept_pcs ~ YEAR, data = pdata_pcs, type = "n")
lines(p2_mod_pcs+intercept_pcs ~ YEAR, data = pdata_pcs)
lines(unlist(mod_pcs.dsig$incr)+intercept_pcs ~ YEAR, data = pdata_pcs, col = "blue", lwd = 3)
lines(unlist(mod_pcs.dsig$decr)+intercept_pcs ~ YEAR, data = pdata_pcs, col = "red", lwd = 3)

linearmod_pcs<- lm(strat.biomass ~ YEAR, data=Piscivore)
summary(linearmod_pcs)

Pcs <- ggplot() + 
  geom_line(data = Piscivore, aes(x = YEAR, y = strat.biomass), color = 'grey53') +
  geom_point(data = Piscivore, aes(x = YEAR, y = strat.biomass), color = 'gray53') + 
  #geom_point(data = Piscivore[54:55,], aes(x= YEAR, y = strat.biomass), shape = 17, size =3) +
  #geom_smooth(data = Piscivore, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_pcs, aes(x = YEAR, y = p2_mod_pcs+intercept_pcs), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_pcs, aes(y = unlist(mod_pcs.dsig$incr)+intercept_pcs, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata_pcs, aes(y = unlist(mod_pcs.dsig$decr)+intercept_pcs, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Biomass", x = '', title = 'Spring') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

### Piscivore Fall
Piscivore_f<-read.csv("Piscivore_fall_2023.csv", header = TRUE)
Piscivore_f


qPiscivore_f = quantile(Piscivore_f$strat.biomass, probs = c(.30, .70))

# find the last 5 years mean
mn_Piscivore_f5 = mean(Piscivore_f$strat.biomass[Piscivore_f$YEAR >= 2019])
# what quintile the data is in
mn_Piscivore_f5 >qPiscivore_f

# Creat a GAM - adjust k and remember to check model
mod_pcf<- gam(strat.biomass ~ s(YEAR, k=10), data = Piscivore_f)
summary(mod_pcf) #check out model
gam.check(mod_pcf)

pdata_pcf <- with(Piscivore_f, data.frame(YEAR = YEAR))
p2_mod_pcf <- predict(mod_pcf, newdata = pdata_pcf,  type = "terms", se.fit = TRUE)
intercept_pcf = 70.21592    # look at p2_mod and extract the intercept
pdata_pcf <- transform(pdata_pcf, p2_mod_pcf = p2_mod_pcf$fit[,1], se2 = p2_mod_pcf$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "YEAR"
mod_pcf.d <- Deriv(mod_pcf, n=58) # n is the number of YEARs
mod_pcf.dci <- confint(mod_pcf.d, term = Term)
mod_pcf.dsig <- signifD(pdata_pcf$p2_mod_pcf, d = mod_pcf.d[[Term]]$deriv,
                        +                    mod_pcf.dci[[Term]]$upper, mod_pcf.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(strat.biomass ~ YEAR, data = Piscivore_f)
lines(strat.biomass ~ YEAR, data = Piscivore_f)
lines(p2_mod_pcf+intercept_pcf ~ YEAR, data = pdata_pcf, type = "n")
lines(p2_mod_pcf+intercept_pcf ~ YEAR, data = pdata_pcf)
lines(unlist(mod_pcf.dsig$incr)+intercept_pcf ~ YEAR, data = pdata_pcf, col = "blue", lwd = 3)
lines(unlist(mod_pcf.dsig$decr)+intercept_pcf ~ YEAR, data = pdata_pcf, col = "red", lwd = 3)

linearmod_pcf<- lm(strat.biomass ~ YEAR, data=Piscivore_f)
summary(linearmod_pcf)

Pc_f <- ggplot() + 
  geom_line(data = Piscivore_f, aes(x = YEAR, y = strat.biomass), color = 'grey53') +
  geom_point(data = Piscivore_f, aes(x = YEAR, y = strat.biomass), color = 'gray53') + 
  geom_point(data = Piscivore_f[58,], aes(x= YEAR, y = strat.biomass), shape = 17, size =3) +
  geom_smooth(data = Piscivore_f, aes(x = YEAR, y = strat.biomass), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_pcf, aes(x = YEAR, y = p2_mod_pcf+intercept_pcf), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_pcf, aes(y = unlist(mod_pcf.dsig$incr)+intercept_pcf, x = YEAR), color = "blue", size = 1) + 
  geom_line(data = pdata_pcf, aes(y = unlist(mod_pcf.dsig$decr)+intercept_pcf, x = YEAR), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Biomass", x = '', title = 'Fall') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#


library(ggpubr)

plot_Pc <- ggarrange(Pcs,Pc_f, 
                     #labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 1)

plot_Piscivore <- annotate_figure(plot_Pc, top = text_grob("Piscivore", 
                                                           color = "black", face = "bold", size = 16))
### ALL
plot_all <- ggarrange(plot_Benthos,
                      plot_Benthivore,
                      plot_Planktivore,
                      plot_Piscivore,
                      #labels = c("A", "B", "C", "D"),
                      ncol = 1, nrow = 4)
