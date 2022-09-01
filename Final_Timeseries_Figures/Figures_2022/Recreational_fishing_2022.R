#####load required functions
#  You will need to download the functions from here https://gist.github.com/gavinsimpson/e73f011fdaaab4bb5a30

setwd("~/Desktop/NYB Indicators/Deriv")
source("Deriv.R")
library(mgcv)
library(ggplot2)
#library(mgcViz)


#######Load the datasets
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022")
RF<-read.csv("Recreation_Fishing_2022.csv", header = TRUE)
RF
Release <- RF[RF$Variable == 'RecRelease_N', ]
Catch <- RF[RF$Variable == 'RecCatch_N', ]
Harv <- RF[RF$Variable == 'Rec_HarvN',]
P_Release <- RF[RF$Variable == 'RecRelease_Pct',]
P_Release$Val <- P_Release$Val * 100 # turn fraction into percent
# Your final time series is (hopefully) a dataframe with a column for the year 
# and a column for whatever the data variable is.  Here I give an example using 
# Hudson river mean flow data where one column is year and the other is flowrate

# Creat a GAM - adjust k and remember to check model
mod_cr<- gam(Val ~ s(Year, k=10), data = Release)
summary(mod_cr) #check out model
gam.check(mod_cr)

pdata_cr <- with(Release, data.frame(Year = Year))
p2_mod_cr <- predict(mod_cr, newdata = pdata_cr,  type = "terms", se.fit = TRUE)
intercept_cr = 31316975  # look at p2_mod and extract the intercept
pdata_cr <- transform(pdata_cr, p2_mod_cr = p2_mod_cr$fit[,1], se2 = p2_mod_cr$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_cr.d <- Deriv(mod_cr, n=42) # n is the number of years
mod_cr.dci <- confint(mod_cr.d, term = Term)
mod_cr.dsig <- signifD(pdata_cr$p2_mod_cr, d = mod_cr.d[[Term]]$deriv,
                    +                    mod_cr.dci[[Term]]$upper, mod_cr.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = Release)
lines(Val ~ Year, data = Release)
lines(p2_mod_cr+intercept_cr ~ Year, data = pdata_cr, type = "n")
lines(p2_mod_cr+intercept_cr ~ Year, data = pdata_cr)
lines(unlist(mod_cr.dsig$incr)+intercept_cr ~ Year, data = pdata_cr, col = "blue", lwd = 3)
lines(unlist(mod_cr.dsig$decr)+intercept_cr ~ Year, data = pdata_cr, col = "red", lwd = 3)

linearmod_cr<- lm(Val ~ Year, data=Release)
summary(linearmod_cr)

CR <- ggplot() + 
  geom_line(data = Release, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = Release, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = Release[42,], aes(x= Year, y = Val), shape = 17, size =3) +
  geom_smooth(data = Release, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_cr, aes(x = Year, y = p2_mod_cr+intercept_cr), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_cr, aes(y = unlist(mod_cr.dsig$incr)+intercept_cr, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_cr, aes(y = unlist(mod_cr.dsig$decr)+intercept_cr, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Fish", x = '', title = 'Recreational Catch and Release') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#
# Percent Released
# Creat a GAM - adjust k and remember to check model
mod_pr<- gam(Val ~ s(Year, k=10), data = P_Release)
summary(mod_pr) #check out model
gam.check(mod_pr)

pdata_pr <- with(P_Release, data.frame(Year = Year))
p2_mod_pr <- predict(mod_pr, newdata = pdata_pr,  type = "terms", se.fit = TRUE)
intercept_pr = 60.60754  # look at p2_mod and extract the intercept
pdata_pr <- transform(pdata_pr, p2_mod_pr = p2_mod_pr$fit[,1], se2 = p2_mod_pr$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_pr.d <- Deriv(mod_pr, n=42) # n is the number of years
mod_pr.dci <- confint(mod_pr.d, term = Term)
mod_pr.dsig <- signifD(pdata_pr$p2_mod_pr, d = mod_pr.d[[Term]]$deriv,
                    +                    mod_pr.dci[[Term]]$upper, mod_pr.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = P_Release)
lines(Val ~ Year, data = P_Release)
lines(p2_mod_pr+intercept_pr ~ Year, data = pdata_pr, type = "n")
lines(p2_mod_pr+intercept_pr ~ Year, data = pdata_pr)
lines(unlist(mod_pr.dsig$incr)+intercept_pr ~ Year, data = pdata_pr, col = "blue", lwd = 3)
lines(unlist(mod_pr.dsig$decr)+intercept_pr ~ Year, data = pdata_pr, col = "red", lwd = 3)

linearmod_pr<- lm(Val ~ Year, data=P_Release)
summary(linearmod_pr)

PR <- ggplot() + 
  geom_line(data = P_Release, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = P_Release, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = P_Release[42,], aes(x= Year, y = Val), shape = 17, size =3) +
  geom_smooth(data = P_Release, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_pr, aes(x = Year, y = p2_mod_pr+intercept_pr), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_pr, aes(y = unlist(mod_pr.dsig$incr)+intercept_pr, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_pr, aes(y = unlist(mod_pr.dsig$decr)+intercept_pr, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Percent", x = 'Year', title = 'Percent Released') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))
#


# HARVEST Creat a GAM - adjust k and remember to check model
mod_hr<- gam(Val ~ s(Year, k=12), data = Harv)
summary(mod_hr) #check out model
gam.check(mod_hr)

pdata_hr <- with(Harv, data.frame(Year = Year))
p2_mod_hr <- predict(mod_hr, newdata = pdata_hr,  type = "terms", se.fit = TRUE)
intercept_hr = 19105734   # look at p2_mod and extract the intercept
pdata_hr <- transform(pdata_hr, p2_mod_hr = p2_mod_hr$fit[,1], se2 = p2_mod_hr$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_hr.d <- Deriv(mod_hr, n=42) # n is the number of years
mod_hr.dci <- confint(mod_hr.d, term = Term)
mod_hr.dsig <- signifD(pdata_hr$p2_mod_hr, d = mod_hr.d[[Term]]$deriv,
                    +                    mod_hr.dci[[Term]]$upper, mod_hr.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = Harv)
lines(Val ~ Year, data = Harv)
lines(p2_mod_hr+intercept_hr ~ Year, data = pdata_hr, type = "n")
lines(p2_mod_hr+intercept_hr ~ Year, data = pdata_hr)
lines(unlist(mod_hr.dsig$incr)+intercept_hr ~ Year, data = pdata_hr, col = "blue", lwd = 3)
lines(unlist(mod_hr.dsig$decr)+intercept_hr ~ Year, data = pdata_hr, col = "red", lwd = 3)

linearmod_hr<- lm(Val ~ Year, data=Harv)
summary(linearmod_hr)

HR <- ggplot() + 
  geom_line(data = Harv, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = Harv, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = Harv[42,], aes(x=Year, y=Val), shape = 17, size = 3) +
  geom_smooth(data = Harv, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_hr, aes(x = Year, y = p2_mod_hr+intercept_hr), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_hr, aes(y = unlist(mod_hr.dsig$incr)+intercept_hr, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_hr, aes(y = unlist(mod_hr.dsig$decr)+intercept_hr, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Fish", x = 'Year', title = 'Recreational Harvest') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

## CATCH
mod_ct<- gam(Val ~ s(Year, k=10), data = Catch)
summary(mod_ct) #check out model
gam.check(mod_ct)

pdata_ct <- with(Catch, data.frame(Year = Year))
p2_mod_ct <- predict(mod_ct, newdata = pdata_ct,  type = "terms", se.fit = TRUE)
intercept_ct =  50422709  # look at p2_mod and extract the intercept
pdata_ct <- transform(pdata_ct, p2_mod_ct = p2_mod_ct$fit[,1], se2 = p2_mod_ct$se.fit[,1])

#  Now that we have the model prediction, the next step is to calculate the first derivative
#  Then determine which increases and decreases are significant
Term = "Year"
mod_ct.d <- Deriv(mod_ct, n=42) # n is the number of years
mod_ct.dci <- confint(mod_ct.d, term = Term)
mod_ct.dsig <- signifD(pdata_ct$p2_mod_ct, d = mod_ct.d[[Term]]$deriv,
                    +                    mod_ct.dci[[Term]]$upper, mod_ct.dci[[Term]]$lower)

# Take a quick look to make sure it appears ok before final plotting
plot(Val ~ Year, data = Catch)
lines(Val ~ Year, data = Catch)
lines(p2_mod_ct+intercept_ct ~ Year, data = pdata_ct, type = "n")
lines(p2_mod_ct+intercept_ct ~ Year, data = pdata_ct)
lines(unlist(mod_ct.dsig$incr)+intercept_ct ~ Year, data = pdata_ct, col = "blue", lwd = 3)
lines(unlist(mod_ct.dsig$decr)+intercept_ct ~ Year, data = pdata_ct, col = "red", lwd = 3)

linearmod_ct<- lm(Val ~ Year, data=Catch)
summary(linearmod_ct)

CT <- ggplot() + 
  geom_line(data = Catch, aes(x = Year, y = Val), color = 'grey53') +
  geom_point(data = Catch, aes(x = Year, y = Val), color = 'gray53') + 
  geom_point(data = Catch[42,], aes(x = Year, y = Val), shape = 17, size = 3) +
  geom_smooth(data = Catch, aes(x = Year, y = Val), method = lm, se = FALSE, color = 'black') + 
  geom_line(data=pdata_ct, aes(x = Year, y = p2_mod_ct+intercept_ct), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  geom_line(data = pdata_ct, aes(y = unlist(mod_ct.dsig$incr)+intercept_ct, x = Year), color = "blue", size = 1) + 
  geom_line(data = pdata_ct, aes(y = unlist(mod_ct.dsig$decr)+intercept_ct, x = Year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = "Number of Fish", x = '', title = 'Recreational Catch') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


#####

library(ggpubr)

ggarrange(CT, CR,HR,PR, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)