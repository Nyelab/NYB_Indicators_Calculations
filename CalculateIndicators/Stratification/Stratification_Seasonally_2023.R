#Purpose: Create Stratification INdicator
#Data: Use World Ocean Data (HowToGetWOD_data.Rmd)
#Data: Combine with SEAWOLF CTD casts (ProcessCTD_SeaWolf.R)
#Fit GAM and use year effect as indicator
#Kurt Heim, Last edited AUG 10, 2020
# Laura Gruenburg, last edited June 3, 2024

library(rgdal)
library(lubridate)
library(mgcv)
library(effects)
library(mgcViz)
library(stringr)
library(tidyr)

#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")

#######Load the datasets (This is in the NYOS megafolder on google drive)
setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
ddd<-read.csv("WOD_CTD_format_D50_June24_2020.csv", header = TRUE)
ddd$date<-as.Date(ddd$date)
ddd$data_source<-"WOD"

setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/")
ddd2<-read.csv("strat_wodb_2023.csv", header = TRUE)
ddd2$date<-as_date(as.character(ddd2$date))
ddd2$data_source<-"WOD"

setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
sea<-read.csv("CTD_seawolf_Dec12_2022.csv")
sea$date<-as.Date(sea$date)
sea$data_source<-"SEAWOLF"

sea2<-read.csv("CTD_seawolf_DEC5_2023.csv")
sea2$date<-as.Date(sea2$date)
sea2$data_source<-"SEAWOLF"

ddd$year<-year(ddd$date)
ddd2018 <- ddd[ddd$year < 2019, ] #update the last 5 years

ddd2 <- ddd2 %>% 
  rename(
    c50_den = c50_dens,
    surf_den = surf_dens
  )
#####Combine the two datasets

common_cols <- intersect(colnames(ddd2018), colnames(ddd2))
df<-rbind(
  subset(ddd2018, select = common_cols), 
  subset(ddd2, select = common_cols)
)

common_cols2 <- intersect(colnames(sea), colnames(sea2))
sea_tot<-rbind(
  subset(sea, select = common_cols2), 
  subset(sea2, select = common_cols2)
)

common_cols3 <- intersect(colnames(sea_tot), colnames(df))
tot<-rbind(
  subset(sea_tot, select = common_cols3), 
  subset(df, select = common_cols3)
)

#cacluate startification index for each CTD cast
tot$strat<-tot$c50_den/1000-tot$surf_den/1000

#Format date
tot$date<-as.Date(tot$date)
tot$year<-year(tot$date)
tot$month<-month(tot$date)
tot$day<-yday(tot$date)
tot$year_fac<-factor(tot$year)



###use the function to label
lazylab<-klab(tot$lat, tot$lon)
ndf<-data.frame(tot, lazylab)
ndf$EPU<-as.character(ndf$EPU)
###Label casts not in an EPU
ndf[is.na(ndf$EPU),"EPU"]<-"Not in NES"


###remove values that are funky
hist(ndf$strat)
ndf<-ndf[ndf$strat > 0,]#####REMOVE NEGATIVE VALUES
hist(ndf$strat)#some are very far on left tail...but


########ANALYSIS FOR THE NYB##########
NYB<-ndf[ndf$NYB %in% "NYB" == TRUE,]
nrow(NYB)#8980 ##8498 casts
minN<-10##min number of obs per year (you choose) to retain data for an estimate. 
ntab<-table(NYB$year_fac)#make a table
ntab
remove_these<-names(ntab[ntab < minN])#remove below minN
###subdat1 removes data for years with less than minNcasts and is used in analysis
subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
nrow(subdat1)#8971 #this is how many casts are used 8383

######Display Data Density + TRIM
table(subdat1$year)#casts per year for data used in analysis
table(NYB$year)#cast per year for whole NYB
#plot of each year with number of casts per month
par(mfrow=c(2,2))
yearz<-unique(NYB$year)
for(i in 1:length(yearz)){
  #i = 1
  daYear<-NYB[NYB$year == yearz[i],]
  barplot(table(daYear$month), main = paste(yearz[i], " ", nrow(daYear)))
}
dev.off()

#seawolf data vs. WOD data
table(NYB$data_source, NYB$year)
table(NYB$data_source)

subdat1$season<-NA
subdat1[subdat1$day %in% 1:90 == TRUE,"season"]<-"winter"
subdat1[subdat1$day %in% 91:181 == TRUE,"season"]<-"spring"
subdat1[subdat1$day %in% 182:273 == TRUE,"season"]<-"summer"
subdat1[subdat1$day %in% 274:366 == TRUE,"season"]<-"fall"
table(subdat1$season)

# WINTER
winter <- subdat1[subdat1$season == 'winter', ]
NYB_winter<-gam(strat ~ s(lon,lat, k = 100)  +  year_fac,  data = winter)
summary(NYB_winter)#check out model
gam.check(NYB_winter)
par(mfrow = c(1,2))
plot(NYB_winter)#month effect is interesting

plot(NYB_winter$fitted.values, NYB_winter$y, pch = 19, cex = .3,
     col = factor(winter$data_source))
abline(0,1)

regexp <- "[[:digit:]]+"
index_vals<-NYB_winter$coefficients[grepl("year_fac",names(NYB_winter$coefficients))]
intercept_winter<- NYB_winter$coefficients[grepl("(Intercept)",names(NYB_winter$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add in first year as a 0
indexNYB_winter<-rbind(data.frame(years = min(winter$year, na.rm = TRUE),index_vals = 0), index_1)
fNYB_winter<-data.frame(Year = indexNYB_winter$years,
                        Variable = "strat",
                        Intercept = intercept_winter,
                        Val = scale(indexNYB_winter$index_vals, scale = FALSE),
                        Loc = "NYB",
                        N = as.vector(table(winter$year)))
plot(fNYB_winter$Year, scale(fNYB_winter$Val, scale = FALSE), type = "b")
plot(fNYB_winter$Year,fNYB_winter$Intercept + fNYB_winter$Val, type = "b")
abline(h = 0)
abline(v=2003)


# spring
spring <- subdat1[subdat1$season == 'spring', ]
NYB_spring<-gam(strat ~ s(lon,lat, k = 100)  +  year_fac,  data = spring)
summary(NYB_spring)#check out model
gam.check(NYB_spring)
par(mfrow = c(1,2))
plot(NYB_spring)#month effect is interesting

plot(NYB_spring$fitted.values, NYB_spring$y, pch = 19, cex = .3,
     col = factor(spring$data_source))
abline(0,1)

regexp <- "[[:digit:]]+"
index_vals<-NYB_spring$coefficients[grepl("year_fac",names(NYB_spring$coefficients))]
intercept_spring<- NYB_spring$coefficients[grepl("(Intercept)",names(NYB_spring$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add in first year as a 0
indexNYB_spring<-rbind(data.frame(years = min(spring$year, na.rm = TRUE),index_vals = 0), index_1)
fNYB_spring<-data.frame(Year = indexNYB_spring$years,
                        Variable = "strat",
                        Intercept = intercept_spring,
                        Val = scale(indexNYB_spring$index_vals, scale = FALSE),
                        Loc = "NYB",
                        N = as.vector(table(spring$year)))
plot(fNYB_spring$Year, scale(fNYB_spring$Val, scale = FALSE), type = "b")
plot(fNYB_spring$Year, fNYB_spring$Intercept + fNYB_spring$Val, type = 'b')
abline(h = 0)
abline(v=2003)

# summer
summer <- subdat1[subdat1$season == 'summer', ]
NYB_summer<-gam(strat ~ s(lon,lat, k = 100)  +  year_fac,  data = summer)
summary(NYB_summer)#check out model
gam.check(NYB_summer)
par(mfrow = c(1,2))
plot(NYB_summer)#month effect is interesting

plot(NYB_summer$fitted.values, NYB_summer$y, pch = 19, cex = .3,
     col = factor(summer$data_source))
abline(0,1)

regexp <- "[[:digit:]]+"
index_vals<-NYB_summer$coefficients[grepl("year_fac",names(NYB_summer$coefficients))]
intercept_summer<- NYB_summer$coefficients[grepl("(Intercept)",names(NYB_summer$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add in first year as a 0
indexNYB_summer<-rbind(data.frame(years = min(summer$year, na.rm = TRUE),index_vals = 0), index_1)
fNYB_summer<-data.frame(Year = indexNYB_summer$years,
                        Variable = "strat",
                        Intercept = intercept_summer,
                        Val = scale(indexNYB_summer$index_vals, scale = FALSE),
                        Loc = "NYB",
                        N = as.vector(table(summer$year)))
plot(fNYB_summer$Year, scale(fNYB_summer$Val, scale = FALSE), type = "b")
plot(fNYB_summer$Year, fNYB_summer$Intercept + fNYB_summer$Val, type = "b")
abline(h = 0)
abline(v=2003)

# fall
fall <- subdat1[subdat1$season == 'fall', ]
NYB_fall<-gam(strat ~ s(lon,lat, k = 100)  +  year_fac,  data = fall)
summary(NYB_fall)#check out model
gam.check(NYB_fall)
par(mfrow = c(1,2))
plot(NYB_fall)#month effect is interesting

plot(NYB_fall$fitted.values, NYB_fall$y, pch = 19, cex = .3,
     col = factor(fall$data_source))
abline(0,1)

regexp <- "[[:digit:]]+"
index_vals<-NYB_fall$coefficients[grepl("year_fac",names(NYB_fall$coefficients))]
intercept_fall<- NYB_fall$coefficients[grepl("(Intercept)",names(NYB_fall$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add in first year as a 0
indexNYB_fall<-rbind(data.frame(years = min(fall$year, na.rm = TRUE),index_vals = 0), index_1)
fNYB_fall<-data.frame(Year = indexNYB_fall$years,
                      Variable = "strat",
                      Intercept = intercept_fall,
                      Val = scale(indexNYB_fall$index_vals, scale = FALSE),
                      Loc = "NYB",
                      N = as.vector(table(fall$year)))
plot(fNYB_fall$Year, scale(fNYB_fall$Val, scale = FALSE), type = "b")
plot(fNYB_fall$Year, fNYB_fall$Intercept + fNYB_fall$Val, type = 'b')
abline(h = 0)
abline(v=2003)

fNYB_winter$season<-'winter'
fNYB_spring$season<-'spring'
fNYB_summer$season<-'summer'
fNYB_fall$season<-'fall'

fNYB <- rbind(fNYB_winter, fNYB_spring, fNYB_summer, fNYB_fall)

setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_timeseries_figures/Timeseries_Files_2023")
write.csv(fNYB, "Strat_seasonally_Jan_10_2024.csv")



#### After this is old.  If you continue to do stratification seasonally you can stop here

#####Calculate NYB indciaotr with a GAM
NYB_m1<-gam(log(strat) ~ s(lon,lat, k = 100) + s(month) +  year_fac, 
        data = subdat1)
summary(NYB_m1)#check out model
gam.check(NYB_m1)
par(mfrow = c(1,2))
plot(NYB_m1)#month effect is interesting

par(mfrow = c(1,1))
plot(NYB_m1, select = 2, main = "effect of Month on stratification in NYB")
abline(h = 0)

####Grab the coefficient for the indicator
regexp <- "[[:digit:]]+"
index_vals<-NYB_m1$coefficients[grepl("year_fac",names(NYB_m1$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add in first year as a 0
indexNYB<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)

###Format for use 
fNYB<-data.frame(Year = indexNYB$years,
                Variable = "Strat_insitu",
                Val = scale(indexNYB$index_vals, scale = FALSE),
                Loc = "NYB",
                N = as.vector(table(subdat1$year)))
plot(fNYB$Year, fNYB$Val, type = "b")

#######Analysis for the MAB########
#repeate steps above but with data for only the MAB
NYB<-ndf[ndf$EPU %in% "MAB" == TRUE,]
nrow(NYB)##20887 casts
minN<-10##min number of obs per year
ntab<-table(NYB$year_fac)#make a table
ntab
remove_these<-names(ntab[ntab < minN])#remove below minN
subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
nrow(subdat1)


######Display Data Density + TRIM
table(subdat1$year)
table(NYB$year)
par(mfrow=c(5,5))
yearz<-unique(NYB$year)
for(i in 1:length(yearz)){
  #i = 1
  daYear<-NYB[NYB$year == yearz[i],]
  barplot(table(daYear$month), main = paste(yearz[i], " ", nrow(daYear)))
}
dev.off()

######Calculate indicator with a GAM
MAB_m1<-gam(log(strat) ~ s(lon,lat, k = 100) + s(month) +  year_fac, 
            data = subdat1)
summary(MAB_m1)#check out model
gam.check(MAB_m1)
par(mfrow = c(1,2))
plot(MAB_m1)

#######Grab the coefficient for the indicator
regexp <- "[[:digit:]]+"
index_vals<-MAB_m1$coefficients[grepl("year_fac",names(MAB_m1$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add it in
indexMAB<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)


MAB<-data.frame(Year = indexMAB$years,
                Variable = "Strat_insitu",
                Val = scale(indexMAB$index_vals, scale = FALSE),
                Loc = "MAB",
                N = as.vector(table(subdat1$year)))

plot(MAB$Year, MAB$Val, type = "b")

########Analysis for the whole NES##########
NYB<-ndf[ndf$EPU %in% "Not in NES" == FALSE,]
nrow(NYB)##58672
NYB<-NYB[is.na(NYB$year) == FALSE,]
minN<-10##min number of obs per year
ntab<-table(NYB$year_fac)#make a table
ntab
remove_these<-names(ntab[ntab < minN])#remove below minN
subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
nrow(subdat1)


######Display Data Density + TRIM
table(subdat1$year)
table(NYB$year)
par(mfrow=c(5,5))
yearz<-unique(NYB$year)
for(i in 1:length(yearz)){
  #i = 1
  daYear<-NYB[NYB$year == yearz[i],]
  barplot(table(daYear$month), main = paste(yearz[i], " ", nrow(daYear)))
}
dev.off()

######DO THE ANALYSIS#############
NES_m1<-gam(log(strat) ~ s(lon,lat, k = 100) + s(month) +  year_fac, 
            data = subdat1)
summary(NES_m1)#check out model
gam.check(NES_m1)
par(mfrow = c(1,2))
plot(NES_m1)

#########Grab the coefficient for the indicator
library(stringr)
regexp <- "[[:digit:]]+"
index_vals<-NES_m1$coefficients[grepl("year_fac",names(NES_m1$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add it in
indexNES<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)


NES<-data.frame(Year = indexNES$years,
                Variable = "Strat_insitu",
                Val = scale(indexNES$index_vals, scale = FALSE),
                Loc = "NES",
                N = as.vector(table(subdat1$year)))
plot(NES$Year, NES$Val, type = "l")

####PLOT and COMPARE THE TWO####
plot(MAB$Year, MAB$Val, type = "b")
points(fNYB$Year, fNYB$Val, type = "b", col = "red")
points(NES$Year, NES$Val, type = "b", col = "green")
abline(h=0)
abline(v = 2012)


#####Write to disk
fff<-rbind(MAB,fNYB, NES)

###write to csv
setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(fNYB, "Strat_insitu_DEC_12_2022.csv")
