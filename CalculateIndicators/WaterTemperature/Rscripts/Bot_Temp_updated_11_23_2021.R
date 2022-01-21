#Purpose: Create Bottom Temp Indicator
#Data: Use World Ocean Data (HowToGetWOD_data_UPDATE.Rmd)
#Data: Combine with SEAWOLF CTD casts (ProcessCTD_SeaWolf.R)
#Fit GAM and use year effect as indicator
#Kurt Heim, Last edited AUG 10, 2020
#Laura Gruenburg, Last edited AUG 31,2021

#Janet noted 2003 should be cold, but it appears warm so looking into it
#DEC 11, 2020

library(rgdal)
library(lubridate)
library(mgcv)
library(effects)
library(mgcViz)
library(stringr)
#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")

#######Load the datasets
setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
ddd<-read.csv("WOD_CTD_XBT_MBT_GLD_August30_2021.csv", header = TRUE)
colnames(ddd)[18] <- "bot_temp"
ddd$date <- as.character(ddd$date)
ddd$date<-as.Date(ddd$date,tryFormats = c("%Y%m%d"))
ddd$date<-as.Date(ddd$date)
ddd$data_source<-"WOD"

sea<-read.csv("CTD_seawolf_NOV23_2021.csv")
sea$date<-as.Date(sea$date)
sea$data_source<-"SEAWOLF"


common_cols <- intersect(colnames(ddd), colnames(sea))
ddd<-rbind(
  subset(ddd, select = common_cols), 
  subset(sea, select = common_cols)
)
#common_cols <- intersect(colnames(ddd), colnames(xxx))
#ddd<-rbind(
#  subset(ddd, select = common_cols), 
#  subset(xxx, select = common_cols)
#)



#cacluate startification index for each CTD cast
#ddd$strat<-ddd$c50_den/1000-ddd$surf_den/1000

#Format date
ddd$date<-as.Date(ddd$date)
ddd$year<-year(ddd$date)
ddd$month<-month(ddd$date)
ddd$day<-yday(ddd$date)
ddd$year_fac<-factor(ddd$year)



###use the function to label
lazylab<-klab(ddd$lat, ddd$lon)
ndf<-data.frame(ddd, lazylab)
ndf$EPU<-as.character(ndf$EPU)
###Label casts not in an EPU
ndf[is.na(ndf$EPU),"EPU"]<-"Not in NES"


########ANALYSIS FOR THE NYB##########
NYB<-ndf[ndf$NYB %in% "NYB" == TRUE,]
nrow(NYB)##22775 (old) now 38088
minN<-20##min number of obs per year (you choose) to retain data for an estimate. 
ntab<-table(NYB$year_fac)#make a table
ntab
remove_these<-names(ntab[ntab < minN])#remove below minN
###subdat1 removes data for years with less than minNcasts and is used in analysis
subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
nrow(subdat1)#this is how many casts are used -> 38186

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


subdat1<-subdat1[subdat1$bot_temp >0,]

#####Investigate warm temps
hist(subdat1[subdat1$data_source == "SEAWOLF","bot_temp"])
hist(subdat1[subdat1$data_source == "WOD","bot_temp"])
#The XBT data have some very warm temps.
#hist(subdat1[subdat1$data_source == "WOD_xbt","bot_temp"])

###I will censor the XBT data to the warmest temps observed at the other more reliable datasets
#warmWOD<-max(subdat1[subdat1$data_source == "WOD","bot_temp"], na.rm = TRUE)
#warmXBT<-subdat1[subdat1$data_source == "WOD_xbt" & subdat1$bot_temp > warmWOD,]

#subdat1<-subdat1[subdat1$bot_temp < warmWOD,]
subdat1<-subdat1[subdat1$bot_temp > 0,]
nrow(subdat1)
hist(subdat1$bot_temp)

#####Calculate NYB indciaotr with a GAM
NYB_m1<-gam(bot_temp ~ s(lon,lat, k = 100) + s(month) +  year_fac,  data = subdat1)
summary(NYB_m1)#check out model
gam.check(NYB_m1)
par(mfrow = c(1,2))
plot(NYB_m1)#month effect is interesting

plot(NYB_m1$fitted.values, NYB_m1$y, pch = 19, cex = .3,
     col = factor(subdat1$data_source))
abline(0,1)

####Grab the coefficient for the indicator
regexp <- "[[:digit:]]+"
index_vals<-NYB_m1$coefficients[grepl("year_fac",names(NYB_m1$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add in first year as a 0
indexNYB<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)

###Format for use 
fNYB<-data.frame(Year = indexNYB$years,
                 Variable = "BT_insitu",
                 Val = scale(indexNYB$index_vals, scale = FALSE),
                 Loc = "NYB",
                 N = as.vector(table(subdat1$year)))
plot(fNYB$Year, scale(fNYB$Val, scale = FALSE), type = "b")
abline(h = 0)
abline(v=2003)

#######Analysis for the MAB########
#repeate steps above but with data for only the MAB
NYB<-ndf[ndf$EPU %in% "MAB" == TRUE,]
nrow(NYB)##20887 casts
minN<-20##min number of obs per year
ntab<-table(NYB$year_fac)#make a table
ntab
remove_these<-names(ntab[ntab < minN])#remove below minN
subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
nrow(subdat1)

#remove some warm temps from XBT data
hist(subdat1[subdat1$data_source == "WOD_xbt", "bot_temp"])
hist(subdat1[subdat1$data_source == "WOD", "bot_temp"])
hist(subdat1[subdat1$data_source == "SEAWOLF", "bot_temp"])

maxT<-max(subdat1[subdat1$data_source == "WOD", "bot_temp"], na.rm = TRUE)
subdat1<-subdat1[subdat1$bot_temp < maxT,]

######Display Data Density + TRIM
table(subdat1$year)
table(NYB$year)
par(mfrow=c(2,2))
yearz<-unique(NYB$year)
for(i in 1:length(yearz)){
  #i = 1
  daYear<-NYB[NYB$year == yearz[i],]
  barplot(table(daYear$month), main = paste(yearz[i], " ", nrow(daYear)))
}
dev.off()

######Calculate indicator with a GAM
MAB_m1<-gam(bot_temp ~ s(lon,lat, k = 100) + s(month) +  year_fac, 
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
                Variable = "BT_insitu",
                Val = scale(indexMAB$index_vals, scale = FALSE),
                Loc = "MAB",
                N = as.vector(table(subdat1$year)))
plot(MAB$Year, MAB$Val, type = "b")


########Analysis for the whole NES##########
NYB<-ndf[ndf$EPU %in% "Not in NES" == FALSE,]
nrow(NYB)##58672

minN<-20##min number of obs per year
ntab<-table(NYB$year_fac)#make a table
ntab
remove_these<-names(ntab[ntab < minN])#remove below minN
subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
nrow(subdat1)

###remove warm temps
#remove some warm temps from XBT data
hist(subdat1[subdat1$data_source == "WOD_xbt", "bot_temp"])
hist(subdat1[subdat1$data_source == "WOD", "bot_temp"])
hist(subdat1[subdat1$data_source == "SEAWOLF", "bot_temp"])

maxT<-max(subdat1[subdat1$data_source == "WOD", "bot_temp"], na.rm = TRUE)
maxT
subdat1<-subdat1[subdat1$bot_temp < maxT,]


######Display Data Density + TRIM
table(subdat1$year)
table(NYB$year)
par(mfrow=c(2,2))
yearz<-unique(NYB$year)
for(i in 1:length(yearz)){
  #i = 1
  daYear<-NYB[NYB$year == yearz[i],]
  barplot(table(daYear$month), main = paste(yearz[i], " ", nrow(daYear)))
}
dev.off()

######DO THE ANALYSIS#############
NES_m1<-gam(bot_temp ~ s(lon,lat, k = 100) + s(month) +  year_fac, 
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
                Variable = "BT_insitu",
                Val = scale(indexNES$index_vals, scale = FALSE),
                Loc = "NES",
                N = as.vector(table(subdat1$year)))
plot(NES$Year, NES$Val, type = "b")

####PLOT and COMPARE THE TWO####
dev.off()

plot(MAB$Year, MAB$Val, type = "b")
points(fNYB$Year, fNYB$Val, type = "b", col = "red")
points(NES$Year, NES$Val, type = "b", col = "green")
abline(v = 2003)



#####Write to disk
fff<-rbind(MAB,fNYB, NES)

###write to csv
#setwd("~/Desktop/NYB Indicators/Final_timeseries")
#write.csv(fff, "BT_insitu_DEC_13_2020.csv")

#saving just for NYB indicator time series
setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(fNYB, "BT_insitu_Nov_23_2021.csv")


####compare to NOAA version#####
library(ecodata)
T<-ecodata::oceantemp_insitu
MAB<-T[T$EPU == "MAB",]
MAB<-MAB[MAB$Var == "bottom temp anomaly in situ",]
MAB$Year<-MAB$Time
waa<-merge(MAB, fNYB, by = "Year", all = TRUE)
plot(waa$Year, waa$Val, type = "b")
points(waa$Year, waa$Value, type = "b", col = "green")

setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(fNYB,'BT_insituNYB_wGLDMBT_AUG31_2021.csv')

linearMod<- lm(bot_temp ~ date, data=subdat1)
gam(bot_temp ~ s(lon,lat, k = 100) + s(month) +  year_fac, 
    data = subdat1)
summary(gam_mod)#check out model
gam.check(gam_mod)

ggplot(subdat1, aes(x = date, y =bot_temp)) + 
  geom_point(color='black') + 
  geom_smooth(method = lm, se = FALSE, color = 'blue') + 
  geom_smooth(method = "gam", data=subdat1, formula = y ~s(lon,lat, k = 100) + s(month) +  year_fac, se = FALSE, color = 'red', linetype = 'twodash') + 
  labs (y = "Temp", x = 'Year', title = 'Summer Surface Temp') + 
  theme(title=element_text(size = 16, face = 'bold'), axis.title=element_text(size = 14), axis.text= element_text(color = 'black', size = 12))