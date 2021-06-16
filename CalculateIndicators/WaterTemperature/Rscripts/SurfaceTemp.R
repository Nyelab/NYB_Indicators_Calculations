#Purpose: Create Sea Surface Temp Indicator
#Data: Use World Ocean Data (HowToGetWOD_data.Rmd)
#Data: Combine with SEAWOLF CTD casts (ProcessCTD_SeaWolf.R)
#Fit GAM and use year effect as indicator
#Kurt Heim, Last edited AUG 10, 2020
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
ddd<-read.csv("WOD_CTD_format_D50_June24_2020.csv", header = TRUE)
ddd$date<-as.Date(ddd$date)
ddd$data_source<-"WOD_ctd"

setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
xxx<-read.csv("WOD_XBT_format_D50_AUG_18_2020.csv", header = TRUE)
xxx$date<-as.Date(xxx$date)
xxx$data_source<-"WOD_xbt"


sea<-read.csv("CTD_seawolf_AUG6.csv")
sea$date<-as.Date(sea$date)
sea$data_source<-"SEAWOLF"



#####Combine the two datasets
common_cols <- intersect(colnames(ddd), colnames(sea))
ddd<-rbind(
  subset(ddd, select = common_cols), 
  subset(sea, select = common_cols)
)
common_cols <- intersect(colnames(ddd), colnames(xxx))
ddd<-rbind(
  subset(ddd, select = common_cols), 
  subset(xxx, select = common_cols)
)



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
nrow(NYB)##8498 casts
table(NYB$data_source)
minN<-10##min number of obs per year (you choose) to retain data for an estimate. 
ntab<-table(NYB$year_fac)#make a table
ntab
remove_these<-names(ntab[ntab < minN])#remove below minN
###subdat1 removes data for years with less than minNcasts and is used in analysis
subdat1<-NYB[NYB$year_fac %in% remove_these == FALSE,]
nrow(subdat1)#this is how many casts are used

######Display Data Density + TRIM
table(subdat1$year)#casts per year for data used in analysis
table(NYB$year)#cast per year for whole NYB
#plot of each year with number of casts per month
par(mfrow=c(5,5))
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


#####Calculate NYB indciaotr with a GAM
NYB_m1<-gam(surf_temp ~ s(lon,lat, k = 100) + s(month) +  year_fac, 
        data = subdat1)
summary(NYB_m1)#check out model
gam.check(NYB_m1)
par(mfrow = c(1,2))
plot(NYB_m1)#month effect is interesting

####Grab the coefficient for the indicator
regexp <- "[[:digit:]]+"
index_vals<-NYB_m1$coefficients[grepl("year_fac",names(NYB_m1$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add in first year as a 0
indexNYB<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)

###Format for use 
fNYB<-data.frame(Year = indexNYB$years,
                Variable = "SST_insitu",
                Val = scale(indexNYB$index_vals, scale = FALSE),
                Loc = "NYB",
                N = as.vector(table(subdat1$year)))
plot(fNYB$Year, fNYB$Val, type = "b")


#######Analysis for the MAB########
#repeate steps above but with data for only the MAB
NYB<-ndf[ndf$EPU %in% "MAB" == TRUE,]
nrow(NYB)##20887 casts
table(NYB$data_source)
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
MAB_m1<-gam(surf_temp ~ s(lon,lat, k = 100) + s(month) +  year_fac, 
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
                Variable = "SST_insitu",
                Val = scale(indexMAB$index_vals, scale = FALSE),
                Loc = "MAB",
                N = as.vector(table(subdat1$year)))

plot(MAB$Year, MAB$Val, type = "b")

########Analysis for the whole NES##########
NYB<-ndf[ndf$EPU %in% "Not in NES" == FALSE,]
nrow(NYB)##104249
table(NYB$data_source)

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
NES_m1<-gam(surf_temp ~ s(lon,lat, k = 100) + s(month) +  year_fac, 
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
                Variable = "SST_insitu",
                Val = scale(indexNES$index_vals, scale = FALSE),
                Loc = "NES",
                N = as.vector(table(subdat1$year)))


####PLOT and COMPARE THE TWO####
plot(MAB$Year, scale(MAB$Val), type = "b")
points(fNYB$Year, scale(fNYB$Val), type = "b", col = "red")
points(NES$Year, scale(NES$Val), type = "b", col = "green")
abline(h=0)

plot(MAB$Year, MAB$Val, type = "b")
points(fNYB$Year, fNYB$Val, type = "b", col = "red")
points(NES$Year, NES$Val, type = "b", col = "green")
abline(h=0)



plot(fNYB$Year, scale(fNYB$Val), type = "b", xlim = c(1970, 2020),
     ylab = "NYB surface temperature index", pch = 19 , xlab = NA,
     main = "Surface water trend in the NYB (0.297 degrees/decade)")
mylm<-lm(Val ~ Year, fNYB)
summary(mylm)
abline(mylm$coefficients, col = "red")

#####Write to disk
fff<-rbind(MAB,fNYB, NES)

###write to csv
setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(fff, "SST_insitu_AUG_18_2020.csv")



