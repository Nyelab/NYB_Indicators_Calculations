###########################
#######Heim et al. 2020####
###########################
###MOST COMMON ZOOPS IN NYB


######Prepare the dataset######
library(readxl)
library(lubridate)
library(rgdal)
library(nngeo)
library(reshape2)

#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")


setwd("~/Desktop/NYB Indicators/CalculateIndicators/Zooplankton/Data")
ZPD=openxlsx::read.xlsx("L1_EcoMon_Plankton_Data_v3_5.xlsx", sheet='Data')


dt=as_date(ZPD$date, origin = "1899-12-30")
DOY=yday(dt) #day of year
month=as.numeric(format(dt, '%m'))
year=as.numeric(format(dt, '%Y'))
ZPD$year=year
ZPD$month=month
ZPD$dt=dt
ZPD$DOY=DOY
ZPD$day=as.numeric(format(dt, '%d'))
ZPD$lat2=ceiling(ZPD$lat) #use for binning into 1 degree bins for removal of undersampled bins
ZPD$lon2=floor(ZPD$lon) #use for binning into 1 degree bins for removal of undersampled bins




######REORDER
ZPDb=ZPD[,c(1:14, 290:296, 106:197)] #####CAHNGED 297 to 296
ZPDb=ZPDb[order(ZPDb$date),]
ZPDb=ZPDb[which(ZPDb$year > 1976),] # remove NA data in years prior to 1977


###label according to NYB
#####CREATE MY LABELS
lazylab<-klab(ZPDb$lat,ZPDb$lon)
ZPDb<-data.frame(ZPDb, lazylab)


nrow(ZPDb)
nrow(ZPDb[ZPDb$NYB %in% "NYB" == TRUE,])
ZPDb<-ZPDb[ZPDb$NYB %in% "NYB" == TRUE,]



########Select only taxa present in yearly data > x percent of samples
X=20 # percent criteria to use as minimum percent in samples
ZPDa=ZPDb
ZPDa=ZPDa[!is.na(ZPDa$zoo_gear),] # Remove NA in zooplankton rows

# Reduce to taxa occurrance > x percent in samples
p.a=ZPDa[,23:113]
p.a[p.a > 0]=1 # presence/absence
count=colSums(p.a)
pct=(count/dim(ZPDa)[1])*100
crit=which(pct>X)
ZPDa=ZPDa[c(1:22,crit+22)]

count<-count

#write out to csv
write.csv(count, "TopZoopzNYB.csv")

