###########################
#######Heim et al. 2020####
###########################
#Put together the final indicator data
library(nngeo)
library(rgdal)
library(sf)
library(ggplot2)
library(lubridate)
library(rerddap)
library(plotdap)
library(rerddapXtracto)
library(reshape2)
library(dplyr)

#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")


########GET THE DATA########
########First get OCCI
xcoord<-c(-77, -65.66667)
ycoord<-c(35.8327,44.66667)
dataInfo <- rerddap::info('pmlEsaCCI42OceanColor8Day')#OC-CCI


# This identifies the parameter to choose - there are > 60 in this dataset1 
parameter <- 'chlor_a'

global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

# if you encouter an error reading the nc file clear the rerrdap cache: 
rerddap::cache_delete_all(force = TRUE)

#started at 1:53 pm, how long does it take? nice took 1 minute
chlOCCCI<-rxtracto_3D(dataInfo,parameter=parameter,
                      tcoord=tcoord,
                      xcoord=xcoord,ycoord=ycoord)

#######Make it shorter
ddd<-chlOCCCI#make shorter name
#ddd<-chlMODIS
#str(ddd)
var<-ddd$chlor_a
dimnames(var)<-list(Longitude=ddd$longitude, Latitude=ddd$latitude, time = as.character(ddd$time))
OC<-melt(var, value.name = "Chl")



#####apply labels OCCI
#create df of unique points in the dataset
OC$ll<-paste(OC$Latitude, OC$Longitude)
ptzOC<-OC[!duplicated(OC$ll),]

#label these and add the ll to it
lazylab<-klab(ptzOC$Latitude, ptzOC$Longitude)


lazylab$ll<-ptzOC$ll
#merge back to the orginal dataframe
OCfin<-inner_join(OC,lazylab, by = "ll")


OCfin$EPU<-as.character(OCfin$EPU)
OCfin[is.na(OCfin$EPU), "EPU"]<-"Not in NES"




#####Format the data frame#####
OCfin$date<-as.Date(OCfin$time)
OCfin$year<-year(OCfin$date)
OCfin$month<-month(OCfin$date)
OCfin$day<-day(OCfin$date)
OCfin$yday<-yday(OCfin$date)



#####NYB indicators########
NYB<-OCfin[OCfin$NYB %in% "NYB" == TRUE,]
yearlyNYB<-aggregate(Chl ~ year, NYB, FUN = mean)
plot(yearlyNYB$year, yearlyNYB$Chl, ylim = c(0.5,1.5),
     pch = 19, type = "l")

NYB<-OCfin[OCfin$EPU %in% "MAB" == TRUE,]
yearlyMAB<-aggregate(Chl ~ year, NYB, FUN = mean)
points(yearlyMAB$year, yearlyMAB$Chl, pch = 19, col ="red", type = "l")


NYB<-OCfin[OCfin$EPU %in% "MAB" == TRUE,]
yearlyMAB<-aggregate(Chl ~ year, NYB, FUN = mean)





daily<-aggregate(Chl ~ date + month + year + yday, NYB, FUN = mean)
plot(daily$date, daily$Chl, type = "l")

Yrz<-unique(daily$year)
for(i in 1:length(Yrz)){
  plot(daily[daily$year == Yrz[i], "yday"],
       daily[daily$year == Yrz[i], "Chl"], type = "l",
       ylim = c(0,2))
}

