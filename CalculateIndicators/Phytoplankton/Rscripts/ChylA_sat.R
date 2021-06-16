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

#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")


########GET THE DATA########
########First get OCCI
xcoord<-c(-77, -65.66667)
ycoord<-c(35.8327,44.66667)
dataInfo <- rerddap::info('pmlEsaCCI31OceanColorMonthly')#OC-CCI



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





#######GET SEAWIFS
#dataInfo <- rerddap::info('erdSWchlamday')
# This identifies the parameter to choose - there are > 60 in this dataset1 
#parameter <- dataInfo$variables$variable_name
#zcoord<-0
#global <- dataInfo$alldata$NC_GLOBAL
#tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
#tcoord <- c(tt[2],"last")

# if you encouter an error reading the nc file clear the rerrdap cache: 
#rerddap::cache_delete_all(force = TRUE)

#started at 1:53 pm, how long does it take? nice took 1 minute
#chlSeaWifs<-rxtracto_3D(dataInfo,parameter=parameter,
#                      tcoord=tcoord,
#                      xcoord=xcoord,ycoord=ycoord, zcoord = zcoord)

#######Make it shorter
#ddd<-chlSeaWifs#make shorter name
#ddd$chlorophyll<-drop(ddd$chlorophyll)
#ddd<-chlMODIS
#str(ddd)
#var<-ddd$chlorophyll
#dimnames(var)<-list(Longitude=ddd$longitude, Latitude=ddd$latitude, time = as.character(ddd$time))
#SW<-melt(var, value.name = "Chl")




#####apply labels OCCI
#create df of one point per
OC$ll<-paste(OC$Latitude, OC$Longitude)
ptzOC<-OC[!duplicated(OC$ll),]
#label these and add the ll to it
lazylab<-klab(ptzOC$Latitude, ptzOC$Longitude)
lazylab$ll<-ptzOC$ll
#merge back to the orginal dataframe
OCfin<-merge(OC,lazylab, by = "ll")
OCfin$EPU<-as.character(OCfin$EPU)
OCfin[is.na(OCfin$EPU), "EPU"]<-"Not in NES"

#####Format the data frame#####
OCfin$date<-as.Date(OCfin$time)
OCfin$year<-year(OCfin$date)
OCfin$month<-month(OCfin$date)
OCfin$day<-day(OCfin$date)
OCfin$yday<-yday(OCfin$date)



#####indicators YEARLY########
NYB<-OCfin[OCfin$NYB %in% "NYB" == TRUE,]
yearlyNYB<-aggregate(Chl ~ year, NYB, FUN = mean)
plot(yearlyNYB$year, yearlyNYB$Chl, ylim = c(0.5,1.5),
     pch = 19, type = "l")

NYB<-OCfin[OCfin$EPU %in% "MAB" == TRUE,]
yearlyMAB<-aggregate(Chl ~ year, NYB, FUN = mean)
points(yearlyMAB$year, yearlyMAB$Chl, pch = 19, col ="red", type = "l")


NYB<-OCfin[OCfin$EPU != "Not in NES",]
yearlyNES<-aggregate(Chl ~ year, NYB, FUN = mean)
points(yearlyNES$year, yearlyNES$Chl, pch = 19, col ="green", type = "l")



#####CREATGE INDICATOR
YNYB<-data.frame(Year = yearlyNYB$year,
                    Variable = "ChylA_sat",
                    Val = yearlyNYB$Chl,
                    Loc = "NYB",
                    N = "NA")
YMAB<-data.frame(Year = yearlyMAB$year,
                 Variable = "ChylA_sat",
                 Val = yearlyMAB$Chl,
                 Loc = "MAB",
                 N = "NA")
YNES<-data.frame(Year = yearlyNES$year,
                 Variable = "ChylA_sat",
                 Val = yearlyNES$Chl,
                 Loc = "NES",
                 N = "NA")





####MONTHLY
NYB<-OCfin[OCfin$NYB %in% "NYB" == TRUE,]
monthlyNYB<-aggregate(Chl ~ year + month, NYB, FUN = mean)

NYB<-OCfin[OCfin$EPU %in% "MAB" == TRUE,]
monthlyMAB<-aggregate(Chl ~ year + month, NYB, FUN = mean)

NYB<-OCfin[OCfin$EPU != "Not in NES",]
monthlyNES<-aggregate(Chl ~ year + month, NYB, FUN = mean)



#####CREATGE INDICATOR
mNYB<-data.frame(Year = monthlyNYB$year,
                 Variable = paste("ChylA_sat_Monthly", monthlyNYB$month, sep = "_"),
                 Val = monthlyNYB$Chl,
                 Loc = "NYB",
                 N = "NA")
mMAB<-data.frame(Year = monthlyMAB$year,
                 Variable = paste("ChylA_sat_Monthly", monthlyMAB$month, sep = "_"),
                 Val = monthlyMAB$Chl,
                 Loc = "MAB",
                 N = "NA")
mNES<-data.frame(Year = monthlyNES$year,
                 Variable = paste("ChylA_sat_Monthly", monthlyNES$month, sep = "_"),
                 Val = monthlyNES$Chl,
                 Loc = "NES",
                 N = "NA")


findat<-rbind(YNYB, YMAB, YNES, mNYB, mMAB, mNES)



theVar<-unique(findat$Variable)
theLoc<-unique(findat$Loc)

for(i in 1:length(theVar)){
plot(findat[findat$Variable == theVar[i] & findat$Loc == "NYB", "Year"],
findat[findat$Variable == theVar[i] & findat$Loc == "NYB", "Val"], type = "l",ylim = c(.4,1.6),
main = theVar[i])
points(findat[findat$Variable == theVar[i] & findat$Loc == "MAB", "Year"],
     findat[findat$Variable == theVar[i] & findat$Loc == "MAB", "Val"], type = "l", col = "red")
points(findat[findat$Variable == theVar[i] & findat$Loc == "NES", "Year"],
       findat[findat$Variable == theVar[i] & findat$Loc == "NES", "Val"], type = "l", col = "green")
}



###write to csv
setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(findat, "Chyl_Sat.csv")

