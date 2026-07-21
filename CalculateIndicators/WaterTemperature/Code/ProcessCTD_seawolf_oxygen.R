###Get seawolf CTD casts ready to use for indicators
library(oce)

###I have ~150 .cnv files in this folder
###They are ALL of the ones taken from the MEGAFOLDER
#setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/CTD_NYOS")
#mypath<-"~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/CTD_NYOS"

### EAch year take the new files from the seawolf
setwd("~/Desktop/NYB_Indicators_Calculations/CalculateIndicators/WaterTemperature/Data/CTD_NYOS/")
mypath<-"~/Desktop/NYB_Indicators_Calculations/CalculateIndicators/WaterTemperature/Data/CTD_NYOS/"

###This will generate of LIST of all of the file names it finds in mypath
CTDfiles<-list.files(mypath)


hey<-read.ctd(CTDfiles[1], debug = 0,
              type = "SBE19")#read in the 100th CTD cast

hey1<-ctdTrim(hey, method = "downcast")
plot(hey)
plot(hey1)
hey1@metadata$date
hey1@metadata$startTime

plot(hey1@data$fluorescence,hey1@data$depth, ylim = c(100,0),
     type = "l")



hd<-data.frame(hey1@data)
hd$rowNum<-1:nrow(hd)
#lastRow<-max(hd$rowNum)#last row (deepest measurement)
#c50Row<-which.min(abs(hd$depth - 50))#row cloastest to50 m


wolf1<-data.frame(lat = hey1@metadata$latitude,
                  lon = hey1@metadata$longitude,
                  date = hey1@metadata$startTime,
                  depth = hd$depth,
                  pressure = hd$pressure,
                  temp = hd$temperature,
                  salt = hd$salinity,
                  density = hd$density,
                  oxygen = hd$oxygen,
                  profn = 1)



for(i in 2:length(CTDfiles)){
  #i=126
  try(hey<-read.ctd(CTDfiles[i]))#read in the 100th CTD cast
  hey1<-ctdTrim(hey, method = "downcast")
  #plot(hey)
  #plot(hey1)
  
  hd<-data.frame(hey1@data)
  hd$rowNum<-1:nrow(hd)
  
  if (!any(hd$oxygen)){
    oxygen = 'NA'
  }
  if (any(hd$oxygen)){
    oxygen = hd$oxygen
  }
  
  wolfnew<-data.frame(lat = hey1@metadata$latitude,
                      lon = hey1@metadata$longitude,
                      date = hey1@metadata$startTime,
                      depth = hd$depth,
                      pressure = hd$pressure,
                      temp = hd$temperature,
                      salt = hd$salinity,
                      density = hd$density,
                      oxygen = oxygen,
                      profn = i)
  wolf1<-rbind(wolf1, wolfnew)
  print(i)
}



setwd("~/Desktop/NYB_Indicators_Calculations/CalculateIndicators/WaterTemperature/Data")
write.csv(wolf1, "CTD_seawolf_Nov_19_2025_Oxygen.csv")
