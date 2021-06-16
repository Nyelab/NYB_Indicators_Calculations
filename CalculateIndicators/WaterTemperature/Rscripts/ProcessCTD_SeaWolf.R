###Get seawolf CTD casts ready to use for indicators
library(oce)

###I have ~150 .cnv files in this folder
###They are ALL of the ones taken from the MEGAFOLDER
setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/CTD_NYOS")
mypath<-"~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/CTD_NYOS"

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
lastRow<-max(hd$rowNum)#last row (deepest measurement)
c50Row<-which.min(abs(hd$depth - 50))#row cloastest to50 m


wolf1<-data.frame(lat = hey1@metadata$latitude,
           lon = hey1@metadata$longitude,
           date = hey1@metadata$startTime,
           bot_depth = NA,
           last_row_depth = hd[lastRow,"depth"],
           bot_temp = hd[lastRow,"temperature"],
           bot_sal = hd[lastRow,"salinity"],
           bot_den = hd[lastRow,"density"],
           first_row_depth = hd[1,"depth"],
           surf_temp = hd[1,"temperature"],
           surf_sal = hd[1,"salinity"],
           surf_den = hd[1,"density"],
           c50_depth = hd[c50Row,"depth"],
           c50_temp = hd[c50Row,"temperature"],
           c50_sal = hd[c50Row,"salinity"],
           c50_den = hd[c50Row,"density"])



for(i in 2:length(CTDfiles)){
  #i=126
  try(hey<-read.ctd(CTDfiles[i]))#read in the 100th CTD cast
  hey1<-ctdTrim(hey, method = "downcast")
  #plot(hey)
  #plot(hey1)
  
  hd<-data.frame(hey1@data)
  hd$rowNum<-1:nrow(hd)
  lastRow<-max(hd$rowNum)#last row (deepest measurement)
  c50Row<-which.min(abs(hd$depth - 50))#row cloastest to50 m
  
  
  wolfnew<-data.frame(lat = hey1@metadata$latitude,
                      lon = hey1@metadata$longitude,
                    date = hey1@metadata$startTime,
                    bot_depth = NA,
                    last_row_depth = hd[lastRow,"depth"],
                    bot_temp = hd[lastRow,"temperature"],
                    bot_sal = hd[lastRow,"salinity"],
                    bot_den = hd[lastRow,"density"],
                    first_row_depth = hd[1,"depth"],
                    surf_temp = hd[1,"temperature"],
                    surf_sal = hd[1,"salinity"],
                    surf_den = hd[1,"density"],
                    c50_depth = hd[c50Row,"depth"],
                    c50_temp = hd[c50Row,"temperature"],
                    c50_sal = hd[c50Row,"salinity"],
                    c50_den = hd[c50Row,"density"])
  wolf1<-rbind(wolf1, wolfnew)
  print(i)
}



setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
write.csv(wolf1, "CTD_seawolf_AUG6.csv")
