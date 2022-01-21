##Heatwave stats
library(reshape2)

library(heatwaveR)
setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
ddd<-read.csv("L1_SST_data_ProcessedAUG30_2021.csv", header = TRUE)#takes awhile
ddd$t<-as.Date(ddd$t)

#####Create Single Time Series (daily avg.) for each EPU
dat<-aggregate(temp ~ t + EPU, FUN =mean, data = ddd)#make the time series for each EPU
dlev<-unique(dat$EPU)#4 levels, one for each EPU
dat<-dat[dat$EPU %in% "MAB" == "TRUE",]
plot(dat$t, dat$temp, type = "l")
dat1<-dat
dat1<-dat1[order(dat1$t),]
plot(dat1$t, dat1$temp, type = "l")
ts<-ts2clm(dat1, climatologyPeriod = c("1982-01-01", "2011-12-31"))#make climatology
mhw<-detect_event(ts)#detect event
mhwCat<-category(mhw)
wholeAvg<-block_average(mhw)
wholeAvg<-melt(wholeAvg, id.vars = c("year"))

MAB<-data.frame(Year = wholeAvg$year,
                          Variable = paste("OISST_HW", wholeAvg$variable, sep = "_"),
                          Val = wholeAvg$value,
                          Loc = "MAB",
                          N = NA)


event_line(mhw, spread = 180, metric = "intensity_cumulative", 
           start_date = "1982-01-01", end_date = "2014-12-31")#very cool plot
lolli_plot(mhw, metric = "intensity_cumulative")#cool plot


#####Create Single Time Series (daily avg.) for each EPU
dat<-aggregate(temp ~ t + NYB, FUN =mean, data = ddd)#make the time series for each EPU
dat<-dat[dat$NYB %in% "NYB" == "TRUE",]
plot(dat$t, dat$temp, type = "l")
dat1<-dat
dat1<-dat1[order(dat1$t),]
plot(dat1$t, dat1$temp, type = "l")
ts<-ts2clm(dat1, climatologyPeriod = c("1982-01-01", "2011-12-31"))#make climatology
mhw<-detect_event(ts)#detect event
mhwCat<-category(mhw)
wholeAvg<-block_average(mhw)
wholeAvg<-melt(wholeAvg, id.vars = c("year"))

NYB<-data.frame(Year = wholeAvg$year,
                          Variable = paste("OISST_HW", wholeAvg$variable, sep = "_"),
                          Val = wholeAvg$value,
                          Loc = "NYB",
                          N = NA)

write.csv(mhw$climatology,'mhw_clim_updated_AUG30_2021.csv')
write.csv(ts,'ts_updated_AUG30_2021.csv')

event_line(mhw, spread = 180, metric = "intensity_cumulative", 
           start_date = "1982-01-01", end_date = "2014-12-31")#very cool plot
lolli_plot(mhw, metric = "intensity_cumulative")#cool plot


#####Create Single Time Series (daily avg.) FOR WHOLE NES
dat<-aggregate(temp ~ t, FUN =mean, data = ddd[ddd$EPU != "Not in NES",])#make the time series for each EPU
plot(dat$t, dat$temp, type = "l")
dat1<-dat
dat1<-dat1[order(dat1$t),]
plot(dat1$t, dat1$temp, type = "l")
ts<-ts2clm(dat1, climatologyPeriod = c("1982-01-01", "2011-12-31"))#make climatology
mhw<-detect_event(ts)#detect event
mhwCat<-category(mhw)
wholeAvg<-block_average(mhw)
wholeAvg<-melt(wholeAvg, id.vars = c("year"))

NES<-data.frame(Year = wholeAvg$year,
                Variable = paste("OISST_HW", wholeAvg$variable, sep = "_"),
                Val = wholeAvg$value,
                Loc = "NES",
                N = NA)


event_line(mhw, spread = 180, metric = "intensity_cumulative", 
           start_date = "1982-01-01", end_date = "2014-12-31")#very cool plot
lolli_plot(mhw, metric = "intensity_cumulative")#cool plot





MHWs<-rbind(NYB, MAB, NES)
MHWs[is.na(MHWs$Val), "Val"]<-0

####PLOT ALL OF THEM COMPARING NYB WITH MAB
varz<-unique(MHWs$Variable)

for(i in 1:length(varz)){
what<-varz[i]
plot(MHWs[MHWs$Variable %in% what == TRUE & MHWs$Loc == "MAB", "Year"],
     MHWs[MHWs$Variable %in% what == TRUE & MHWs$Loc == "MAB", "Val"], type = "l",
     main = varz[i])
points(MHWs[MHWs$Variable %in% what == TRUE & MHWs$Loc == "NYB", "Year"],
     MHWs[MHWs$Variable %in% what == TRUE & MHWs$Loc == "NYB", "Val"], type = "l", col = "red")
points(MHWs[MHWs$Variable %in% what == TRUE & MHWs$Loc == "NES", "Year"],
       MHWs[MHWs$Variable %in% what == TRUE & MHWs$Loc == "NES", "Val"], type = "l", col = "green")

}




###write to csv
setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(MHWs, "MarineHeatwaves_AUG_31_2021.csv")
