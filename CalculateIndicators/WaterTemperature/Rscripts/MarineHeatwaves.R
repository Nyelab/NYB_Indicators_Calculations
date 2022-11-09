##Heatwave stats
library(reshape2)

library(heatwaveR)
setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
ddd<-read.csv("L1_SST_data_ProcessedSEP30_2022.csv", header = TRUE)#takes awhile
ddd$t<-as.Date(ddd$t)

bbb<-read.csv("Glorys_bottomT_SEP30_2022.csv", header = TRUE)#takes awhile
bbb$t<-as.Date(bbb$t)

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

#####Create Single Time Series (daily avg.) for each EPU
dat2<-aggregate(temp ~ t + EPU, FUN =mean, data = bbb)#make the time series for each EPU
dlev2<-unique(dat2$EPU)#4 levels, one for each EPU
dat2<-dat2[dat2$EPU %in% "MAB" == "TRUE",]
plot(dat2$t, dat2$temp, type = "l")
dat12<-dat2
dat12<-dat12[order(dat12$t),]
plot(dat12$t, dat12$temp, type = "l")
ts2<-ts2clm(dat12, climatologyPeriod = c("1993-01-01", "2011-12-31"))#make climatology
mhw2<-detect_event(ts2)#detect event
mhwCat2<-category(mhw2)
wholeAvg2<-block_average(mhw2)
wholeAvg2<-melt(wholeAvg2, id.vars = c("year"))

### Surface
MAB<-data.frame(Year = wholeAvg$year,
                          Variable = paste("OISST_HW", wholeAvg$variable, sep = "_"),
                          Val = wholeAvg$value,
                          Loc = "MAB",
                          N = NA)


event_line(mhw, spread = 180, metric = "intensity_cumulative", 
           start_date = "1982-01-01", end_date = "2014-12-31")#very cool plot
lolli_plot(mhw, metric = "intensity_cumulative")#cool plot


### Bottom
MAB_b<-data.frame(Year = wholeAvg2$year,
                Variable = paste("GLORYS12", wholeAvg2$variable, sep = "_"),
                Val = wholeAvg2$value,
                Loc = "MAB",
                N = NA)


event_line(mhw2, spread = 180, metric = "intensity_cumulative", 
           start_date = "1993-01-01", end_date = "2014-12-31")#very cool plot
lolli_plot(mhw2, metric = "intensity_cumulative")#cool plot

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

write.csv(mhw$climatology,'mhw_clim_updated_SEP30_2022.csv')
write.csv(ts,'ts_updated_SEP30_2022.csv')

event_line(mhw, spread = 180, metric = "intensity_cumulative", 
           start_date = "1982-01-01", end_date = "2014-12-31")#very cool plot
lolli_plot(mhw, metric = "intensity_cumulative")#cool plot

#####Create Single Time Series (daily avg.) for each EPU
dat2<-aggregate(temp ~ t + NYB, FUN =mean, data = bbb)#make the time series for each EPU
dat2<-dat2[dat2$NYB %in% "NYB" == "TRUE",]
plot(dat2$t, dat2$temp, type = "l")
dat12<-dat2
dat12<-dat12[order(dat12$t),]
plot(dat12$t, dat12$temp, type = "l")
ts2<-ts2clm(dat12, climatologyPeriod = c("1993-01-01", "2011-12-31"))#make climatology
mhw2<-detect_event(ts2)#detect event
mhwCat2<-category(mhw2)
wholeAvg2<-block_average(mhw2)
wholeAvg2<-melt(wholeAvg2, id.vars = c("year"))

NYB_b<-data.frame(Year = wholeAvg2$year,
                Variable = paste("GLORYS12", wholeAvg2$variable, sep = "_"),
                Val = wholeAvg2$value,
                Loc = "NYB",
                N = NA)

write.csv(mhw2$climatology,'mhw_b_clim_updated_SEP30_2022.csv')
write.csv(ts2,'ts_b_updated_SEP30_2022.csv')

event_line(mhw2, spread = 180, metric = "intensity_cumulative", 
           start_date = "1993-01-01", end_date = "2014-12-31")#very cool plot
lolli_plot(mhw2, metric = "intensity_cumulative")#cool plot


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





MHWs<-rbind(NYB, MAB)#, NES)
MHWs[is.na(MHWs$Val), "Val"]<-0

MHWs_b<-rbind(NYB_b, MAB_b)#, NES)
MHWs_b[is.na(MHWs_b$Val), "Val"]<-0

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
####PLOT ALL OF THEM COMPARING NYB WITH MAB
varz_b<-unique(MHWs_b$Variable)

for(i in 1:length(varz_b)){
  what<-varz_b[i]
  plot(MHWs_b[MHWs_b$Variable %in% what == TRUE & MHWs_b$Loc == "MAB", "Year"],
       MHWs_b[MHWs_b$Variable %in% what == TRUE & MHWs_b$Loc == "MAB", "Val"], type = "l",
       main = varz_b[i])
  points(MHWs_b[MHWs_b$Variable %in% what == TRUE & MHWs_b$Loc == "NYB", "Year"],
         MHWs_b[MHWs_b$Variable %in% what == TRUE & MHWs_b$Loc == "NYB", "Val"], type = "l", col = "red")
}



###write to csv
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022")
write.csv(MHWs, "MarineHeatwaves_Sep_30_2022.csv")
write.csv(MHWs_b, 'Bottom_MarineHeatwaves_Sep_30_2022.csv')