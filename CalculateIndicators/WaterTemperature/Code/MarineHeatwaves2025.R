##Heatwave stats
library(reshape2)

library(heatwaveR)
setwd("~/Desktop/NYB_Indicators_Calculations/CalculateIndicators/WaterTemperature/Data")
# L1 data is in the NYOS megafolder
ddd<-read.csv("sst_nyb_daily_2025.csv", header = TRUE)#takes awhile
ddd$time<-as.Date(ddd$time)

setwd("~/Desktop/NYB_Indicators_Calculations/CalculateIndicators/WaterTemperature/Data/")
bbb1<-read.csv("gl_bt_1993_2021nyb.csv", header = TRUE)#takes awhile
bbb2<-read.csv("gl_bt_2021_2025nyb.csv", header = TRUE)#takes awhile
bbb <- rbind(bbb1,bbb2)
bbb$time<-as.Date(bbb$time)


#####Create Single Time Series (daily avg.) for NYB
dat2<-aggregate(sst ~ time, FUN = mean, data = ddd)#make the time series for each EPU
plot(dat2$time, dat2$sst, type = "l")
dat12<-dat2
dat12<-dat12[order(dat12$time),]
plot(dat12$time, dat12$sst, type = "l")
dat12$t <- dat12$time
dat12$temp <- dat12$sst
ts2<-ts2clm(dat12, climatologyPeriod = c("1982-01-01", "2011-12-31"))#make climatology
mhw2<-detect_event(ts2)#detect event
mhwCat2<-category(mhw2)
wholeAvg2<-block_average(mhw2)
wholeAvg2<-melt(wholeAvg2, id.vars = c("year"))

NYB_surf <- data.frame(Year = wholeAvg2$year,
                       Variable = paste('OISST_HW', wholeAvg2$variable, sep = "_"),
                       Val = wholeAvg2$value,
                       Loc = "NYB",
                       N = NA)

event_line(mhw2, spread = 180, metric = "intensity_cumulative", 
           start_date = "1982-01-01", end_date = "2014-12-31")#very cool plot
lolli_plot(mhw2, metric = "intensity_cumulative")#cool plot


#####Create Single Time Series (daily avg.) for NYB
dat2<-aggregate(bottomT ~ time, FUN = mean, data = bbb)#make the time series for each EPU
plot(dat2$time, dat2$bottomT, type = "l")
dat12<-dat2
dat12<-dat12[order(dat12$time),]
plot(dat12$time, dat12$bottomT, type = "l")
dat12$t <- dat12$time
dat12$temp <- dat12$bottomT
ts2<-ts2clm(dat12, climatologyPeriod = c("1993-01-01", "2011-12-31"))#make climatology
mhw2<-detect_event(ts2)#detect event
mhwCat2<-category(mhw2)
wholeAvg2<-block_average(mhw2)
wholeAvg2<-melt(wholeAvg2, id.vars = c("year"))

NYB_bot <- data.frame(Year = wholeAvg2$year,
                       Variable = paste('GLORYS_HW', wholeAvg2$variable, sep = "_"),
                       Val = wholeAvg2$value,
                       Loc = "NYB",
                       N = NA)



write.csv(NYB_surf,'mhw_2025_surf_static.csv')
write.csv(NYB_bot,'mhw_2025_bot_static.csv')


## Moving_baseline
start_dates = c("1982-01-01", "1983-01-01","1984-01-01","1985-01-01","1986-01-01","1987-01-01","1988-01-01","1989-01-01","1990-01-01","1991-01-01","1992-01-01","1993-01-01", "1994-01-01", "1995-01-01")
end_dates = c("2011-12-31", "2012-12-31", "2013-12-31", "2014-12-31", "2015-12-31", "2016-12-31","2017-12-31", "2018-12-31", "2019-12-31", "2020-12-31", "2021-12-31", "2022-12-31", '2023-12-31', '2024-12-31')
yrs = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)

for(i in 1:14){
  dat<-aggregate(sst ~ t, FUN =mean, data = ddd)#make the time series for each EPU
  #plot(dat$t, dat$sst, type = "l")
  dat1<-dat
  dat1<-dat1[order(dat1$t),]
  #plot(dat1$t, dat1$sst, type = "l")
  dat1$temp <- dat1$sst
  ts<-ts2clm(dat1, climatologyPeriod = c(start_dates[i], end_dates[i]))#make climatology
  mhw<-detect_event(ts)#detect event
  mhwCat<-category(mhw)
  wholeAvg<-block_average(mhw)
  wholeAvg<-melt(wholeAvg, id.vars = c("year"))
  yr_wholeavg<- wholeAvg[wholeAvg$year == yrs[i], ]
  if(i == 1){
    moving_baseline = yr_wholeavg
  }
  if(i > 1){
    moving_baseline = rbind(moving_baseline, yr_wholeavg)
  }
  
}

NYB_surf_moving<-data.frame(Year = moving_baseline$year,
                       Variable = paste("OISST_HW_moving", moving_baseline$variable, sep = "_"),
                       Val = moving_baseline$value,
                       Loc = "NYB",
                       N = NA)


write.csv(NYB_surf_moving,'mhw_surf_2025_moving.csv')


## Moving_baseline
start_dates = c("1993-01-01", "1994-01-01", "1995-01-01", "1996-01-01", "1997-01-01", "1998-01-01","1999-01-01", "2000-01-01", "2001-01-01", "2002-01-01", "2003-01-01", "2004-01-01", "2005-01-01")
end_dates = c("2012-12-31", "2013-12-31", "2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31","2018-12-31", "2019-12-31", "2020-12-31", "2021-12-31", "2022-12-31", "2023-12-31", "2024-12-31")
yrs = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)

for(i in 1:13){
  dat2<-aggregate(bottomT ~ time, FUN =mean, data = bbb)#make the time series for each EPU
  #plot(dat2$t, dat2$temp, type = "l")
  dat12<-dat2
  dat12<-dat12[order(dat12$time),]
  #plot(dat12$t, dat12$temp, type = "l")
  dat12$t <- dat12$time
  dat12$temp <- dat12$bottomT
  ts2<-ts2clm(dat12, climatologyPeriod = c(start_dates[i], end_dates[i]))#make climatology
  mhw<-detect_event(ts2)#detect event
  mhwCat2<-category(mhw2)
  wholeAvg2<-block_average(mhw2)
  wholeAvg2<-melt(wholeAvg2, id.vars = c("year"))
  yr_wholeavg2<- wholeAvg2[wholeAvg2$year == yrs[i], ]
  if(i == 1){
    moving_baseline2 = yr_wholeavg2
  }
  if(i > 1){
    moving_baseline2 = rbind(moving_baseline2, yr_wholeavg2)
  }
  
}

NYB_moving_b<-data.frame(Year = moving_baseline2$year,
                         Variable = paste("GLORYS12_moving", moving_baseline2$variable, sep = "_"),
                         Val = moving_baseline2$value,
                         Loc = "NYB",
                         N = NA)


write.csv(NYB_moving_b,'mhw_bot_2025_moving.csv')


