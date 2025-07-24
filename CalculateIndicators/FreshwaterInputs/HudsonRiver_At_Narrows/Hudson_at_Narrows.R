library(rgdal)
library(lubridate)
library(mgcv)
library(effects)
library(mgcViz)
library(stringr)
#####load required functiosn and shapefiles
#setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations")

#Current meter at the Narrows since April 2023 https://tidesandcurrents.noaa.gov/cdata/DataPlot?id=n03020&bin=0&bdate=20230310&edate=20230409&unit=0&timeZone=UTC#
# Data available for download in 31 day chunks
# ok thought experiment lets say the narrows are like 1200m wide and 15m deep
# surface area 18000 m2

ddd1 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020.csv')
ddd2 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020(1).csv')
ddd3 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020(2).csv')
ddd4 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020(3).csv')
ddd5 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020(4).csv')
ddd6 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020(5).csv')
ddd7 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020(6).csv')
ddd8 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020(7).csv')
ddd9 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020(8).csv')
#ddd10 <- read.csv('/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/CalculateIndicators/FreshwaterInputs/HudsonRiver_at_Narrows/n03020(9).csv')

ddd<- rbind(ddd1, ddd2, ddd3, ddd4, ddd5, ddd6, ddd7, ddd8, ddd9)
ddd$date <- as.character(ddd$Date.Time)
ddd$date<-as.Date(ddd$date)#,tryFormats = c("%Y%m%d"))
ddd$year<-format(ddd$date, format="%Y")
ddd$month<-month(ymd(ddd$date))
ddd$day<-day(ymd(ddd$date))
ddd$yday<-yday(ymd(ddd$date))


ddd[ddd$Dir..true. > 270, ]$Speed..cm.sec. <- ddd[ddd$Dir..true. > 270, ]$Speed..cm.sec. * -1
ddd[ddd$Dir..true. < 90, ]$Speed..cm.sec. <- ddd[ddd$Dir..true. < 90, ]$Speed..cm.sec. * -1

ddd$m_sec <- ddd$Speed..cm.sec./100

# Daily mean speed

mds<- ddd %>%
  group_by(yday) %>%
  summarise(mean_daily_spd = mean(m_sec),
            vol_transport = mean_daily_spd*18000)

mms<-ddd %>%
  group_by(month) %>%
  summarise(mean_monthly_spd = mean(m_sec),
            vol_transport = mean_monthly_spd*18000) 


write.csv(mds, '/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/Hudson_at_Narrows2023daily.csv')
write.csv(mms, '/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2023/Hudson_at_Narrows2023monthly.csv')



