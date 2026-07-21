#library(rgdal)
library(lubridate)
#library(mgcv)
library(effects)
#library(mgcViz)
library(stringr)
#####load required functiosn and shapefiles
setwd("~/Desktop/NYB_Indicators_Calculations/CalculateIndicators/Rfunctions")
source("LabelPoints_sf.R")

setwd("~/Desktop/NYB_Indicators_Calculations/CalculateIndicators/WaterTemperature/Data")
sea<-read.csv("CTD_seawolf_Nov_19_2025_Oxygen.csv")
sea$date<-as.Date(sea$date)
sea$data_source<-"SEAWOLF"
sea$year<-format(sea$date, format="%Y")
sea$month<-month(ymd(sea$date))

lazylab<-klab(sea$lat, sea$lon)
ndf<-data.frame(sea, lazylab)
ndf$EPU<-as.character(ndf$EPU)
###Label casts not in an EPU
ndf[is.na(ndf$EPU),"EPU"]<-"Not in NES"


NYB<-ndf[ndf$NYB %in% "NYB" == TRUE,]
nrow(NYB)

i = 3
df = NYB[NYB$profn == i, ]
seafloor = df[df$pressure == max(df$pressure), ]
if (nrow(seafloor) > 1){
  seafloor <- seafloor[1, ]}

for (i in unique(NYB$profn)){
  if (i > 3){
    df = NYB[NYB$profn == i, ]
    bottom = df[df$pressure == max(df$pressure), ]
    if (nrow(bottom) > 1){
    bottom <- bottom[1, ]
    }
    seafloor = rbind(seafloor, bottom)
  }
}

seafloor$season <- 'winter'

seafloor[seafloor$month == 3, ]$season <- 'spring'
seafloor[seafloor$month == 4, ]$season <- 'spring'
seafloor[seafloor$month == 5, ]$season <- 'spring'

seafloor[seafloor$month == 6, ]$season <- 'summer'
seafloor[seafloor$month == 7, ]$season <- 'summer'
#seafloor[seafloor$month == 8, ]$season <- 'summer'. #NO data

seafloor[seafloor$month == 9, ]$season <- 'autumn'
seafloor[seafloor$month == 10, ]$season <- 'autumn'
#seafloor[seafloor$month == 11, ]$season <- 'autumn' #NO data

seafloor$season_year <- seafloor$year
seafloor[seafloor$month == 12, ]$season_year <- as.numeric(seafloor[seafloor$month == 12, ]$year) + 1
table(seafloor$season)


oxygen_avg <- seafloor %>% group_by(season, season_year) %>%
  summarise(btm_oxygen = mean(oxygen), rm.na = TRUE,
            nprof = length(unique(profn)))

setwd("~/Desktop/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_files_2025")
write.csv(oxygen_avg, 'Oxygen2025_profn.csv')
