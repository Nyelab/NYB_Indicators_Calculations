# GET HISTORICAL NOAA BUOY DATA FROM STATIONS OF INTEREST
# This script will grab standard meteorological data from NOAA National Buoy Data Center


# Ellie Heywood

library(rnoaa) # package to access the NOAA Buoy Data Center
library(stringr)
library(ncdf4)
library(lubridate)
library(tidyverse)

# Available stations can be viewed and metadata availabe at https://www.ndbc.noaa.gov

stations <- buoy_stations()

SOI <- stations[grep(stations$description, pattern = "Texas Tower"), ] # Grabs the "STATION OF INTEREST" NOAA Texas Tower Buoy with station ID: 44066

#run it like this
years <- seq(2009, 2019, 1)
buoy_data <- buoy(dataset = 'stdmet', buoyid = SOI$station, year = years)
buoy_data$data$datetime <- as.POSIXct(strptime(buoy_data$data$time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
summary(buoy_data$data$datetime) # only grabs 6 months


####This function solves the problem
#it allows you to get ALL buoy data from a buoy at once..
getallyears<-function(buoy_id){
  first_year <- buoy(dataset = 'stdmet', buoyid = buoy_id[[1]],
                     year = buoy_id[[2]][1])$data#get first year of data
  for(theyear in buoy_id[[2]][2:length(buoy_id[[2]])]){#get the rest of the years of data
    first_year<-rbind(first_year,
                      buoy(dataset = 'stdmet', buoyid = buoy_id[[1]],
                           year = theyear)$data)#stick them together
  }
  first_year#print final dataset
}


###Running the function takes a list of the bouy ID and then years
###Like this here below
end_year<-2021 # designate the end year for last year of data

#here you gotta pick one, then you will need to change a few things below
#buoy_id1<-list("SDHN4", 2004:end_year)#Sandy Hook, NJ. 2004 to present
#buoy_id2<-list("44065", 2008:end_year)#NY HARBOR 2008 to present
#buoy_id3<-list("44091", 2014:end_year)#Barnegat, NJ 2014 to present
buoy_id4<-list("44025", c(1975:1980, 1991:end_year))#Long Island 30 miles offshore from ISLIP
buoy_id5<-list("44066",2009:end_year)#Texas Tower, 75 M from Long Beach, NJ. 2009 to present
#buoy_id6<-list("44069",2016:end_year)# Great South Bay, NY 2016 to present
#buoy_id7<-list("44017", c(2002:2011,2013:end_year))# Montauk Point Offshore, 2002 to present
#buoy_id8<-list("MTKN6", 2004:end_year)# Montauk Point, Inshore 2004 to present
#buoy_id9<-list("44097", 2009:end_year)#Block Island, RI offshore 2009 to present



###Here is an example of what the input looks like
buoy_id4#this is your input tothe function
texas_tower_buoy <- getallyears(buoy_id4) 

rawdat <- texas_tower_buoy

datetime<-as.POSIXct(strptime(rawdat$time,"%Y-%m-%dT%H:%M:%OS", tz = "UTC"))

rawdat$datetime<-datetime
rawdat$year<-year(rawdat$datetime)
rawdat$yday<-yday(rawdat$datetime)
rawdat$month<-month(rawdat$datetime, label = TRUE)
str(rawdat) # the dataset has a buncha shit

#################### WIND SPEED #######################################
# Put windspeed in comprehensible terms (currently m/s)
rawdat$wind_spd_kts <- rawdat$wind_spd*(1/1852)*(3600/1) # multiply wind speed in meters per second by 1nm/1852m and 3600sec/hr

dailymean <- rawdat %>% group_by(yday) %>% summarise(median_daily_windspeed = median(wind_spd_kts, na.rm = TRUE))

rawdat2 <- left_join(x = rawdat, y = dailymean, by = "yday")

library(ggplot2)
ggplot(data = rawdat2, mapping = aes(x = month, y = median_daily_windspeed)) +
  geom_boxplot() +
  ggtitle(label = "Texas Tower Median Daily Wind Speed Summarized by Month (2009-2019)", subtitle = "Daily medians calculated by aggregating wind speed data from past 10 years by julian day") +
  xlab(label = "Months (2009-2019)") +
  ylab(label = "Median Daily Wind Speed (kts)")

#################### WIND GUSTS #######################################
# Put windspeed in comprehensible terms (currently m/s)
rawdat$wind_gust_kts <- rawdat$gust*(1/1852)*(3600/1) # multiply wind speed in meters per second by 1nm/1852m and 3600sec/hr

dailymean <- rawdat %>% group_by(yday) %>% summarise(median_daily_gusts = median(wind_gust_kts, na.rm = TRUE))

rawdat2 <- left_join(x = rawdat2, y = dailymean, by = "yday")

ggplot(data = rawdat2, mapping = aes(x = month, y = median_daily_gusts)) +
  geom_boxplot() +
  ggtitle(label = "Texas Tower Median Daily Wind Gusts Summarized by Month (2009-2019)", subtitle = "Daily medians calculated by aggregating wind gust data from past 10 years by julian day") +
  xlab(label = "Months (2009-2019)") +
  ylab(label = "Median Daily Wind Gusts (kts)")


### WIND SPEED AND GUSTS TOGETHER

ggplot(data = rawdat2, mapping = aes(x = month, y = median_daily_windspeed)) +
  geom_boxplot(color = "goldenrod", fill = "goldenrod", alpha = 0.8) +
  geom_boxplot(mapping = aes(x = month, y = median_daily_gusts), color = "slateblue3", fill = "slateblue3", alpha = 0.5) +
  
  
  ggtitle(label = "Buoy 44025 Median Daily Wind Speed and Gusts Summarized by Month", subtitle = "Daily medians calculated by aggregating wind speed and gust data from past 10 years by julian day") +
  xlab(label = "Months (1975-2019)") + # Months(2009-2019)
  ylab(label = "Median Daily Wind Speed (yellow) and gusts (blue) (kts)")



#################### Wave Height #######################################
dailymean <- rawdat %>% group_by(yday) %>% summarise(median_daily_waveheight = median(wave_height, na.rm = TRUE))

rawdat2 <- left_join(x = rawdat2, y = dailymean, by = "yday")

ggplot(data = rawdat2, mapping = aes(x = month, y = wave_height)) +
  geom_boxplot() +
  ggtitle(label = "Texas Tower Median Daily Wave Height Summarized by Month (2009-2019)", subtitle = "Daily medians calculated by aggregating wave height data from past 10 years by julian day") +
  xlab(label = "Months (2009-2019)") +
  ylab(label = "Median Daily Wave Height (m)")

################### Wind Stress #########################################
gas_constant <- 287.058
drag_coefficient <- 0.0015
#1hPa = 100Pa
rawdat$rho_air <- (rawdat$air_pressure*100)/(gas_constant * (rawdat$air_temperature+273.15))
rawdat$wind_stress <- rawdat$rho_air*drag_coefficient*(rawdat$wind_spd^2) # bouy wind speed measurement at 5m
rawdat$year <- format(rawdat$time, "%Y")
rawdat$date <- rawdat$datetime
rawdat$seas <- mkseas(x = rawdat, width = "DJF")
df2 <- aggregate(wind_stress ~ seas + year, data = rawdat, mean)

###write to csv
setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(df2, "Mean_seasonal_wind_stress_JAN_04_2022.csv")

