# GET HISTORICAL NOAA BUOY DATA FROM STATIONS OF INTEREST
# This script will grab standard meteorological data from NOAA National Buoy Data Center


# Ellie Heywood

library(rnoaa) # package to access the NOAA Buoy Data Center
library(stringr)
library(ncdf4)
library(lubridate)
library(tidyverse)
library(seas)

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
end_year<-2022 # designate the end year for last year of data

#here you gotta pick one, then you will need to change a few things below
#buoy_id1<-list("SDHN4", 2004:end_year)#Sandy Hook, NJ. 2004 to present
buoy_id2<-list("44065", 2008:end_year)#NY HARBOR 2008 to present
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

buoy4 <- texas_tower_buoy

datetime<-as.POSIXct(strptime(buoy4$time,"%Y-%m-%dT%H:%M:%OS", tz = "UTC"))

buoy4$datetime<-datetime
buoy4$year<-year(buoy4$datetime)
buoy4$yday<-yday(buoy4$datetime)
buoy4$month<-month(buoy4$datetime, label = TRUE)
str(buoy4) # the dataset has a buncha shit
###
buoy_id2# NY Harbor
ny_harbor_buoy <- getallyears(buoy_id2) 

buoy2 <- ny_harbor_buoy

datetime<-as.POSIXct(strptime(buoy2$time,"%Y-%m-%dT%H:%M:%OS", tz = "UTC"))

buoy2$datetime<-datetime
buoy2$year<-year(buoy2$datetime)
buoy2$yday<-yday(buoy2$datetime)
buoy2$month<-month(buoy4$datetime, label = TRUE)
str(buoy2) # the dataset has a buncha shit

### South of Islip

buoy_id5#this is your input to the function
islip_buoy <- getallyears(buoy_id5) 

buoy5 <- islip_buoy

datetime<-as.POSIXct(strptime(buoy5$time,"%Y-%m-%dT%H:%M:%OS", tz = "UTC"))

buoy5$datetime<-datetime
buoy5$year<-year(buoy5$datetime)
buoy5$yday<-yday(buoy5$datetime)
buoy5$month<-month(buoy5$datetime, label = TRUE)
str(buoy5) # the dataset has a buncha shit

#################### WIND SPEED #######################################
# Put windspeed in comprehensible terms (currently m/s)
buoy4$wind_spd_kts <- buoy4$wind_spd*(1/1852)*(3600/1) # multiply wind speed in meters per second by 1nm/1852m and 3600sec/hr

dailymean <- buoy4 %>% group_by(yday) %>% summarise(median_daily_windspeed = median(wind_spd_kts, na.rm = TRUE))

buoy42 <- left_join(x = buoy4, y = dailymean, by = "yday")

library(ggplot2)
ggplot(data = buoy42, mapping = aes(x = month, y = median_daily_windspeed)) +
  geom_boxplot() +
  ggtitle(label = "Texas Tower Median Daily Wind Speed Summarized by Month (2009-2019)", subtitle = "Daily medians calculated by aggregating wind speed data from past 10 years by julian day") +
  xlab(label = "Months (2009-2022)") +
  ylab(label = "Median Daily Wind Speed (kts)")

#################### WIND GUSTS #######################################
# Put windspeed in comprehensible terms (currently m/s)
buoy4$wind_gust_kts <- buoy4$gust*(1/1852)*(3600/1) # multiply wind speed in meters per second by 1nm/1852m and 3600sec/hr

dailymean <- buoy4 %>% group_by(yday) %>% summarise(median_daily_gusts = median(wind_gust_kts, na.rm = TRUE))

buoy4 <- left_join(x = buoy4, y = dailymean, by = "yday")

ggplot(data = buoy4, mapping = aes(x = month, y = median_daily_gusts)) +
  geom_boxplot() +
  ggtitle(label = "Texas Tower Median Daily Wind Gusts Summarized by Month (2009-2019)", subtitle = "Daily medians calculated by aggregating wind gust data from past 10 years by julian day") +
  xlab(label = "Months (2009-2019)") +
  ylab(label = "Median Daily Wind Gusts (kts)")


### WIND SPEED AND GUSTS TOGETHER

ggplot(data = buoy42, mapping = aes(x = month, y = median_daily_windspeed)) +
  geom_boxplot(color = "goldenrod", fill = "goldenrod", alpha = 0.8) +
  geom_boxplot(mapping = aes(x = month, y = median_daily_gusts), color = "slateblue3", fill = "slateblue3", alpha = 0.5) +
  
  
  ggtitle(label = "Buoy 44025 Median Daily Wind Speed and Gusts Summarized by Month", subtitle = "Daily medians calculated by aggregating wind speed and gust data from past 10 years by julian day") +
  xlab(label = "Months (1975-2019)") + # Months(2009-2019)
  ylab(label = "Median Daily Wind Speed (yellow) and gusts (blue) (kts)")



#################### Wave Height #######################################
dailymean <- buoy4 %>% group_by(yday) %>% summarise(median_daily_waveheight = median(wave_height, na.rm = TRUE))

buoy42 <- left_join(x = buoy42, y = dailymean, by = "yday")

ggplot(data = buoy42, mapping = aes(x = month, y = wave_height)) +
  geom_boxplot() +
  ggtitle(label = "Texas Tower Median Daily Wave Height Summarized by Month (2009-2019)", subtitle = "Daily medians calculated by aggregating wave height data from past 10 years by julian day") +
  xlab(label = "Months (2009-2019)") +
  ylab(label = "Median Daily Wave Height (m)")

################### Wind Stress #########################################
gas_constant <- 287.058
drag_coefficient <- 0.0015
#1hPa = 100Pa
buoy4$rho_air <- (buoy4$air_pressure*100)/(gas_constant * (buoy4$air_temperature+273.15))
buoy4$wind_stress <- buoy4$rho_air*drag_coefficient*(buoy4$wind_spd^2) # bouy wind speed measurement at 5m
buoy4$year2 <- format(buoy4$time, "%Y")
buoy4$date <- buoy4$datetime
buoy4$seas <- mkseas(x = buoy4, width = "DJF")
df2_4 <- aggregate(wind_stress ~ seas + year, data = buoy4, mean)
df3_4 <- aggregate(wind_dir ~ seas + year, data = buoy4, mean)
df4_4 <- aggregate(wind_spd ~ seas + year, data = buoy4, mean)
wind_spd = df4_4$wind_spd[1:142]
df5_4 = cbind(df3_4, wind_spd)

df5_4$wind_dir[df5_4$wind_dir == 0] <- 360
df5_4$u <- (1 * df5_4$wind_spd) * sin((df5_4$wind_dir * pi / 180.0))
df5_4$v <- (1 * df5_4$wind_spd) * cos((df5_4$wind_dir * pi / 180.0))

wind_winter_4 = df5_4[df5_4$seas == 'DJF',]
wind_spring_4 = df5_4[df5_4$seas == 'MAM',]
wind_summer_4 = df5_4[df5_4$seas == 'JJA',]
wind_fall_4 = df5_4[df5_4$seas == 'SON',]
wind_scale <- 1
y_axis <- seq(-10, 5, 5)
ggplot(data = wind_winter_4, aes(x = year, y = y_axis)) +
  # Here we create the wind vectors as a series of segments with arrow tips
  geom_segment(aes(x = year, xend = year + u*wind_scale, y = 0, yend = v*wind_scale), 
               arrow = arrow(length = unit(0.15, 'cm')), size = 0.5, alpha = 0.7) +
  # I think adding points at the base of the vectors makes the figure easier to read
  geom_point(aes(x = year, y = 0), alpha = 0.5, size = 1) +
  # Changing the dates to better match the range shown on the x axis
  #scale_x_date(labels = date_format('%Y-%m-%d'), breaks = date_breaks('4 days')) +
  # Change the y axis labels to make sense
  scale_y_continuous(breaks = y_axis, labels = as.character(abs(y_axis)/wind_scale)) +
  # Change the x and y axis labels
  labs(x = NULL, y = 'Wind Speed (m/s)', title = 'Winter') +
  coord_equal(ylim = c(min(y_axis/wind_scale), max(y_axis/wind_scale))) +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.4, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
  plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


ggplot(data = wind_spring, aes(x = year, y = y_axis)) +
  # Here we create the wind vectors as a series of segments with arrow tips
  geom_segment(aes(x = year, xend = year + u*wind_scale, y = 0, yend = v*wind_scale), 
               arrow = arrow(length = unit(0.15, 'cm')), size = 0.5, alpha = 0.7) +
  # I think adding points at the base of the vectors makes the figure easier to read
  geom_point(aes(x = year, y = 0), alpha = 0.5, size = 1) +
  # Changing the dates to better match the range shown on the x axis
  #scale_x_date(labels = date_format('%Y-%m-%d'), breaks = date_breaks('4 days')) +
  # Change the y axis labels to make sense
  scale_y_continuous(breaks = y_axis, labels = as.character(abs(y_axis)/wind_scale)) +
  # Change the x and y axis labels
  labs(x = NULL, y = 'Wind Speed (m/s)', title = 'Spring') +
  coord_equal(ylim = c(min(y_axis/wind_scale), max(y_axis/wind_scale))) +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.4, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

ggplot(data = wind_summer, aes(x = year, y = y_axis)) +
  # Here we create the wind vectors as a series of segments with arrow tips
  geom_segment(aes(x = year, xend = year + u*wind_scale, y = 0, yend = v*wind_scale), 
               arrow = arrow(length = unit(0.15, 'cm')), size = 0.5, alpha = 0.7) +
  # I think adding points at the base of the vectors makes the figure easier to read
  geom_point(aes(x = year, y = 0), alpha = 0.5, size = 1) +
  # Changing the dates to better match the range shown on the x axis
  #scale_x_date(labels = date_format('%Y-%m-%d'), breaks = date_breaks('4 days')) +
  # Change the y axis labels to make sense
  scale_y_continuous(breaks = y_axis, labels = as.character(abs(y_axis)/wind_scale)) +
  # Change the x and y axis labels
  labs(x = NULL, y = 'Wind Speed (m/s)', title = 'Summer') +
  coord_equal(ylim = c(min(y_axis/wind_scale), max(y_axis/wind_scale))) +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.4, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

ggplot(data = wind_fall, aes(x = year, y = y_axis)) +
  # Here we create the wind vectors as a series of segments with arrow tips
  geom_segment(aes(x = year, xend = year + u*wind_scale, y = 0, yend = v*wind_scale), 
               arrow = arrow(length = unit(0.15, 'cm')), size = 0.5, alpha = 0.7) +
  # I think adding points at the base of the vectors makes the figure easier to read
  geom_point(aes(x = year, y = 0), alpha = 0.5, size = 1) +
  # Changing the dates to better match the range shown on the x axis
  #scale_x_date(labels = date_format('%Y-%m-%d'), breaks = date_breaks('4 days')) +
  # Change the y axis labels to make sense
  scale_y_continuous(breaks = y_axis, labels = as.character(abs(y_axis)/wind_scale)) +
  # Change the x and y axis labels
  labs(x = NULL, y = 'Wind Speed (m/s)', title = 'Autumn') +
  coord_equal(ylim = c(min(y_axis/wind_scale), max(y_axis/wind_scale))) +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.4, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

###

buoy2$rho_air <- (buoy2$air_pressure*100)/(gas_constant * (buoy2$air_temperature+273.15))
buoy2$wind_stress <- buoy2$rho_air*drag_coefficient*(buoy2$wind_spd^2) # bouy wind speed measurement at 5m
buoy2$year <- format(buoy2$time, "%Y")
buoy2$date <- buoy2$datetime
buoy2$seas <- mkseas(x = buoy2, width = "DJF")
df2_2 <- aggregate(wind_stress ~ seas + year, data = buoy2, mean)
df3_2 <- aggregate(wind_dir ~ seas + year, data = buoy2, mean)
df4_2 <- aggregate(wind_spd ~ seas + year, data = buoy2, mean)
###

buoy5$rho_air <- (buoy5$air_pressure*100)/(gas_constant * (buoy5$air_temperature+273.15))
buoy5$wind_stress <- buoy5$rho_air*drag_coefficient*(buoy5$wind_spd^2) # bouy wind speed measurement at 5m
buoy5$year <- format(buoy5$time, "%Y")
buoy5$date <- buoy5$datetime
buoy5$seas <- mkseas(x = buoy5, width = "DJF")
df2_5 <- aggregate(wind_stress ~ seas + year, data = buoy5, mean)
df3_5 <- aggregate(wind_dir ~ seas + year, data = buoy5, mean)
df4_5 <- aggregate(wind_spd ~ seas + year, data = buoy5, mean)

##
library(tidyverse)
df4_4 %>% 
  rename(
    Texas_Tower = wind_spd
  )

df4_5 %>% 
  rename(
    Long_Island = wind_spd
  )

df4_2 %>% 
  rename(
    NY_Harbor = wind_spd
  )

###write to csv
setwd("~/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022")
write.csv(df4_4, "Mean_seasonal_wind_stress_TT_Nov_15_2022.csv")
write.csv(df4_5, "Mean_seasonal_wind_stress_LI_Nov_15_2022.csv")
write.csv(df4_2, "Mean_seasonal_wind_stress_NY_Nov_15_2022.csv")

