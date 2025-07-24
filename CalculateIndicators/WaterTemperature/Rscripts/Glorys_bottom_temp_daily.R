## Bottom Temperature from GLORYS

#**Laura Gruenburg, lagruenburg@gmail.com**
#**LAST UPDATED: September, 30 2022**
  
  
  
### Datasets
#Here I use the 1/12 degree glorys daily reanalysis from Copernicus (https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/services)

### Getting the dataset(s)
#I have found the easiset way to get the data is using the map view subsetter on the above website. 



```{r, message = FALSE}
library(dplyr)
library(rerddap)
library(ncdf4)
library(rgdal)
library(lubridate)
library(nngeo)
library(reshape2)

setwd("/Users/nyelab/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")


```{r}
#This is the function to get the needed data from the .nc files that were created above
#it grabs them and makes them far easier to work with 
T_prep <- function(filename){
  # Open the NetCDF connection
  nc <- nc_open(filename)
  # Extract the SST values and add the lon/lat/time dimension names
  res <- ncvar_get(nc)#, varid = "sea_water_potential_temperature_at_sea_floor") #theres only one var here
  dimnames(res) <- list(lon = nc$dim$longitude$vals,
                        lat = nc$dim$latitude$vals,
                        t = nc$dim$time$vals)
  # Convert the data into a 'long' dataframe for use in the 'tidyverse' ecosystem
  res <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>% 
    mutate(t = as.Date(as.POSIXct(t*3600, origin = "1950-01-01 00:00:00")),
           temp = round(temp, 2))
  # Close the NetCDF connection and finish
  nc_close(nc)
  return(res)#gives you the new easy to work with file as a dataframe
}

res1 <- T_prep('cmems_mod_glo_phy_my_0.083_P1D-m_1701275664284.nc')
res2 <- T_prep('cmems_mod_glo_phy_anfc_0.083deg_P1D-m_1701276060150.nc')
res<- rbind(res1,res2)

#save an intermediate file so we don't have to do this twice
setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/Glorys_daily_BT")
write.csv(res, "Glorys_botT_NOV20_2023.csv", row.names = FALSE)#takes awhile


####First, use a single net cdf file to make a 'key' for clipping and area assigmment




#setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/CV/OISST_v2.1/")
#d1<-OISST_prep("OISST_daily_v2.1_1982_1985.nc")
#d1$lon<-(360-d1$lon)*-1#fix lat long, dont need to do this anymore with new ERDDAP version
res$lat_lon<-paste(res$lat, res$lon, sep = "_")#this is just a unique id for each grid point
ressub<-res[!duplicated(res$lat_lon),]#get rid of all duplicate dmeasuremnts at a point

#and here is the labeling function, inside it does lots of spatial overlays and fancystuff
#####load required functiosn and shapefiles######
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")


lazylab<-klab(ressub$lat, ressub$lon)
tlabs<-data.frame(ressub, lazylab)
tlabs$EPU<-as.character(tlabs$EPU)
tlabs[is.na(tlabs$EPU),"EPU"]<-"Not in NES"
tlabs$NYB<-as.character(tlabs$NYB)
tlabs[is.na(tlabs$NYB),"NYB"]<-"Not in NYB"

tlabs<-tlabs[,c(5,6,7)]
```

All of the above code was done to make an object called **tlabs** which now will be used to label all of the points in the full OISST dataset.

```{r}
###NOW the full dataset can be brought in (all the data gathered in the first section)
###And it can be labled according to the tlabs ddataframe
####These are all of the data created with GetSST
####ddd$lon<-(360-ddd$lon)*-1#convert to correct lat longNOT NEEDED
#res$lat_lon<-paste(res$lat, ddd$lon, sep = "_") #takes awhile, makes a unique lat-long identfier
###remove poitns not within EPU AND not in NYB
res<-res[res$lat_lon %in% tlabs[tlabs$EPU == "Not in NES" & tlabs$NYB == "Not in NYB", "lat_lon"] == FALSE,]#get rid of points not in an EPU or in the NYB
nrow(res)
res$year<-year(res$t)#add year
res$month<-month(res$t)#add month numeric
res$day<-day(res$t)#add day of month
res$yday<-yday(res$t)#add yearday

###ddd is the final dataset to work with for summary stats calculations
res<-merge(res,tlabs, by = "lat_lon")

###SAVE THIS DATA FRAME
### Then you dont need torepeat above steps next time you want to work on this

setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/Glorys_daily_BT")
write.csv(res, "Glorys_bottomT_NOV29_2023.csv", row.names = FALSE)#takes awhile
```

