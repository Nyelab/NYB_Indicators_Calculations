## Sea Surface Temperature

**Laura Gruenburg, lagruenburg@gmail.com**
  **LAST UPDATED: September, 30 2022**
  
  
  
  ### Datasets
  Here I use the Optimally Interpolated Sea Surface Temperature (OISST) dataset provided by NOAA. [This is the main webpage for the dataset](https://www.ncdc.noaa.gov/oisst) for more details. It is a global dataset at 1/4 degree resolution. For each cell, there are 365 measurements per year (1 temp per day). 

### Getting the dataset(s)
I have found the easiset way to get the OISST data is to get it directly in R using a server connection. The first section below will download a bunch of netcdf files from within R studio. The second section (Prossess OISST) will process the ncdf files, assign the 'points' to spatial areas of interst, and generate a final dataset that can be used for indicator calculations. 

## Section 1: Get SST data
This code creates a function to download a piece of OISST data based on specified time period and lat long bounding box. The code is adapted from the [following website](https://cran.r-project.org/web/packages/heatwaveR/vignettes/OISST_preparation.html). To use the function, one needs to change the working directory to the place where you want some netcdf files of OISST data saved.

The benefit of this approach to getting OISST data is that it does not need to get the whole world dataset..you can just get the pieces you want. It will take 15 - 20 minutes to acquire all of the six files below; run it on a test first (OISST1) before committing to run the full data grab with all of them. The reason I split it up into six time 'chunks' is that in case one fails then the whole thing doesnt fail, you can just start on the ones it didnt finish yet. 

Also note, if you have already run this and have the files, then you dont need to run it again. 

```{r, message = FALSE}
library(dplyr)
library(rerddap)
library(ncdf4)
library(rgdal)
library(lubridate)
library(nngeo)
library(reshape2)

setwd("/Users/nyelab/Downloads")

## Section 2: Process the SST data

The OISST data have 365 temperature estimates per year for each grid cell in the region. What we want to do is figure out which spatial unit each grid point falls into (i.e., Mid Atlantic, Gulf of Maine, etc.). Because there are thousands of estimates, at the same grid cells (i.e., grid points are fixed with repeated measures)...we only need to figure out where each grid cell is (i.e., which epu, or NYB, etc.) a single time. If we know what it is once, we know what it is always. 

Therefore the code does a spatial join on only a subset of the full SST data, then merges the labels back to the full dataset. Once these points are labeled, they can be easily summarized by location. 


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

setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/")
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

setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
write.csv(res, "Glorys_bottomT_NOV29_2023.csv", row.names = FALSE)#takes awhile
```

