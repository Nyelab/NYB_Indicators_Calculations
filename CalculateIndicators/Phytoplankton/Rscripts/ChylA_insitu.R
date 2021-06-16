###########################
#######Heim et al. 2020####
###########################
#Create Chyl-a indicator from WOD data
#Use World Ocean Data (processed in THIS script)
#Avergaae CHYL-A over 0 - 30 m
#Label the points according to spatial unit
#Fit GAM and use year effect as indicator for CHYL-A
#Kurt Heim, Last edited Aug17, 2020


###Data Access Instructions###
###Go to https://www.nodc.noaa.gov/OC5/SELECT/dbsearch/dbsearch.html
###Click geographic coordinates and measured variables
##Click Build a query
#Enter these



###Western edge = -78.00 
##Eastern edge = -64.00
##Northern edge = 45.00
##Southern edge = 35.00.  

#For Chlorophyll hit both 1 and 2 boxes
#Click get an inventory
#Click download data
#Click ragged array then enter email and hit extract data
#Save these two files and then these get added below (file1 and file2)

#####PREPARE THE DATASET#######
library(ncdf4)#the packge for .nc files
library(ggplot2)
library(lubridate)
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Phytoplankton/Data")
file1<-("ocldb1597673826.24310_CTD.nc")#straight from WODselect download site
file2<-("ocldb1597673826.24310_OSD.nc")#straight from WODselect download site
con1 <- ncdf4::nc_open(file1)#the file connection
con2<-ncdf4::nc_open(file2)#the file connection



###heres just a look at the dataset, a map with points colored according to chlorophyll data
cfunk<-colorRampPalette(c("red","blue"))
col = cfunk(100)[as.numeric(cut(as.vector(ncdf4::ncvar_get(con1, "Chlorophyll")), breaks = 100))]
plot(as.vector(ncdf4::ncvar_get(con1, "lon")),as.vector(ncdf4::ncvar_get(con1, "lat")),
     col = col, pch = 19, cex = .1)#locations
points(as.vector(ncdf4::ncvar_get(con2, "lon")),as.vector(ncdf4::ncvar_get(con2, "lat")),
      pch = 19, cex = .1, col = "blue")#locations
dmap<-map_data("state")
for(i in 1:63){
  polygon(dmap[dmap$group ==i, "long"],
          dmap[dmap$group == i, "lat"], col = "grey")
}

########MAKE CASTS FUNCTION###########
make_casts<-function(connection){
  ddd<-data.frame(platform = as.vector(ncdf4::ncvar_get(connection, "Platform")),
                  originators_cruise_identifier = as.vector(ncdf4::ncvar_get(connection, "originators_cruise_identifier")),
                  wod_unique_cast = as.vector(ncdf4::ncvar_get(connection, "wod_unique_cast")),
                  Institute = as.vector(ncdf4::ncvar_get(connection, "Institute")),
                  z_row_size = as.vector(ncdf4::ncvar_get(connection, "z_row_size")),
                  Chlorophyll_row_size = as.vector(ncdf4::ncvar_get(connection, "Chlorophyll_row_size")),
                  lat = as.vector(ncdf4::ncvar_get(connection, "lat")),
                  lon = as.vector(ncdf4::ncvar_get(connection, "lon")),
                  date = as.Date(as.character(as.vector(ncdf4::ncvar_get(connection, "date"))), format = "%Y%m%d")
                  #bot_depth = as.vector(ncdf4::ncvar_get(connection, "Bottom_Depth"))
                  #barometric_pressure = as.vector(ncdf4::ncvar_get(connection, "Barometric_Pres"))
  )
  ddd[is.na(ddd$z_row_size),"z_row_size"]<-0#needs to be 0 instead of na or cumsum fails below
  #ddd[is.na(ddd$Salinity_row_size),"Salinity_row_size"]<-0
  ddd[is.na(ddd$Chlorophyll_row_size),"Chlorophyll_row_size"]<-0
  
  ##Add necessary indexes for generating individual CTD cast dataframes
  ##This says..for the cast in this row, which section of the vector do I go get my data from...
  ddd$end_indexZ<-cumsum(ddd$z_row_size)#ending index of z
  ddd$start_indexZ<-ddd$end_indexZ-ddd$z_row_size+1#start index of z
  
  #ddd$end_indexT<-cumsum(ddd$Temperature_row_size)#ending index of Temperature
  #ddd$start_indexT<-ddd$end_indexT-ddd$Temperature_row_size+1#start index of z
  
  ddd$end_indexC<-cumsum(ddd$Chlorophyll_row_size)#ending index of Pressure
  ddd$start_indexC<-ddd$end_indexC-ddd$Chlorophyll_row_size+1#start index Pressure
  return(ddd)
}

####Make CTD FUNCTION#########
create.ctd<-function(castN){
  depth = z[ddd[castN,"start_indexZ"]:ddd[castN,"end_indexZ"]]#adds a depth column
  
  if(ddd$Chlorophyll_row_size[castN] >0){
    chyl = c[ddd[castN,"start_indexC"]:ddd[castN,"end_indexC"]]}else{
      chyl = NA
    }

  tempd<-data.frame(depth,chyl)
  return(tempd)
}



####RUN FUNCTIONS; first part of dataset######
ddd<-make_casts(con1)
ctds1<-list()#this is the list that will contain individual CTD casts
z<-as.vector(ncdf4::ncvar_get(con1, "z"))#get vectors of variables
c<-as.vector(ncdf4::ncvar_get(con1, "Chlorophyll"))#get vectors of variables
finalDF<-data.frame(id = 1:nrow(ddd), lat = ddd$lat, lon = ddd$lon, 
                    date = ddd$date, depth1 = NA, depth2 = NA, chyl1 = NA, chyl2 = NA,
                    mean_chyl_surf = NA)
#this is the depth; shallower than this gets averaged and put inl ast column
cutoff_depth<-30
for(i in 1:nrow(finalDF)){
#  i=1
thedata<-create.ctd(i)
measures<-thedata[thedata$depth < cutoff_depth,"chyl"]
if(length(measures) == 0){
  next
}


finalDF[i,5]<-thedata[1,1]#first depth
finalDF[i,6]<-thedata[2,1]#second depth
finalDF[i,7]<-thedata[1,2]#first Chyla
finalDF[i,8]<-thedata[2,2]#second Chyla
finalDF[i,9]<-mean(measures)#second Chyla
print(i)
}
finalDF1<-finalDF


#####RUN FUNCTIONS second part of dataset#########
ddd<-make_casts(con2)
ctds1<-list()#this is the list that will contain individual CTD casts
z<-as.vector(ncdf4::ncvar_get(con2, "z"))#get vectors of variables
c<-as.vector(ncdf4::ncvar_get(con2, "Chlorophyll"))#get vectors of variables
finalDF<-data.frame(id = 1:nrow(ddd), lat = ddd$lat, lon = ddd$lon, 
                    date = ddd$date, depth1 = NA, depth2 = NA, chyl1 = NA, chyl2 = NA,
                    mean_chyl_surf = NA)
#this is the depth; shallower than this gets averaged and put inl ast column
cutoff_depth<-30
for(i in 1:nrow(finalDF)){
  #  i=1
  thedata<-create.ctd(i)
  measures<-thedata[thedata$depth < cutoff_depth,"chyl"]
  if(length(measures) == 0){
    next
  }
  
  
  finalDF[i,5]<-thedata[1,1]#first depth
  finalDF[i,6]<-thedata[2,1]#second depth
  finalDF[i,7]<-thedata[1,2]#first Chyla
  finalDF[i,8]<-thedata[2,2]#second Chyla
  finalDF[i,9]<-mean(measures)#second Chyla
  print(i)
}
finalDF2<-finalDF




hist(finalDF1$mean_chyl_surf, breaks = 100)
hist(finalDF2$mean_chyl_surf, breaks = 100, add = TRUE)

finalDF<-rbind(finalDF1, finalDF2)
plot(finalDF$lon, finalDF$lat, pch = 19, cex = .1)


faa<-paste(finalDF$date, finalDF$lat, finalDF$lon)
length(faa)
length(unique(faa))
finalDF$uniqueID<-paste(finalDF$date, finalDF$lat, finalDF$lon)
fdf<-finalDF[!duplicated(finalDF$uniqueID),]
nrow(finalDF) - nrow(fdf)

finalDF<-fdf


finalDF$year<-year(finalDF$date)
finalDF$day<-yday(finalDF$date)
finalDF$month<-month(finalDF$date)
table(finalDF$year)



######label the dataset#######
library(rgdal)
library(lubridate)
library(mgcv)
library(effects)
library(mgcViz)
library(stringr)
#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")

###use the function to label
lazylab<-klab(finalDF$lat, finalDF$lon)
ndf<-data.frame(finalDF, lazylab)

ndf$EPU<-as.character(ndf$EPU)
###Label casts not in an EPU
ndf[is.na(ndf$EPU),"EPU"]<-"Not in NES"

table(ndf$EPU, ndf$year)
table(ndf$NYB, ndf$year)##Not much data at in in NYB

setwd("~/Desktop/NYB Indicators/CalculateIndicators/Phytoplankton/Data")
write.csv(ndf, "WOD_CHYLa_AUG17.csv")


##############################################
#############Indicator calculation############
##############################################

setwd("~/Desktop/NYB Indicators/CalculateIndicators/Phytoplankton/Data")
ndf<-read.csv("WOD_CHYLa_AUG17.csv", header = TRUE)
ndf$NYB<-as.character(ndf$NYB)
ndf[is.na(ndf$NYB), "NYB"]<-"Not in NYB"
ndf$year_fac<-factor(ndf$year)

#ndf<-ndf[ndf$year > 1979,]
ndf<-ndf[!is.na(ndf$mean_chyl_surf),]

hist(ndf$mean_chyl_surf)
ndf[ndf$mean_chyl_surf < 0 ,"mean_chyl_surf"]<-0


#Create response value (log + small constant)
constantMin<-min(ndf[ndf$mean_chyl_surf != 0, "mean_chyl_surf"])
ndf$Y<-log(ndf$mean_chyl_surf+constantMin)
ndfFull<-ndf



######DO FOR MAB and NYB####
ndf<-ndfFull[ndfFull$NYB != "Not in NYB" | ndfFull$EPU == "MAB",]
minN<-30
ntab<-table(ndf$year_fac)
remove_these<-names(ntab[ntab < minN])
nrow(ndf)
subdat1<-ndf[ndf$year_fac %in% remove_these == FALSE,]
nrow(subdat1)
subdat1[is.na(subdat1$Y),]

#subdat1$mean_chyl_surf

###Run the first GAM model
m1<-gam(Y ~ s(lon,lat,k = 100) + s(month) +  year_fac, 
        data = subdat1)
summary(m1)#check out model
plot(m1$y, m1$fitted.values, cex = .1)#view residuals
abline(0,1,col = "red")#add 1:1 line
gam.check(m1)#check


#m2<-gam(Y ~ s(lon,lat,k = 100) + s(month) +  s(year), 
#        data = subdat1)
#summary(m2)#check out model
#plot(m2)



#########Grab the coefficient for the indicator
library(stringr)
regexp <- "[[:digit:]]+"
index_vals<-m1$coefficients[grepl("year_fac",names(m1$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add it in
indexMAB<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)


MAB<-data.frame(Year = indexMAB$years,
                Variable = "Chyl_insitu",
                Val = scale(indexMAB$index_vals, scale = FALSE),
                Loc = "MAB",
                N = as.vector(table(subdat1$year)))

plot(MAB$Year, MAB$Val, type = "b")






####DO IT FOR WHOLE NES####

######DO FOR MAB and NYB####
ndf<-ndfFull[ndfFull$EPU != "Not in NES",]
minN<-30
ntab<-table(ndf$year_fac)
remove_these<-names(ntab[ntab < minN])
nrow(ndf)
subdat1<-ndf[ndf$year_fac %in% remove_these == FALSE,]
nrow(subdat1)
subdat1[is.na(subdat1$Y),]

#subdat1$mean_chyl_surf

###Run the first GAM model
m2<-gam(Y ~ s(lon,lat,k = 100) + s(month) +  year_fac, 
        data = subdat1)
summary(m2)#check out model
plot(m2$y, m2$fitted.values, cex = .1)#view residuals
abline(0,1,col = "red")#add 1:1 line
gam.check(m2)#check

#########Grab the coefficient for the indicator
library(stringr)
regexp <- "[[:digit:]]+"
index_vals<-m2$coefficients[grepl("year_fac",names(m2$coefficients))]
years<-as.numeric(str_extract(names(index_vals), regexp))
index_1<-data.frame(years, index_vals)#THe first year of the data is the baseline
#so need to add it in
indexNES<-rbind(data.frame(years = min(subdat1$year, na.rm = TRUE),index_vals = 0), index_1)


NES<-data.frame(Year = indexNES$years,
                Variable = "Chyl_insitu",
                Val = scale(indexNES$index_vals, scale = FALSE),
                Loc = "NES",
                N = as.vector(table(subdat1$year)))

plot(NES$Year, NES$Val, type = "b")
points(MAB$Year, MAB$Val, type = "b", col = "red")
