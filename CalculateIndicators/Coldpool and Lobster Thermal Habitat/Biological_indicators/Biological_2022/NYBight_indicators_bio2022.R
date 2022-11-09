## ---------------------------
## Script name: NYBight_indicators.R
##
## Purpose of script: Calculate indicators for the NY Bight for DEC.
##
## Author: Brandon Beltz, Laura Gruenburg
##
## Last updated: 2022-09-23
##
## Email: brandon.beltz@stonybrook.edu, laura.gruenburg@stonybrook.edu
## ---------------------------
##
## Notes: Final version.
##
## ---------------------------

## Set working directoryremotes::install_github("NOAA-EDAB/survdat",build_vignettes = TRUE)
#setwd("C:/Users/beven/Desktop/DEC Indicators")
setwd("/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Biological_indicators_code_2021")
## Load libraries, packages and functions

library(data.table); library(rgdal); library(survdat); library(tidyverse); library(ggplot2); library(dplyr); library(readr)

## Load Survdat, species list and strata
load("Survdat.RData")
load('NEFSC_survey_2020_2022.RData')
load("Species_codes.RData")
strata<-readOGR('strata','strata')

survey2 <- survey$survdat # extract the data from the most updated file
dt = rbind(survdat[survdat$YEAR < 2020, ], survey2) # get rid of any over lapping data and merge old and new 

##Calculate mean stratified biomass for FALL and SPRING

## Generate area table
#strat.area<-get_area(strata, 'STRATA') # this did not work for Laura
#setnames(strat.area,'STRATA','STRATUM')

area <- sf::st_read(dsn=system.file("extdata","strata.shp",package="survdat"))
strat.area <- get_area(areaPolygon = area, areaDescription="STRATA")

## Identify strata
NYB.strata<-c(1010:1080)

## Subset by season and strata
NYB.fall<-dt[SEASON == 'FALL' & STRATUM %in% NYB.strata,]
NYB.spring<-dt[SEASON == 'SPRING' & STRATUM %in% NYB.strata,]

## Run stratification prep
NYB.prep.fall<-strat_prep(NYB.fall,areaPolygon = area, areaDescription="STRATA")#,strat.col = 'STRATUM',area.col = 'Area')
NYB.prep.spring<-strat_prep(NYB.spring,areaPolygon = area, areaDescription="STRATA")

## Calculate stratified means
mean.biomass.fall<-strat_mean(NYB.prep.fall,groupDescription = 'SVSPP', areaDescription = 'STRATA',seasonFlag = T,poststratFlag = T)#group.col = 'SVSPP',strat.col = 'STRATUM')
mean.biomass.spring<-strat_mean(NYB.prep.spring,groupDescription = 'SVSPP', areaDescription = 'STRATA',seasonFlag = T,poststratFlag = T)

## Merge with RPATH names
mean.biomass.fall<-merge(mean.biomass.fall,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')
mean.biomass.spring<-merge(mean.biomass.spring,spp[,list(SVSPP,RPATH,SCINAME,Spring.q)], by = 'SVSPP')

## Calculate total biomass from swept area
total.biomass.fall<-swept_area(NYB.prep.fall, mean.biomass.fall, groupDescription = 'SVSPP', areaDescription = 'STRATA')#, strat.col = 'STRATUM', area.col = 'Area')
total.biomass.spring<-swept_area(NYB.prep.spring, mean.biomass.spring, groupDescription = 'SVSPP', areaDescription = 'STRATA')

## Merge with RPATH names
total.biomass.fall<-merge(total.biomass.fall,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')
total.biomass.spring<-merge(total.biomass.spring,spp[,list(SVSPP,RPATH,SCINAME,Spring.q)], by = 'SVSPP')

## Calculate total area
NYB.strat.area<-strat.area[STRATUM %in% NYB.strata,sum(Area)]

## Convert to b/a in mt
total.biomass.fall<-total.biomass.fall[, biomass.t_area :=(tot.biomass*.001)/(Fall.q*NYB.strat.area)]
total.biomass.spring<-total.biomass.spring[, biomass.t_area :=(tot.biomass*.001)/(Spring.q*NYB.strat.area)]

##Output to .csv and .RData
save(total.biomass.fall, file = 'NYB_biomass_fall_2022.RData')
write.csv(total.biomass.fall, file = "NYB_biomass_fall2022.csv")

save(total.biomass.spring, file = 'NYB_biomass_spring_2022.RData')
write.csv(total.biomass.spring, file = "NYB_biomass_spring_2022.csv")

##Calculate 30-year summary of latitude
NYB.indicators.fall<-spp[,list(SVSPP,RPATH,SCINAME,COMNAME)]
NYB.Lat30_mean.fall<-NYB.fall[YEAR %in% 1968:1997, mean(LAT), by = SVSPP]
setnames(NYB.Lat30_mean.fall,'V1','MeanLAT')
NYB.indicators.fall<-merge(NYB.indicators.fall,NYB.Lat30_mean.fall)
NYB.Lat30_10th.fall<-NYB.fall[YEAR %in% 1968:1997, quantile(LAT,.1), by = SVSPP]
setnames(NYB.Lat30_10th.fall,'V1','LAT 10th')
NYB.indicators.fall<-merge(NYB.indicators.fall,NYB.Lat30_10th.fall)
NYB.Lat30_90th.fall<-NYB.fall[YEAR %in% 1968:1997, quantile(LAT,.9), by = SVSPP]
setnames(NYB.Lat30_90th.fall,'V1','LAT 90th')
NYB.indicators.fall<-merge(NYB.indicators.fall,NYB.Lat30_90th.fall)

NYB.indicators.spring<-spp[,list(SVSPP,RPATH,SCINAME,COMNAME)]
NYB.Lat30_mean.spring<-NYB.spring[YEAR %in% 1968:1997, mean(LAT), by = SVSPP]
setnames(NYB.Lat30_mean.spring,'V1','MeanLAT')
NYB.indicators.spring<-merge(NYB.indicators.spring,NYB.Lat30_mean.spring)
NYB.Lat30_10th.spring<-NYB.spring[YEAR %in% 1968:1997, quantile(LAT,.1), by = SVSPP]
setnames(NYB.Lat30_10th.spring,'V1','LAT 10th')
NYB.indicators.spring<-merge(NYB.indicators.spring,NYB.Lat30_10th.spring)
NYB.Lat30_90th.spring<-NYB.spring[YEAR %in% 1968:1997, quantile(LAT,.9), by = SVSPP]
setnames(NYB.Lat30_90th.spring,'V1','LAT 90th')
NYB.indicators.spring<-merge(NYB.indicators.spring,NYB.Lat30_90th.spring)

##Output to .csv and .RData
save(NYB.indicators.fall, file = 'NYB_summary.fall_2022.RData')
write.csv(NYB.indicators.fall, file = "NYB_summary.fall_2022.csv")

save(NYB.indicators.spring, file = 'NYB_summary.spring_2022.RData')
write.csv(NYB.indicators.spring, file = "NYB_summary.spring_2022.csv")

##Create data.frame for mean latitude time series
NYB.timeseries.fall<-data.frame(total.biomass.fall$SVSPP, total.biomass.fall$YEAR, total.biomass.fall$strat.biomass)
setnames(NYB.timeseries.fall, 'total.biomass.fall.SVSPP','SVSPP')
setnames(NYB.timeseries.fall, 'total.biomass.fall.YEAR','YEAR')
setnames(NYB.timeseries.fall, 'total.biomass.fall.strat.biomass','MSB')

NYB.timeseries.spring<-data.frame(total.biomass.spring$SVSPP, total.biomass.spring$YEAR, total.biomass.spring$strat.biomass)
setnames(NYB.timeseries.spring, 'total.biomass.spring.SVSPP','SVSPP')
setnames(NYB.timeseries.spring, 'total.biomass.spring.YEAR','YEAR')
setnames(NYB.timeseries.spring, 'total.biomass.spring.strat.biomass','MSB')

##Calculate mean latitude by SVSPP by year
subset.fall<-NYB.timeseries.fall %>% filter(SVSPP == 1)
latbyyear.fall<-NYB.fall[SVSPP %in% 1, mean(LAT), by = YEAR]
setnames(latbyyear.fall,'V1','MeanLat')
merge.fall<-merge(subset.fall,latbyyear.fall)
NYB.latbyyear.fall<-merge.fall

for(i in 2:998) {
  subset.fall<-NYB.timeseries.fall %>% filter(SVSPP == i)
  latbyyear.fall<-NYB.fall[SVSPP %in% i, mean(LAT), by = YEAR]
  setnames(latbyyear.fall,'V1','MeanLat')
  merge2.fall<-merge(subset.fall,latbyyear.fall)
  NYB.latbyyear.fall<-rbind(NYB.latbyyear.fall,merge2.fall)
}

subset.spring<-NYB.timeseries.spring %>% filter(SVSPP == 1)
latbyyear.spring<-NYB.spring[SVSPP %in% 1, mean(LAT), by = YEAR]
setnames(latbyyear.spring,'V1','MeanLat')
merge.spring<-merge(subset.spring,latbyyear.spring)
NYB.latbyyear.spring<-merge.spring

for(i in 2:998) {
  subset.spring<-NYB.timeseries.spring %>% filter(SVSPP == i)
  latbyyear.spring<-NYB.spring[SVSPP %in% i, mean(LAT), by = YEAR]
  setnames(latbyyear.spring,'V1','MeanLat')
  merge2.spring<-merge(subset.spring,latbyyear.spring)
  NYB.latbyyear.spring<-rbind(NYB.latbyyear.spring,merge2.spring)
}

## Creating and saving time series for each indicator

# Longfin squid biomass
longfin.fall<-subset(total.biomass.fall, SCINAME == 'LOLIGO PEALEII',select = c("YEAR","strat.biomass"))
longfin.spring<-subset(total.biomass.spring, SCINAME == 'LOLIGO PEALEII',select = c("YEAR","strat.biomass"))
write.csv(longfin.fall,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/longfin_fall_2022.csv")
write.csv(longfin.spring,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/longfin_spring_2022.csv")

# Northern shortfin squid biomass
shortfin.fall<-subset(total.biomass.fall, SCINAME == 'ILLEX ILLECEBROSUS',select = c("YEAR","strat.biomass"))
shortfin.spring<-subset(total.biomass.spring, SCINAME == 'ILLEX ILLECEBROSUS',select = c("YEAR","strat.biomass"))
write.csv(shortfin.fall,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/shortfin_fall_2022.csv")
write.csv(shortfin.spring,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/shortfin_spring_2022.csv")

# Black sea bass biomass
bsb.fall<-subset(total.biomass.fall, SCINAME == 'CENTROPRISTIS STRIATA',select = c("YEAR","strat.biomass"))
bsb.spring<-subset(total.biomass.spring, SCINAME == 'CENTROPRISTIS STRIATA',select = c("YEAR","strat.biomass"))
write.csv(bsb.fall,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/bsb_fall_2022.csv")
write.csv(bsb.spring,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/bsb_spring_2022.csv")

# Summer flounder biomass
summerflounder.fall<-subset(total.biomass.fall, SCINAME == 'PARALICHTHYS DENTATUS',select = c("YEAR","strat.biomass"))
summerflounder.spring<-subset(total.biomass.spring, SCINAME == 'PARALICHTHYS DENTATUS',select = c("YEAR","strat.biomass"))
write.csv(summerflounder.fall,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/summerflounder_fall_2022.csv")
write.csv(summerflounder.spring,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/summerflounder_spring_2022.csv")

# Jonah crab biomass
jonah.fall<-subset(total.biomass.fall, SCINAME == 'CANCER BOREALIS',select = c("YEAR","strat.biomass"))
jonah.spring<-subset(total.biomass.spring, SCINAME == 'CANCER BOREALIS',select = c("YEAR","strat.biomass"))
write.csv(jonah.fall,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/jonah_fall_2022.csv")
write.csv(jonah.spring,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/jonah_spring_2022.csv")

# American lobster biomass
amlob.fall<-subset(total.biomass.fall, SCINAME == 'HOMARUS AMERICANUS',select = c("YEAR","strat.biomass"))
amlob.spring<-subset(total.biomass.spring, SCINAME == 'HOMARUS AMERICANUS',select = c("YEAR","strat.biomass"))
write.csv(amlob.fall,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/amlob_fall_2022.csv")
write.csv(amlob.spring,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/amlob_spring_2022.csv")

# Total trawl biomass
trawl.fall<-total.biomass.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(trawl.fall,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/trawl_fall_2022.csv")

trawl.spring<-total.biomass.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(trawl.spring,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/trawl_spring_2022.csv")

# Assign species preferences from Janet
species_prefs <- read_csv("species_prefs.csv")
total.biomass.fall<-merge(total.biomass.fall,species_prefs,by='SVSPP')
total.biomass.spring<-merge(total.biomass.spring,species_prefs,by='SVSPP')

# Northern v Summer species
north.fall<-subset(total.biomass.fall, NORTH.SOUTH == 'N',select = c("YEAR","strat.biomass"))
north.fall<-north.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(north.fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/north_fall_2022.csv")

north.spring<-subset(total.biomass.spring, NORTH.SOUTH == 'N',select = c("YEAR","strat.biomass"))
north.spring<-north.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(north.spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/north_spring_2022.csv")

south.fall<-subset(total.biomass.fall, NORTH.SOUTH == 'S',select = c("YEAR","strat.biomass"))
south.fall<-south.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(south.fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/south_fall_2022.csv")

south.spring<-subset(total.biomass.spring, NORTH.SOUTH == 'S',select = c("YEAR","strat.biomass"))
south.spring<-south.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(south.spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/south_spring_2022.csv")

# Benthic v Pelagic species
benthic.fall<-subset(total.biomass.fall, BENTH.PEL == 'Benthic',select = c("YEAR","strat.biomass"))
benthic.fall<-benthic.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthic.fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/benthic_fall_2022.csv")

benthic.spring<-subset(total.biomass.spring, BENTH.PEL == 'Benthic',select = c("YEAR","strat.biomass"))
benthic.spring<-benthic.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthic.spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/benthic_spring_2022.csv")

pelagic.fall<-subset(total.biomass.fall, BENTH.PEL == 'Pelagic',select = c("YEAR","strat.biomass"))
pelagic.fall<-pelagic.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(pelagic.fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/pelagic_fall_2022.csv")

pelagic.spring<-subset(total.biomass.spring, BENTH.PEL == 'Pelagic',select = c("YEAR","strat.biomass"))
pelagic.spring<-pelagic.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(pelagic.spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/pelagic_spring_2022.csv")

# Average TL of community by year
avgtl.fall<-subset(total.biomass.fall, SVSPP != '950')
avgtl.fall<-avgtl.fall %>%
  group_by(YEAR) %>%
  summarise(TL = weighted.mean(TL,strat.biomass))
write.csv(avgtl.fall,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/avgtl_fall_2022.csv")

avgtl.spring<-subset(total.biomass.spring, SVSPP != '950')
avgtl.spring<-avgtl.spring %>%
  group_by(YEAR) %>%
  summarise(TL = weighted.mean(TL,strat.biomass))
write.csv(avgtl.spring,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/avgtl_spring_2022.csv")

# Average T preference of community by year
avgtemp.fall<-subset(total.biomass.fall, wt_mean_temp != 'NA')
avgtemp.fall<-avgtemp.fall %>%
  group_by(YEAR) %>%
  summarise(wt_mean_temp = weighted.mean(wt_mean_temp,strat.biomass))
write.csv(avgtemp.fall,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/avgtemp_fall_2022.csv")

avgtemp.spring<-subset(total.biomass.spring, wt_mean_temp != 'NA')
avgtemp.spring<-avgtemp.spring %>%
  group_by(YEAR) %>%
  summarise(wt_mean_temp = weighted.mean(wt_mean_temp,strat.biomass))
write.csv(avgtemp.spring,file = "/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/avgtemp_spring_2022.csv")

# Assign NEIEA categories
neiea_spp <- read_csv("forage_spp.csv")
total.biomass.fall<-merge(total.biomass.fall,neiea_spp,by='SVSPP')
total.biomass.spring<-merge(total.biomass.spring,neiea_spp,by='SVSPP')

# Forage fish species biomass
forage.fall<-subset(total.biomass.fall, NEIEA == 'Forage Fish',select = c("YEAR","strat.biomass"))
forage.fall<-forage.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(forage.fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/forage_fall_2022.csv")

forage.spring<-subset(total.biomass.spring, NEIEA == 'Forage Fish',select = c("YEAR","strat.biomass"))
forage.spring<-forage.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(forage.spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/forage_spring_2022.csv")

# Assign NOAA feeding guilds
noaa_guilds <- read_csv("noaa_guilds.csv")
total.biomass.fall<-merge(total.biomass.fall,noaa_guilds,by='SVSPP')
total.biomass.spring<-merge(total.biomass.spring,noaa_guilds,by='SVSPP')

# Benthos biomass
benthos.fall<-subset(total.biomass.fall, Feeding.guild == 'Benthos',select = c("YEAR","strat.biomass"))
benthos.fall<-benthos.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthos.fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/benthos_fall_2022.csv")

benthos.spring<-subset(total.biomass.spring, Feeding.guild == 'Benthos',select = c("YEAR","strat.biomass"))
benthos.spring<-benthos.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthos.spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/benthos_spring_2022.csv")

# Benthivore biomass
benthivore.fall<-subset(total.biomass.fall, Feeding.guild == 'Benthivore',select = c("YEAR","strat.biomass"))
benthivore.fall<-benthivore.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthivore.fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/benthivore_fall_2022.csv")

benthivore.spring<-subset(total.biomass.spring, Feeding.guild == 'Benthivore',select = c("YEAR","strat.biomass"))
benthivore.spring<-benthivore.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthivore.spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/benthivore_spring_2022.csv")

# Planktivore biomass
planktivore.fall<-subset(total.biomass.fall, Feeding.guild == 'Planktivore',select = c("YEAR","strat.biomass"))
planktivore.fall<-planktivore.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(planktivore.fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/planktivore_fall_2022.csv")

planktivore.spring<-subset(total.biomass.spring, Feeding.guild == 'Planktivore',select = c("YEAR","strat.biomass"))
planktivore.spring<-planktivore.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(planktivore.spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/planktivore_spring_2022.csv")

# Piscivore biomass
piscivore.fall<-subset(total.biomass.fall, Feeding.guild == 'Piscivore',select = c("YEAR","strat.biomass"))
piscivore.fall<-piscivore.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(piscivore.fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/piscivore_fall_2022.csv")

piscivore.spring<-subset(total.biomass.spring, Feeding.guild == 'Piscivore',select = c("YEAR","strat.biomass"))
piscivore.spring<-piscivore.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(piscivore.spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/piscivore_spring_2022.csv")

# Indicate which species are fish
fish_spp<-read_csv("fish_spp.csv")
total.biomass.fall<-merge(total.biomass.fall,fish_spp,by='SVSPP')
total.biomass.spring<-merge(total.biomass.spring,fish_spp,by='SVSPP')

# Fish species richness
library(dplyr)
unique_spp_fall<-subset(total.biomass.fall, Fish == 'Y',select = c("SVSPP","YEAR"))
unique_spp_fall<-distinct(unique_spp_fall)
unique_spp_fall<-count(unique_spp_fall, 'YEAR')
write.csv(unique_spp_fall,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/richness_fall_2022.csv")

unique_spp_spring<-subset(total.biomass.spring, Fish == 'Y',select = c("SVSPP","YEAR"))
unique_spp_spring<-distinct(unique_spp_spring)
unique_spp_spring<-count(unique_spp_spring, 'YEAR')
write.csv(unique_spp_spring,file="/Users/nyelab/Desktop/NYB Indicators/NYB_Indicators_Calculations/Final_Timeseries_Figures/Timeseries_Files_2022/richness_spring_2022.csv")
detach(package:dplyr)