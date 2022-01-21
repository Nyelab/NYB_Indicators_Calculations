## ---------------------------
## Script name: NYBight_indicators.R
##
## Purpose of script: Calculate indicators for the NY Bight for DEC.
##
## Author: Brandon Beltz
##
## Last updated: 2021-01-21
##
## Email: brandon.beltz@stonybrook.edu
## ---------------------------
##
## Notes: Final version.
##
## ---------------------------

## Set working directory

setwd("C:/Users/beven/Desktop/DEC Indicators")

## Load libraries, packages and functions

library(data.table); library(rgdal); library(Survdat); library(tidyverse); library(ggplot2); library(dplyr); library(readr)

## Load Survdat, species list and strata
load("Survdat.RData")
load("Species_codes.RData")
strata<-readOGR('strata','strata')

##Calculate mean stratified biomass for FALL and SPRING

## Generate area table
strat.area<-getarea(strata, 'STRATA')
setnames(strat.area,'STRATA','STRATUM')

## Identify strata
NYB.strata<-c(1010:1080)

## Subset by season and strata
NYB.fall<-survdat[SEASON == 'FALL' & STRATUM %in% NYB.strata,]
NYB.spring<-survdat[SEASON == 'SPRING' & STRATUM %in% NYB.strata,]

## Run stratification prep
NYB.prep.fall<-stratprep(NYB.fall,strat.area,strat.col = 'STRATUM',area.col = 'Area')
NYB.prep.spring<-stratprep(NYB.spring,strat.area,strat.col = 'STRATUM',area.col = 'Area')

## Calculate stratified means
mean.biomass.fall<-stratmean(NYB.prep.fall,group.col = 'SVSPP',strat.col = 'STRATUM')
mean.biomass.spring<-stratmean(NYB.prep.spring,group.col = 'SVSPP',strat.col = 'STRATUM')

## Merge with RPATH names
mean.biomass.fall<-merge(mean.biomass.fall,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')
mean.biomass.spring<-merge(mean.biomass.spring,spp[,list(SVSPP,RPATH,SCINAME,Spring.q)], by = 'SVSPP')

## Calculate total biomass from swept area
total.biomass.fall<-sweptarea(NYB.prep.fall, mean.biomass.fall, strat.col = 'STRATUM', area.col = 'Area')
total.biomass.spring<-sweptarea(NYB.prep.spring, mean.biomass.spring, strat.col = 'STRATUM', area.col = 'Area')

## Merge with RPATH names
total.biomass.fall<-merge(total.biomass.fall,spp[,list(SVSPP,RPATH,SCINAME,Fall.q)], by = 'SVSPP')
total.biomass.spring<-merge(total.biomass.spring,spp[,list(SVSPP,RPATH,SCINAME,Spring.q)], by = 'SVSPP')

## Calculate total area
NYB.strat.area<-strat.area[STRATUM %in% NYB.strata,sum(Area)]

## Convert to b/a in mt
total.biomass.fall<-total.biomass.fall[, biomass.t_area :=(tot.biomass*.001)/(Fall.q*NYB.strat.area)]
total.biomass.spring<-total.biomass.spring[, biomass.t_area :=(tot.biomass*.001)/(Spring.q*NYB.strat.area)]

##Output to .csv and .RData
save(total.biomass.fall, file = 'NYB_biomass_fall.RData')
write.csv(total.biomass.fall, file = "NYB_biomass_fall.csv")

save(total.biomass.spring, file = 'NYB_biomass_spring.RData')
write.csv(total.biomass.spring, file = "NYB_biomass_spring.csv")

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
save(NYB.indicators.fall, file = 'NYB_summary.fall.RData')
write.csv(NYB.indicators.fall, file = "NYB_summary.fall.csv")

save(NYB.indicators.spring, file = 'NYB_summary.spring.RData')
write.csv(NYB.indicators.spring, file = "NYB_summary.spring.csv")

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
write.csv(longfin.fall,file = "Timeseries/longfin_fall.csv")
write.csv(longfin.spring,file = "Timeseries/longfin_spring.csv")

# Northern shortfin squid biomass
shortfin.fall<-subset(total.biomass.fall, SCINAME == 'ILLEX ILLECEBROSUS',select = c("YEAR","strat.biomass"))
shortfin.spring<-subset(total.biomass.spring, SCINAME == 'ILLEX ILLECEBROSUS',select = c("YEAR","strat.biomass"))
write.csv(shortfin.fall,file = "Timeseries/shortfin_fall.csv")
write.csv(shortfin.spring,file = "Timeseries/shortfin_spring.csv")

# Black sea bass biomass
bsb.fall<-subset(total.biomass.fall, SCINAME == 'CENTROPRISTIS STRIATA',select = c("YEAR","strat.biomass"))
bsb.spring<-subset(total.biomass.spring, SCINAME == 'CENTROPRISTIS STRIATA',select = c("YEAR","strat.biomass"))
write.csv(bsb.fall,file = "Timeseries/bsb_fall.csv")
write.csv(bsb.spring,file = "Timeseries/bsb_spring.csv")

# Summer flounder biomass
summerflounder.fall<-subset(total.biomass.fall, SCINAME == 'PARALICHTHYS DENTATUS',select = c("YEAR","strat.biomass"))
summerflounder.spring<-subset(total.biomass.spring, SCINAME == 'PARALICHTHYS DENTATUS',select = c("YEAR","strat.biomass"))
write.csv(summerflounder.fall,file = "Timeseries/summerflounder_fall.csv")
write.csv(summerflounder.spring,file = "Timeseries/summerflounder_spring.csv")

# Jonah crab biomass
jonah.fall<-subset(total.biomass.fall, SCINAME == 'CANCER BOREALIS',select = c("YEAR","strat.biomass"))
jonah.spring<-subset(total.biomass.spring, SCINAME == 'CANCER BOREALIS',select = c("YEAR","strat.biomass"))
write.csv(jonah.fall,file = "Timeseries/jonah_fall.csv")
write.csv(jonah.spring,file = "Timeseries/jonah_spring.csv")

# American lobster biomass
amlob.fall<-subset(total.biomass.fall, SCINAME == 'HOMARUS AMERICANUS',select = c("YEAR","strat.biomass"))
amlob.spring<-subset(total.biomass.spring, SCINAME == 'HOMARUS AMERICANUS',select = c("YEAR","strat.biomass"))
write.csv(amlob.fall,file = "Timeseries/amlob_fall.csv")
write.csv(amlob.spring,file = "Timeseries/amlob_spring.csv")

# Total trawl biomass
trawl.fall<-total.biomass.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(trawl.fall,file = "Timeseries/trawl_fall.csv")

trawl.spring<-total.biomass.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(trawl.spring,file = "Timeseries/trawl_spring.csv")

# Assign species preferences from Janet
species_prefs <- read_csv("species_prefs.csv")
total.biomass.fall<-merge(total.biomass.fall,species_prefs,by='SVSPP')
total.biomass.spring<-merge(total.biomass.spring,species_prefs,by='SVSPP')

# Northern v Summer species
north.fall<-subset(total.biomass.fall, NORTH.SOUTH == 'N',select = c("YEAR","strat.biomass"))
north.fall<-north.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(north.fall,file="Timeseries/north_fall.csv")

north.spring<-subset(total.biomass.spring, NORTH.SOUTH == 'N',select = c("YEAR","strat.biomass"))
north.spring<-north.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(north.spring,file="Timeseries/north_spring.csv")

south.fall<-subset(total.biomass.fall, NORTH.SOUTH == 'S',select = c("YEAR","strat.biomass"))
south.fall<-south.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(south.fall,file="Timeseries/south_fall.csv")

south.spring<-subset(total.biomass.spring, NORTH.SOUTH == 'S',select = c("YEAR","strat.biomass"))
south.spring<-south.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(south.spring,file="Timeseries/south_spring.csv")

# Benthic v Pelagic species
benthic.fall<-subset(total.biomass.fall, BENTH.PEL == 'Benthic',select = c("YEAR","strat.biomass"))
benthic.fall<-benthic.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthic.fall,file="Timeseries/benthic_fall.csv")

benthic.spring<-subset(total.biomass.spring, BENTH.PEL == 'Benthic',select = c("YEAR","strat.biomass"))
benthic.spring<-benthic.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthic.spring,file="Timeseries/benthic_spring.csv")

pelagic.fall<-subset(total.biomass.fall, BENTH.PEL == 'Pelagic',select = c("YEAR","strat.biomass"))
pelagic.fall<-pelagic.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(pelagic.fall,file="Timeseries/pelagic_fall.csv")

pelagic.spring<-subset(total.biomass.spring, BENTH.PEL == 'Pelagic',select = c("YEAR","strat.biomass"))
pelagic.spring<-pelagic.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(pelagic.spring,file="Timeseries/pelagic_spring.csv")

# Average TL of community by year
avgtl.fall<-subset(total.biomass.fall, SVSPP != '950')
avgtl.fall<-avgtl.fall %>%
  group_by(YEAR) %>%
  summarise(TL = weighted.mean(TL,strat.biomass))
write.csv(avgtl.fall,file = "Timeseries/avgtl_fall.csv")

avgtl.spring<-subset(total.biomass.spring, SVSPP != '950')
avgtl.spring<-avgtl.spring %>%
  group_by(YEAR) %>%
  summarise(TL = weighted.mean(TL,strat.biomass))
write.csv(avgtl.spring,file = "Timeseries/avgtl_spring.csv")

# Average T preference of community by year
avgtemp.fall<-subset(total.biomass.fall, wt_mean_temp != 'NA')
avgtemp.fall<-avgtemp.fall %>%
  group_by(YEAR) %>%
  summarise(wt_mean_temp = weighted.mean(wt_mean_temp,strat.biomass))
write.csv(avgtemp.fall,file = "Timeseries/avgtemp_fall.csv")

avgtemp.spring<-subset(total.biomass.spring, wt_mean_temp != 'NA')
avgtemp.spring<-avgtemp.spring %>%
  group_by(YEAR) %>%
  summarise(wt_mean_temp = weighted.mean(wt_mean_temp,strat.biomass))
write.csv(avgtemp.spring,file = "Timeseries/avgtemp_spring.csv")

# Assign NEIEA categories
neiea_spp <- read_csv("forage_spp.csv")
total.biomass.fall<-merge(total.biomass.fall,neiea_spp,by='SVSPP')
total.biomass.spring<-merge(total.biomass.spring,neiea_spp,by='SVSPP')

# Forage fish species biomass
forage.fall<-subset(total.biomass.fall, NEIEA == 'Forage Fish',select = c("YEAR","strat.biomass"))
forage.fall<-forage.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(forage.fall,file="Timeseries/forage_fall.csv")

forage.spring<-subset(total.biomass.spring, NEIEA == 'Forage Fish',select = c("YEAR","strat.biomass"))
forage.spring<-forage.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(forage.spring,file="Timeseries/forage_spring.csv")

# Assign NOAA feeding guilds
noaa_guilds <- read_csv("noaa_guilds.csv")
total.biomass.fall<-merge(total.biomass.fall,noaa_guilds,by='SVSPP')
total.biomass.spring<-merge(total.biomass.spring,noaa_guilds,by='SVSPP')

# Benthos biomass
benthos.fall<-subset(total.biomass.fall, Feeding.guild == 'Benthos',select = c("YEAR","strat.biomass"))
benthos.fall<-benthos.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthos.fall,file="Timeseries/benthos_fall.csv")

benthos.spring<-subset(total.biomass.spring, Feeding.guild == 'Benthos',select = c("YEAR","strat.biomass"))
benthos.spring<-benthos.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthos.spring,file="Timeseries/benthos_spring.csv")

# Benthivore biomass
benthivore.fall<-subset(total.biomass.fall, Feeding.guild == 'Benthivore',select = c("YEAR","strat.biomass"))
benthivore.fall<-benthivore.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthivore.fall,file="Timeseries/benthivore_fall.csv")

benthivore.spring<-subset(total.biomass.spring, Feeding.guild == 'Benthivore',select = c("YEAR","strat.biomass"))
benthivore.spring<-benthivore.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(benthivore.spring,file="Timeseries/benthivore_spring.csv")

# Planktivore biomass
planktivore.fall<-subset(total.biomass.fall, Feeding.guild == 'Planktivore',select = c("YEAR","strat.biomass"))
planktivore.fall<-planktivore.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(planktivore.fall,file="Timeseries/planktivore_fall.csv")

planktivore.spring<-subset(total.biomass.spring, Feeding.guild == 'Planktivore',select = c("YEAR","strat.biomass"))
planktivore.spring<-planktivore.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(planktivore.spring,file="Timeseries/planktivore_spring.csv")

# Piscivore biomass
piscivore.fall<-subset(total.biomass.fall, Feeding.guild == 'Piscivore',select = c("YEAR","strat.biomass"))
piscivore.fall<-piscivore.fall %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(piscivore.fall,file="Timeseries/piscivore_fall.csv")

piscivore.spring<-subset(total.biomass.spring, Feeding.guild == 'Piscivore',select = c("YEAR","strat.biomass"))
piscivore.spring<-piscivore.spring %>%
  group_by(YEAR) %>%
  summarise(strat.biomass = sum(strat.biomass))
write.csv(piscivore.spring,file="Timeseries/piscivore_spring.csv")

# Indicate which species are fish
fish_spp<-read_csv("fish_spp.csv")
total.biomass.fall<-merge(total.biomass.fall,fish_spp,by='SVSPP')
total.biomass.spring<-merge(total.biomass.spring,fish_spp,by='SVSPP')

# Fish species richness
library(dplyr)
unique_spp_fall<-subset(total.biomass.fall, Fish == 'Y',select = c("SVSPP","YEAR"))
unique_spp_fall<-distinct(unique_spp_fall)
unique_spp_fall<-count(unique_spp_fall, 'YEAR')
write.csv(unique_spp_fall,file="Timeseries/richness_fall.csv")

unique_spp_spring<-subset(total.biomass.spring, Fish == 'Y',select = c("SVSPP","YEAR"))
unique_spp_spring<-distinct(unique_spp_spring)
unique_spp_spring<-count(unique_spp_spring, 'YEAR')
write.csv(unique_spp_spring,file="Timeseries/richness_spring.csv")
detach(package:dplyr)