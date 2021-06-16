##########################
#######FISH###############
###########################
###########################
#######Heim et al. 2020####
###########################
#Create Trawl Biomass Feedging groups
#Use survdat from Sean Lucy
#Label the points according to spatial unit
#Calculate mean CPUE of fish groups according to One that has Forag Fish

#Kurt Heim, Last edited June 24, 2020
####RUN FOR ALL SCRIPTS IN INDICAZTOR SCALEZS DATA PREP####
library(nngeo)
library(rgdal)
library(sf)
library(ggplot2)
library(lubridate)


#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")


###BRING IN FISHY DATA
setwd("~/Google Drive/Manuscripts/Indicator Sensitivity to Scale/Data")
load("Survdat_Nye_allseason_SEP6_2019.RData")#fish data




##Subset to the Offshore strata
surv<-survdat[survdat$STRATUM %in% 1010:1760 == TRUE &
                survdat$SEASON %in% c("SPRING","FALL") == TRUE,]#just keep offshore strata
surv1<-surv[!duplicated(surv[c("ID", "SVSPP", "CATCHSEX")]),]


#just looking at some stuff
ids<-unique(surv1$ID)


###Label points
towz<-surv1[!duplicated(surv1$ID),c("ID","DECDEG_BEGLAT", "DECDEG_BEGLON")]


###Label points
lazylab<-klab(towz$DECDEG_BEGLAT,towz$DECDEG_BEGLON)
labels<-data.frame(towz, lazylab)
plot(labels$DECDEG_BEGLON, labels$DECDEG_BEGLAT, col = labels$EPU,
     pch = 19, cex = .4)

head(labels)
ndf<-merge(surv1,labels, by = "ID")
nrow(ndf)
head(ndf)


####Bring in fish info
spp<-read.csv("SP_INFO_NOAA.csv",header = TRUE)#spp info
spp<-spp[spp$NEIEA != "",]#get rid of empty level
table(spp$NEIEA)
spp<-spp[spp$NEIEA != "",]#get rid of empty level
spp<-droplevels(spp)
table(spp$NEIEA)
nrow(spp)
spp<-spp[!duplicated(spp$SVSPP),]
nrow(spp)
head(spp)
table(spp$SOE_18)

########CALCULATE INDICATORS###############
ddd<-ndf
ddd<-droplevels(ddd)
lenU<-function(x){length(unique(x))}# a function to count unique tows per year


ddd$EPU<-as.character(ddd$EPU)
ddd[is.na(ddd$EPU), "EPU"]<-"Not in NES"
table(ddd$EPU)

ddd$NYB<-as.character(ddd$NYB)
ddd[is.na(ddd$NYB), "NYB"]<-"Not in NYB"
table(ddd$NYB)



#########This will do it for ALL species in the data
#First, do for NES and make into dataframe
tows_year_season_Pts100<-aggregate(ID ~ EST_YEAR + SEASON, ddd[ddd$EPU != "Not in NES",], FUN = lenU, drop = FALSE)
tows_year_season_Pts100$ID[is.na(tows_year_season_Pts100$ID)]<-0
#plot(tows_year_season_Pts100$EST_YEAR, tows_year_season_Pts100$ID, col = tows_year_season_Pts100$SEASON)
names(tows_year_season_Pts100)[3]<-"Towperyear"

#sum biomass of species by year species season and node
sum_sp<-aggregate(BIOMASS ~ EST_YEAR + SEASON + SVSPP, ddd[ddd$EPU != "Not in NES",], FUN = sum)
#bind to count data
sum_sp<-merge(sum_sp, tows_year_season_Pts100, by = c("EST_YEAR","SEASON"), all = TRUE)
#calculate mean species biomass/tow for each year
sum_sp$MeanSP<-sum_sp$BIOMASS/sum_sp$Towperyear


#aggregate by species groupings
ccc<-merge(sum_sp, spp, by = "SVSPP")##THIS WAS WRONG< FIXED IT!!!!!!
##PROBLEM WAs that some recrods in sum_sp were getting duplicated becase
##of the size category column in spp

#here it is important to use drop = FALSE because it then puts NAs for where zeros should be
fin<-aggregate(MeanSP ~ NEIEA + EST_YEAR + SEASON, ccc, FUN = sum, drop = FALSE)
fin$MeanSP[is.na(fin$MeanSP)]<-0#these are REAL zeros
fin<-merge(fin, tows_year_season_Pts100, by=c("EST_YEAR", "SEASON"), all = TRUE)
#fill in NAs in appropriate spot (i.e., where there was actually no fishing)
fin[fin$MeanSP == 0 & fin$Towperyear == 0,"MeanSP"]<-NA

firstDF<-data.frame(Year = fin$EST_YEAR,
                    Variable = paste(fin$SEASON, fin$NEIEA, sep = "_"),
                    Val = fin$MeanSP,
                    Loc = "NES",
                    N = fin$Towperyear)

plot(firstDF[firstDF$Variable == "FALL_Forage Fish","Year"],
     firstDF[firstDF$Variable == "FALL_Forage Fish","Val"], type = "b")
plot(firstDF[firstDF$Variable == "FALL_Elasmobranchs","Year"],
     firstDF[firstDF$Variable == "FALL_Elasmobranchs","Val"], type = "b")


data.frame(1:ncol(ddd), colnames(ddd))

for(i in 48:49){
  #i=48
  ddd$EPU<-ddd[,i]
  tows_year_season_epu<-aggregate(ID ~ EST_YEAR + SEASON + EPU, ddd, FUN = lenU, drop = FALSE)
  tows_year_season_epu$ID[is.na(tows_year_season_epu$ID)]<-0
  names(tows_year_season_epu)[4]<-"Towperyear"
  #sum biomass by year species season and EPU
  sum_sp<-aggregate(BIOMASS ~ EST_YEAR + SEASON + EPU + SVSPP, ddd, FUN = sum)
  #bind to count data
  sum_sp<-merge(sum_sp, tows_year_season_epu, by = c("EST_YEAR","SEASON","EPU"), all = TRUE)
  #calculate mean species biomass/tow for each year 
  sum_sp$MeanSP<-sum_sp$BIOMASS/sum_sp$Towperyear
  #aggregate by species groupings
  
  ccc<-merge(sum_sp, spp, by = "SVSPP")##THIS WAS WRONG< FIXED IT!!!!!!
  ##PROBLEM WAs that some recrods in sum_sp were getting duplicated becase
  ##of the size category column in spp
  
  #here it is important to use drop = FALSE because it then puts NAs for where zeros should be
  fin<-aggregate(MeanSP ~ NEIEA + EST_YEAR + SEASON + EPU, ccc, FUN = sum, drop = FALSE)
  fin$MeanSP[is.na(fin$MeanSP)]<-0#these are REAL zeros
  fin<-merge(fin, tows_year_season_epu, by=c("EST_YEAR", "SEASON", "EPU"), all = TRUE)
  #fill in NAs in appropriate spot (i.e., where there was actually no fishing)
  fin[fin$MeanSP == 0 & fin$Towperyear == 0,"MeanSP"]<-NA
  
  secondDF<-data.frame(Year = fin$EST_YEAR,
                      Variable = paste(fin$SEASON, fin$NEIEA, sep = "_"),
                      Val = fin$MeanSP,
                      Loc = fin$EPU,
                      N = fin$Towperyear)

  firstDF<-rbind(firstDF, secondDF)
}


firstDF<-firstDF[firstDF$Loc %in% c("MAB", "NES", "NYB") == TRUE,]


setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(firstDF, "Fish_Groups_NEIEA_AUG24.csv")


