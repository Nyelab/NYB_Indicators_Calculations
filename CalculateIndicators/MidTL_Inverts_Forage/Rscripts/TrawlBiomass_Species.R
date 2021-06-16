##########################
#######FISH###############
###########################
#Create Trawl Biomass of fishs pecies
#Use survdat from Sean Lucy
#Label the points according to spatial unit
#Calculate mean CPUE of the species
library(nngeo)
library(rgdal)
library(sf)
library(ggplot2)
library(lubridate)



#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")



###BRING IN FISHY DATA
setwd("~/Desktop/NYB Indicators/CalculateIndicators/MidTL_Inverts_Forage/Data")
load("Survdat_Nye_allseason_SEP6_2019.RData")#fish data


surv<-survdat
surv<-survdat[survdat$STRATUM %in% 1010:1760 == TRUE &
                survdat$SEASON %in% c("SPRING","FALL") == TRUE,]#just keep offshore strata
surv1<-surv[!duplicated(surv[c("ID", "SVSPP", "CATCHSEX")]),]


####LABEL POINTS
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
spp<-spp[spp$SOE_18 != "",]#get rid of empty level
spp<-droplevels(spp)
nrow(spp)
spp<-spp[!duplicated(spp$SVSPP),]
nrow(spp)
head(spp)



########CALCULATE INDICATORS###############
ddd<-ndf
ddd<-droplevels(ddd)
lenU<-function(x){length(unique(x))}# a function to count unique tows per year



###FIX NA
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
sum_sp<-aggregate(BIOMASS ~ EST_YEAR + SEASON + SVSPP + COMNAME, ddd[ddd$EPU != "Not in NES",], FUN = sum)
#bind to count data
sum_sp<-merge(sum_sp, tows_year_season_Pts100, by = c("EST_YEAR","SEASON"), all = TRUE)
#calculate mean species biomass/tow for each year
sum_sp$MeanSP<-sum_sp$BIOMASS/sum_sp$Towperyear


NES<-data.frame(Year = sum_sp$EST_YEAR,
                 Variable = paste(sum_sp$SEASON, sum_sp$COMNAME, sep = "_"),
                 Val = sum_sp$MeanSP,
                 Loc = "NES",
                 N = sum_sp$Towperyear)

plot(NES[NES$Variable == "SPRING_AMERICAN LOBSTER","Year"],
     NES[NES$Variable == "SPRING_AMERICAN LOBSTER","Val"], type = "b")
points(NES[NES$Variable == "FALL_AMERICAN LOBSTER","Year"],
     NES[NES$Variable == "FALL_AMERICAN LOBSTER","Val"], type = "b")



data.frame(1:ncol(ddd), colnames(ddd))

for(i in 48:49){
  #i=48
  ddd$EPU<-ddd[,i]
  tows_year_season_epu<-aggregate(ID ~ EST_YEAR + SEASON + EPU, ddd, FUN = lenU, drop = FALSE)
  tows_year_season_epu$ID[is.na(tows_year_season_epu$ID)]<-0
  names(tows_year_season_epu)[4]<-"Towperyear"
  #sum biomass by year species season and EPU
  sum_sp<-aggregate(BIOMASS ~ EST_YEAR + SEASON + EPU + SVSPP + COMNAME, ddd, FUN = sum)
  #bind to count data
  sum_sp<-merge(sum_sp, tows_year_season_epu, by = c("EST_YEAR","SEASON","EPU"), all = TRUE)
  #calculate mean species biomass/tow for each year 
  sum_sp$MeanSP<-sum_sp$BIOMASS/sum_sp$Towperyear
  
  secondDF<-data.frame(Year = sum_sp$EST_YEAR,
                      Variable = paste(sum_sp$SEASON, sum_sp$COMNAME, sep = "_"),
                      Val = sum_sp$MeanSP,
                      Loc = sum_sp$EPU,
                      N = sum_sp$Towperyear)
  

  NES<-rbind(NES, secondDF)
}



nrow(NES[NES$Loc %in% c("Not in NES", "Not in NYB") == FALSE,])
nrow(NES)
wa<-NES[NES$Loc %in% c("Not in NES", "Not in NYB") == FALSE,]
wa<-wa[wa$Loc %in% c("NES", "MAB", "NYB") == TRUE,]
wa<-droplevels(wa)


####Keep those with aveage greater than 0.5 kg per tow over time series
NYB<-wa[wa$Loc == "NYB",]
cutEm<-data.frame(table(NYB$Variable))
Keep<-cutEm[cutEm$Freq > 20,"Var1"]


wa<-wa[wa$Variable %in% Keep == TRUE,]
varz<-unique(wa$Variable)
for(i in 1:length(varz)){
  plot(wa[wa$Variable == varz[i] & wa$Loc == "NES", "Year"],
       wa[wa$Variable == varz[i] & wa$Loc == "NES", "Val"], main = varz[i])
}



setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(wa, "Fish_Species_AUG24.csv")






