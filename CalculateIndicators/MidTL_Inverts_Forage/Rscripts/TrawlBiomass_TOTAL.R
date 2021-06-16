###########################
#######Heim et al. 2020####
###########################
#Create Trawl Biomass Indicator
#Use survdat from Sean Lucy
#Label the points according to spatial unit
#Calculate mean CPUE of all stuff caught

#Kurt Heim, Last edited June 24, 2020
####RUN FOR ALL SCRIPTS IN INDICAZTOR SCALEZS DATA PREP####
library(nngeo)
library(rgdal)
library(sf)
library(ggplot2)
library(lubridate)
library(mgcv)
library(effects)
library(mgcViz)

#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")


###BRING IN FISHY DATA
setwd("~/Google Drive/Manuscripts/Indicator Sensitivity to Scale/Data")
load("Survdat_Nye_allseason_SEP6_2019.RData")#fish data

###BRING IN FISHY DATA
setwd("~/Desktop/NYB Indicators/CalculateIndicators/MidTL_Inverts_Forage/Data")
load("Survdat_Nye_allseason_SEP6_2019.RData")#fish data


surv<-survdat
surv<-survdat[survdat$STRATUM %in% 1010:1760 == TRUE &
                survdat$SEASON %in% c("SPRING","FALL") == TRUE,]#just keep offshore strata
surv1<-surv[!duplicated(surv[c("ID", "SVSPP", "CATCHSEX")]),]



####label towz accorsing to spatail units
towz<-surv1[!duplicated(surv1$ID),c("ID","DECDEG_BEGLAT", "DECDEG_BEGLON")]


###Label points

###Label points
lazylab<-klab(towz$DECDEG_BEGLAT,towz$DECDEG_BEGLON)
labels<-data.frame(towz, lazylab)
plot(labels$DECDEG_BEGLON, labels$DECDEG_BEGLAT, col = labels$EPU,
     pch = 19, cex = .4)

head(labels)
ndf<-merge(surv1,labels, by = "ID")
nrow(ndf)
head(ndf)


########CALCULATE INDICATORS###############
ddd<-ndf
ddd<-droplevels(ddd)
ddd$EPU<-as.character(ddd$EPU)
ddd[is.na(ddd$EPU), "EPU"]<-"Not in NES"
table(ddd$EPU)

ddd$NYB<-as.character(ddd$NYB)
ddd[is.na(ddd$NYB), "NYB"]<-"Not in NYB"
table(ddd$NYB)

lenU<-function(x){length(unique(x))}# a function to count unique tows per year


#########This will do it for ALL species in the data
#First, do for NES and make into dataframe
tows_year<-aggregate(ID ~ EST_YEAR + SEASON, ddd[ddd$EPU != "Not in NES",], FUN = lenU, drop = FALSE)
tows_year$ID[is.na(tows_year$ID)]<-0
names(tows_year)[2]<-"SEASON"
names(tows_year)[3]<-"Towperyear"

#sum biomass of species by year species season
sum_sp<-aggregate(BIOMASS ~ EST_YEAR + SEASON, ddd[ddd$EPU != "Not in NES",], FUN = sum)

#bind to count data
sum_sp<-merge(sum_sp, tows_year, by = c("EST_YEAR", "SEASON"), all = TRUE)

#calculate mean species biomass/tow for each year
sum_sp$MeanSP<-sum_sp$BIOMASS/sum_sp$Towperyear
NES<-data.frame(Year = sum_sp$EST_YEAR,
                Variable = paste(sum_sp$SEASON, "TrawlBio", sep = "_"),
                Val = sum_sp$MeanSP,
                Loc = "NES",
                N = sum_sp$Towperyear)
NES

data.frame(1:ncol(ddd), colnames(ddd))

for(i in 48:49){
  ddd$EPU<-ddd[,i]
  tows_year_epu<-aggregate(ID ~ EST_YEAR +  SEASON + EPU, ddd, FUN = lenU, drop = FALSE)
  tows_year_epu$ID[is.na(tows_year_epu$ID)]<-0
  names(tows_year_epu)[4]<-"Towperyear"
  
  #sum biomass by year species season and EPU
  sum_sp<-aggregate(BIOMASS ~ EST_YEAR +  SEASON + EPU, ddd, FUN = sum)
  #bind to count data
  sum_sp<-merge(sum_sp, tows_year_epu, by = c("EST_YEAR","EPU", "SEASON"), all = TRUE)
  #calculate mean species biomass/tow for each year 
  sum_sp$MeanSP<-sum_sp$BIOMASS/sum_sp$Towperyear
  
  secondDF<-data.frame(Year = sum_sp$EST_YEAR,
                  Variable = paste(sum_sp$SEASON, "TrawlBio", sep = "_"),
                  Val = sum_sp$MeanSP,
                  Loc = sum_sp$EPU,
                  N = sum_sp$Towperyear)
  
  NES<-rbind(NES, secondDF)
}


NES<-NES[NES$Loc %in% c("NES", "NYB", "MAB") == TRUE,]

plot(NES[NES$Variable == "SPRING_TrawlBio" & NES$Loc == "NES", "Year"],
     NES[NES$Variable == "SPRING_TrawlBio" & NES$Loc == "NES", "Val"], type = "l",
ylim =c(0,500))

points(NES[NES$Variable == "SPRING_TrawlBio" & NES$Loc == "NYB", "Year"],
       NES[NES$Variable == "SPRING_TrawlBio" & NES$Loc == "NYB", "Val"], type = "l", lwd = 3, col = "red")

points(NES[NES$Variable == "SPRING_TrawlBio" & NES$Loc == "MAB", "Year"],
       NES[NES$Variable == "SPRING_TrawlBio" & NES$Loc == "MAB", "Val"], type = "l")

setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(NES, "Fish_totalBio_AUG24.csv")

