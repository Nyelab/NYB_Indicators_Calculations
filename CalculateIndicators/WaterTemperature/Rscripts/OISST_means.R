setwd("~/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data")
ddd<-read.csv("L1_SST_data_ProcessedAUG30_2021.csv", header = TRUE)#takes awhile

####The Seasons
ddd$season<-NA
ddd[ddd$yday %in% 1:90 == TRUE,"season"]<-"winter"
ddd[ddd$yday %in% 91:181 == TRUE,"season"]<-"spring"
ddd[ddd$yday %in% 182:273 == TRUE,"season"]<-"summer"
ddd[ddd$yday %in% 274:365 == TRUE,"season"]<-"fall"
table(ddd$season)



#Do the summary stats
nes_SST<-aggregate(temp ~ year + season, FUN = mean, data = ddd[ddd$EPU != "Not in NES",])#ALL NES
lme_SST<-aggregate(temp ~ year + season + EPU, FUN = mean, data = ddd)#by EPU
MAB_SST<-lme_SST[lme_SST$EPU == "MAB",]
NYB_SST<-aggregate(temp ~ year + season + NYB, FUN = mean, data = ddd)#NYB
NYB_SST<-NYB_SST[NYB_SST$NYB == "NYB",]


season="fall"
season="spring"
season="winter"
season="summer"

plot(NYB_SST[NYB_SST$season %in% season == "TRUE", "year"],
     NYB_SST[NYB_SST$season %in% season == "TRUE", "temp"],
     type = "l", ylim =c(5,30))
points(MAB_SST[MAB_SST$season %in% season == "TRUE", "year"],
       MAB_SST[MAB_SST$season %in% season == "TRUE", "temp"],
     type = "l", col = "red")
points(nes_SST[nes_SST$season %in% season == "TRUE", "year"],
       nes_SST[nes_SST$season %in% season == "TRUE", "temp"],
       type = "l", col = "red")





####MAKE INTO LONG DF for LATER USE
MAB<-data.frame(Year = MAB_SST$year,
                    Variable = paste(MAB_SST$season, "OISST",sep ="_"),
                    Val = lme_SST$temp,
                    Loc = "MAB",
                    N = NA)

SST_nyb<-data.frame(Year = NYB_SST$year,
                    Variable = paste(NYB_SST$season,"OISST", sep = "_"),
                    Val = NYB_SST$temp,
                    Loc = "NYB",
                    N = NA)

NES<-data.frame(Year = nes_SST$year,
                    Variable = paste(nes_SST$season,"OISST", sep = "_"),
                    Val = nes_SST$temp,
                    Loc = "NES",
                    N = NA)


SSTindicators<-rbind(MAB, SST_nyb, NES)


plot(SSTindicators[SSTindicators$Variable == "summer_OISST", "Year"],
     SSTindicators[SSTindicators$Variable == "summer_OISST", "Val"],
     col = SSTindicators[SSTindicators$Variable == "fall_OISST", "Loc"])


#write combined dataset to file
setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(SSTindicators, "OISST_Means_AUG_31_2021.csv")
