###########################
#######Heim et al. 2020####
###########################
#Create Zoop abundance anomalies
#Use ECOMON
#Label the points according to spatial unit
#Calculate abundance anomolies
#Kurt Heim, Last edited AUG 24, 2020

#The script NEEDS to be reviewed because the order of the columns will change in future years
#I have tried to annoate the parts where changes will be needed in the future


######Prepare the dataset######
library(readxl)
library(lubridate)
library(rgdal)
library(nngeo)
library(reshape2)

#####load required functiosn and shapefiles
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Rfunctions")
source("LabelPoints.R")


setwd("~/Desktop/NYB Indicators/CalculateIndicators/Zooplankton/Data")
ZPD=openxlsx::read.xlsx("L1_EcoMon_Plankton_Data_v3_5.xlsx", sheet='Data')


dt=as_date(ZPD$date, origin = "1899-12-30")
DOY=yday(dt) #day of year
month=as.numeric(format(dt, '%m'))
year=as.numeric(format(dt, '%Y'))
ZPD$year=year
ZPD$month=month
ZPD$dt=dt
ZPD$DOY=DOY
ZPD$day=as.numeric(format(dt, '%d'))
ZPD$lat2=ceiling(ZPD$lat) #use for binning into 1 degree bins for removal of undersampled bins
ZPD$lon2=floor(ZPD$lon) #use for binning into 1 degree bins for removal of undersampled bins




######REORDER
ZPDb=ZPD[,c(1:14, 290:296, 106:197)] #####CAHNGED 297 to 296
ZPDb=ZPDb[order(ZPDb$date),]
ZPDb=ZPDb[which(ZPDb$year > 1976),] # remove NA data in years prior to 1977



########Select only taxa present in yearly data > x percent of samples
X=20 # percent criteria to use as minimum percent in samples
ZPDa=ZPDb
ZPDa=ZPDa[!is.na(ZPDa$zoo_gear),] # Remove NA in zooplankton rows

# Reduce to taxa occurrance > x percent in samples
p.a=ZPDa[,23:113]
p.a[p.a > 0]=1 # presence/absence
count=colSums(p.a)
pct=(count/dim(ZPDa)[1])*100
crit=which(pct>X)
ZPDa=ZPDa[c(1:22,crit+22)]


#####Take median date from cruize
cruises=unique(ZPDa$cruise_name)
#get median day of year of the cruise
for (i in 1:length(cruises)){
  ZPDa$medmonth[ZPDa$cruise_name == cruises[i]]=median(ZPDa$DOY[ZPDa$cruise_name == cruises[i]])
}

#storage for bimonthly categories
ZPDa$bmm=NA
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(0,59))]=1
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(60,120))]=3
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(121,181))]=5
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(182,243))]=7
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(244,304))]=9
ZPDa$bmm[which(as.integer(ZPDa$medmonth) %in% seq(305,366))]=11

ZPDa[,14]=as.numeric(ZPDa[,14])#CHANGE volumne to NUMBER
ZPDa[,22]=as.numeric(ZPDa[,22])#Change bolumne 100m3 to number
ZPDsave=ZPDa #not sure what this is for



############CALCULATE INDICATORS#########
SEASON='Yearly'
ZPDa=ZPDsave

# LOG transform data using ZPDa from above (select season first)

test=log10(ZPDa[,22:49]+1) #choose columns with zooplankton data, 22:49 are the zoops so these columns might need changing in future
ZPDlog=ZPDa
ZPDlog[,22:49]=test#the columns that hold zoop data
nm=matrix(colnames(ZPDlog))



#####CREATE MY LABELS
lazylab<-klab(ZPDlog$lat,ZPDlog$lon)
ZPDlog<-data.frame(ZPDlog, lazylab)

##change to character and fill in NAs
ZPDlog$NYB<-as.character(ZPDlog$NYB)
ZPDlog[is.na(ZPDlog$NYB),"NYB"]<-"Not in NYB"
ZPDlog$EPU<-as.character(ZPDlog$EPU)
ZPDlog[is.na(ZPDlog$EPU),"EPU"]<-"Not in NES"
colnames(ZPDlog)[54]<-"NES"
ZPDlog[ZPDlog$EPU != "Not in NES", "NES"]<-"NES"
ZPDlog[ZPDlog$EPU == "Not in NES", "NES"]<-"Not in NES"


#doubple check sspatial boundaries
NES<-ZPDlog[ZPDlog$NES == "NES",]
plot(NES$lat, NES$lon)

NYB<-ZPDlog[ZPDlog$NYB == "NYB",]
points(NYB$lat, NYB$lon, col = "red")

#The areas to do it for.
areas<-unique(ZPDlog$EPU)
zoop_anomaly<-list()
data.frame(1:length(ZPDlog), colnames(ZPDlog))
all_this_scale<-list()

for(col_sep in 52:54){ ###loop over scale 1, scale 2 etc.
  #col_sep = 52
  print(col_sep)
  
  
  #col_sep = 55#which column is the scale unit designation
  areas<-unique(ZPDlog[,col_sep])#units to loop over
  n_areas<-length(areas)#number of units to loop over
  zoop_anomaly<-list()#empty list to put in zoop data for scale 

for(k in 1:n_areas){

  #k=1
  print(k)
  gbk.yr.spln=data.frame()
  ZPDlog$epu<-ZPDlog[,col_sep]
  
  for (i in 22:49){  #22 to 49 are the columns that have zoop data
    #num=23#first taxa 
    num=i
    #num=23
    name=nm[num,1]
    mean.loc.x=aggregate(ZPDlog[which(ZPDlog$epu==areas[k]),num], by=list(ZPDlog$bmm[which(ZPDlog$epu==areas[k])]), FUN=mean, na.rm=T)
    func = splinefun(mean.loc.x[,1], y=mean.loc.x[,2], method="natural",  ties = mean)  
    x.daily=func(seq(1, 12, 0.0302)) #365 days
    gbk.yr.spln=rbind(gbk.yr.spln,x.daily)
  }
  gbk.yr.spln=t(gbk.yr.spln)
  rownames(gbk.yr.spln)=seq(1:365)
  colnames(gbk.yr.spln)=nm[22:49,1] #22 thrhrou 49 are the columns with zoop data
  
  #gbk.yr.spln is the spline expectations for each bug (in columns) for each day of the year(in rows)
  
  #make a new dataframe to work with for just the area of interest
  #gbk.anom=ZPDlog[which(ZPDlog$epu=='GBK'),] #why is which used here? extra words
  gbk.anom=ZPDlog[ZPDlog$epu==areas[k],]
  #make empty frame to put things
  gbk.anom.b=data.frame(matrix(NA, nrow = dim(gbk.anom)[1], ncol = dim(gbk.anom)[2]))
  
  #calculate the anomly as the measeruemnt subracted from the spline expectation
  #for the day that the sample was taken!  I get it.
  for (i in 1:dim(gbk.anom)[1]){

    gbk.anom.b[i,22:49]=gbk.anom[i,22:49]-gbk.yr.spln[which(gbk.anom$DOY[i]==rownames(gbk.yr.spln)),]
    print(i)
  }
  
  gbk.yr.anom=aggregate(gbk.anom.b, by=list(gbk.anom$year), FUN=mean, na.rm=T)
  rownames(gbk.yr.anom)=gbk.yr.anom[,1]
  gbk.yr.anom[,1]=NULL
  colnames(gbk.yr.anom)=colnames(gbk.anom)
  df1<-data.frame(gbk.yr.anom)
  df1<-df1[,22:49]
  df1$Scale1<-colnames(ZPDlog)[col_sep]
  df1$Scale2<-areas[k]
  df1$year<-row.names(df1)
  

  ####ADD in small- large copepod abundance anomaly
  #####MAKE SURE THAT THESE COLUMNS CORRESPOND WITH THE LISTED SPECIES
  lgtx=c(3) #column for Calanus finmarchicus
  smtx=c(4,2,6,5)#Pseudocalanus spp, Centropoges typicus, Centropages hamatus, Temora longicornis
  
  lgZ<-df1[,3]
  smZ<-df1[,c(4,2,6,5)]
  df1$sm_lg<-rowMeans(smZ)-lgZ
  df1<-melt(df1,id.vars = c("year","Scale1","Scale2"))
  
  
  ###Counts per year in this area N size######
  yearcounts<-data.frame(year = names(table(ZPDlog[ZPDlog$epu %in% areas[k] == TRUE,"year"])),
                         counts_year = as.vector(table(ZPDlog[ZPDlog$epu %in% areas[k] == TRUE,"year"])))
  df1<-merge(df1, yearcounts, by = "year")
  
  
  zoop_anomaly[[k]]<-df1
}

  all_this_scale[[col_sep-51]]<-do.call('rbind',zoop_anomaly)
                     print(col_sep)     
}
 
###what the f is do.call

######GET IT ALL BACK TOGETHER######
ddd<-do.call('rbind', all_this_scale)


#get rid of stuff not needed
NYB<-ddd[ddd$Scale2 == "NYB",]
NES<-ddd[ddd$Scale2 == "NES",]
MAB<-ddd[ddd$Scale2 == "MAB",]


fd<-rbind(NYB, NES, MAB)

finalTS<-data.frame(Year = fd$year,
                Variable = fd$variable,
                Val = fd$value,
                Loc = fd$Scale2,
                N = fd$counts_year)
finalTS$Year<-as.numeric(as.character(finalTS$Year))

varz<-unique(finalTS$Variable)
for(i in 1:length(varz)){
plot(finalTS[finalTS$Variable %in% varz[i] == TRUE & finalTS$Loc == "NES","Year"],
     finalTS[finalTS$Variable %in% varz[i] == TRUE & finalTS$Loc == "NES","Val"],
     type = "l", main = varz[i])
points(finalTS[finalTS$Variable %in% varz[i] == TRUE & finalTS$Loc == "MAB","Year"],
     finalTS[finalTS$Variable %in% varz[i] == TRUE & finalTS$Loc == "MAB","Val"],
     type = "l", main = varz[i], col = "red")
points(finalTS[finalTS$Variable %in% varz[i] == TRUE & finalTS$Loc == "NYB","Year"],
     finalTS[finalTS$Variable %in% varz[i] == TRUE & finalTS$Loc == "NYB","Val"],
     type = "l", main = varz[i], col = "blue")
}


setwd("~/Desktop/NYB Indicators/Final_timeseries")
write.csv(finalTS, file = "Zoops.csv")




