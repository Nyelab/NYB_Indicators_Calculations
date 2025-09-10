# trans dates from grided data

library(raster)

############################################################ choice
#justmeans = "yes"  # if yes trans based on just the local mean
justmeans = "no"  # if no trans based on local mean plus minus .1

############################################################ choice
# data output
#outfile="nes ecoreg half deg grid trans.csv"
#outfile="shark OUT.csv"
#outfile="nes half deg stand area grid trans local means.csv"
outfile="NYB_plus_minus_means.csv"

############################################################ choice
# is the last year a partial year 
#  NEED 182 in last partial year
partial="yes"
#partial="no"

############################################################ choice
# start end year
firstyr=1982
# last full data year
lastyr=2019
tsyears=lastyr-firstyr+1
tsrows=365*tsyears

############################################################ choice
# path for SST files
napath="/Users/nyelab/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/"
setwd(napath)
#napath="E:/4_rs_data_temperature/oisst_25deg_daily/FULL_AV.TEMP.1DAY/SHARK"
#setwd(napath)

############################################################ choice
# get the locals
#localsfile="C:/1_analyses_ne_shelf/trans date/nes ecoreg half deg grid.csv"
#localsfile="C:/1_analyses_ne_shelf/trans date/nes ecoreg half deg grid gom sub.csv"
#localsfile="C:/1_analyses_ne_shelf/shark/trans/shark half deg grid.csv"
#localsfile="C:/3_rs_prog/masks_shp/half deg for full stand area/nes full stand area grid.csv"

#locals=read.csv(localsfile,header = TRUE)
#numlocals=nrow(locals)
#plot(locals$lon,locals$lat)

############################################################ choice
# read input data
#tdata = read.csv("TS_SHP_nes half deg all ecoregionsPoly.csv")
tdata = read.csv("/Users/nyelab/Desktop/NYB Indicators/CalculateIndicators/WaterTemperature/Data/L1_SST_data_ProcessedAUG30_2021.csv")
tdata <- tdata[tdata$NYB == 'NYB',] #only data points within NYB
numlocals=length(unique(tdata$lat_lon))
plot(tdata$lon,tdata$lat)
# ################################################# END SET

df <- data.frame(point_num=integer(), lat_lon=character(), lat=integer(), lon=integer(), year=integer(), spr_trans=integer(), fall_trans=integer(), maxday=integer(), sumlen=integer())



# loop for each lat lon point
for (locs in 1:numlocals){
  time=Sys.time()
  marker=paste("loc",locs,time)
  print(marker)
  
  
  localdata = tdata[which(tdata$lat_lon==unique(tdata$lat_lon)[locs]),]
  local_lat = localdata$lat[locs]
  local_lon = localdata$lon[locs]
  
  #localdata=localdata[-1]
  
  
  
  # detemine long term mean
  # clim is first 30 years
  localdatamean=mean(localdata[localdata[,6]<2012,5], na.rm=T)
  minld=min(localdata[,5])
  maxld=max(localdata[,5])
  
  spr_localdatamean = localdatamean
  fall_localdatamean = localdatamean
  
  if(justmeans=="no"){
    spr_localdatamean = localdatamean*.9
    fall_localdatamean = localdatamean*1.1
  }
  
  # new year loop that find trans dates
  for(year in firstyr:lastyr){
    
    sprtrans=NA
    falltrans=NA
    maxday=NA
    
    subset = localdata[localdata[,6]==year,]
    subset = subset[order(subset[,9], decreasing = FALSE),]
    
    slocaldata=movingFun(subset$temp, 5, fun=mean, type='around', circular=FALSE, na.rm=T) 
    
    maxslocaldata=max(slocaldata)
    
    if (is.finite(localdatamean)){
      
      for (dayc in 105:195){
        if (slocaldata[dayc] > spr_localdatamean){
          sprtrans=dayc
          break
        }
      }
      
      for (dayc in 275:365){
        if (slocaldata[dayc] < fall_localdatamean){
          falltrans=dayc
          break
        }
      }
      
      for (dayc in 1:365){
        if (slocaldata[dayc] == maxslocaldata){
          maxday=dayc
          break
        }
      }
    } # end if
    
    sumlen=falltrans-sprtrans
    
    df[nrow(df) + 1 ,]=c(locs,unique(tdata$lat_lon)[locs],local_lat, local_lon, year, sprtrans, falltrans, maxday, sumlen)
    
    #outline=paste(locs,tdata$lat_lon[locs],year, sprtrans, falltrans, maxday, sumlen)
    #write.table(outline,file=outfile,row.name=F,col.names=F,append=TRUE)
    
  } # end second year  loop
  
} # end locals loop

df[, c(1,3:9)] <- sapply(df[, c(1,3:9)], as.numeric)
yrmean_sp <- rep(NA, 38)
yrmean_fa <- rep(NA, 38)
yrmean_md <- rep(NA, 38)
yrmean_sl <- rep(NA, 38)
a = 1
for (year in firstyr:lastyr){
  yrmean_sp[a] <- mean(df[which(df[,3]==year),4])
  yrmean_fa[a] <- mean(df[which(df[,3]==year),5])
  yrmean_md[a] <- mean(df[which(df[,3]==year),6])
  yrmean_sl[a] <- mean(df[which(df[,3]==year),7])
  a <- a+1
}

mean_spr <-aggregate(spr_trans~year, df, FUN=mean)
mean_fall <- aggregate(fall_trans~year, df, FUN=mean)

linearMod<- lm(spr_trans ~ year, data=mean_spr)
summary(linearMod)

linearMod<- lm(fall_trans ~ year, data=mean_fall)
summary(linearMod)

spr <- ggplot() + 
  #geom_line(data = df, aes(x = year, y = flowrate), color = 'grey') +
  geom_point(data = df, aes(x = year, y = spr_trans), color = 'gray') + 
  geom_point(data = mean_spr, aes(x = year, y = spr_trans), color = 'gray53') +
  geom_line(data = mean_spr, aes(x = year, y = spr_trans), color = 'grey53', size = 1) +
  geom_smooth(data = mean_spr, aes(x = year, y = spr_trans), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Day of Year', x = 'Year', title = 'Spring Transition Day') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


fall <- ggplot() + 
  #geom_line(data = df, aes(x = year, y = flowrate), color = 'grey') +
  geom_point(data = df, aes(x = year, y = fall_trans), color = 'gray') + 
  geom_point(data = mean_fall, aes(x = year, y = fall_trans), color = 'gray53') +
  geom_line(data = mean_fall, aes(x = year, y = fall_trans), color = 'grey53', size = 1) +
  geom_smooth(data = mean_fall, aes(x = year, y = fall_trans), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Day of Year', x = 'Year', title = 'Fall Transition Day') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))

sum_len <- ggplot() + 
  #geom_line(data = df, aes(x = year, y = flowrate), color = 'grey') +
  geom_point(data = df, aes(x = year, y = sumlen), color = 'gray') + 
  #geom_point(data = mean_fall, aes(x = year, y = sum_len), color = 'gray53') +
  #geom_line(data = mean_fall, aes(x = year, y = sum_len), color = 'grey53', size = 1) +
  #geom_smooth(data = mean_fall, aes(x = year, y = sum_len), method = lm, se = FALSE, color = 'black') + 
  #geom_line(data=pdata, aes(x = year, y = p2_mod+intercept), se = FALSE, color = 'black', linetype = 'twodash', size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$incr)+intercept, x = year), color = "blue", size = 1) + 
  #geom_line(data = pdata, aes(y = unlist(mod.dsig$decr)+intercept, x = year), color = 'red', size = 1) + 
  theme_bw() +
  labs (y = 'Day of Year', x = 'Year', title = 'Fall Transition Day') + 
  theme(plot.title=element_text(size = 16,face = 'bold',hjust = 0.5), axis.title=element_text(size = 14, face = 'bold'), axis.text= element_text(color = 'black', size = 12))


# Now plot all 4 together
library(ggpubr)

ggarrange(spr,fall,nrow=1,ncol=2)
