####Read in stranding data
setwd("~/Desktop/NYB Indicators/CalculateIndicators/Whales/Data")
whale<-read.csv("GAR_AllLargeWhale_2018_Retrieved29Nov19.csv", header = TRUE)

#remove periods from row names, create new abbreviated colnames that are easier to type
colnameLookup<-data.frame(old_names = colnames(whale),
                          new_names = abbreviate(gsub("[.]", "", colnames(whale))))
#change colnames of whale
colnames(whale)<-colnameLookup$new_names


#whats up with lat long
colnameLookup
#Stat is state
#YrfO is yea rof obs
#MntO is month of obs
#DyfO
#Spcs is species

table(whale$Spcs)


RecordCounts<-aggregate(DyfO ~ Stat + YrfO + Spcs, FUN = length, data = whale, drop = FALSE)
RecordCounts[is.na(RecordCounts$DyfO), "DyfO"]<-0


states<-unique(RecordCounts$Stat)
species<-unique(RecordCounts$Spcs)
par(mfrow = c(1,3))
MyStat<-c("NY", "MA", "ME")
for(i in 1:3){
plot(RecordCounts[RecordCounts$Stat == MyStat[i] & RecordCounts$Spcs == species[2], "YrfO"],
     RecordCounts[RecordCounts$Stat == MyStat[i]  & RecordCounts$Spcs == species[2], "DyfO"],
     type = "b", xlim =c(1990,2020),
     axes = FALSE, ylab = "",
     xlab = "", pch = 19, main = MyStat[i] )

  points(RecordCounts[RecordCounts$Stat == MyStat[i]  & RecordCounts$Spcs == species[7], "YrfO"],
     RecordCounts[RecordCounts$Stat == MyStat[i]  & RecordCounts$Spcs == species[7], "DyfO"],
     type = "b", col = "blue", pch = 19)

axis(1, pos = 0)
axis(2,las = 2)
if(i ==1){
legend("topleft", legend = c("acutorostrata", "novaeangliae"), 
       lty = 1, pch = 19, col = c(1,4))
mtext("Records in stranding database", 2, line = 2)

}

}

dev.off()
par(mfrow =c(3,4))

for(i in 1:length(states)){
  plot(RecordCounts[RecordCounts$Stat == states[i] & RecordCounts$Spcs == species[2], "YrfO"],
       RecordCounts[RecordCounts$Stat == states[i] & RecordCounts$Spcs == species[2], "DyfO"],
       type = "b", main = states[i], axes = FALSE, ylab = "", xlab = "", xlim = c(1980,2020),
       ylim =c(0,12))
  points(RecordCounts[RecordCounts$Stat == states[i] & RecordCounts$Spcs == species[7], "YrfO"],
       RecordCounts[RecordCounts$Stat == states[i] & RecordCounts$Spcs == species[7], "DyfO"],
       type = "b", col = "blue")
  
  axis(1)
  axis(2, las = 2)
  
  if(i == 5){
    mtext("Number of recrods in database",2, 3)
  }
}



data.frame()
     