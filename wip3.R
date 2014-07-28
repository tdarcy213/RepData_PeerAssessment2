## load the required R packages, then display our operating environment information
library(data.table)
library(ggplot2)
library(gridExtra)
Sys.info()
sessionInfo()
## download the file, then read into memory
downloadUrl<-"http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
downloadFile<-"FStormData.csv.bz2"
download.file(downloadUrl,downloadFile)

stormData<-data.table(read.table(downloadFile,sep=",",header=T,stringsAsFactors=F))
## Here are the characteristics of the file:
summary(stormData)
stormData$BGN_DATE<-as.Date(stormData$BGN_DATE,format="%m/%d/%Y")
stormData$EV_YEAR<-as.numeric(format(stormData$BGN_DATE,"%Y"))
stormData$PROP_DMG<-(
        ifelse(stormData$PROPDMGEXP=="H",100,
               ifelse(stormData$PROPDMGEXP=="K",1000,
                      ifelse(stormData$PROPDMGEXP=="M",1000000,
                             ifelse(stormData$PROPDMGEXP=="B",1000000000,1))))*stormData$PROPDMG)
stormData$CROP_DMG<-(
        ifelse(stormData$CROPDMGEXP=="H",100,
               ifelse(stormData$CROPDMGEXP=="K",1000,
                      ifelse(stormData$CROPDMGEXP=="M",1000000,
                             ifelse(stormData$CROPDMGEXP=="B",1000000000,1))))*stormData$CROPDMG)

stormDataSmry<-stormData[,list(SUM_HEALTH=sum(FATALITIES+INJURIES),SUM_ECON=sum(PROP_DMG+CROP_DMG)),by=c("EV_YEAR","EVTYPE")]
stormDataMax<-stormDataSmry[,list(MAX_HEALTH=max(SUM_HEALTH),MAX_ECON=max(SUM_ECON)),by=c("EV_YEAR")]
stormDataSmry<-merge(stormDataSmry,stormDataMax,by=c("EV_YEAR"))
summary(stormDataSmry)


##There appears to be an error in the PROPDMGEXP for 2006 NAPA RIVER FLOOD
stormData[stormData$PROP_DMG==max(stormData$PROP_DMG),]
##LOOKING @ DATA, IT APPEARS "B" EXP IN ANNOMALOUS, LET'S CHANGE TO "M"
stormData$PROPDMGEXP[stormData$PROP_DMG==max(stormData$PROP_DMG)]
stormData$CROPDMGEXP[stormData$PROP_DMG==max(stormData$PROP_DMG)]
stormData$PROPDMGEXP[stormData$PROP_DMG==max(stormData$PROP_DMG)]<-"M"
##NOW, GO BACK AND RECALCULATE THE SUMMARY DATA

stormData$PROP_DMG<-(
        ifelse(stormData$PROPDMGEXP=="H",100,
               ifelse(stormData$PROPDMGEXP=="K",1000,
                      ifelse(stormData$PROPDMGEXP=="M",1000000,
                             ifelse(stormData$PROPDMGEXP=="B",1000000000,1))))*stormData$PROPDMG)
stormData$CROP_DMG<-(
        ifelse(stormData$CROPDMGEXP=="H",100,
               ifelse(stormData$CROPDMGEXP=="K",1000,
                      ifelse(stormData$CROPDMGEXP=="M",1000000,
                             ifelse(stormData$CROPDMGEXP=="B",1000000000,1))))*stormData$CROPDMG)

stormDataSmry<-stormData[,list(SUM_HEALTH=sum(FATALITIES+INJURIES),SUM_ECON=sum(PROP_DMG+CROP_DMG)),by=c("EV_YEAR","EVTYPE")]
stormDataMax<-stormDataSmry[,list(MAX_HEALTH=max(SUM_HEALTH),MAX_ECON=max(SUM_ECON)),by=c("EV_YEAR")]
stormDataSmry<-merge(stormDataSmry,stormDataMax,by=c("EV_YEAR"))
summary(stormDataSmry)
healthSmry<-subset(stormDataSmry,SUM_HEALTH==MAX_HEALTH,select=c("EV_YEAR","EVTYPE","SUM_HEALTH"))
econSmry<-subset(stormDataSmry,SUM_ECON==MAX_ECON,select=c("EV_YEAR","EVTYPE","SUM_ECON"))
maxeconSmry<-econSmry[econSmry$SUM_ECON==max(econSmry$SUM_ECON),]
maxhealthSmry<-healthSmry[healthSmry$SUM_HEALTH==max(healthSmry$SUM_HEALTH),]
phealthSmry<-ggplot(healthSmry[order(EV_YEAR)],aes(x=EV_YEAR,y=SUM_HEALTH,fill=EVTYPE))+geom_bar(color="black",stat="identity",position=position_dodge())+ggtitle(expression(atop("NOAA STORM DATA", atop(italic("1950 - 2011"), ""))))+xlab("YEAR")+ylab("TOTAL HEALTH EVENTS")+scale_fill_brewer(palette="Paired")+geom_text(data = maxhealthSmry, aes(label=SUM_HEALTH),hjust=1.5, vjust=1,size=3,color="blue4")
peconSmry<-ggplot(econSmry[order(EV_YEAR)],aes(x=EV_YEAR,y=SUM_ECON,fill=EVTYPE))+geom_bar(color="black",stat="identity",position=position_dodge())+ggtitle(expression(atop("NOAA STORM DATA", atop(italic("1950 - 2011"), ""))))+xlab("YEAR")+ylab("TOTAL ECONOMIC DAMAGE")+scale_fill_brewer(palette="Paired")+geom_text(data = maxeconSmry, aes(label=SUM_ECON),hjust=1.25, vjust=1,size=3,color="blue4")
grid.arrange(peconSmry,phealthSmry,nrow=2)

stormDataTot<-stormDataSmry[,list(TOT_HEALTH=sum(SUM_HEALTH),TOT_ECON=sum(SUM_ECON)),by=c("EVTYPE")]
maxeconTot<-stormDataTot[stormDataTot$TOT_ECON==max(stormDataTot$TOT_ECON),]
maxhealthTot<-stormDataTot[stormDataTot$TOT_HEALTH==max(stormDataTot$TOT_HEALTH),]
top5health<-stormDataTot[order(TOT_HEALTH,decreasing=T),][1:5]
top5econ<-stormDataTot[order(TOT_ECON,decreasing=T),][1:5]
ptop5health<-ggplot(top5health[order(TOT_HEALTH)],aes(x=EVTYPE,y=TOT_HEALTH,fill=EVTYPE))+geom_bar(color="black",stat="identity",position=position_dodge())+ggtitle(expression(atop("NOAA STORM DATA", atop(italic("1950 - 2011"), ""))))+xlab("EVENT")+ylab("TOTAL HEALTH EVENTS")+scale_fill_brewer(palette="Paired")+geom_text(data = maxhealthTot, aes(label=TOT_HEALTH),hjust=2.5, vjust=1,size=4,color="blue4")+theme(axis.text.x = element_blank())
ptop5econ<-ggplot(top5econ[order(TOT_ECON)],aes(x=EVTYPE,y=TOT_ECON,fill=EVTYPE))+geom_bar(color="black",stat="identity",position=position_dodge())+ggtitle(expression(atop("NOAA STORM DATA", atop(italic("1950 - 2011"), ""))))+xlab("EVENT")+ylab("ECON")+scale_fill_brewer(palette="Paired")+geom_text(data = maxeconTot, aes(label=TOT_ECON),hjust=1.5, vjust=1,size=4,color="blue4")+theme(axis.text.x = element_blank())
grid.arrange(ptop5health,ptop5econ,nrow=2)
