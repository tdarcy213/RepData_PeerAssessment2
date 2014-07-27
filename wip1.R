downloadUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
downloadFile<-"FStormData.csv.bz2"
download.file(downloadUrl,downloadFile)
library(data.table)
stormData<-read.table(downloadFile,sep=",",header=T,stringsAsFactors=F)
stormData<-data.table(stormData)
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

stormDataSmry<-stormData[,list(SUM_HUMAN=sum(FATALITIES+INJURIES),SUM_ECON=sum(PROP_DMG+CROP_DMG)),by=c("EV_YEAR","EVTYPE")]
stormDataMax<-stormDataSmry[,list(MAX_HUMAN=max(SUM_HUMAN),MAX_ECON=max(SUM_ECON)),by=c("EV_YEAR")]
stormDataSmry<-merge(stormDataSmry,stormDataMax,by=c("EV_YEAR"))
##There appears to be an error in the PROPDMGEXP for 2006 NAPA RIVER FLOOD
stormData[stormData$PROP_DMG==max(stormData$PROP_DMG),]
##LOOKING @ DATA, IT APPEARS "B" EXP IN ANNOMALOUS, LET'S CHANGE TO "M"
stormData$PROPDMGEXP[stormData$PROP_DMG==max(stormData$PROP_DMG)]<-"M"
##NOW, GO BACK AND RECALCULATE THE SUMMARY DATA

stormData$PROPDMGEXP[stormData$PROP_DMG==max(stormData$PROP_DMG)]
humanSmry<-subset(stormDataSmry,SUM_HUMAN==MAX_HUMAN,select=c("EV_YEAR","EVTYPE","SUM_HUMAN"))
econSmry<-subset(stormDataSmry,SUM_ECON==MAX_ECON,select=c("EV_YEAR","EVTYPE","SUM_ECON"))
phumanSmry<-ggplot(humanSmry[order(EV_YEAR)],aes(x=EV_YEAR,y=SUM_HUMAN,fill=EVTYPE))
phumanSmry+geom_bar(color="black",stat="identity",position=position_dodge())+ggtitle(expression(atop("NOAA STORM DATA", atop(italic("1950 - 2011"), ""))))+xlab("YEAR")+ylab("HUMAN CASUALTIES")
peconSmry<-ggplot(econSmry[order(EV_YEAR)],aes(x=EV_YEAR,y=SUM_ECON,fill=EVTYPE))
peconSmry+geom_bar(color="black",stat="identity",position=position_dodge())+ggtitle(expression(atop("NOAA STORM DATA", atop(italic("1950 - 2011"), ""))))+xlab("YEAR")+ylab("TOTAL ECONOMIC DAMAGE")
