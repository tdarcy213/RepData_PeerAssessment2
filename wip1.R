downloadUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
downloadFile<-"FStormData.csv.bz2"
download.file(downloadUrl,downloadFile)
stormData<-read.table(downloadFile,sep=",")
