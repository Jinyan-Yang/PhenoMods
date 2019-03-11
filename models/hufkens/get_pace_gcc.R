library(DEoptim)
library(Evapotranspiration)
library(HIEv)
library(data.table)
library(DoBy)

if(!dir.exists("download"))dir.create("download")
source('models/hufkens/phenoGrass.R')
source('models/hufkens/pen_mon.R')

header.df <- read.csv('download/SubPlotTreatmentsMaster 2019-02-13.csv')
header.df <- subset(header.df,select = -c(x1,x2,y1,y2))
gcc.pace.df <- read.csv('download/Colour May-Aug 2018.csv')
names(gcc.pace.df)[names(gcc.pace.df) =='CamSubPlot'] <- 'CameraSubplot'

gcc.plot.df <- merge(header.df,gcc.pace.df,by=c('CameraSubplot','Subplot'),all=TRUE)
gcc.plot.df$ï..TIMESTAMP <- as.POSIXlt(as.character(gcc.plot.df$ï..TIMESTAMP),'%d/%m/%Y %H:%M',tz = 'UTC')
names(gcc.plot.df)[names(gcc.plot.df) == "ï..TIMESTAMP"] <- 'DateTime'
gcc.plot.df <- gcc.plot.df[,c('DateTime','CameraSubplot','Subplot','SubplotID',
                              'Shelter','Plot','Species','Precipitation','Temperature',
                              'Camera','GCC')]

gcc.plot.df$Date <- as.Date(gcc.plot.df$DateTime)
gcc.plot.df$SubplotID <- as.character(gcc.plot.df$SubplotID)
gcc.plot.df$Subplot <- as.character(gcc.plot.df$Subplot)
gcc.plot.df <- subset(gcc.plot.df,select = -DateTime)
# getting the mean of the four gcc for now
gcc.plot.df <- summaryBy(GCC~.,data = gcc.plot.df,FUN=mean,na.rm=TRUE,keep.names = TRUE)
