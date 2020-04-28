# library(DEoptim)
# library(Evapotranspiration)
gcc.pace.df <- readRDS('cache/pace.gcc.2019.rds')
gcc.pace.df$Date <- gcc.pace.df$DateTime
gcc.pace.df.daily <- doBy::summaryBy(GCC ~  Date + Cam + SubplotID +
                                       Species + Precipitation + Temperature,
                                     FUN = mean, na.rm=T,keep.names = T,
                                     data = gcc.pace.df)
saveRDS(gcc.pace.df.daily,'cache/gcc.pace.daily.2019.rds')

with(gcc.pace.df.daily[gcc.pace.df.daily$SubplotID == 'S1P1A',],plot(GCC~Date))
# library(HIEv)
# # library(data.table)
# library(doBy)
# 
# if(!dir.exists("download"))dir.create("download")
# # source('models/hufkens/phenoGrass.R')
# # source('models/hufkens/pen_mon.R')
# 
# header.df <- read.csv('download/SubPlotTreatmentsMaster 2019-02-13.csv')
# header.df <- subset(header.df,select = -c(SubPlotID,x1,x2,y1,y2))
# # gcc.pace.df <- read.csv('download/Colour May-Aug 2018.csv')
# # names(gcc.pace.df)[names(gcc.pace.df) =='CamSubPlot'] <- 'CameraSubplot'
# # 
# gcc.pace.df <- read.csv('download/ColourCoordinates2018.csv')
# 
# saveRDS(gcc.pace.df.daily,'cache/gcc.pace.daily.2019.rds')
# gcc.pace.df$GCC <- gcc.pace.df$G / (gcc.pace.df$R + gcc.pace.df$G + gcc.pace.df$B)
# gcc.pace.df$Date <- as.Date(as.character(gcc.pace.df$ï..DateTime),'%d/%m/%Y')
# # gcc.pace.df$Date <- gcc.pace.df$DateTime
# 
# 
# # gcc.pace.df.sd <- doBy::summaryBy(GCC~Date + Cam + Subplot,
# #                                      FUN = sd, na.rm=T,
# #                                      data = gcc.pace.df)
# gcc.pace.df.daily <- doBy::summaryBy(GCC~Date + Cam + Subplot,
#                                      FUN = mean, na.rm=T,keep.names = T,
#                                      data = gcc.pace.df)
# 
# gcc.pace.df.daily$Shelter <- ceiling(gcc.pace.df.daily$Cam / 8)
# #
# gcc.pace.df.daily$Plot <- gcc.pace.df.daily$Cam %% 8
# gcc.pace.df.daily$Plot[gcc.pace.df.daily$Plot == 0] <- 8
# 
# # saveRDS(gcc.pace.df.daily,'cache/gcc.pace.daily.rds')
# 
# # # names(gcc.pace.df.daily) <- c('Date','Cam','Subplot','RCC','GCC','BCC','Shelter','Plot')
# # 
# gcc.plot.df <- merge(header.df,gcc.pace.df.daily,by=c('Shelter','Plot','Subplot'),all.y=TRUE)
# gcc.plot.df <- gcc.plot.df[,c('Date','SubplotID','GCC',
#                               'Species', 'Precipitation','Temperature')]
# 
# saveRDS(gcc.plot.df,'cache/gcc.pace.daily.2018.rds')
# # # gcc.plot.df$ï..TIMESTAMP <- as.POSIXlt(as.character(gcc.plot.df$ï..TIMESTAMP),'%d/%m/%Y %H:%M',tz = 'UTC')
# # names(gcc.plot.df)[names(gcc.plot.df) == "Date"] <- 'DateTime'
# # gcc.plot.df <- gcc.plot.df[,c('DateTime','SubplotID','Subplot',
# #                               'Shelter','Plot','Species','Precipitation','Temperature',
# #                               'GCC','Camera')]
# # 
# # gcc.plot.df$Date <- as.Date(gcc.plot.df$DateTime)
# # gcc.plot.df$SubplotID <- as.character(gcc.plot.df$SubplotID)
# # gcc.plot.df$Subplot <- as.character(gcc.plot.df$Subplot)
# # gcc.plot.df <- subset(gcc.plot.df,select = -DateTime)
# # # getting the mean of the four gcc for now
# # # gcc.plot.df <- summaryBy(GCC~.,data = gcc.plot.df,FUN=mean,na.rm=TRUE,keep.names = TRUE)
# 
