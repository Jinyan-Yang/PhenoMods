# soil moisture 
# PACE_AUTO_S6_BLWGRND_R_20190228.dat

# par RH and tair
# PACE_AUTO_S6_ABVGRND_R_20190228.dat


# download.path <- file.path("download/")
# setToPath(download.path)

sensor.df <- read.csv('download/FIELD_SoilSensor Installation Master.csv')
# get met for pace
startDate = '2018-04-01'
endDate = '2018-08-30'

shelter.vec <- 1:6
swc.all.ls <- list()
for (shelter.num in shelter.vec){
  fn <- sprintf('PACE_AUTO_S%s_BLWGRND',shelter.num)
  library(HIEv)
  sw.df <- downloadTOA5(fn, 
                        maxnfiles = 100, 
                        rowbind=TRUE,
                        startDate = startDate,
                        endDate = endDate)      
  
  
  library(doBy)
  sw.df.sum <- summaryBy(.~Date,data = sw.df,FUN=mean,na.rm=TRUE,keep.names = TRUE)
  sw.df.sum <- subset(sw.df.sum,select = c(Date:VW_Avg.16.))
  sw.df.sum <- subset(sw.df.sum,select = -c(RECORD))
  # 
  sensor.df.sub <- sensor.df[sensor.df$Shelter == shelter.num,]
  # names(sw.df.sum)[names(sw.df.sum) == sprintf('VW_Avg.%s.',1)]
  swc.tmp.ls <- list()
  for (i in 1:(nrow(sensor.df.sub))){
    swc.tmp.ls[[i]] <- cbind(sensor.df.sub[sensor.df.sub$SensorNum == i,],
                             sw.df.sum[,c(1,which(names(sw.df.sum) == sprintf('VW_Avg.%s.',i)))])
    
    names(swc.tmp.ls[[i]])[names(swc.tmp.ls[[i]]) == sprintf('VW_Avg.%s.',i)] <- 'vwc'
  }
  
  swc.all.ls[[shelter.num]] <- do.call(rbind,swc.tmp.ls)
}

swc.all.df <- do.call(rbind,swc.all.ls)

swc.all.df$SubplotID <- as.character(swc.all.df$SubPlotID)
swc.all.df$Subplot <- as.character(swc.all.df$Subplot)
gcc.swc.df <- merge(gcc.plot.df,swc.all.df[,c('SubplotID','Location','vwc','Date','Shelter','Plot','Subplot')],
                    all = TRUE,by=c('Date','SubplotID','Subplot'))

