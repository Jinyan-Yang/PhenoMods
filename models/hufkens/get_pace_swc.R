# soil moisture 
# PACE_AUTO_S6_BLWGRND_R_20190228.dat

# par RH and tair
# PACE_AUTO_S6_ABVGRND_R_20190228.dat
library(RCurl)
library(HIEv)
download.path <- file.path("download/")
setToPath(download.path)

sensor.df <- read.csv('download/FIELD_SoilSensor Installation Master.csv')
# get met for pace
startDate = '2018-03-01'
endDate = '2019-12-31'
# startDate = '2019-01-01'
# endDate = '2019-12-31'

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
swc.all.df$Date <- as.Date(as.character(swc.all.df$Date),'%Y-%m-%d')
# swc.all.df$SubplotID <- as.character(swc.all.df$SubplotID)
swc.all.df$Subplot <- as.character(swc.all.df$Subplot)
saveRDS(swc.all.df,'cache/swc.rds')


# swc.all.df <- readRDS('cache/swc.rds')
gcc.swc.df <- merge(gcc.plot.df,swc.all.df[,c('SubplotID','Location','vwc','Date','Shelter','Plot','Subplot','Species')],
                    all = TRUE,by=c('Date','SubplotID','Shelter','Plot','Subplot','Species'))

# gcc.swc.df$Shelter <- substr(gcc.swc.df$SubplotID,2,2)
# gcc.swc.df$Plot <- substr(gcc.swc.df$SubplotID,4,4)

# plot #####################
header.df <- read.csv('download/SubPlotTreatmentsMaster 2019-02-13.csv')
header.df <- subset(header.df,select = c(SubplotID,Precipitation,Temperature))
header.df$SubplotID <- as.character(header.df$SubplotID)
header.df$Precipitation <- as.character(header.df$Precipitation)
header.df$Temperature <- as.character(header.df$Temperature)

swc.df <- merge(swc.all.df,header.df)
swc.df$plot.factor <- as.factor(swc.df$SubplotID )
names(swc.df)
swc.df <- summaryBy(vwc ~SubplotID+Date+Shelter+Plot+Subplot+
                    Temperature+Precipitation + Species,
                    data = swc.df,FUN=mean,na.rm=T,keep.names = T)
plot.vwc.func <- function(cWaT.df){
  # palette(viridis(6))
  # wave.fit.cWaT <- gam(GCC~s(yday(Date),k=20),data = cWaT.df)
  # plot(GCC~Date,data = cWaT.df,col = 'grey',xlab='2018',pch=paste0(facto.vec[plot.factor]))
  # points(wave.fit.cWaT$fitted.values~cWaT.df$Date,type='l',col='black',lwd=3)
  
  plots.vec <- unique(cWaT.df$Shelter)
  y.min <- floor(min(cWaT.df$vwc,na.rm=T)*1000)/1000
  y.max <- ceiling(max(cWaT.df$vwc,na.rm=T)*1000)/1000
  # facto.vec <- unique(cWaT.df$Shelter)
  for(plot.num in seq_along(plots.vec)){
    plot.df <- cWaT.df[cWaT.df$Shelter == plots.vec[plot.num],]
    if(plot.num == 1){
     #  wave.fit.cWaT <- gam(vwc~s(yday(Date),k=80),data = plot.df)
     #  plot(vwc~Date,data = plot.df,col = plot.num,xlab=' ',pch=paste0(Shelter),
     #       cex=0.7,ylim=c(y.min,y.max))
     # with(plot.df,points(wave.fit.cWaT$fitted.values~Date,type='l',col=Shelter,lwd=2))
      
      
      #  wave.fit.cWaT <- gam(vwc~s(yday(Date),k=80),data = plot.df)
       plot(vwc~Date,data = plot.df,col = plot.num,xlab=' ',type='l',
            lwd=2,ylim=c(y.min,y.max))
      # with(plot.df,points(wave.fit.cWaT$fitted.values~Date,type='l',col=Shelter,lwd=2))
    }else{
      # wave.fit.cWaT <- gam(vwc~s(yday(Date),k=20),data = plot.df)
      # points(vwc~Date,data = plot.df,col = Shelter,pch=paste0(Shelter),cex=0.7)
      # with(plot.df,points(wave.fit.cWaT$fitted.values~Date,type='l',col=Shelter,lwd=2))
      points(vwc~Date,data = plot.df,col = Shelter,type='l',lwd=2)
    }
  }
  legend('topleft',legend = paste0(unique(cWaT.df$Species),'-',
                                   unique(cWaT.df$Precipitation),'-',
                                   unique(cWaT.df$Temperature)),bty='n')
  legend('topright',legend = unique(cWaT.df$Shelter),col=palette(),pch=16,bty='n',horiz = T)
}

species.vec <- unique(as.character(swc.df$Species))
pdf('pace_vwc_species_treat.pdf',width = 12,height = 12*0.618)
library("viridisLite")  
palette(viridis(6))
par(mar=c(3,5,1,1))


for(i in seq_along(species.vec)){
  
  plot.df <- swc.df[swc.df$Species == species.vec[i],]
  
  # plot.df <- complete(plot.df,vwc)
  plot.df <- plot.df[order(plot.df$Date),]
  plot.df$Species <- droplevels(plot.df$Species)
  
  cWaT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Ambient',]
  dWaT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Ambient',]
  
  cWeT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Elevated',]
  dWeT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Elevated',]
  
  par(mfrow = c(2,2))
  
  plot.vwc.func(cWaT.df)
  # legend('topleft',legend = 'cWaT',bty='n')
  plot.vwc.func(dWaT.df)
  # legend('topleft',legend = 'dWaT',bty='n')
  
  if(nrow(cWeT.df) > 0){
    plot.vwc.func(cWeT.df)
    # legend('topleft',legend = 'cWeT',bty='n')
    plot.vwc.func(dWeT.df)
    # legend('topleft',legend = 'dWeT',bty='n')
    
  }else{
    plot(1,col='white',ann=F,axes = F)
    plot(1,col='white',ann=F,axes = F)
  }
}

dev.off()