# par RH and tair
# PACE_AUTO_S6_ABVGRND_R_20190228.dat
library(HIEv)

download.path <- file.path("download/")
setToPath(download.path)
startDate = '2018-01-01'
endDate = '2019-12-31'

shelter.vec <- 1:6
met.ls <- list()
for (shelter.num in shelter.vec){
  fn <- sprintf('PACE_AUTO_S%s_ABVGRND_R_',shelter.num)

  met.df <- downloadTOA5(fn, 
                        maxnfiles = 100, 
                        rowbind=TRUE,
                        startDate = startDate,
                        endDate = endDate)      
  met.df$RH_Avg[met.df$RH_Avg > 100] <- 100
  met.df$RH_Avg[met.df$RH_Avg < 0] <- 0
  
  met.df$PAR_Avg[met.df$PAR_Avg < 0] <- 0

  library(data.table)
  met.ls[[shelter.num]] <- data.table(met.df)[,list(Tair = mean(AirT2_Avg,na.rm=TRUE),
                                            Tmin = min(AirT2_Avg,na.rm=TRUE),
                                            Tmax = max(AirT2_Avg,na.rm=TRUE),
                                            par = 1800*sum(PAR_Avg, na.rm=TRUE)*10^-6/4.57,
                                            rh = mean(RH_Avg, na.rm=TRUE),
                                            RHmax = max(RH_Avg, na.rm=TRUE),
                                            RHmin = min(RH_Avg, na.rm=TRUE)),
                                      by=Date]
  
  met.ls[[shelter.num]]$Shelter <- shelter.num
}

met.all.df <- do.call(rbind,met.ls)




# get ros par to replace the bad data in pace####
library(HIEv)
download.path <- file.path("download/")
setToPath(download.path)
setToken()
ros05 <- downloadTOA5("ROS_WS_Table05",
                      startDate = startDate,
                      endDate = endDate)

ros05$PPFD[ros05$PPFD_Avg < 0] <- 0

library(doBy)
ros.day.df <- summaryBy(PPFD_Avg~Date,data = ros05,FUN=sum,na.rm=T,keep.names = T)

ros.day.df$PAR.ros <- 300 * ros.day.df$PPFD_Avg * 10^-6 / 4.57

met.df <- merge(met.all.df,ros.day.df[,c('Date','PAR.ros')],by='Date',all.x=T)
irrig.wd.df <- readRDS('cache/irrig.wd.df.rds')
#####
saveRDS(met.df,'cache/pace.met.rds')






# 
# gcc.swc.irg.ws.met.df <- merge(gcc.swc.irg.ws.df,met.all.df,by=c('Date','Shelter'),all=T)
# 
# gcc.swc.irg.ws.met.df <- gcc.swc.irg.ws.met.df
# 
# gcc.swc.irg.ws.met.df$irrigsum[is.na(gcc.swc.irg.ws.met.df$irrigsum)] <- 0



# library(viridisLite)
# palette(c('red',cividis(6)[1:5]))
# plot(par~Date,data = met.all.df,pch=paste0(Shelter),col=Shelter,cex=1)
# legend('top',legend = 1:6,pch=16,col=palette(),horiz = T)



