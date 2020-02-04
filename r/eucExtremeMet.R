# 
startDate = as.Date('2013-01-01')
endDate = Sys.Date()
# n.days <- as.numeric(endDate - startDate)
# get ros par to replace the bad data in pace####
library(HIEv)
download.path <- file.path("download/")
setToPath(download.path)
setToken()
ros15 <- downloadTOA5("ROS_WS_Table15",
                      startDate = startDate,
                      endDate = endDate,
                      maxnfiles = 500)
ros.rain.day.df <- doBy::summaryBy(Rain_mm_Tot~Date,data = ros15,
                                   FUN=sum,keep.names = T,na.rm=T)
# ros.rain.day.df <- (irrig.df[irrig.df$Shelter == 1 & irrig.df$Plot == 6,])
# library(doBy)
# 
# ros.rain.day.df <- summaryBy(irrigsum~Date + Shelter + Plot,FUN=sum,na.rm=T,
#                      data = ros.rain.day.df,keep.names = T)

ros05 <- downloadTOA5("ROS_WS_Table05",
                      startDate = startDate,
                      endDate = endDate,
                      maxnfiles = 500)

ros05$PPFD[ros05$PPFD_Avg < 0] <- 0
ros05$par <- 5*60 * ros05$PPFD_Avg * 10^-6 / 4.57
# 
getVPD <- function(RH,TAIR){
  
  VPD <- (1-RH)*0.61375*exp(17.502*TAIR/(240.97+TAIR))
  
  return(VPD)
}

ros05$vpd <- getVPD(ros05$RH/100,ros05$AirTC_Avg)

max(ros05$vpd)
max(ros05$vpd[ros05$Date == as.Date('2013-01-18')])
unique(ros05$Date[ros05$vpd > 8])
ros05[ros05$vpd > 8,]


ros05 <- ros05[order(ros05$DateTime),]
# 
pdf('eucface recent met.pdf',width = 8,height = 8*0.618)

par(mar=c(5,5,1,1))
par(mfrow=c(2,2))
plot(AirTC_Avg~DateTime,data = ros05[as.Date(ros05$DateTime)  == as.Date('2020-1-4'),],
     xlab=' ',type='l',lwd=3,ylab='Tair (Celsius)')
title('2020-1-4')
plot(RH~DateTime,data = ros05[as.Date(ros05$DateTime)  == as.Date('2020-1-4'),],
     xlab=' ',type='l',lwd=3,ylab='RH (%)')
title('2020-1-4')
plot(vpd~DateTime,data = ros05[as.Date(ros05$DateTime)  == as.Date('2020-1-4'),],
     xlab=' ',type='l',lwd=3,ylab='VPD (kPa)')
legend('topleft',legend = c(paste0('Max VPD (kPa): ',format(max(ros05$vpd),digits = 2))),bty='n')
title('2020-1-4')
plot(vpd~DateTime,data = ros05[as.Date(ros05$DateTime) > as.Date('2019-12-1') ,],
     xlab=' ',type='l',lwd=3,ylab='VPD (kPa)')
abline(h=2.5,col='coral',lty='dashed')
abline(h=4,col='red',lty='dashed')
title('2019-2020 DJ')

par(mfrow=c(1,1))
plot(Rain_mm_Tot~Date,data = ros.rain.day.df[ros.rain.day.df$Date> as.Date('2020-1-1'),],
     xlab=' ',type='s',lwd=3,ylab='Rainfall (mm/d)')
title('Jan 2020')
legend('topleft',
       legend = c(paste0('Jan to date total: ',sum(ros.rain.day.df$Rain_mm_Tot[ros.rain.day.df$Date > as.Date('2020-1-1')]))),
       bty='n',title = 'Rainfall (mm)')
dev.off()

# 
pdf('dayMet.pdf',width = 8,height = 2*8*0.618)

par(mar=c(5,5,1,1),
    mfrow=c(1,1))
plot(AirTC_Avg~seq(0,24,length.out = 12*24), data = ros05[ros05$Date == as.Date('2017-2-8'),],
     xlab=' ',col='blue',ylim=c(18,30),pch=16)

points(AirTC_Avg~seq(0,24,length.out = 12*24), data = ros05[ros05$Date == as.Date('2020-1-2'),],
       col='coral',pch=16)

legend('topleft',legend = c('2017-2-8','2020-1-2'),pch=16,col=c('blue','coral'),bty='n')

# 
plot(vpd~seq(0,24,length.out = 12*24), data = ros05[ros05$Date == as.Date('2017-2-8'),],
     xlab=' ',col='blue',ylim=c(0,2.5),pch=16)

points(vpd~seq(0,24,length.out = 12*24), data = ros05[ros05$Date == as.Date('2020-1-2'),],
       col='coral',pch=16)

legend('topleft',legend = c('2017-2-8','2020-1-2'),pch=16,col=c('blue','coral'),bty='n')

dev.off()

# 
pdf('2017_2020Met.pdf',width = 8,height = 8*0.618)

par(mar=c(5,5,1,1),
    mfrow=c(1,1))
plot(AirTC_Avg~DateTime, data = ros05[ros05$Date >= as.Date('2017-1-1'),],
     xlab=' ',col='coral',ylim=c(0,50),pch=16)

# points(vpd~DateTime, data = ros05[ros05$Date == as.Date('2020-1-2'),],
#        col='coral',pch=16)

# legend('topleft',legend = c('2017-2-8','2020-1-2'),pch=16,col=c('blue','coral'),bty='n')

# # 
plot(vpd~DateTime, data = ros05[ros05$Date >= as.Date('2017-1-1'),],
     xlab=' ',col='coral',ylim=c(0,10),pch=16)
# 
# points(vpd~seq(0,24,length.out = 12*24), data = ros05[ros05$Date == as.Date('2020-1-2'),],
#        col='coral',pch=16)

# legend('topleft',legend = c('2017-2-8','2020-1-2'),pch=16,col=c('blue','coral'),bty='n')

dev.off()