# 
library(lubridate)
library(doBy)
species.vec <- c('Dig','Kan','Fes','Pha')
# read obs#####
gcc.met.pace.df <- readRDS('cache/gcc.met.pace.df.rds')
gcc.met.pace.df = gcc.met.pace.df[gcc.met.pace.df$Date >= as.Date('2018-10-1')&
                                    gcc.met.pace.df$Date <= as.Date('2019-12-1'), ]
# 
gcc.sub.df <- gcc.met.pace.df[gcc.met.pace.df$Species %in% species.vec,]
gcc.sub.df <- gcc.sub.df[gcc.sub.df$Temperature == 'Ambient',]
# 

# read.model####
pred.ls <- list()
for (i in seq_along(species.vec)) {
  # 
  fn.con <- sprintf('tmp/pred.smsmv13.2q.chain.%s.Control.Ambient.rds',
                    species.vec[i])
  
  dat.con <- readRDS(fn.con)
  dat.con <- dat.con[,c('Date','GCC.norm',
                        'GCC.norm.sd','vwc','cover.hufken','Rain')]
  names(dat.con) <- c("Date","GCC.mean.con",
                      'GCC.sd.con',"vwc.mean.con",'cover.pred.con','rain.con')
  # 
  fn.drt <- sprintf('tmp/pred.smsmv13.2q.chain.%s.Control.predict.Ambient.rds',
                    species.vec[i])
  dat.drought <- readRDS(fn.drt)
  dat.drought <- dat.drought[,c('Date','GCC.norm',
                                'GCC.norm.sd','vwc','cover.hufken','Rain')]
  names(dat.drought) <- c("Date","GCC.mean.drt",
                          'GCC.sd.drt',"vwc.mean.drt",'cover.pred.drt','rain.drt')
  
  dat.both.df <- merge(dat.con,dat.drought,
                       by = c('Date'))
  
  # dat.both.df <- dat.both.df[month(dat.both.df$Date) %in% 6:11,]
  dat.both.df$spc <- species.vec[i]
  pred.ls[[i]] <- dat.both.df
}
pred.df <- do.call(rbind,pred.ls)

pred.df$gcc.pred.con <- pred.df$cover.pred.con *0.13 + 0.3
pred.df$gcc.pred.drt <- pred.df$cover.pred.drt *0.13 + 0.3
# make plot####
plot.treat.func <- function(plot.df,spc.in){
  # 
  par(mar=c(5,5,1,1),
      mfrow=c(1,1))

  plot(GCC~Date,data = plot.df[plot.df$Precipitation =='Control',],
            ylim=c(0.3,0.45),col='navy',pch=16,
            xlab='',xaxt='n')
  points(GCC~Date,data = plot.df[plot.df$Precipitation =='Drought',],
         col='red',pch=16)

  # 
  date.range = range(plot.df$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  
  mon.c <- format(mons.vec,'%m')
  axis(1,at = mons.vec,labels = mon.c)
  # 
  yr.vec <- unique(year(plot.df$Date))
  where.c <-which(mon.c =='01') / length(mon.c)
  num.yr <- length(where.c)
  mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  # 
  legend('topleft',legend = spc.in,bty='n')
  
  # # 
  # 
  # rain.df <- summaryBy(irrig.tot~Date + Precipitation,
  #                      data = plot.df,FUN=sum,na.rm=T,keep=T)
  # # 
  # rain.df.con <- rain.df[rain.df$Precipitation == 'Control',]
  # names(rain.df.con) <- c('Date','treat','rain.ctr')
  # # 
  # rain.df.drt <- rain.df[rain.df$Precipitation == 'Drought',]
  # names(rain.df.drt) <- c('Date','treat','rain.drt')
  # rain.df.mer <- merge(rain.df.con,rain.df.drt,by='Date')
  # rain.df.mer$rain.diff <- rain.df.mer$rain.ctr - rain.df.mer$rain.drt
  # # 
  # plot(rain.diff~Date,data = rain.df.mer,
  #      col='navy',type='s',
  #      xlab='',xaxt='n',ylim=c(-50,150),
  #      ylab='contrl - drought')
  # # 
  # date.range = range(rain.df.mer$Date,na.rm=T)
  # mons.vec =  seq(date.range[1],date.range[2],by='mon')
  # 
  # mon.c <- format(mons.vec,'%m')
  # axis(1,at = mons.vec,labels = mon.c)
  # # 
  # yr.vec <- unique(year(rain.df.mer$Date))
  # where.c <-which(mon.c =='01') / length(mon.c)
  # num.yr <- length(where.c)
  # mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  # # 
  # legend('topleft',legend = spc.in,bty='n')
}

# 
pdf('figures/check.drought.subsplot.pdf',width = 8,height = 8*.618)

plot.df <- gcc.sub.df[gcc.sub.df$Species =='Dig',]
plot.treat.func(plot.df,'Dig')
# 
points(gcc.pred.con~Date,data = pred.df[pred.df$spc == 'Dig',],type='l',
       lty='solid',col='darkseagreen',lwd=6)
points(gcc.pred.drt~Date,data = pred.df[pred.df$spc == 'Dig',],type='l',
       lty='solid',col='grey',lwd=6)
# 
plot.df <- gcc.sub.df[gcc.sub.df$Species =='Kan',]
plot.treat.func(plot.df,'Kan')
points(gcc.pred.con~Date,data = pred.df[pred.df$spc == 'Kan',],type='l',
       lty='solid',col='darkseagreen',lwd=6)
points(gcc.pred.drt~Date,data = pred.df[pred.df$spc == 'Kan',],type='l',
       lty='solid',col='grey',lwd=6)
# 
plot.df <- gcc.sub.df[gcc.sub.df$Species =='Fes',]
plot.treat.func(plot.df,'Fes')
points(gcc.pred.con~Date,data = pred.df[pred.df$spc == 'Fes',],type='l',
       lty='solid',col='darkseagreen',lwd=6)
points(gcc.pred.drt~Date,data = pred.df[pred.df$spc == 'Fes',],type='l',
       lty='solid',col='grey',lwd=6)
# 
plot.df <- gcc.sub.df[gcc.sub.df$Species =='Pha',]
plot.treat.func(plot.df,'Pha')
points(gcc.pred.con~Date,data = pred.df[pred.df$spc == 'Pha',],type='l',
       lty='solid',col='darkseagreen',lwd=6)
points(gcc.pred.drt~Date,data = pred.df[pred.df$spc == 'Pha',],type='l',
       lty='solid',col='grey',lwd=6)
dev.off()

