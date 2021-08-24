# 
library(lubridate)
library(doBy)
# 
gcc.met.pace.df <- readRDS('cache/gcc.met.pace.df.rds')
gcc.met.pace.df = gcc.met.pace.df[gcc.met.pace.df$Date >= as.Date('2018-10-1')&
                                    gcc.met.pace.df$Date <= as.Date('2019-12-1'), ]
# 
gcc.sub.df <- gcc.met.pace.df[gcc.met.pace.df$Species %in% c('Dig','Kan','Fes','Pha'),]
gcc.sub.df <- gcc.sub.df[gcc.sub.df$Temperature == 'Ambient',]
# 

plot.treat.func <- function(plot.df,spc.in){
  # 
  par(mar=c(5,5,1,1),
      mfrow=c(2,1))

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
  
  # 
  
  rain.df <- summaryBy(irrig.tot~Date + Precipitation,
                       data = plot.df,FUN=sum,na.rm=T,keep=T)
  # 
  rain.df.con <- rain.df[rain.df$Precipitation == 'Control',]
  names(rain.df.con) <- c('Date','treat','rain.ctr')
  # 
  rain.df.drt <- rain.df[rain.df$Precipitation == 'Drought',]
  names(rain.df.drt) <- c('Date','treat','rain.drt')
  rain.df.mer <- merge(rain.df.con,rain.df.drt,by='Date')
  rain.df.mer$rain.diff <- rain.df.mer$rain.ctr - rain.df.mer$rain.drt
  # 
  plot(rain.diff~Date,data = rain.df.mer,
       col='navy',type='s',
       xlab='',xaxt='n',ylim=c(-50,150),
       ylab='contrl - drought')
  # 
  date.range = range(rain.df.mer$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  
  mon.c <- format(mons.vec,'%m')
  axis(1,at = mons.vec,labels = mon.c)
  # 
  yr.vec <- unique(year(rain.df.mer$Date))
  where.c <-which(mon.c =='01') / length(mon.c)
  num.yr <- length(where.c)
  mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  # 
  legend('topleft',legend = spc.in,bty='n')
}

# 
pdf('figures/check.drought.subsplot.pdf',width = 8,height = 8*.618*2)

plot.df <- gcc.sub.df[gcc.sub.df$Species =='Dig',]
plot.treat.func(plot.df,'Dig')
# 
plot.df <- gcc.sub.df[gcc.sub.df$Species =='Kan',]
plot.treat.func(plot.df,'Kan')
# 
plot.df <- gcc.sub.df[gcc.sub.df$Species =='Fes',]
plot.treat.func(plot.df,'Fes')
# 
plot.df <- gcc.sub.df[gcc.sub.df$Species =='Pha',]
plot.treat.func(plot.df,'Pha')
dev.off()