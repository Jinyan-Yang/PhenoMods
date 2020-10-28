day.lag <- 1
source('r/pace_data_process.R')
source('r/ym_data_process.R')

library(lubridate)

# 
ym.18.df <- get.ym.func(18)

pdf('figures/gcc_T_PAR.pdf',width = 6,height = 6*3*.618)

par(mfrow=c(3,1))

par(mar=c(2,5,2,1))
plot(GCC~Date,data = ym.18.df,xlab='',xaxt='n')
par(new=T)
plot(swc~Date,data = ym.18.df,ann=F,axes=F,type='s',col='lightskyblue')

par(mar=c(2,5,2,1))
plot(Tair~Date,data = ym.18.df,xlab='',xaxt='n')

par(mar=c(4,5,1,1))
plot(PAR.ros~Date,data = ym.18.df,xlab='',xaxt='n')
# 
date.range = range(ym.18.df$Date,na.rm=T)
mons.vec =  seq(date.range[1],date.range[2],by='mon')

mon.c <- format(mons.vec,'%m')
axis(1,at = mons.vec,labels = mon.c)
yr.vec <- unique(year(ym.18.df$Date))
where.c <-which(mon.c =='01') / length(mon.c)
num.yr <- length(where.c)
mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)

dev.off()

