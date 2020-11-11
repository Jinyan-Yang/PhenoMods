day.lag <- 1
source('r/pace_data_process.R')
source('r/ym_data_process.R')
source('r/process_cw_gcc.R')
source('r/v13_common_fun.R')
library(lubridate)

gcc.met.cw.df <- readRDS('cache/gcc.met.cw.df.rds')
ym.18.df.tmp <- get.ym.func(18)
ym.18.df <- get.pace.func(ym.18.df.tmp,
                          species.in = 'ym',
                          prep.in = 'Control',
                                    temp.in ='Ambient',
                                    subplot = NA)

# read modis
ym.modis.df.19 <- readRDS('cache/modisFC/YM2019.rds')
ym.modis.df.20 <- readRDS('cache/modisFC/YM2020.rds')

ym.modis.df <- rbind(ym.modis.df.19,ym.modis.df.20)

plot.gcc.fc.func <- function(ym.modis.df,ym.18.df,site.nm){
  plot(GCC.norm.smooth~Date,data = ym.18.df,ylim=c(0,1),type='b',lwd = 3,
       ylab = 'GCC / Live plant cover',pch=16,xlab='',xaxt='n')
  points((FC/100)~Date,data = ym.modis.df,type='b',lwd=3,col='grey',pch=16)
  # 
  date.range = range(ym.18.df$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  
  mon.c <- format(mons.vec,'%m')
  axis(1,at = mons.vec,labels = mon.c)
  # mtext('2018',side = 1,adj=0,line = 3)
  # mtext('2019',side = 1,adj=0.5,line = 3)
  yr.vec <- unique(year(ym.18.df$Date))
  where.c <-which(mon.c =='01') / length(mon.c)
  num.yr <- length(where.c)
  mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  # 
  legend('topleft',legend=site.nm,bty='n')
  legend('topright',legend=c('MODIS','GCC'),lty=1,col=c('grey',1),bty='n')
}

pdf('figures/compare.gcc.fc.pdf',width = 6,height = 6*.618)

plot.gcc.fc.func(ym.modis.df,ym.18.df,'YM')
# 
ym.modis.df.19 <- readRDS('cache/modisFC/YM2019.rds')
ym.modis.df.20 <- readRDS('cache/modisFC/YM2020.rds')

ym.modis.df <- rbind(ym.modis.df.19,ym.modis.df.20)
# 
site.vec <- unique(gcc.met.cw.df$Code)
site.vec <- as.character(site.vec)

plot.nm.vec <- paste0(site.vec,rep(2014:2015,each=12))

for(i in seq_along(site.vec)){
  cw.site.df <- gcc.met.cw.df[gcc.met.cw.df$Code == site.vec[i],]
  # 
  cw.tmp.df <- try(get.pace.func(cw.site.df,
                            species.in = site.vec[i],
                            prep.in = 'Control',
                            temp.in ='Ambient',
                            subplot = NA))
  
  if(class(cw.tmp.df) != 'try-error'){
    # 
    fn1 <- paste0('cache/modisFC/',site.vec[i],2014,'.rds')
    tmp.df1 <- readRDS(fn1)
    
    fn2 <- paste0('cache/modisFC/',site.vec[i],2015,'.rds')
    tmp.df2 <- readRDS(fn2)
    
    tmp.df <- rbind(tmp.df1,tmp.df2)
    
    plot.gcc.fc.func(tmp.df,cw.tmp.df,site.vec[i])
  }
  

}

dev.off()



# plot(GCC~Date,data = ym.18.df,ylim=c(0,0.4),type='b',lwd = 3,ylab = 'GCC / Live plant cover',pch=16)
# points((FC/100)~Date,data = ym.modis.df,type='b',lwd=3,col='grey',pch=16)
# 
# legend('topleft',legend='YM',bty='n')
