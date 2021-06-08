devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(doBy)
library(lubridate)

plot.ts.func <- function(hufken.pace.pred){
  plot(GCC.norm~Date,data = hufken.pace.pred,type='p',pch=16,#lwd='2',
       xlab=' ',ylab='Cover',ylim=c(0,1),col = col.df$iris[4],
       xaxt='n')
  
  
  date.range = range(hufken.pace.pred$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  
  mon.c <- format(mons.vec,'%m')
  axis(1,at = mons.vec,labels = mon.c)
  # mtext('2018',side = 1,adj=0,line = 3)
  # mtext('2019',side = 1,adj=0.5,line = 3)
  yr.vec <- unique(year(hufken.pace.pred$Date))
  where.c <-which(mon.c =='01') / length(mon.c)
  num.yr <- length(where.c)
  mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  
  # plot model pred
  points(cover.hufken~Date,data = hufken.pace.pred,type='l',lwd='2',col=col.df$auLandscape[2],lty='solid')
  # plot irrig
  par(new=TRUE)
  plot(irrig.tot~Date,data = hufken.pace.pred,type='s',
       ann=F,axes=F,col = 'navy')
  max.irrig = ceiling(max(hufken.pace.pred$irrig.tot,na.rm=T))
  axis(4,at = seq(0,max.irrig,by=10),labels = seq(0,max.irrig,by=10))
  mtext('irrigation (mm)',side = 4,line = 3)
}

# species.vec <- c("Bis",    "Dig",  "Fes",    "Kan",    
#                  "Luc",  "Rho",    "Rye",
#                  'ym')
species.vec <- c('Bis','Luc','Dig','Kan','Rho','Fes','Pha','Rye','YM')

# #########################################
palette(c(col.df$iris,col.df$daisy))
png('figures/obs_fit_TS_scatter.png',height = 400*2,width = 400/.618)
par(mfrow =c(2,1))

# plot obs cover
par(mar=c(5,5,1,5))
fn <- 'tmp/pred.smv13.2qchain.flux.Control.Ambient.rds'
hufken.pace.pred <- readRDS(fn)

plot.ts.func(hufken.pace.pred)
legend('topleft',legend = '(a) Flux Tower',bty='n')
legend('topright',legend = c('OBS','MOD'),
       pch=c(16,NA),lty=c(NA,'solid'),
       col=c( col.df$iris[4],col.df$auLandscape[2]),
       bty='n')

for (i in seq_along(species.vec)){
  fn <- sprintf('tmp/pred.smv13.2q.chain.%s.Control.Ambient.rds',species.vec[i])
  hufken.pace.pred <- readRDS(fn)
  
  if(i == 1){
    plot(GCC.norm~cover.hufken,data = hufken.pace.pred,
         xlim=c(0,1),ylim=c(0,1),
         xlab='MOD_cover',ylab = 'OBS_cover',pch=16,col=i)
  }else{
    points(GCC.norm~cover.hufken,data = hufken.pace.pred,
         xlim=c(0,1),ylim=c(0,1),
         xlab='MOD_GCC',ylab = 'OBS_GCC',pch=16,col=i)
  }
  legend('bottomright',legend = species.vec,col=palette(),
         pch=16,bty='n')
  legend('topleft',legend = '(b)',bty='n')
  abline(a=0,b=1,lty='dashed',col='grey',lwd=2)

}

dev.off()

# ###########
# species.vec <- c("Bis","Dig", "Fes", "Kan",    
#                  "Luc",  "Rho",    "Rye")

species.vec <- c('Bis','Luc','Dig','Kan','Rho','Fes','Pha','Rye')
png('figures/fit_pred_TS_scatter.png',height = 400*2,width = 400/.618)
par(mfrow =c(2,1))
palette(c(col.df$iris,col.df$daisy))
# plot obs cover
par(mar=c(5,5,1,5))
fn <- 'tmp/pred.smv13.2qchain.ym.Control.predict.Ambient.rds'
hufken.pace.pred <- readRDS(fn)

plot.ts.func(hufken.pace.pred)
legend('topleft',legend = '(a) YM',bty='n')
legend('topright',legend = c('OBS','MOD'),
       pch=c(16,NA),lty=c(NA,'solid'),
       col=c( col.df$iris[4],col.df$auLandscape[2]),
       bty='n')

for (i in seq_along(species.vec)){
  fn <- sprintf('tmp/pred.smv13.2q.chain.%s.Control.predict.Ambient.rds',species.vec[i])
  hufken.pace.pred <- readRDS(fn)
  
  if(i == 1){
    plot(GCC.norm~cover.hufken,data = hufken.pace.pred,
         xlim=c(0,1),ylim=c(0,1),
         xlab='MOD_cover',ylab = 'OBS_cover',pch=16,col=i)
  }else{
    points(GCC.norm~cover.hufken,data = hufken.pace.pred,
           xlim=c(0,1),ylim=c(0,1),pch=16,col=i)
  }
  legend('bottomright',legend = species.vec,col=palette(),
         pch=16,bty='n')
  legend('topleft',legend = '(b)',bty='n')
  abline(a=0,b=1,lty='dashed',col='grey',lwd=2)
  
}

dev.off()

