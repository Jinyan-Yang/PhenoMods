day.lag <- 1
source('r/pace_data_process.R')
source('r/ym_data_process.R')
source('r/process_cw_gcc.R')
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)

# 
predict.mcmc.func.2q = function(df = gcc.met.pace.df,
                                rds.nm,
                                species.in,prep.in,temp.in,subplot=NULL,
                                nm.note='',use.smooth=FALSE,
                                my.fun = phenoGrass.func.v11,
                                swc.in.cap = 0.13,swc.in.wilt = 0.05,bucket.size =300,norm.min.max=NULL){
  
  if(is.null(subplot)){
    gcc.met.pace.df.16 <- get.pace.func(df,
                                        species.in = species.in,
                                        prep.in = prep.in,
                                        temp.in =temp.in,
                                        norm.min.max = norm.min.max)
  }else{
    species.in = subplot
    prep.in = ''
    temp.in =''
    gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,subplot = subplot)

  }
  rds.nm = rds.nm
  # fn='cache/smv10.testchain.Fes.Control.Ambient.rds'
  # gcc.met.pace.df.16 <- gcc.met.pace.df.16[(gcc.met.pace.df.16$Date) < as.Date('2019-11-26'),]
  gcc.met.pace.df.16$map <- 760
  
  # read chains 
  in.chain =  readRDS(rds.nm)
  
  
  
  if(is.list(in.chain)){
    # assuming 1/3 burn in
    burnIn = 1
    chain.3.ls.new = lapply(in.chain,function(m.in)m.in[round(nrow(m.in)/3):nrow(m.in),])
    
    chain.fes <- do.call(rbind,chain.3.ls.new)
  }else{
    burnIn = nrow(in.chain)/3
    chain.fes <- in.chain
  }
  
  # # see how it works#####
  par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
    f.t.opt = c(10,15,20,NA,NA,NA),
    f.extract = c(0.05,0.075,0.1,NA,NA,NA),
    f.sec = c(0.05,0.1,0.15,NA,NA,NA),
    f.growth = c(0.1,0.2,0.3,NA,NA,NA),
    q = c(0.001,1,2,NA,NA,NA),
    q.s = c(0.001,1,2,NA,NA,NA))
  row.names(par.df) <- c('min','initial','max','fit','stdv','prop')
  par.df["fit",] <- colMeans(chain.fes[burnIn:nrow(chain.fes),])
  # par.df["fit",] <- colMeans(luc.d.a.df[burnIn:nrow(luc.d.a.df),])
  # 
  # bucket.size = 300
  hufken.pace.pred <- my.fun(gcc.met.pace.df.16,
                             f.h = 222,
                             f.t.opt = par.df["fit",1],
                             f.extract = par.df["fit",2],
                             f.sec= par.df["fit",3],
                             f.growth = par.df["fit",4],
                             q =  par.df["fit",5],
                             q.s =  par.df["fit",6],
                             bucket.size = bucket.size,
                             swc.wilt = swc.in.wilt ,
                             swc.capacity = swc.in.cap ,
                             t.max = 45,
                             day.lay = day.lag,use.smooth = use.smooth)
  
  return(hufken.pace.pred)
  
  
}

ym.38.df <- get.ym.func(38)
ym.14.df <- get.ym.func(14)
ym.27.df <- get.ym.func(27)

plot(GCC~Date,data = ym.38.df,ylim=c(.2,.47),pch=16)
points(GCC~Date,data = ym.14.df,pch=16,col='grey')
points(GCC~Date,data = ym.27.df,pch=16,col='red')
# ym.drt.daily.df <- get.pace.func(ym.38.df,species.in = 'ym',prep.in = 'Drought',temp.in = 'Ambient')
# 
# plot(GCC.norm.smooth~Date,data = ym.drt.daily.df)
ym.14.pred.df <- predict.mcmc.func.2q(ym.14.df,
                                      rds.nm = 'cache/smv13.2qchain.ym.Control.Ambient.rds',
                                      'ym','Drought','Ambient',
                                      my.fun = phenoGrass.func.v13,
                                      nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)

ym.27.pred.df <- predict.mcmc.func.2q(ym.27.df,
                                      rds.nm = 'cache/smv13.2qchain.ym.Control.Ambient.rds',
                                      'ym','Drought','Ambient',
                                      my.fun = phenoGrass.func.v13,
                                      nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)

ym.38.pred.df <- predict.mcmc.func.2q(ym.38.df,
                                      rds.nm = 'cache/smv13.2qchain.ym.Control.Ambient.rds',
                                      'ym','Drought','Ambient',
                                      my.fun = phenoGrass.func.v13,
                                      nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)
# 
pre.pace.ls <- list()
species.vec <- c('Bis','Dig','Luc','Fes','Rye','Kan','Rho')
for (spc.nm in seq_along(species.vec)) {
  pre.pace.ls[[spc.nm]] <- predict.mcmc.func.2q(gcc.met.pace.df,
                                        rds.nm = sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds',species.vec[spc.nm]),
                                        species.vec[spc.nm],'Drought','Ambient',
                                        my.fun = phenoGrass.func.v13,
                                        nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.13,swc.in.wilt = 0.05,bucket.size = 300)
}


# 
plot.4panel.func <- function(hufken.pace.pred,swc.in.cap = 0.3){
  par(mar=c(5,5,1,5))
  par(mfrow=c(2,2))
  
  # plot swc
  par(mar=c(5,5,1,5))
  plot(vwc.hufken~Date,data = hufken.pace.pred,type='s',
       ann=F,axes=F,col = col.df$bushBySea[3],ylim=c(0,0.3),lwd='2')
  points(vwc~Date,data = hufken.pace.pred,type='s',lty='dashed',
         col = col.df$bushBySea[3],lwd='2')
  
  legend('topleft',legend = c('OBS','MOD'),lty = c('solid','dashed'),col='black')
  
  max.irrig = round(max(hufken.pace.pred$irrig.tot,na.rm=T))
  step.tmp <- floor((swc.in.cap /5)*100)/100
  axis(2,at = seq(0,0.3,by=step.tmp),labels = seq(0,0.3,by=step.tmp))
  mtext('VWC',side = 2,line = 3)
  
  date.range = range(hufken.pace.pred$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  
  mon.c <- format(mons.vec,'%m')
  axis(1,at = mons.vec,labels = mon.c)
  yr.vec <- unique(year(hufken.pace.pred$Date))
  where.c <-which(mon.c =='01') / length(mon.c)
  num.yr <- length(where.c)
  mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  # plot irrig
  par(new=TRUE)
  plot(irrig.tot~Date,data = hufken.pace.pred,type='s',
       ann=F,axes=F,col = 'navy')
  max.irrig = ceiling(max(hufken.pace.pred$irrig.tot,na.rm=T))
  axis(4,at = seq(0,max.irrig,by=10),labels = seq(0,max.irrig,by=10))
  mtext('irrigation (mm)',side = 4,line = 3)
  
  # plot obs cover
  par(mar=c(5,5,1,5))
  plot(cover~Date,data = hufken.pace.pred,type='l',lwd='2',#pch=16,
       xlab=' ',ylab=expression(f[cover]),ylim=c(0,1),col = col.df$iris[4],
       xaxt='n')
  
  date.range = range(hufken.pace.pred$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  
  mon.c <- format(mons.vec,'%m')
  axis(1,at = mons.vec,labels = mon.c)

  yr.vec <- unique(year(hufken.pace.pred$Date))
  where.c <-which(mon.c =='01') / length(mon.c)
  num.yr <- length(where.c)
  mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  
  # plot model pred
  points(cover.hufken~Date,data = hufken.pace.pred,type='l',lwd='2',col=col.df$auLandscape[2],lty='dashed')

  
  clip(min(hufken.pace.pred$Date), max(hufken.pace.pred$Date), 0.0, 0.1)
  abline(v = hufken.pace.pred$Date[hufken.pace.pred$harvest ==1],lty='dotted')
  
  # vwc scater
  plot(vwc~vwc.hufken,data = hufken.pace.pred,pch=16,col='grey',
       xlab='MOD_VWC',ylab='OBS_VWC')
  abline(a=0,b=1)
  
  # scatter plot
  plot(cover~cover.hufken,data = hufken.pace.pred,pch=16,col='grey',
       xlab='MOD_GCC',ylab='OBS_GCC')
  abline(a=0,b=1)
}

plot.title.func=function(species.in){
  par(mfrow=c(1,1),new=T,mar=c(1,1,1,1))
  plot(0,ann=F,axes=F,pch=' ')
  title(main = species.in,line = 0)
}

# 
pdf('figures/predict.drought.pdf',width = 8,height = 8*.618)
plot.4panel.func(ym.14.pred.df)
plot.title.func('YM 14 with 18 fits')

plot.4panel.func(ym.27.pred.df)
plot.title.func('YM 27 with 18 fits')

# plot.4panel.func(ym.38.pred.df)
# plot.title.func('YM 38 with 18 fits')


species.vec <- c('Bis','Dig','Luc','Fes','Rye','Kan','Rho')
for (spc.nm in seq_along(species.vec)) {
plot.4panel.func(pre.pace.ls[[spc.nm]],swc.in.cap = 0.13)
  plot.title.func(species.vec[spc.nm])
}
dev.off()

