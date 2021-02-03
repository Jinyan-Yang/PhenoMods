day.lag <- 1
source('r/pace_data_process.R')
source('r/ym_data_process.R')
source('r/process_cw_gcc.R')
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)

# fit pace
species.vec <- c('Luc','Fes','Rye',
                 'Dig', 'DigBis', 'Kan', 'KanWal', 
                 'Pha', 'PhaSub', 'Rho',  'Wal')
# seq_along(species.vec)
for(i in c(1,2,3)){
  fit.mcmc.2q.func(df = gcc.met.pace.df,
                     species.in=species.vec[i],prep.in = 'Control', temp.in ='Ambient',
                     my.fun = phenoGrass.func.v13,out.nm.note='v13.2q',use.smooth = TRUE)
  
}

# fit YM
ym.18.df <- get.ym.func(18)
fit.mcmc.2q.func(df=ym.18.df,n.iter = 20000,
                   species.in='ym',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13.2q',use.smooth = TRUE,
                   swc.capacity = 0.3,swc.wilt = 0.05,day.lag=5,bucket.size = 1000)

# tmp.df <- get.pace.func(ym.18.df,
#                         species.in ='ym',
#                         prep.in = 'Control', temp.in ='Ambient',
#                         subplot = NA)

# see.huf.pred.df <- phenoGrass.func.v13(tmp.df,
#                                        f.h=200,
#                                        f.t.opt=30,
#                                        f.extract=0.2,
#                                        f.sec=0.001,
#                                        f.growth=0.001,
#                                        swc.wilt=0.05 ,
#                                        swc.capacity =0.3,
#                                        bucket.size=1000,
#                                        t.max=35,
#                                        day.lay=2,
#                                        use.smooth=T)

# fit to cw sites####
gcc.met.cw.df <- readRDS('cache/gcc.met.cw.df.rds')
for (i in seq_along(site.vec)) {
  fit.mcmc.2q.func(df=gcc.met.cw.df,
                     species.in=site.vec[i],prep.in = 'Control', temp.in ='Ambient',
                     my.fun = phenoGrass.func.v13,out.nm.note='v13.2q',use.smooth = TRUE,
                     swc.capacity = 0.3,swc.wilt = 0.05,n.iter = 10000,bucket.size = 1000)
  }
  
# make plots
plot.mcmc.func.2q = function(df = gcc.met.pace.df,
                          species.in,prep.in,temp.in,subplot=NULL,
                          nm.note='',use.smooth=FALSE,
                          my.fun = phenoGrass.func.v11,
                          swc.in.cap = 0.13,swc.in.wilt = 0.05,bucket.size =300){
  
  if(is.null(subplot)){
    gcc.met.pace.df.16 <- get.pace.func(df,
                                        species.in = species.in,
                                        prep.in = prep.in,
                                        temp.in =temp.in)
    
    if(use.smooth){
      sm.nm='sm'
    }else{
      sm.nm=''
    }
    
    fn=paste0('cache/',sm.nm,nm.note,'chain.',
              species.in,'.',prep.in,'.',temp.in,'.rds')
    rds.nm = paste0('tmp/pred.',sm.nm,nm.note,'chain.',species.in,'.',prep.in,'.',temp.in,'.rds')
    # fn=paste0('cache/chain.',species.in,'.',prep.in,'.',temp.in,'.rds')
    
    
  }else{
    species.in = subplot
    prep.in = ''
    temp.in =''
    gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,subplot = subplot)
    
    if(use.smooth){
      sm.nm='sm'
    }else{
      sm.nm=''
    }
    
    fn=paste0('cache/',sm.nm,nm.note,'chain.',subplot,'.rds')
    rds.nm = paste0('tmp/pred.',sm.nm,nm.note,'chain.',subplot,'.rds')
  }
  
  # fn='cache/smv10.testchain.Fes.Control.Ambient.rds'
  # gcc.met.pace.df.16 <- gcc.met.pace.df.16[(gcc.met.pace.df.16$Date) < as.Date('2019-11-26'),]
  gcc.met.pace.df.16$map <- 760
  
  # chain.fes <- readRDS('cache/chain.Rye.Control.Ambient.rds')
  # read chains 
  in.chain =  readRDS(fn)
  
  
  
  if(is.list(in.chain)){
    # assuming 1/3 burn in
    burnIn = 1
    chain.3.ls.new = lapply(in.chain,function(m.in)m.in[round(nrow(m.in)/3):nrow(m.in),])
    
    chain.fes <- do.call(rbind,chain.3.ls.new)
  }else{
    burnIn = nrow(in.chain)/3
    chain.fes <- in.chain
  }
  
  # # check acceptance so that the 
  
  # acceptance = 1-mean(duplicated(chain.fes[-(1:burnIn),])) #should be >20% but <60%; 20-25% were suggested
  # 
  # # 
  # hist(chain.fes[8000:30000,1])
  # plot(chain.fes[,1])
  # plot(chain.fes[,2])
  # plot(chain.fes[,3])
  # plot(chain.fes[,4])
  # 
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
  
  # save prediction for future use
  
  saveRDS(hufken.pace.pred,rds.nm)
  # hufken.pace.pred <- readRDS('tmp/pred.smv13chain.ym.Control.Ambient.rds')
  # hufken.pace.pred$water.norm <- hufken.pace.pred$water.avi / (0.13-0.05)/300
  # library(viridisLite)
  # palette(viridis(8))
  par(mar=c(5,5,1,5))
  par(mfrow=c(2,2))
  
  
  # par(mar=c(0,5,1,5))C
  # plot(irrig.tot~Date,data = hufken.pace.pred,type='s',
  #      ann=F,axes=F,col = 'lightskyblue')
  # max.irrig = round(max(hufken.pace.pred$irrig.tot,na.rm=T))
  # axis(2,at = seq(0,max.irrig,by=10),labels = seq(0,max.irrig,by=10))
  # mtext('irrigation (mm)',side = 2,line = 3)
  
  # plot swc
  par(mar=c(5,5,1,5))
  plot(vwc.hufken~Date,data = hufken.pace.pred,type='s',
       ann=F,axes=F,col = col.df$bushBySea[3],ylim=c(0,swc.in.cap))
  points(vwc~Date,data = hufken.pace.pred,type='s',lty='dotted',
         col = col.df$bushBySea[3])
  
  legend('topleft',legend = c('OBS','MOD'),lty = c('solid','dotted'),col='black')
  
  max.irrig = round(max(hufken.pace.pred$irrig.tot,na.rm=T))
  step.tmp <- floor((swc.in.cap /5)*100)/100
  axis(2,at = seq(0,swc.in.cap,by=step.tmp),labels = seq(0,swc.in.cap,by=step.tmp))
  mtext('VWC',side = 2,line = 3)
  
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
  # plot irrig
  par(new=TRUE)
  plot(irrig.tot~Date,data = hufken.pace.pred,type='s',
       ann=F,axes=F,col = 'navy')
  max.irrig = ceiling(max(hufken.pace.pred$irrig.tot,na.rm=T))
  axis(4,at = seq(0,max.irrig,by=10),labels = seq(0,max.irrig,by=10))
  mtext('irrigation (mm)',side = 4,line = 3)
  
  # hufken.pace.pred <- readRDS('tmp/pred.smv13chain.ym.Control.Ambient.rds')
  # min(hufken.pace.pred$swc)
  # max(hufken.pace.pred$swc)
  
  # plot(irrig.tot~Date,data = hufken.pace.pred,type='s',
  #      col = 'lightskyblue')
  
  # plot obs cover
  par(mar=c(5,5,1,5))
  plot(cover~Date,data = hufken.pace.pred,type='l',#pch=16,
       xlab=' ',ylab=expression(f[cover]),ylim=c(0,1),col = col.df$iris[4],
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
  points(cover.hufken~Date,data = hufken.pace.pred,type='l',col=col.df$auLandscape[2],lty='dotted')
  
 
  # legend('topleft',legend = paste0(species.in,prep.in,temp.in),bty='n')
  
  clip(min(hufken.pace.pred$Date), max(hufken.pace.pred$Date), 0.0, 0.1)
  abline(v = hufken.pace.pred$Date[hufken.pace.pred$harvest ==1],lty='dashed')
  
  # par(new=T)
  # 
  # plot(irrig.tot~Date,data = hufken.pace.pred,type='s',
  #      ann=F,axes=F,col = 'lightskyblue')
  # max.irrig = round(max(hufken.pace.pred$irrig.tot,na.rm=T))
  # axis(4,at = seq(0,max.irrig,by=10),labels = seq(0,max.irrig,by=10))
  # mtext('irrigation (mm)',side = 4)
  
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
plot.mcmc.func.2q(gcc.met.pace.df,'Luc','Control','Ambient',
               my.fun = phenoGrass.func.v13,
               nm.note='v13.2q',use.smooth = TRUE)

pdf('figures/v13.2q.pdf',width = 8,height = 8*0.618)

# ym
plot.mcmc.func.2q(ym.18.df,'ym','Control','Ambient',
                  my.fun = phenoGrass.func.v13,
                  nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)
# pace
for (i in c(1,2,3)) {
  plot.mcmc.func.2q(gcc.met.pace.df,species.vec[i],'Control','Ambient',
                    my.fun = phenoGrass.func.v13,
                    nm.note='v13.2q',use.smooth = TRUE)
  plot.title.func(species.vec[i]) 

}

# for(i in seq_along(site.vec[1:7])){
#   fn <- sprintf('cache/smv13chain.%s.Control.Ambient.rds',site.vec[i])
#   if(file.exists(fn)){
#     
#     plot.mcmc.func.2q(gcc.met.cw.df,site.vec[i],'Control','Ambient',subplot = NULL,nm.note = 'v13',use.smooth = TRUE,my.fun =phenoGrass.func.v13,
#                    swc.in.wilt = 0.05,swc.in.cap = 0.3,bucket.size=1000)
#     plot.title.func(site.vec[i]) 
#   }
# }
dev.off()

# # plot ym only
# pdf('figures/v13.ym.2q.pdf',width = 8,height = 8*0.618)
# plot.mcmc.func.2q(ym.18.df,'ym','Control','Ambient',
#                   my.fun = phenoGrass.func.v13,
#                   nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)
# 
# dev.off()

plot.check.mcmc.func=function(chain.in,species.in='',nm.vec = c('Topt','f.extract','senescence','growth','q','qs')){
  
  burnIn = round(nrow(chain.in) / 5)
  
  par(mfrow=c(3,2),mar=c(5,5,1,1))
  
  for(i in 1:ncol(chain.in)){
    hist(chain.in[burnIn:nrow(chain.in),i],xlab = nm.vec[i],
         main='')
    
  }
  plot.title.func(species.in = species.in)
}

pdf('figures/v13.2q.diag.pdf',width = 8,height = 8*0.618)
for(i in c(1,2,3)){
  fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds',species.vec[i])
  chain.3.ls = readRDS(fn)
  lapply(chain.3.ls, plot.check.mcmc.func,species.in=species.vec[i])
}

fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds','ym')
chain.3.ls = readRDS(fn)
lapply(chain.3.ls, plot.check.mcmc.func,species.in='ym')

dev.off()

# 

# pdf('figures/v13.ym.2q.diag.pdf',width = 8,height = 8*0.618)
# fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds','ym')
# chain.3.ls = readRDS(fn)
# lapply(chain.3.ls, plot.check.mcmc.func,species.in='ym')
# dev.off()
