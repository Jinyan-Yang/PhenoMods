source('r/functions_mcmc_v12.r')
mh.MCMC.func <- function(iterations,par.df,
                         gcc.met.pace.df.16,
                         bucket.size,swc.wilt ,
                         swc.capacity,day.lay,
                         my.fun,
                         use.smooth){
  
  # get prior 
  prior.prob <- prior.func(par.df)
  
  # start MC ####
  # intial
  chain = array(dim = c(iterations+1,ncol(par.df)))
  chain[1,] = as.numeric(par.df['initial',])
  
  # chain move on
  for (i in 1:iterations){
    proposal = proposal.func(chain[i,],par.df)
    
    # prior.prob,data,data.sd,bucket.size = 300,...
    probab = exp(posterior.func(prior.prob,FUN = my.fun,
                                gcc.df = gcc.met.pace.df.16,
                                f.h = 222,
                                f.t.opt = proposal[1],
                                f.extract = proposal[2],
                                f.sec = proposal[3],
                                f.growth = proposal[4] ,
                                q = proposal[5] ,
                                t.max = 45,
                                day.lay = day.lay,
                                bucket.size = bucket.size,
                                swc.wilt = swc.wilt ,
                                swc.capacity = swc.capacity,
                                use.smooth = use.smooth) - 
                   posterior.func(prior.prob,FUN = my.fun,
                                  gcc.df = gcc.met.pace.df.16,
                                  f.h = 222,
                                  f.t.opt = chain[i,1],
                                  f.extract = chain[i,2],
                                  f.sec = chain[i,3],
                                  f.growth = chain[i,4] ,
                                  q = chain[i,5] ,
                                  t.max = 45,
                                  day.lay = day.lay,
                                  swc.wilt = swc.wilt ,
                                  swc.capacity = swc.capacity,
                                  bucket.size = bucket.size,
                                  use.smooth = use.smooth))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}
source('models/hufkens/hufkensV13.R')
source('r/plot.mcmc.r')
day.lag <- 3
source('r/pace_data_process.R')
source('r/ym_data_process.R')
# packages
library(doBy)
library(zoo)

fit.mcmc.pace.func <- function(gcc.met.pace.df = gcc.met.pace.df,
                               species.in = 'Luc',prep.in = 'Control', 
                               temp.in ='Ambient',subplot =NA,
                               my.fun = phenoGrass.func.v11,
                               out.nm.note = '',use.smooth = FALSE,
                               day.lag = 3){
  s.time <- Sys.time()
  gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
                                      species.in =species.in,
                                      prep.in = prep.in,
                                      temp.in =temp.in,
                                      subplot = subplot)
  # gcc.met.pace.df.16 <- gcc.met.pace.df.16[gcc.met.pace.df.16$Date<as.Date('2019-09-01'),]
  gcc.met.pace.df.16$map <- 760

  # para values####
  par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
    f.t.opt = c(10,15,20,NA,NA,NA),
    f.extract = c(0.05,0.075,0.1,NA,NA,NA),
    f.sec = c(0.05,0.1,0.15,NA,NA,NA),
    f.growth = c(0.1,0.2,0.3,NA,NA,NA),
    q = c(0.001,1,2,NA,NA,NA))
  row.names(par.df) <- c('min','initial','max','fit','stdv','prop')
  
  # this assume 100% of the data falls into the max min range 
  # in a normal distribution for proposal.func
  par.df['stdv',] <- ((par.df['max',] - par.df['min',])/10)
  
  # start mcmc fiting######
  bucket.size <- 300
  # soil.water.var <- quantile(gcc.met.pace.df.16$vwc,c(.1,.99))
  chain.fes=list()
  for(n.chain in 1:3){
    chain.fes[[n.chain]] = mh.MCMC.func(10000,
                                        par.df,
                                        gcc.met.pace.df.16,
                                        bucket.size = bucket.size,
                                        day.lay = day.lag,
                                        swc.capacity = 0.13,
                                        swc.wilt = 0.05,
                                        my.fun = my.fun,
                                        use.smooth = use.smooth)
  }
  
  if(use.smooth==TRUE){
    smooth.nm='sm'
  }else{
    smooth.nm=''
  }
  
  if(is.na(subplot)){
    out.name <- sprintf('cache/%s%schain.%s.%s.%s.rds',smooth.nm,out.nm.note,species.in,prep.in,temp.in)
  }else{
    out.name <- sprintf('cache/%schain.%s.rds',out.nm.note,subplot)
  }
  
  saveRDS(chain.fes,out.name)
  
  print(Sys.time() - s.time)
}

# fit v13
fit.mcmc.pace.func(species.in='Luc',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13',use.smooth = TRUE)

fit.mcmc.pace.func(species.in='Fes',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13',use.smooth = TRUE)

fit.mcmc.pace.func(species.in='Rye',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13',use.smooth = TRUE)

# fit.mcmc.pace.func(species.in='Rye',prep.in = 'Control', temp.in ='Ambient',
#                    my.fun = phenoGrass.func.v13,out.nm.note='v13',use.smooth = TRUE)

species.vec <- c('Dig', 'DigBis', 'Kan', 'KanWal', 
                 'Pha', 'PhaSub', 'Rho',  'Wal')

for(i in seq_along(species.vec)){
  fit.mcmc.pace.func(species.in=species.vec[i],prep.in = 'Control', temp.in ='Ambient',
                     my.fun = phenoGrass.func.v13,out.nm.note='v13',use.smooth = TRUE)

}

# fit ym data
ym.con.df <- get.ym.func('Control')

fit.mcmc.pace.func(gcc.met.pace.df=ym.con.df,
                   species.in='ym',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13',use.smooth = TRUE)


# plot v13
plot.mcmc.func = function(gcc.met.pace.df = gcc.met.pace.df,
                          species.in,prep.in,temp.in,subplot=NULL,nm.note='',use.smooth=FALSE,my.fun = phenoGrass.func.v11){
  
  if(is.null(subplot)){
    gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
                                        species.in = species.in,
                                        prep.in = prep.in,
                                        temp.in =temp.in)
    
    if(use.smooth){
      sm.nm='sm'
    }else{
      sm.nm=''
    }
    
    fn=paste0('cache/',sm.nm,nm.note,'chain.',species.in,'.',prep.in,'.',temp.in,'.rds')
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
  gcc.met.pace.df.16 <- gcc.met.pace.df.16[(gcc.met.pace.df.16$Date) < as.Date('2019-11-26'),]
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
    chain.fes <-in.chain
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
    q = c(0.001,1,2,NA,NA,NA))
  row.names(par.df) <- c('min','initial','max','fit','stdv','prop')
  par.df["fit",] <- colMeans(chain.fes[burnIn:nrow(chain.fes),])
  # par.df["fit",] <- colMeans(luc.d.a.df[burnIn:nrow(luc.d.a.df),])
  # 
  bucket.size = 300
  hufken.pace.pred <- my.fun(gcc.met.pace.df.16,
                             f.h = 222,
                             f.t.opt = par.df["fit",1],
                             f.extract = par.df["fit",2],
                             f.sec= par.df["fit",3],
                             f.growth = par.df["fit",4],
                             q =  par.df["fit",5],
                             bucket.size = bucket.size,
                             swc.wilt = 0.05 ,
                             swc.capacity = 0.13 ,
                             t.max = 45,
                             day.lay = day.lag)
  
  # save prediction for future use
  
  saveRDS(hufken.pace.pred,rds.nm)
  # hufken.pace.pred$water.norm <- hufken.pace.pred$water.avi / (0.13-0.05)/300
  library(viridisLite)
  palette(viridis(8))
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
       ann=F,axes=F,col = palette()[8],ylim=c(0,.15))
  points(vwc~Date,data = hufken.pace.pred,type='s',
         col = palette()[6])
  max.irrig = round(max(hufken.pace.pred$irrig.tot,na.rm=T))
  axis(2,at = seq(0,.15,by=0.03),labels = seq(0,.15,by=.03))
  mtext('VWC',side = 2,line = 3)
  
  date.range = range(hufken.pace.pred$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  
  axis(1,at = mons.vec,labels = format(mons.vec,'%m'))
  mtext('2018',side = 1,adj=0,line = 3)
  mtext('2019',side = 1,adj=0.5,line = 3)
  # plot irrig
  par(new=TRUE)
  plot(irrig.tot~Date,data = hufken.pace.pred,type='s',
       ann=F,axes=F,col = 'lightskyblue')
  max.irrig = round(max(hufken.pace.pred$irrig.tot,na.rm=T))
  axis(4,at = seq(0,max.irrig,by=10),labels = seq(0,max.irrig,by=10))
  mtext('irrigation (mm)',side = 4,line = 3)
  
  
  # plot obs cover
  par(mar=c(5,5,1,5))
  plot(cover~Date,data = hufken.pace.pred,type='l',#pch=16,
       xlab=' ',ylab=expression(f[cover]),ylim=c(0,0.8),col = palette()[6],
       xaxt='n')
  
  date.range = range(hufken.pace.pred$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  
  axis(1,at = mons.vec,labels = format(mons.vec,'%m'))
  mtext('2018',side = 1,adj=0,line = 3)
  mtext('2019',side = 1,adj=0.5,line = 3)
  # plot model pred
  points(cover.hufken~Date,data = hufken.pace.pred,type='l',col=palette()[8])
  
  legend('topright',legend = c('OBS','MOD'),lty = 1,col=palette()[c(6,8)])
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

pdf('figures/plot.v13.pdf',width = 8,height = 8*0.618)
plot.mcmc.func(gcc.met.pace.df,'Luc','Control','Ambient',subplot = NULL,nm.note = 'v13',use.smooth = TRUE,my.fun =phenoGrass.func.v13 )
plot.title.func('Luc')


plot.mcmc.func(gcc.met.pace.df,'Fes','Control','Ambient',subplot = NULL,nm.note = 'v13',use.smooth = TRUE,my.fun =phenoGrass.func.v13 )
plot.title.func('Fes')

plot.mcmc.func(gcc.met.pace.df,'Rye','Control','Ambient',subplot = NULL,nm.note = 'v13',use.smooth = TRUE,my.fun =phenoGrass.func.v13 )
plot.title.func('Rye')

species.vec <- c('Bis', 'Dig', 'DigBis', 'Kan', 'KanWal', 
                 'Pha', 'PhaSub', 'Rho',  'Wal')

for(i in seq_along(species.vec)){
  plot.mcmc.func(gcc.met.pace.df,species.vec[i],'Control','Ambient',subplot = NULL,nm.note = 'v13',use.smooth = TRUE,my.fun =phenoGrass.func.v13 )
  plot.title.func(species.vec[i]) 
}

# plot ym
ym.con.df <- get.ym.func('Control')
ym.pred.df <- readRDS('tmp/pred.smv13chain.ym.Control.Ambient.rds')
plot.mcmc.func(gcc.met.pace.df=ym.con.df,'ym','Control','Ambient',subplot = NULL,nm.note = 'v13',use.smooth = TRUE,my.fun =phenoGrass.func.v13 )
plot.title.func('YM')

dev.off()

pdf('figures/plot.diag.v13.pdf',width = 6,height = 9*0.618)

plot.check.mcmc.func=function(chain.in,burnIn =3000,species.in=''){
  par(mfrow=c(3,2),mar=c(5,5,1,1))
  for(i in 1:ncol(chain.in)){
    hist(chain.in[burnIn:nrow(chain.in),i],xlab = c('Topt','f.extract','senescence','growth','q')[i],
         main='')
    
  }
  plot.title.func(species.in = species.in)
}
chain.3.ls = readRDS('cache/smv13chain.Luc.Control.Ambient.rds')
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Luc')

chain.3.ls = readRDS('cache/smv13chain.Fes.Control.Ambient.rds')
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Fes')

chain.3.ls = readRDS('cache/smv13chain.Rye.Control.Ambient.rds')
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Rye')
# 
species.vec <- c('Bis', 'Dig', 'DigBis', 'Kan', 'KanWal', 
                 'Pha', 'PhaSub', 'Rho',  'Wal','ym')

for(i in seq_along(species.vec)){
  fn <- sprintf('cache/smv13chain.%s.Control.Ambient.rds',species.vec[i])
  chain.3.ls = readRDS(fn)
  lapply(chain.3.ls, plot.check.mcmc.func,species.in=species.vec[i])
}

dev.off()

# pred.13.df <- readRDS('tmp/pred.smv13chain.Luc.Control.Ambient.rds')
# pred.11.df <- readRDS('tmp/pred.smv11chain.Luc.Control.Ambient.rds')
