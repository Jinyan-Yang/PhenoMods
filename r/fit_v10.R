source('r/functions_mcmc_v12.r')
source('models/hufkens/pG_v10.R')
source('r/plot.mcmc.r')
day.lag <- 1
source('r/pace_data_process.R')
source('r/ym_data_process.R')
source('r/process_cw_gcc.R')
# packages
library(doBy)
library(zoo)

fit.mcmc.pace.func <- function(df = gcc.met.pace.df,
                               species.in = 'Luc',prep.in = 'Control', 
                               temp.in ='Ambient',subplot =NA,
                               my.fun = phenoGrass.func.v11,
                               out.nm.note = '',use.smooth = FALSE,
                               day.lag = 3,
                               bucket.size = 300,
                               swc.capacity = 0.13,
                               swc.wilt = 0.05,
                               n.iter = 10000){
  s.time <- Sys.time()
  gcc.met.pace.df.16 <- get.pace.func(df,
                                      species.in =species.in,
                                      prep.in = prep.in,
                                      temp.in =temp.in,
                                      subplot = subplot)
  gcc.met.pace.df.16$map <- 760
  
  # para values####
  par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
    f.t.opt = c(10,15,20,NA,NA,NA),
    f.extract = c(0.05,0.075,0.1,NA,NA,NA),
    f.sec = c(0.05,0.1,0.15,NA,NA,NA),
    f.growth = c(0.1,0.2,0.3,NA,NA,NA))
  row.names(par.df) <- c('min','initial','max','fit','stdv','prop')
  
  # this assume 100% of the data falls into the max min range
  # in a normal distribution for proposal.func
  par.df['stdv',] <- ((par.df['max',] - par.df['min',])/10)
  
  # start mcmc fiting######
  
  # soil.water.var <- quantile(gcc.met.pace.df.16$vwc,c(.1,.99))
  chain.fes=list()
  for(n.chain in 1:3){
    chain.fes[[n.chain]] = mh.MCMC.func(n.iter,
                                        par.df,
                                        gcc.met.pace.df.16,
                                        bucket.size = bucket.size,
                                        day.lay = day.lag,
                                        swc.capacity = swc.capacity,
                                        swc.wilt = swc.wilt,
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

# fit#####
# fit.mcmc.pace.func(subplot = 'S3P3B')

# get fit by species but with original hufkens 
# fit.mcmc.pace.func(species.in='Luc',prep.in = 'Control', temp.in ='Ambient',
#                    my.fun = phenoGrass.func.v10,out.nm.note='v10.test')

# fit original hufkens
species.vec <- c('Luc','Fes','Rye',
                 'Dig', 'DigBis', 'Kan', 'KanWal', 
                 'Pha', 'PhaSub', 'Rho',  'Wal')

for(i in seq_along(species.vec)){
  fit.mcmc.pace.func(species.in=species.vec[i],prep.in = 'Control', temp.in ='Ambient',
                     my.fun = phenoGrass.func.v10,out.nm.note='v10',use.smooth = TRUE)
  
}
# fit ym
ym.18.df <- get.ym.func(18)
fit.mcmc.pace.func(df=ym.18.df,n.iter = 20000,
                   species.in='ym',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v10,out.nm.note='v10',use.smooth = TRUE,
                   swc.capacity = 0.3,swc.wilt = 0.05)

# fit ym no lag
ym.18.df <- get.ym.func(18)
fit.mcmc.pace.func(df=ym.18.df,n.iter = 20000,
                   species.in='ym',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v10,out.nm.note='v10NOLAG',use.smooth = TRUE,
                   swc.capacity = 0.3,swc.wilt = 0.05,day.lag = 1)

# fit cw
gcc.met.cw.df <- readRDS('cache/gcc.met.cw.df.rds')
for (i in seq_along(site.vec)) {
  fit.mcmc.pace.func(df=gcc.met.cw.df,
                     species.in=site.vec[i],prep.in = 'Control', temp.in ='Ambient',
                     my.fun = phenoGrass.func.v10,out.nm.note='v10',use.smooth = TRUE,
                     swc.capacity = 0.3,swc.wilt = 0.05,n.iter = 20000)
}

# ####plot outcome#####
pdf('figures/plot.v10.pdf',width = 8,height = 8*0.618)
species.vec <- c('Luc','Fes','Rye',
                 'Bis', 'Dig', 'DigBis', 'Kan', 'KanWal', 
                 'Pha', 'PhaSub', 'Rho',  'Wal')

for(i in seq_along(species.vec)){
  plot.mcmc.func.noQ(gcc.met.pace.df,species.vec[i],'Control','Ambient',
                     subplot = NULL,nm.note = 'v10',use.smooth = TRUE,my.fun =phenoGrass.func.v10 )
  plot.title.func(species.vec[i]) 
}

# plot ym
ym.con.df <- get.ym.func(18)
plot.mcmc.func.noQ(df=ym.con.df,'ym','Control','Ambient',
               subplot = NULL,nm.note = 'v10',use.smooth = TRUE,
               my.fun =phenoGrass.func.v10 ,swc.in.wilt = 0.05,swc.in.cap = 0.3)
plot.title.func('YM')

# 
plot.mcmc.func.noQ(df=ym.con.df,'ym','Control','Ambient',
               subplot = NULL,nm.note = 'v10NOLAG',use.smooth = TRUE,
               my.fun =phenoGrass.func.v10 ,swc.in.wilt = 0.05,swc.in.cap = 0.3,day.lag = 1)
plot.title.func('YM-NOLAG')

# CW
for(i in seq_along(site.vec)){
  fn <- sprintf('cache/smv13chain.%s.Control.Ambient.rds',site.vec[i])
  if(file.exists(fn)){
    
    plot.mcmc.func.noQ(gcc.met.cw.df,site.vec[i],'Control','Ambient',subplot = NULL,nm.note = 'v10',use.smooth = TRUE,my.fun =phenoGrass.func.v10 )
    plot.title.func(site.vec[i]) 
  }
  
}

dev.off()

# plot diag####
pdf('figures/plot.diag.v10.pdf',width = 6,height = 9*0.618)

plot.check.mcmc.func=function(chain.in,burnIn =3000,species.in=''){
  par(mfrow=c(2,2),mar=c(5,5,1,1))
  for(i in 1:ncol(chain.in)){
    hist(chain.in[burnIn:nrow(chain.in),i],xlab = c('Topt','f.extract','senescence','growth')[i],
         main='')
    
  }
  plot.title.func(species.in = species.in)
}
# 
species.vec <- c('Luc','Fes','Rye',
                 'Bis', 'Dig', 'DigBis', 'Kan', 'KanWal', 
                 'Pha', 'PhaSub', 'Rho',  'Wal')

for(i in seq_along(species.vec)){
  fn <- sprintf('cache/smv10chain.%s.Control.Ambient.rds',species.vec[i])
  chain.3.ls = readRDS(fn)
  lapply(chain.3.ls, plot.check.mcmc.func,species.in=species.vec[i])
}

# ym
chain.3.ls <- readRDS('cache/smv10chain.ym.Control.Ambient.rds')
# plot(chain.3.ls[[2]][,4])
lapply(chain.3.ls, plot.check.mcmc.func,species.in='ym')

chain.3.ls <- readRDS('cache/smv10NOLAGchain.ym.Control.Ambient.rds')
# plot(chain.3.ls[[2]][,4])
lapply(chain.3.ls, plot.check.mcmc.func,species.in='ym-NOLAG')

# cw
for(i in seq_along(site.vec)){
  fn <- sprintf('cache/smv10chain.%s.Control.Ambient.rds',site.vec[i])
  if(file.exists(fn)){
    chain.3.ls = readRDS(fn)
    lapply(chain.3.ls, plot.check.mcmc.func,species.in=site.vec[i])
  }
  
}

dev.off()
