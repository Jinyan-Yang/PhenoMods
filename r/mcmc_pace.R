source('r/functions_mcmc_v12.r')
source('models/hufkens/pG_v10.R')
source('r/plot.mcmc.r')
day.lag <- 3
source('r/pace_data_process.R')
# packages
library(doBy)
library(zoo)

fit.mcmc.pace.func <- function(species.in = 'Luc',prep.in = 'Control', 
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
    f.growth = c(0.1,0.2,0.3,NA,NA,NA))
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

# # $$$$####
# fit.mcmc.pace.func(subplot = 'S3P3B')

# get fit by species but with original hufkens 
# fit.mcmc.pace.func(species.in='Luc',prep.in = 'Control', temp.in ='Ambient',
#                    my.fun = phenoGrass.func.v10,out.nm.note='v10.test')

# fit original hufkens
fit.mcmc.pace.func(species.in='Luc',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v10,out.nm.note='v10',use.smooth = TRUE)

fit.mcmc.pace.func(species.in='Fes',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v10,out.nm.note='v10',use.smooth = TRUE)


fit.mcmc.pace.func(species.in='Rye',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v10,out.nm.note='v10',use.smooth = TRUE)

# fit v11
fit.mcmc.pace.func(species.in='Luc',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v11,out.nm.note='v11',use.smooth = TRUE)

fit.mcmc.pace.func(species.in='Fes',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v11,out.nm.note='v11',use.smooth = TRUE)

fit.mcmc.pace.func(species.in='Rye',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v11,out.nm.note='v11',use.smooth = TRUE)


# make the plots
# fit.mcmc.pace.func(subplot = 'S3P3B',my.fun = phenoGrass.func.v11,out.nm.note='v10')
# luc.c.a.v10.df= readRDS('cache/v10chain.Luc.Control.Ambient.rds')

# plot.mcmc.func('Luc','Control','Ambient',subplot = NULL,nm.note = '')

# pdf('umsm.v10.pdf',width = 6,height = 3*6*0.618)
# plot.mcmc.func('Luc','Control','Ambient',subplot = NULL,nm.note = 'v10')
# dev.off()


# pdf('sm.v10.3chain.pdf',width = 10,height = 10*0.618)
# plot.mcmc.func('Luc','Control','Ambient',subplot = NULL,nm.note = 'v10',use.smooth = TRUE,my.fun =phenoGrass.func.v10 )
# dev.off()
# 
# 
# pdf('sm.v10.3chain.diag.pdf',width = 6,height = 6*0.618)
# chain.3.ls = readRDS('cache/smv10.testchain.Luc.Control.Ambient.rds')
# plot.check.mcmc.func=function(chain.in,burnIn =3000){
#   par(mfrow=c(2,2))
#   for(i in 1:ncol(chain.in)){
#     hist(chain.in[burnIn:nrow(chain.in),i],xlab = c('Topt','f.extract','senescence','growth')[i],
#          main='')
# 
#   }
# 
# }
# lapply(chain.3.ls, plot.check.mcmc.func)
# dev.off()

# 
# pdf('sm.v11.3chain.pdf',width =10,height = 10*0.618)
# plot.mcmc.func('Luc','Control','Ambient',subplot = NULL,nm.note = 'v11',use.smooth = TRUE,my.fun =phenoGrass.func.v11 )
# dev.off()

