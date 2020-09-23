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
    print(paste0(i,' / ',iterations))
  }
  return(chain)
}
source('models/hufkens/hufkensV13.R')
source('r/plot.mcmc.r')

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
    f.growth = c(0.1,0.2,0.3,NA,NA,NA),
    q = c(0.001,1,2,NA,NA,NA))
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