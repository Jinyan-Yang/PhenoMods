# limits.vec <- limit.ls[[i]]
# gcc.met.modis.site <- get.pace.func(gcc.met.pace.df=gcc.met.modis.df,
#                                     species.in=modis.sites.vec[i],
#                                     prep.in='Control',
#                                     temp.in='Ambient',
#                                     norm.min.max=limits.vec)

library(DEoptim)
# library(rootSolve)
# Function to be passed to DE optim
model.de.v10.func <- function(pars,dat,
                              bucket.size,swc.in.wilt,swc.in.cap,
                              day.lag,use.smooth){
  
  hufken.pace.pred <- phenoGrass.func.v10(dat,
                                          f.h = 222,
                                          f.t.opt = pars[1],
                                          f.extract = pars[2],
                                          f.sec= pars[3],
                                          f.growth = pars[4],
                                          bucket.size = bucket.size,
                                          swc.wilt = swc.in.wilt ,
                                          swc.capacity = swc.in.cap ,
                                          t.max = 45,
                                          day.lay = day.lag,
                                          use.smooth = use.smooth)
  
  
  #
  sd.gcc <- sd(hufken.pace.pred$cover,na.rm = T)
  
  resid.gs <- ((hufken.pace.pred$cover.hufken - hufken.pace.pred$cover)/sd.gcc)^2
  
  return(sum(resid.gs,na.rm = T))
}



#- Call to DEoptim
# years <-(year(gcc.met.pace.df.16$Date))

get.de.v10.func <- function(par.df,...){
  # setting control parameters and limits to values
  lower <- unname(par.df['min',])
  upper <- unname(par.df['max',]) 
  NPmax <- 100
  maxiter <- 100
  # 
  set.seed(1935)
  OptBB.de.fit <- DEoptim(fn=model.de.v10.func,lower=lower,upper=upper,
                          dat=gcc.met.pace.df.16,
                          DEoptim.control(VTR = 1,
                                          NP = NPmax,itermax=maxiter,trace=1,parallelType =0,
                                          parVar = globalenv(),
                                          packages=list("lubridate",'Evapotranspiration')),
                          use.smooth = use.smooth,
                          swc.in.cap = swc.capacity,
                          swc.in.wilt = swc.wilt,
                          bucket.size = bucket.size,
                          day.lag=day.lag,...)
  
  initial.vec <- unname(OptBB.de.fit$optim$bestmem)
  return(initial.vec)
}



# wrpped function to fit mcmc to a df
fit.de.v10.func <- function(df = gcc.met.pace.df,
                            species.in = 'Luc',prep.in = 'Control', 
                            temp.in ='Ambient',subplot =NA,
                            my.fun = phenoGrass.func.v10,
                            out.nm.note = '',use.smooth = FALSE,
                            day.lag = 3,
                            bucket.size = 300,
                            swc.capacity = 0.13,
                            swc.wilt = 0.05,
                            n.iter = 10000,
                            norm.min.max=NULL,
                            par.df){
  # 
  s.time <- Sys.time()
  gcc.met.pace.df.16 <<- get.pace.func(df,
                                       species.in =species.in,
                                       prep.in = prep.in,
                                       temp.in =temp.in,
                                       subplot = subplot,
                                       norm.min.max = norm.min.max)
  
  my.fun <<-  my.fun
  use.smooth <<-  use.smooth
  day.lag <<- day.lag
  bucket.size <<-  bucket.size
  swc.capacity <<- swc.capacity
  swc.wilt <<-  swc.wilt
  n.iter <<-  n.iter

  fit.par.df <- get.de.v10.func(par.df = par.df)
  
  out.fn <- paste0('cache/de.v10.',species.in,'.rds')
  saveRDS(fit.par.df,out.fn)
  
}