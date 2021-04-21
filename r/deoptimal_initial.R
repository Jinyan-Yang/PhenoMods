# limits.vec <- limit.ls[[i]]
# gcc.met.modis.site <- get.pace.func(gcc.met.pace.df=gcc.met.modis.df,
#                                     species.in=modis.sites.vec[i],
#                                     prep.in='Control',
#                                     temp.in='Ambient',
#                                     norm.min.max=limits.vec)

library(DEoptim)
# library(rootSolve)
# Function to be passed to DE optim
model.de.func <- function(pars,dat,bucket.size,swc.in.wilt,swc.in.cap,day.lag,use.smooth){

  #- pull out the parameters from the pars vector

  hufken.pace.pred <- phenoGrass.func.v13(dat,
                                          f.h = 222,
                                          f.t.opt = pars[1],
                                          f.extract = pars[2],
                                          f.sec= pars[3],
                                          f.growth = pars[4],
                                          q =  pars[5],
                                          q.s =  pars[6],
                                          bucket.size = bucket.size,
                                          swc.wilt = swc.in.wilt ,
                                          swc.capacity = swc.in.cap ,
                                          t.max = 45,
                                          day.lay = day.lag,use.smooth = use.smooth)
  
  
  #
  sd.gcc <- sd(hufken.pace.pred$cover,na.rm = T)

  resid.gs <- ((hufken.pace.pred$cover.hufken - hufken.pace.pred$cover)/sd.gcc)^2
  
  return(sum(resid.gs,na.rm = T))
}

# setting control parameters and limits to values
lower <- c(1, 0.01,0.001,0.001,0.01,0.01) 
upper <- c(40,0.5,   0.5, 0.5,  5  ,5)
NPmax <- 100
maxiter <- 30

#- Call to DEoptim
# years <-(year(gcc.met.pace.df.16$Date))

get.ini.func <- function(){
  OptBB.de.fit <- DEoptim(fn=model.de.func,lower=lower,upper=upper,
                          dat=gcc.met.pace.df.16,
                          DEoptim.control(VTR = 1,
                                          NP = NPmax,itermax=maxiter,trace=1,parallelType =0,
                                          parVar = globalenv(),
                                          packages=list("lubridate",'Evapotranspiration')),
                          use.smooth = use.smooth,
                          swc.in.cap = swc.capacity,
                          swc.in.wilt = swc.wilt,
                          bucket.size = bucket.size,
                          day.lag=day.lag)
  
  initial.vec <- unname(OptBB.de.fit$optim$bestmem)
  return(initial.vec)
}


