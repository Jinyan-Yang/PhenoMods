library(DEoptim)
library(Evapotranspiration)
library(HIEv)
library(data.table)
library(doBy)

if(!dir.exists("download"))dir.create("download")
source('models/hufkens/phenoGrass.R')
source('models/hufkens/pen_mon.R')

if(!file.exists('cache/gcc.met.df.rds')){
  source('models/hufkens/get_gcc_met_Euc.R')
}else{
  gcc.met.df <- readRDS('cache/gcc.met.df.rds')
}

# # to be fitted
# # f.h,f.t.opt, f.extract,f.sec
# # needed par values
# # swc.wilt,swc.capacity,swc.initial
# hufkens used pars values in Tabl S5
swc.wilt <- 100
swc.capacity <- 400
t.max <- 45

# # met data is needed 16 days before gcc 

# target function to minimise
target.func <- function(dat,
                        pars,
                        swc.wilt = 100,
                        swc.capacity = 400,
                        t.max = 45){
  
  f.h=pars[1]
  f.t.opt=pars[2]
  f.extract <- pars[3]
  f.sec <- pars[4]
  f.growth <- pars[5]
  
  
  
  dat$cover.pred.vec <- phenoGrass.func(dat,f.h,f.t.opt,f.extract,f.sec,f.growth,
                                        swc.wilt = swc.wilt,
                                        swc.capacity = swc.capacity,
                                        t.max = t.max)$swc.pred.vec
  
  cvmae <- sum((dat$cover - dat$cover.pred.vec),na.rm=TRUE) / mean(dat$cover,na.rm=TRUE)
  
  return(abs(cvmae))
}


# # get the fitting running with deoptim
# order of pars
# f.h=200
# f.t.opt=30
# f.extract <- 0.5
# f.sec <- 0.05
# f.growth <- 0.002
lower <- c(100, 10, 0.4, 0.01, 0.001) 
upper <- c(300, 40, 0.9, 0.09, 0.005)
NPmax <- 100
maxiter <- 60

#- set seed for repeatability
set.seed(1234)

#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
#- model with no change in photosynthetic capacity
library(DEoptim)
library(Evapotranspiration)
data("constants") #this is used for penman et value; data from Adleide

# fit eucface with Deoptim, It seems this is not too different from MCMC
hufkens.fit <- DEoptim(fn=target.func,lower=lower,upper=upper,
                       dat=gcc.met.df,
                       DEoptim.control(NP = NPmax,itermax=maxiter,
                                       trace=T,parallelType = 0,
                                       parVar = list("pet.func","scaling.f.func",
                                                     "t.func",'phenoGrass.func','constants')))

hufkens.fit.best <- unname(hufkens.fit$optim$bestmem)

# make predictions
gcc.met.df$c.pred<- phenoGrass.func(gcc.met.df,264.807,30.046,0.707,0.05,0.002698,
                                    swc.wilt = swc.wilt,
                                    swc.capacity = swc.capacity,
                                    t.max = t.max)$swc.pred.vec

# plots
# 1:1
plot(c.pred~GCC.norm,data = gcc.met.df)
abline(a=0,b=1)

# time 
plot(GCC.norm~Date,data = gcc.met.df)
points(c.pred~Date,data = gcc.met.df,pch=16,col='coral')
legend('topleft',legend = c('Obs','MOD'),pch=16,col=c('black','coral'))
