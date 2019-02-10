# # to be fitted
# # f.h,f.t.opt, f.extract,f.sec
# # needed par values
# # swc.wilt,swc.capacity,swc.initial

# hufkens did not mention what field capacity and wilting points are and where from
# they used an arbitory tmax of 45
swc.wilt <- 100
swc.capacity <- 400
t.max <- 45

# get GCC and normalis
# # met data is needed 16 days before gcc 
# gcc.df <- data.frame(Date=NA,
#                      GCC=0.3 + runif(32,-0.1,.1),
#                      map=800,
#                      tmean = rep(35,32),
#                      rad = 1800 + runif(32,-100,100),
#                      ppt = floor(runif(32,0,1.1))*20)
# gcc.df$GCC.norm <- (gcc.df$GCC - min(gcc.df$GCC)) / 
#   (min(gcc.df$GCC) + max(gcc.df$GCC))

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
                                        t.max = t.max)
  
  cvmae <- sum((dat$cover - dat$cover.pred.vec),na.rm=TRUE) / mean(dat$cover,na.rm=TRUE)
  
  return(abs(cvmae))
}

# target.func(gcc.met.df,c(200,30,0.8,0.5))
# # get the fitting running with deoptim
# f.h=200
# f.t.opt=30
# f.extract <- 0.5
# f.sec <- 0.05
# f.growth <- 0.002

lower <- c(100, 10, 0.4, 0.01, 0.001) # changed minimum Kmax from 2 to 5
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
data("constants")
hufkens.fit <- DEoptim(fn=target.func,lower=lower,upper=upper,
                       dat=gcc.met.df,
                       DEoptim.control(NP = NPmax,itermax=maxiter,
                                       trace=T,parallelType = 1,
                                       parVar = list("pet.func","scaling.f.func",
                                                     "t.func",'phenoGrass.func','constants')))

hufkens.fit.best <- unname(hufkens.fit$optim$bestmem)


gcc.met.df$c.pred<- phenoGrass.func(gcc.met.df,264.807,30.046,0.707,0.05,0.002698,
                                    swc.wilt = swc.wilt,
                                    swc.capacity = swc.capacity,
                                    t.max = t.max)

plot(c.pred~GCC.norm,data = gcc.met.df)
abline(a=0,b=1)


plot(GCC.norm~Date,data = gcc.met.df)
# abline(a=0,b=1)

points(c.pred~Date,data = gcc.met.df,pch=16,col='coral')

legend('topleft',legend = c('Obs','MOD'),pch=16,col=c('black','coral'))
