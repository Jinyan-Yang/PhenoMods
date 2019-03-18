library(zoo)
# source('models/hufkens/get_pace_gcc.R')
source('models/hufkens/phenoGrass.R')
source('models/hufkens/pen_mon.R')
gcc.met.pace.df <- readRDS('pace_gcc_met.rds')
gcc.met.pace.df$map=760

plot(GCC~Date,data = gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P3C',])

gcc.met.pace.df$u2 <- na.locf(gcc.met.pace.df$WS_ms_Avg)
gcc.met.pace.df$Rain <- na.locf(gcc.met.pace.df$irrigsum)
gcc.met.pace.df$RH <- na.locf(gcc.met.pace.df$rh)
gcc.met.pace.df$GCC.norm <- ((gcc.met.pace.df$GCC - min(gcc.met.pace.df$GCC,na.rm=T)) / 
  (-min(gcc.met.pace.df$GCC,na.rm=T) + max(gcc.met.pace.df$GCC,na.rm=T)))
gcc.met.pace.df$PPFD <- na.locf(gcc.met.pace.df$par)

# need to get 16 dates of met before gcc
# test.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S5P5B',]
test.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P3C',]


start.date <- test.df$Date[min(which(!is.na(test.df$GCC)))]

gcc.met.pace.df.na.rm <- test.df[!is.na(test.df$GCC) & test.df$Date >= start.date,]

gcc.met.pace.df.16 <- rbind(gcc.met.pace.df.na.rm,
                            test.df[test.df$Date <= start.date &
                                      test.df$Date > (start.date-16),])

gcc.met.pace.df.16 <- gcc.met.pace.df.16[order(gcc.met.pace.df.16$Date),]
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
  
  # dat <- dat[1:max(which(!is.na(dat$GCC.norm))),]
  # 
  # dat <- dat[(min(which(!is.na(dat$GCC.norm)))-16):nrow(dat),]
  
  sf.value <- scaling.f.func(mean(dat$map,na.rm=TRUE),f.h)
  
  dat$cover <- sf.value * dat$GCC.norm 
  
  pred.df <- phenoGrass.func(dat,f.h,f.t.opt,f.extract,f.sec,f.growth,
                                        swc.wilt = swc.wilt,
                                        swc.capacity = swc.capacity,
                                        t.max = t.max)
  dat$cover.pred <- pred.df$cover.pred.vec
  dat$swc.pred <- pred.df$swc.pred.vec
  
  cvmae <- sum((dat$cover - dat$cover.pred.vec),na.rm=TRUE) / mean(dat$cover,na.rm=TRUE)
  
  cvmae.swc <- sum((dat$vwc - dat$swc.pred),na.rm=TRUE) / mean(dat$vwc,na.rm=TRUE)
  
  return(c(abs(cvmae) + abs(cvmae.swc)))
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
maxiter <- 100

#- set seed for repeatability
set.seed(1234)

#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
#- model with no change in photosynthetic capacity
library(DEoptim)
library(Evapotranspiration)
data("constants") #this is used for penman et value; data from Adleide

# fit eucface with Deoptim, It seems this is not too different from MCMC
hufkens.fit.pace <- DEoptim(fn=target.func,lower=lower,upper=upper,
                            dat=gcc.met.pace.df.16,
                            DEoptim.control(NP = NPmax,itermax=maxiter,
                                            trace=T,parallelType = 1,
                                            parVar = list("pet.func","scaling.f.func",
                                                          "t.func",'phenoGrass.func',
                                                          'constants')))

hufkens.fit.best.pace <- unname(hufkens.fit.pace$optim$bestmem)

# make predictions
out.pred.df <- phenoGrass.func(gcc.met.pace.df.16,
                               hufkens.fit.best.pace[1],
                               hufkens.fit.best.pace[2],
                               hufkens.fit.best.pace[3],
                               hufkens.fit.best.pace[4],
                               hufkens.fit.best.pace[5],
                               swc.wilt = swc.wilt,
                               swc.capacity = swc.capacity,
                               t.max = t.max)

gcc.met.pace.df.16$c.pred <- out.pred.df$cover.pred.vec
gcc.met.pace.df.16$swc.pred <- out.pred.df$swc.pred.vec

sf.value <- scaling.f.func(mean(gcc.met.pace.df.16$map,na.rm=TRUE),hufkens.fit.best.pace[1])

gcc.met.pace.df.16$cover <- sf.value * gcc.met.pace.df.16$GCC.norm 

# plots
# 1:1
plot(c.pred~cover,data = gcc.met.pace.df.16)
abline(a=0,b=1)
title('S1P3C LUC Drought Warm')

# time 
plot(cover~Date,data = gcc.met.pace.df.16)
points(c.pred~Date,data = gcc.met.pace.df.16,type='s',col='coral')
legend('topleft',legend = c('OBS','MOD'),pch=16,col=c('black','coral'))
title('S1P3C LUC comtrol amb')
