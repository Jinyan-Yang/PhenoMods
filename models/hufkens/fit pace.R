library(zoo)
# source('models/hufkens/get_pace_gcc.R')
source('models/hufkens/phenoGrass.R')
source('models/hufkens/pen_mon.R')
gcc.met.pace.df.in <- readRDS('cache/gcc.met.pace.df.rds')
gcc.met.pace.df.in$map=760
# start.date <- as.Date('2018-04-1')
gcc.met.pace.df.sum <- doBy::summaryBy(.~Date + Shelter + Plot + SubplotID + Subplot + CameraSubplot + Species, 
                                       FUN = mean, data =  gcc.met.pace.df.in,na.rm=TRUE,
                                   keep.names = T)

header.df <- read.csv('download/SubPlotTreatmentsMaster 2019-02-13.csv')
header.df <- subset(header.df,select = c(SubplotID,Precipitation,Temperature))
header.df$SubplotID <- as.character(header.df$SubplotID)
header.df$Precipitation <- as.character(header.df$Precipitation)
header.df$Temperature <- as.character(header.df$Temperature)

gcc.met.pace.df <- merge(gcc.met.pace.df.sum,header.df)
gcc.met.pace.df <- gcc.met.pace.df[gcc.met.pace.df$Date >= as.Date('2018-04-1') &
                                     gcc.met.pace.df$Date < as.Date('2018-8-22'),]


gcc.met.pace.df$GCC.norm <- (gcc.met.pace.df$GCC - min(gcc.met.pace.df$GCC,na.rm=T)) / 
                               (max(gcc.met.pace.df$GCC,na.rm=T)-min(gcc.met.pace.df$GCC,na.rm=T))


# 
# plot(GCC~Date,data = gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P3C',])
# par(new=TRUE)
# plot(vwc~Date,data = gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P3C',],
#      ann=F,axes=F,type='s',col='lightskyblue')

gcc.met.pace.df$u2 <- na.locf(gcc.met.pace.df$WS_ms_Avg)
gcc.met.pace.df$Rain <- na.locf(gcc.met.pace.df$irrig.tot)
gcc.met.pace.df$RH <- na.locf(gcc.met.pace.df$rh)
gcc.met.pace.df$PPFD <- na.locf(gcc.met.pace.df$par)
# gcc.met.pace.ls <- split(gcc.met.pace.df,gcc.met.pace.df$SubplotID)
# gcc.met.pace.ls <- lapply(gcc.met.pace.ls,
#                           function(df){
#                             df$GCC.norm = ((df$GCC - min(df$GCC,na.rm=T)) / (max(df$GCC,na.rm=T)-min(df$GCC,na.rm=T)))
#                             df$vwc.norm = ((df$vwc - min(df$vwc,na.rm=T)) /  (max(df$vwc,na.rm=T)-min(df$vwc,na.rm=T)))
#                             return(df)
#                           })
# gcc.met.pace.df <- do.call(rbind,gcc.met.pace.ls)
# need to get 16 dates of met before gcc
# test.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S5P5B',]
saveRDS(gcc.met.pace.df,'cache/gcc.met.pace.df.processed.rds')


# 
gcc.met.pace.df <- readRDS('cache/gcc.met.pace.df.processed.rds')

temp.df <- gcc.met.pace.df[gcc.met.pace.df$Species == 'Luc'&
                                     gcc.met.pace.df$Precipitation == 'Control'&
                                     gcc.met.pace.df$Temperature == 'Ambient',]
  #gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P3C',]

sd.df <- summaryBy(GCC.norm + vwc ~ Date,
                   data = temp.df,FUN=c(sd),na.rm=TRUE,keep.names = F)

mean.df <- summaryBy(.~ Date,
                     data = temp.df,FUN=c(mean),na.rm=TRUE,keep.names = T)

test.df <- merge(mean.df,sd.df)
# test.df$gcc.sd <- sd.df$GCC.sd
# test.df$vwc.sd <- sd.df$vwc.sd

# test.df$GCC <- zoo::na.locf(test.df$GCC,fromLast=F)
test.df$GCC.norm <- zoo::na.locf(test.df$GCC.norm,fromLast=F)
# test.df$GCC.norm[test.df$Date <= as.Date('2018-06-5')] <- NA
# 
# 
# test.df$GCC.norm <- ((test.df$GCC - min(test.df$GCC,na.rm=T)) / 
#                                (-min(test.df$GCC,na.rm=T) + max(test.df$GCC,na.rm=T)))

test.df <- test.df[order(test.df$Date),]

start.date <- test.df$Date[min(which(!is.na(test.df$GCC.norm)))]

gcc.met.pace.df.na.rm <- test.df[!is.na(test.df$GCC) & test.df$Date > start.date,]

gcc.met.pace.df.16 <- rbind(gcc.met.pace.df.na.rm,
                            test.df[test.df$Date <= start.date &
                                      test.df$Date > (start.date-17),])

gcc.met.pace.df.16 <- gcc.met.pace.df.16[order(gcc.met.pace.df.16$Date),]


hufken.pace.pred <- phenoGrass.func(gcc.met.pace.df.16,
                                    f.h = 222,
                                    f.t.opt = 33,
                                    f.extract = 0.519348383 ,
                                    f.growth= 0.00227958267,
                                    f.sec = 0.0755224228 ,
                                    swc.wilt = 10,
                                    swc.capacity = 120,
                                    t.max = 50)

gcc.met.pace.df.16$cover.hufken <- hufken.pace.pred$cover.pred.vec
gcc.met.pace.df.16$swc.hufken <- hufken.pace.pred$swc.pred.vec

plot(c(GCC.norm*0.774)~Date,data = gcc.met.pace.df.16,ylim=c(0,0.6))

points(cover.hufken~Date,data = gcc.met.pace.df.16,pch=16,col='red')

par(new=T)

plot(Rain~Date,data = gcc.met.pace.df.16,ann=F,axes=F,type='s',col='lightskyblue')


plot(c(GCC.norm*0.774)~cover.hufken,data = gcc.met.pace.df.16)
abline(a=0,b=1)

summary(lm(c(GCC.norm*0.774)~cover.hufken,data = gcc.met.pace.df.16))
# dat =gcc.met.pace.df.16
# gcc.df = gcc.met.pace.df.16

# # hufkens used pars values in Tabl S5 (not anymore)
swc.wilt <- 10
swc.capacity <- 120
t.max <- 45

# drought.expect <- 5
# # met data is needed 16 days before gcc 

# target function to minimise
target.func <- function(dat,
                        pars,
                        swc.wilt,
                        swc.capacity,
                        t.max){
  # drought.expect = 5
  # drought.expect=pars[1]
  f.h = pars[1]
  f.t.opt=pars[2]
  f.extract <- pars[3]
  f.sec <- pars[4]
  f.growth <- pars[5]
  
  # dat <- dat[1:max(which(!is.na(dat$GCC.norm))),]
  # 
  # dat <- dat[(min(which(!is.na(dat$GCC.norm)))-16):nrow(dat),]
  # 
  sf.value <- scaling.f.func(mean(dat$map,na.rm=TRUE),f.h)
  # 
  dat$cover <- sf.value * dat$GCC.norm
  
  pred.df <- phenoGrass.func(dat,f.h,f.t.opt,f.extract,f.sec,f.growth,
                             swc.wilt = swc.wilt,
                             swc.capacity = swc.capacity,
                             t.max = t.max)
  dat$cover.pred <- pred.df$cover.pred.vec
  dat$swc.pred <- pred.df$swc.pred.vec
  
  # cvmae <- sum((dat$cover - dat$cover.pred),na.rm=TRUE) / mean(dat$cover,na.rm=TRUE) / nrow(dat)
  # 
  # cvmae.swc <- sum((dat$vwc - dat$swc.pred/100),na.rm=TRUE) / mean(dat$vwc,na.rm=TRUE) / nrow(dat)
  # return(c(abs(cvmae) + abs(cvmae.swc)))
  
  resid.gcc <- ((dat$cover - dat$cover.pred)/mean(dat$cover,na.rm=TRUE))^2
  resid.vwc <- ((dat$vwc - dat$swc.pred/1000)/mean(dat$vwc,na.rm=TRUE))^2
  
  resid.gcc[is.na(resid.gcc)] <- 0
  resid.vwc[is.na(resid.vwc)] <- 0
  
  
  resid.sum <- sum(resid.gcc)
  
  return(resid.sum)
}


# # get the fitting running with deoptim
# order of pars
# f.h=200
# f.t.opt=30
# f.extract <- 0.1
# f.sec <- 0.005
# f.growth <- 0.002
# lower <- c(200, 10, 0.1,  0.001, 0.1) 
# upper <- c(200, 10, 0.9,  0.05,  0.4)

lower <- c(200, 30, 0.01,  0.01,  0.002) 
upper <- c(240, 35, 0.5,   0.09,  0.02)
NPmax <- 100
maxiter <- 50

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
                            swc.wilt = swc.wilt,
                            swc.capacity = swc.capacity,
                            t.max = t.max,
                            DEoptim.control(NP = NPmax,itermax=maxiter,
                                            trace=T,parallelType = 1,
                                            parVar = list("pet.func","scaling.f.func",
                                                          "t.func",'phenoGrass.func',
                                                          'constants')))

hufkens.fit.best.pace <- unname(hufkens.fit.pace$optim$bestmem)

# make predictions
sf.value <- scaling.f.func(mean(gcc.met.pace.df.16$map,na.rm=TRUE),hufkens.fit.best.pace[1])

gcc.met.pace.df.16$cover <- sf.value * gcc.met.pace.df.16$GCC.norm 

out.pred.df <- phenoGrass.func(gcc.met.pace.df.16,
                               f.h=hufkens.fit.best.pace[1],
                               f.t.opt= hufkens.fit.best.pace[2],
                               f.extract = hufkens.fit.best.pace[3],
                               f.sec = hufkens.fit.best.pace[4],
                               f.growth = hufkens.fit.best.pace[5],
                               swc.wilt = swc.wilt,
                               swc.capacity = swc.capacity,
                               t.max = t.max)

gcc.met.pace.df.16$c.pred <- out.pred.df$cover.pred.vec
gcc.met.pace.df.16$swc.pred <- out.pred.df$swc.pred.vec



# plots
# 1:1
plot(c.pred~cover,data = gcc.met.pace.df.16)
abline(a=0,b=1)
title('S1P3C FES Drought Warm')


par(mfrow=c(2,1))
par(mar=c(1,5,5,1))
# time 
plot(cover~Date,data = gcc.met.pace.df.16,pch=16,xaxt = 'n',xlab='')
points(c.pred~Date,data = gcc.met.pace.df.16,type='p',col='coral',pch=16)
legend('topleft',legend = c('OBS','MOD'),pch=16,col=c('black','coral'))
# title('S1P3C FES control amb')

# plot(c.pred~Date,data = gcc.met.pace.df.16,type='p',col='coral')

par(mar=c(5,5,1,1))
plot(vwc~Date,data = gcc.met.pace.df.16,pch=16)
points(c(swc.pred/1000)~Date,data = gcc.met.pace.df.16,type='p',col='coral',pch=16)
legend('topleft',legend = c('OBS','MOD'),pch=16,col=c('black','coral'))
# title('S1P3C FES control amb')
