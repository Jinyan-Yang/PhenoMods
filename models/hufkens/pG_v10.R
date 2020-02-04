phenoGrass.func.v10 <- function(gcc.df,
                                f.h,
                                f.t.opt,
                                f.extract,
                                f.sec,
                                f.growth,
                                swc.wilt ,
                                swc.capacity ,
                                t.max){
  
  # set the lag factor; in num of days
  day.lay <- 16
  sf.value <- scaling.f.func(mean(gcc.df$map,na.rm=TRUE),f.h)
  
  gcc.df$cover <-  gcc.df$GCC.norm * sf.value
  # set up the inital conditions
  swc.vec <- c()
  swc.vec[1:16] <- gcc.df$vwc[1:16] * 1 * 1000
  et <- c()
  cover.pred.vec <- c()
  cover.pred.vec[day.lay] <- gcc.df$cover[!is.na(gcc.df$cover)][1]
  water.avi <- c()
  water.avi <- swc.vec - swc.wilt
  water.lag <- c()
  water.lag <- water.avi
  t.m <- growth.vec <- senescence.vec <- evap.vec <- transp.vec <- c()
  
  # calcualte the par values
  cover.max <- max(gcc.df$cover,na.rm=TRUE)
  rad.min <-  min(gcc.df$PPFD,na.rm=TRUE)
  rad.max <-  max(gcc.df$PPFD,na.rm=TRUE)
  gcc.df$rad.norm <- (gcc.df$PPFD - rad.min) / (rad.max - rad.min)
  
  # model start
  for (nm.day in (day.lay+1):(nrow(gcc.df))){
    # plant avialbe water
    water.avi[nm.day] <- max(0,swc.vec[nm.day-1]- swc.wilt)
    
    # # define the legency effect 
    i=0
    while(i+1<day.lay & (nm.day-i)>0){
      i=i+1
    }
    
    # water.lag[nm.day] <- min(water.avi[(nm.day-i):nm.day],na.rm=TRUE) #note this is different from the Hufkens
    water.lag[nm.day] <- water.avi[nm.day-i]
    t.m[nm.day] <- mean(gcc.df$Tair[(nm.day-i):nm.day],na.rm=TRUE) #hufkens used 15 days
    # hufkens used evaportanspiration from Hargreaves 1985
    # here is from evapotranspiration R package
    et[nm.day] <- pet.func(gcc.df$Date[nm.day],gcc.df$PPFD[nm.day],
                           gcc.df$Tair[nm.day],gcc.df$Tmax[nm.day], gcc.df$Tmin[nm.day],
                           gcc.df$RHmax[nm.day],gcc.df$RHmin[nm.day], gcc.df$u2[nm.day])
    
    # get plant cover
    # check whether water allow leaf drop
    if(water.lag[nm.day] <  water.lag[nm.day-1]){
      d = 1
    }else{
      d = 0
    }
    
    # check radiation  for leaf drop
    if(gcc.df$rad.norm[nm.day] <= gcc.df$rad.norm[nm.day-1]){
      d=0
    }else{
      d=1
    }
    
    
    # get g value; t depedence
    g.value <- t.func(t.m[nm.day],f.t.opt,t.max)
    # # calculate plant cover
    
    water.lag.norm <- (water.lag[nm.day]) / (swc.capacity - swc.wilt)
    water.avi.norm <- water.avi[nm.day]/(swc.capacity - swc.wilt)
    # plant cover
    growth.vec[nm.day] <- g.value * f.growth * water.lag.norm * (1 - cover.pred.vec[nm.day-1] / cover.max)
    senescence.vec[nm.day] <- d * f.sec * (1-water.avi.norm)
    
    cover.pred.vec[nm.day] <- cover.pred.vec[nm.day-1] + growth.vec[nm.day] - senescence.vec[nm.day]                 
    # give min
    cover.pred.vec[nm.day] <- max(0,min(cover.pred.vec[nm.day],cover.max))
    
    # calculate swc
    evap.vec[nm.day] <- (1 - cover.pred.vec[nm.day-1]) * (water.avi.norm)^2 * et[nm.day]
    transp.vec[nm.day] <- f.extract * water.lag.norm * cover.pred.vec[nm.day]
    
    swc.vec[nm.day] <- swc.vec[nm.day-1] + gcc.df$Rain[nm.day] - evap.vec[nm.day] - transp.vec[nm.day]
    
    swc.vec[nm.day] <- max(0,min(swc.capacity,swc.vec[nm.day]))
    
  }
  
  out.df <- data.frame(cover.pred.vec = cover.pred.vec,
                       swc.pred.vec = swc.vec)
  
  return(out.df)
}


# 
# hufken.pace.pred <- phenoGrass.func.v10(gcc.met.pace.df.16,
#                                         f.h = 222,
#                                         f.t.opt = 17,
#                                         f.extract = 0.05 ,
#                                         f.growth= 0.1,
#                                         f.sec = 0.01 ,
#                                         swc.wilt = 10,
#                                         swc.capacity = 120,
#                                         t.max = 45)
# 
# gcc.met.pace.df.16$cover.hufken <- hufken.pace.pred$cover.pred.vec
# gcc.met.pace.df.16$swc.hufken <- hufken.pace.pred$swc.pred.vec
# 
# plot(c(GCC.norm*0.783)~Date,data = gcc.met.pace.df.16,ylim=c(0.2,0.5),
#      xlab='',ylab='cover')
# 
# points(cover.hufken~Date,data = gcc.met.pace.df.16,pch=16,col='red')
# 
# par(new=T)
# 
# plot(vwc~Date,data = gcc.met.pace.df.16,ann=F,axes=F,type='s',col='lightskyblue')
# 
# 
# 
# 
# gcc.df = gcc.met.pace.df.16
# 
# f.h = 222
# f.t.opt = 33
# f.extract = 0.01
# f.growth= 0.02
# f.sec = 0.02 
# swc.wilt = 10
# swc.capacity = 120
# t.max = 45
