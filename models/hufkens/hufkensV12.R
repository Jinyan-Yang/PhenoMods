source('models/hufkens/hufkens_common_fun.R')
source('r/function_hydro.R')

# 
phenoGrass.func.v12 <- function(gcc.df,
                                f.h,
                                f.t.opt,
                                f.extract,
                                f.sec,
                                f.growth,
                                swc.wilt ,
                                swc.capacity ,
                                bucket.size,
                                t.max,
                                day.lay){
  
  # set the lag factor; in num of days
  start.date <- gcc.df$Date[min(which(!is.na(gcc.df$GCC.norm)))]
  
  gcc.df <- gcc.df[gcc.df$Date > (start.date - day.lay),]
  sf.value <- scaling.f.func(mean(gcc.df$map,na.rm=TRUE),f.h)
  
  gcc.df$cover <-  gcc.df$GCC.norm * sf.value
  
  # set up the inital conditions
  swc.vec <- gcc.df$vwc * bucket.size
  et <- c()
  cover.pred.vec <- c()
  cover.pred.vec[day.lay] <- gcc.df$cover[!is.na(gcc.df$cover)][1]
  water.avi <- c()
  water.avi <- (swc.vec - swc.wilt) * bucket.size
  water.lag <- c()
  water.lag <- water.avi
  t.m <- growth.vec <- senescence.vec <- evap.vec <- transp.vec <- c()
  
  # check whether it rained in the past three days
  rained.vec <- c()
  rained.vec[1:day.lay] <- 0
  for(i in (day.lay + 1):nrow(gcc.df)){
    if(sum(gcc.df$Rain[(i-day.lay):i],na.rm=T)>0){
      rained.vec[i] <- sum(gcc.df$Rain[(i-day.lay):i],na.rm=T)
    }else{
      rained.vec[i] <- 0
    }
  }
  
  # calcualte the par values
  cover.max <- max(gcc.df$cover,na.rm=TRUE)
  # rad.min <-  min(gcc.df$PPFD,na.rm=TRUE)
  # rad.max <-  max(gcc.df$PPFD,na.rm=TRUE)
  # gcc.df$rad.norm <- (gcc.df$PPFD - rad.min) / (rad.max - rad.min)
  
  # model start
  for (nm.day in (day.lay+1):nrow(gcc.df)){
    
    # plant avialbe water
    # water.avi[nm.day] <- max(0,swc.vec[nm.day-1]- swc.wilt) * k.soil / k.sat

    # loss of supply
    # loss.f <- (1-weibull.func(swc.vec[nm.day-1],1000,0.2)+0.01)
    
    # reduction of soil conductivity
    loss.f.soil <- k.soil.func(swc.vec[nm.day-1]/bucket.size,
                               psi.e = -0.03e-3,#KPa
                               b = 4.26,
                               swc.sat = swc.capacity,
                               k.sat = 79.8) / 79.8

    # loss.f <- e.frac.func(swc.vec[nm.day-1]/bucket.size,
    #                       k.plant = 0.02,
    #                       swc.sat = 0.38,
    #                       psi.e = -0.13e-3,#KPa
    #                       b = 6.7,
    #                       k.sat = 25.2,

    loss.f <- e.frac.func(swc.vec[nm.day-1] / bucket.size,
                          swc.sat = swc.capacity,
                          lai=4,
                          k.plant = 0.08,
                          psi.e = -0.03e-3,#MPa
                          b = 4.26, 
                          psi.min = -2,
                          k.sat = 79.8)

    # loss.f <- max(0,(swc.vec[nm.day-1]- swc.wilt*bucket.size))
    # / (swc.capacity*bucket.size - swc.wilt*bucket.size)

    #                       psi.min = -2)
    # k.sat=11.4
    # loss.f <- k.soil.func(swc.vec[nm.day-1]/bucket.size,k.sat = k.sat) / k.sat

    if(is.na(loss.f))loss.f=0
    loss.f <- min(loss.f,1)
    loss.f <- max(loss.f,0)

    if(is.na(loss.f.soil))loss.f=0
    loss.f.soil <- min(loss.f.soil,1)
    loss.f.soil <- max(loss.f.soil,0)
    
    water.avi[nm.day] <- max(0,(swc.vec[nm.day-1]- swc.wilt*bucket.size))
    
    # water.avi[nm.day] <- loss.f
    # # define the legency effect 
    i=0
    while(i+1<day.lay & (nm.day-i)>0){
      i=i+1
    }
    
    # water.lag[nm.day] <- min(water.avi[(nm.day-i):nm.day],na.rm=TRUE) #note this is different from the Hufkens
    # water.lag[nm.day] <- water.avi[nm.day-i]
    t.m[nm.day] <- gcc.df$Tair[nm.day]#mean(gcc.df$Tair[(nm.day-15):nm.day],na.rm=TRUE) #hufkens used 15 days
    # hufkens used evaportanspiration from Hargreaves 1985
    # here is from evapotranspiration R package
    et[nm.day] <- pet.func(gcc.df$Date[nm.day],gcc.df$PPFD[nm.day],
                           gcc.df$Tair[nm.day],gcc.df$Tmax[nm.day], gcc.df$Tmin[nm.day],
                           gcc.df$RHmax[nm.day],gcc.df$RHmin[nm.day], gcc.df$u2[nm.day])
    
    #  # # get plant cover
    # if(water.lag[nm.day] > water.lag[nm.day-1]){
    #   g = 1
    #   d = 0
    # }else{
    #   g = 0
    #   d = 1
    # }
    
    # if(rained.vec[nm.day] > 0){
    #   g = 1
    #   d = 0
    # }else{
    #   g = 0
    #   d = 1
    # }
    
    # # this could work
    # if(loss.f <= .2){
    # g = 0
    # d = 1
    # }else{
    #   g = 1
    #   d = 0
    # }
    
    # # 
    # if(loss.f <= .5){
    #   g = 0
    #   d = 1
    # }else{
    #     g = 1
    #     d = 0
    #   }

    g = 1
    d = 1
    
    # # calculate plant cover
    # water.lag.norm <- water.lag[nm.day]  / (swc.capacity - swc.wilt)
    # water.lag.norm <- min(1,water.lag.norm)
    # water.avi.norm <- water.avi[nm.day] / (swc.capacity - swc.wilt) / bucket.size
    # water.avi.norm <- min(1,water.avi.norm)
    
    # plant cover
    g.value <- t.func(t.m[nm.day],f.t.opt,t.max)
    growth.vec[nm.day] <- g * g.value * f.growth * 
      loss.f * 
      #water.avi.norm*
      (1 - cover.pred.vec[nm.day-1] / cover.max)
    senescence.vec[nm.day] <- d * f.sec * (1 - loss.f) #* cover.pred.vec[nm.day-1] / cover.max
    
    cover.pred.vec[nm.day] <- cover.pred.vec[nm.day-1] + growth.vec[nm.day] - senescence.vec[nm.day]                 
    
    # give min
    cover.pred.vec[nm.day] <- max(0,min(cover.pred.vec[nm.day],cover.max))
    
    # # account for harvest
    # if(gcc.df$harvest[nm.day] == 1){
    #   cover.pred.vec[nm.day] <- gcc.df$cover[nm.day+2]
    # }
    # calculate swc
    evap.vec[nm.day] <- (1 - cover.pred.vec[nm.day-1]) * 
      # loss.f^2*
      loss.f.soil*#^2*
      # ((swc.vec[nm.day-1]/bucket.size - swc.wilt)/(swc.capacity-swc.wilt))^2 *
      et[nm.day]
    transp.vec[nm.day] <- f.extract * 
      # swc.vec[nm.day-1] * 
      # water.avi.norm*
      loss.f *
      cover.pred.vec[nm.day] / cover.max
    
    swc.vec[nm.day] <- swc.vec[nm.day-1] + gcc.df$Rain[nm.day] - evap.vec[nm.day] - transp.vec[nm.day]
    
    swc.vec[nm.day] <- max(0,min(swc.capacity * bucket.size,swc.vec[nm.day]))
    
  }
  gcc.df$ppt <- gcc.df$Rain
  gcc.df$cover.hufken <- cover.pred.vec
  gcc.df$swc.hufken <- swc.vec
  gcc.df$vwc.hufken <- swc.vec / bucket.size
  gcc.df$water.avi <- water.avi
  gcc.df$evap <- evap.vec 
  gcc.df$tran <- transp.vec
  # out.df <- data.frame(gcc.df)
  # out.df <- out.df[!is.na(out.df$cover),]
  
  return(gcc.df)
}
