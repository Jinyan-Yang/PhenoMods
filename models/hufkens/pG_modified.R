phenoGrass.func.modified <- function(gcc.df,
                            f.h,
                            f.t.opt,
                            f.extract,
                            f.sec,
                            f.growth,
                            swc.wilt ,
                            swc.capacity ,
                            t.max,
                            drought.expect,
                            choler = FALSE){
  
  # set the lag factor; in num of days
  day.lay <- 16
  sf.value <- scaling.f.func(mean(gcc.df$map,na.rm=TRUE),f.h)
  # set up the inital conditions
  swc.vec <- c()
  swc.vec[1:day.lay] <- gcc.df$vwc[1:day.lay] * 1 * 1000
  et <- c()
  cover.pred.vec <- c()
  cover.pred.vec[day.lay] <- gcc.df$cover[gcc.df$Date == min(gcc.df$Date[!is.na(gcc.df$GCC)])] * sf.value
  water.avi <- c()
  water.avi <- swc.vec - swc.wilt
  water.lag <- c()
  water.lag <- water.avi
  t.m <- c()
  e.plant <- c()
  # calcualte the par values
  cover.max <- 1#max(gcc.df$cover,na.rm=TRUE)
  # rad.min <-  min(gcc.df$PPFD,na.rm=TRUE)
  # rad.max <-  max(gcc.df$PPFD,na.rm=TRUE)
  # gcc.df$rad.norm <- (gcc.df$PPFD - rad.min) / (rad.max - rad.min)
  
  # model start
  for (nm.day in day.lay:(nrow(gcc.df)-1)){
    # plant avialbe water
    water.avi[nm.day] <- max(0,swc.vec[nm.day]- swc.wilt)
    
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
    
    
    # check whether water allow leaf drop
    # if(water.lag[nm.day] <  water.lag[nm.day-1]){
    #   d = 1
    # }else{
    #   d = 0
    # }
    
    # # check radiation  for leaf drop 
    # if(gcc.df$rad.norm[nm.day] <= gcc.df$rad.norm[nm.day-1]){
    #   d=0
    # }else{
    #   d=1
    # }
    
    # give min
    cover.pred.vec[nm.day] <- max(0,min(cover.pred.vec[nm.day],cover.max))
    # get g value; t depedence
    g.value <- 1#t.func(t.m[nm.day],f.t.opt,t.max)
    
    # plant water use
    e.plant[nm.day] <- g.value * f.extract * water.avi[nm.day] * cover.pred.vec[nm.day]
    
    # check whether water allow leaf growth
    # drought.expect <- 5
    
    if(water.avi[nm.day] > mean(e.plant[(nm.day-drought.expect):nm.day],na.rm = T) * drought.expect){
      g = 1
      d = 0
    }else{
      g = 0
      d = 1
    }
    
    
    # calculate plant cover
    cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] + 
      g * f.growth * g.value * sf.value * water.avi[nm.day] / (swc.capacity - swc.wilt) *#water.lag[nm.day] *
      # gcc.df$rad.norm[nm.day] * 
      (1 - cover.pred.vec[nm.day] / cover.max) * cover.pred.vec[nm.day]  -
      d * f.sec  * cover.pred.vec[nm.day] #* (1 - cover.pred.vec[nm.day])
    
    cover.pred.vec[nm.day+1] <- max(0,min(cover.max,cover.pred.vec[nm.day+1]))
    
    if(choler == TRUE){
      # # M1
      # cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] +
      #   0.002 *  (water.lag[nm.day] -  water.lag[nm.day-1])
      #   # 0.015 * water.lag[nm.day] / (swc.capacity - swc.wilt)  -
      #   #   0.03 * cover.pred.vec[nm.day]
      cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] + 0.007 *  
        max(water.lag[nm.day],0) / water.lag[nm.day] * cover.pred.vec[nm.day]
      
      
      # # M2 modified
      # cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] +
      #   (0.02) * water.lag[nm.day] / (swc.capacity - swc.wilt) *
      #      (1 - cover.pred.vec[nm.day] / cover.max) -
      #   0.07 * cover.pred.vec[nm.day]
      # cover.pred.vec[nm.day+1] <- max(cover.pred.vec[nm.day+1],0.15)
      
      # cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] + 
      #   g.value * sf.value * f.growth * water.lag[nm.day] *
      #   # gcc.df$rad.norm[nm.day] * 
      #   (1 - cover.pred.vec[nm.day] / cover.max)  -
      #   d * f.sec * (1 - cover.pred.vec[nm.day]) * cover.pred.vec[nm.day]
    }
    
    # calculate swc
    swc.vec[nm.day] <- max(0,min(swc.capacity,swc.vec[nm.day]))
    
    swc.vec[nm.day+1] <- swc.vec[nm.day] + gcc.df$Rain[nm.day] - 
      (1 - cover.pred.vec[nm.day]) * 
      (water.avi[nm.day]/(swc.capacity - swc.wilt))^2 * et[nm.day] -
      e.plant[nm.day]
    
    swc.vec[nm.day+1] <- max(0,min(swc.capacity,swc.vec[nm.day+1]))
    
  }
  
  out.df <- data.frame(cover.pred.vec = cover.pred.vec,
                       swc.pred.vec = swc.vec)
  
  return(out.df)
}
