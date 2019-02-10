# get a scaling factor
scaling.f.func <- function(map,f.h){
  # map is input, h should be fitted
  map / (map + f.h)
}
# t response function
t.func <- function(t.mean,f.t.opt,t.max){
  return((t.max-t.mean)/(t.max-f.t.opt)*(t.mean/f.t.opt)^(f.t.opt/(t.max-f.t.opt)))
}
# tmean.vec<- seq(0,40)
# g.vec <- t.func(tmean.vec,33,45)
# plot(g.vec~tmean.vec)

# build the pheno grass model as in Hufkens 2016
# the Penman equation is a different version though
# here used the equation modified from the evapotranspiration R package
phenoGrass.func <- function(gcc.df,
                            f.h,
                            f.t.opt,
                            f.extract,
                            f.sec,
                            f.growth,
                            swc.wilt ,
                            swc.capacity ,
                            t.max){
  day.lay <- 16
  sf.value <- scaling.f.func(mean(gcc.df$map),f.h)
  gcc.df$cover <- sf.value * gcc.df$GCC.norm 
  # set up the inital conditions
  swc.vec <- c()
  swc.vec[day.lay] <- 200
  et <- c()
  # swc.vec[1] <- swc.inital
  cover.pred.vec <- c()
  cover.pred.vec[day.lay] <- gcc.df$cover[!is.na(gcc.df$cover)][1]
  water.avi <- c()
  # water.avi[1] <- swc.inital - swc.wilt
  water.lag <- c()
  water.lag[day.lay -1] <- water.avi.inital <- 50 #need to be calculated in the future
  t.m <- c()

  # calcualte the par values
  cover.max <- max(gcc.df$cover,na.rm=TRUE)
  rad.min <-  min(gcc.df$PPFD,na.rm=TRUE)
  rad.max <-  max(gcc.df$PPFD,na.rm=TRUE)
  gcc.df$rad.norm <- (gcc.df$PPFD - rad.min) / (rad.max - rad.min)
  
  for (nm.day in day.lay:(nrow(gcc.df)-1)){
    # plant avialbe water
    water.avi[nm.day] <- max(0,swc.vec[nm.day]- swc.wilt)
    
    i=0
    while(i<day.lay & (nm.day-i)>0){
      i=i+1
    }
    
    water.lag[nm.day] <- min(water.avi[(nm.day-i):nm.day],na.rm=TRUE)
    t.m[nm.day] <- mean(gcc.df$Tair[(nm.day-i):nm.day],na.rm=TRUE)
    # evaportanspiration from Hargreaves 1985
    # Date,PPFD,Tair,Tmax,Tmin,RHmax,RHmin,u2
    et[nm.day] <- pet.func(gcc.df$Date[nm.day],gcc.df$PPFD[nm.day],
                           gcc.df$Tair[nm.day],gcc.df$Tmax[nm.day], gcc.df$Tmin[nm.day],
                           gcc.df$RHmax[nm.day],gcc.df$RHmin[nm.day], gcc.df$u2[nm.day])
    
    # get plant cover
    # check whether water allow leaf drop
    if(water.lag[nm.day] < water.lag[nm.day-1]){
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
    
    cover.pred.vec[nm.day] <- max(0,min(cover.pred.vec[nm.day],
                                        cover.max))
    g.value <- t.func(t.m[nm.day],f.t.opt,t.max)
 
    cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] + 
      g.value * sf.value * f.growth * water.lag[nm.day] *
      gcc.df$rad.norm[nm.day] * 
      (1 - cover.pred.vec[nm.day] / cover.max)  -
      d * f.sec * (1 - cover.pred.vec[nm.day]) * cover.pred.vec[nm.day]
    
    
    # calculate swc
    swc.vec[nm.day] <- max(0,min(swc.capacity,swc.vec[nm.day]))
    
    swc.vec[nm.day+1] <- swc.vec[nm.day] + gcc.df$Rain[nm.day] - 
      (1 - cover.pred.vec[nm.day]) * 
      (water.avi[nm.day]/(swc.capacity - swc.wilt))^2 * et[nm.day] -
      g.value * f.extract * water.lag[nm.day] * 
      cover.pred.vec[nm.day]
    
    swc.vec[nm.day+1] <- max(0,min(swc.capacity,swc.vec[nm.day+1]))
    
  }
  
  return(cover.pred.vec)
}

# # gcc.df,
# # f.h,f.t.opt,f.extract,f.sec,
# # swc.wilt ,
# # swc.capacity ,
# # t.max

# phenoGrass.func(gcc.df,200,25,0.5,0.05,.001,100,400,45)

