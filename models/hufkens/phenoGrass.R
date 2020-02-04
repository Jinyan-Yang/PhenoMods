# get a scaling factor####
scaling.f.func <- function(map,f.h){
  # map is input, h should be fitted
  map / (map + f.h)
}
# t response function####
t.func <- function(t.mean,f.t.opt,t.max){
  return((t.max-t.mean)/(t.max-f.t.opt)*(t.mean/f.t.opt)^(f.t.opt/(t.max-f.t.opt)))
}
# code to test t.func
# tmean.vec<- seq(0,40)
# g.vec <- t.func(tmean.vec,33,45)
# plot(g.vec~tmean.vec)

# build the pheno grass model as in Hufkens 2016#####
# the Penman equation is a different version 
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
  t.m <- c()
  
  # calcualte the par values
  cover.max <- max(gcc.df$cover,na.rm=TRUE)
  rad.min <-  min(gcc.df$PPFD,na.rm=TRUE)
  rad.max <-  max(gcc.df$PPFD,na.rm=TRUE)
  gcc.df$rad.norm <- (gcc.df$PPFD - rad.min) / (rad.max - rad.min)
  
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
    
    # give min
    cover.pred.vec[nm.day] <- max(0,min(cover.pred.vec[nm.day],
                                        cover.max))
    # get g value; t depedence
    g.value <- t.func(t.m[nm.day],f.t.opt,t.max)
    # # calculate plant cover
    
    cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] + 
      g.value * sf.value * f.growth * water.lag[nm.day] * (1 - cover.pred.vec[nm.day] / cover.max) -
      d * f.sec * (1 - cover.pred.vec[nm.day]) * cover.pred.vec[nm.day]
    
    # calculate swc
    swc.vec[nm.day] <- max(0,min(swc.capacity,swc.vec[nm.day]))
    
    swc.vec[nm.day+1] <- swc.vec[nm.day] + gcc.df$Rain[nm.day] -
      (1 - cover.pred.vec[nm.day]) *
      (water.avi[nm.day]/(swc.capacity - swc.wilt))^2 * et[nm.day] -
      g.value * f.extract * water.lag[nm.day] * cover.pred.vec[nm.day]
    
    
    swc.vec[nm.day+1] <- max(0,min(swc.capacity,swc.vec[nm.day+1]))
    
  }
  
  out.df <- data.frame(cover.pred.vec = cover.pred.vec,
                       swc.pred.vec = swc.vec)
  
  return(out.df)
}
# phenoGrass.func <- function(gcc.df,
#                             f.h,
#                             f.t.opt,
#                             f.extract,
#                             f.sec,
#                             f.growth,
#                             swc.wilt ,
#                             swc.capacity ,
#                             t.max,
#                             choler = F){
#   
#   # set the lag factor; in num of days
#   day.lay <- 16
#   sf.value <- scaling.f.func(mean(gcc.df$map,na.rm=TRUE),f.h)
#   
#   gcc.df$cover <-  gcc.df$GCC.norm * sf.value
#   # set up the inital conditions
#   swc.vec <- c()
#   swc.vec[1:16] <- gcc.df$vwc[1:16] * 1 * 1000
#   et <- c()
#   cover.pred.vec <- c()
#   cover.pred.vec[day.lay] <- gcc.df$cover[!is.na(gcc.df$cover)][1]
#   water.avi <- c()
#   water.avi <- swc.vec - swc.wilt
#   water.lag <- c()
#   water.lag <- water.avi
#   t.m <- c()
# 
#   # calcualte the par values
#   cover.max <- max(gcc.df$cover,na.rm=TRUE)
#   rad.min <-  min(gcc.df$PPFD,na.rm=TRUE)
#   rad.max <-  max(gcc.df$PPFD,na.rm=TRUE)
#   gcc.df$rad.norm <- (gcc.df$PPFD - rad.min) / (rad.max - rad.min)
#   
#   # model start
#   for (nm.day in day.lay:(nrow(gcc.df)-1)){
#     # plant avialbe water
#     water.avi[nm.day] <- max(0,swc.vec[nm.day]- swc.wilt)
#     
#     # # define the legency effect 
#     i=0
#     while(i+1<day.lay & (nm.day-i)>0){
#       i=i+1
#     }
#     
#     # water.lag[nm.day] <- min(water.avi[(nm.day-i):nm.day],na.rm=TRUE) #note this is different from the Hufkens
#     water.lag[nm.day] <- water.avi[nm.day-i]
#     t.m[nm.day] <- mean(gcc.df$Tair[(nm.day-i):nm.day],na.rm=TRUE) #hufkens used 15 days
#     # hufkens used evaportanspiration from Hargreaves 1985
#     # here is from evapotranspiration R package
#     et[nm.day] <- pet.func(gcc.df$Date[nm.day],gcc.df$PPFD[nm.day],
#                            gcc.df$Tair[nm.day],gcc.df$Tmax[nm.day], gcc.df$Tmin[nm.day],
#                            gcc.df$RHmax[nm.day],gcc.df$RHmin[nm.day], gcc.df$u2[nm.day])
#     
#     # get plant cover
#     # check whether water allow leaf drop
#     if(water.lag[nm.day] <  water.lag[nm.day-1]){
#       d = 1
#     }else{
#       d = 0
#     }
#     
#     # check radiation  for leaf drop
#     if(gcc.df$rad.norm[nm.day] <= gcc.df$rad.norm[nm.day-1]){
#       d=0
#     }else{
#       d=1
#     }
#     
#     # give min
#     cover.pred.vec[nm.day] <- max(0,min(cover.pred.vec[nm.day],
#                                         cover.max))
#     # get g value; t depedence
#     g.value <- t.func(t.m[nm.day],f.t.opt,t.max)
#     # # calculate plant cover
#     # cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] + 
#     #   g.value * sf.value * f.growth * water.lag[nm.day] *
#     #   # gcc.df$rad.norm[nm.day] * 
#     #   (1 - cover.pred.vec[nm.day] / cover.max)  -
#     #   d * f.sec * (1 - cover.pred.vec[nm.day]) * cover.pred.vec[nm.day]
#     
#     # normalised lagged water avilibility
#     water.lag.norm <- (water.lag[nm.day] - min(water.lag,na.rm=T)) / (max(water.lag,na.rm=T) - min(water.lag,na.rm=T))
#     water.avi.norm <- water.avi[nm.day]/(swc.capacity - swc.wilt)
#     
#     cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] + 
#       sf.value * f.growth * water.lag[nm.day] * (1 - cover.pred.vec[nm.day] / cover.max) -
#       d * f.sec * (1 - cover.pred.vec[nm.day]) * cover.pred.vec[nm.day]
#     
#     if(choler == TRUE){
#       # cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] + f.growth *  (water.lag[nm.day] -  water.lag[nm.day-1])
#       
#       # cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] *
#       #   (1 + f.growth * water.avi[nm.day] /
#       #      (swc.capacity - swc.wilt) *
#       #      (1 - cover.pred.vec[nm.day] / cover.max)) -
#       #   f.sec * cover.pred.vec[nm.day]
#       
#       cover.pred.vec[nm.day+1] <- cover.pred.vec[nm.day] +
#         g.value * sf.value * f.growth * water.lag[nm.day] *
#         gcc.df$rad.norm[nm.day] *
#         (1 - cover.pred.vec[nm.day] / cover.max)  -
#         d * f.sec * (1 - cover.pred.vec[nm.day]) * cover.pred.vec[nm.day]
#     }
#     
#     # calculate swc
#     swc.vec[nm.day] <- max(0,min(swc.capacity,swc.vec[nm.day]))
#     
#     swc.vec[nm.day+1] <- swc.vec[nm.day] + gcc.df$Rain[nm.day] -
#       (1 - cover.pred.vec[nm.day]) *
#       (water.avi[nm.day]/(swc.capacity - swc.wilt))^2 * et[nm.day] -
#       g.value * f.extract * water.lag[nm.day] * cover.pred.vec[nm.day]
#     
#     
#     
#     # swc.vec[nm.day + 1] <- swc.vec[nm.day] + gcc.df$Rain[nm.day] -
#     #   (1 - cover.pred.vec[nm.day]) *
#     #   (water.avi.norm)^2 * et[nm.day] -
#     #   g.value * f.extract * water.avi.norm * cover.pred.vec[nm.day] * water.avi[nm.day]
#     
#     swc.vec[nm.day+1] <- max(0,min(swc.capacity,swc.vec[nm.day+1]))
#     
#   }
#   
#   out.df <- data.frame(cover.pred.vec = cover.pred.vec,
#                        swc.pred.vec = swc.vec)
#   
#   return(out.df)
# }
# 
# # phenoGrass.func(gcc.met.df,264.807,30.046,0.707,0.05,0.002698,100,400,45)
# # 264.807,30.046,0.707,0.05,0.002698,
# 
# # f.h = 246
# # f.t.opt = 30
# # f.extract = 0.7
# # f.sec = 0.05
# # f.growth = 0.002
# # swc.wilt = 100
# # swc.capacity =400
# # t.max =45
# 
# # plot(cover.pred.vec)
