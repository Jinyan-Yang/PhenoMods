library(Evapotranspiration)
library(doBy)
library(zoo)
data("constants") #this is used for penman et value; data from Adleide

pet.func <- function(Date,PPFD,Tair,Tmax,Tmin,RHmax,RHmin,u2,P = 101.3,lat = 33.618891 ){
  gcc.met.df <- data.frame(Date = as.Date(Date),
                           PPFD = PPFD,
                           Tair = Tair,
                           Tmax = Tmax,
                           Tmin = Tmin,
                           RHmax = RHmax,
                           RHmin = RHmin,
                           u2=u2)
  
  
  gcc.met.df$J <- lubridate::yday(gcc.met.df$Date)
  gcc.met.df$n <-  gcc.met.df$PPFD * 10^-6/4.57 * 3600 *24
  R_s <-  gcc.met.df$PPFD * 10^-6/4.57 * 3600 *24
  # data <- gcc.met.df
  
  Ta <- gcc.met.df$Tair
  P <- 101.3 
  delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta + 237.3)^2)
  gamma <- 0.00163 * P/constants$lambda
  d_r2 <- 1 + 0.033 * cos(2 * pi/365 * gcc.met.df$J)
  delta2 <- 0.409 * sin(2 * pi/365 * gcc.met.df$J - 1.39)
  
  w_s <- acos(tan(lat * pi /180) * tan(delta2))
  N <- 24/pi * w_s
  R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) * 
                                               sin(delta2) + cos(constants$lat_rad) * cos(delta2) * 
                                               sin(w_s))
  R_so <- (0.75 + (2 * 10^-5) * constants$Elev) * R_a
  
  # R_s <- (constants$as + constants$bs * (gcc.met.df$n/N)) * R_a
  
  vs_Tmax <- 0.6108 * exp(17.27 * gcc.met.df$Tmax/(gcc.met.df$Tmax + 
                                                     237.3))
  vs_Tmin <- 0.6108 * exp(17.27 * gcc.met.df$Tmin/(gcc.met.df$Tmin + 
                                                     237.3))
  vas <- (vs_Tmax + vs_Tmin)/2
  vabar <- (vs_Tmin * gcc.met.df$RHmax/100 + vs_Tmax * gcc.met.df$RHmin/100)/2
  
  R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) * 
    ((gcc.met.df$Tmax + 273.2)^4 + (gcc.met.df$Tmin + 273.2)^4)/2 * 
    (1.35 * R_s/R_so - 0.35)
  
  alpha <- 0.67
  R_ns <- (1 - alpha) * R_s
  R_n = R_ns - R_nl
  # if (windfunction_ver == "1948") {
  f_u = 2.626 + 1.381 * gcc.met.df$u2
  #   # else if (windfunction_ver == "1956") {
  #     f_u = 1.313 + 1.381 * u2
  
  Ea = f_u * (vas - vabar)
  Epenman.Daily <- delta/(delta + gamma) * (R_n/constants$lambda) + 
    gamma/(delta + gamma) * Ea
  Epenman.Daily <- max(0,Epenman.Daily)
  return(Epenman.Daily)
}
# pet.func('2017-01-01',PPFD=800,25,30,20,80,20,2)

# get a scaling factor####
scaling.f.func <- function(map,f.h){
  # map is input, h should be fitted
  map / (map + f.h)
}
# t response function####
t.func <- function(t.mean,f.t.opt,t.max){
  return((t.max-t.mean)/(t.max-f.t.opt)*(t.mean/f.t.opt)^(f.t.opt/(t.max-f.t.opt)))
}

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
  swc.vec <- c()
  swc.vec[1:day.lay] <- gcc.df$vwc[1:day.lay] * bucket.size
  et <- c()
  cover.pred.vec <- c()
  cover.pred.vec[day.lay] <- gcc.df$cover[!is.na(gcc.df$cover)][1]
  water.avi <- c()
  water.avi <- swc.vec - swc.wilt
  water.lag <- c()
  water.lag <- water.avi
  t.m <- growth.vec <- senescence.vec <- evap.vec <- transp.vec <- c()
  
  # check whether it rained in the past three days
  rained.vec <- c()
  rained.vec[1:day.lay] <- 0
  for(i in (day.lay + 1):nrow(gcc.df)){
    if(sum(gcc.df$Rain[(i-day.lay):i],na.rm=T)>0){
      rained.vec[i] <- 1
    }else{
      rained.vec[i] <- 0
    }
  }
  
  # calcualte the par values
  cover.max <- max(gcc.df$cover,na.rm=TRUE)
  rad.min <-  min(gcc.df$PPFD,na.rm=TRUE)
  rad.max <-  max(gcc.df$PPFD,na.rm=TRUE)
  gcc.df$rad.norm <- (gcc.df$PPFD - rad.min) / (rad.max - rad.min)
  
  # model start
  for (nm.day in (day.lay+1):nrow(gcc.df)){
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
    
    #  # # get plant cover
    # if(water.lag[nm.day] > water.lag[nm.day-1]){
    #   g = 1
    #   d = 0
    # }else{
    #   g = 0
    #   d = 1
    # }
    
    if(rained.vec[nm.day] == 1){
      g = 1
      d = 0
    }else{
      g = 0
      d = 1
    }
    
    # # calculate plant cover
    water.lag.norm <- (water.lag[nm.day]) / (swc.capacity - swc.wilt)
    water.avi.norm <- water.avi[nm.day]/(swc.capacity - swc.wilt)
    # plant cover
    g.value <- t.func(t.m[nm.day],f.t.opt,t.max)
    growth.vec[nm.day] <- g * g.value * f.growth * water.avi.norm * (1 - cover.pred.vec[nm.day-1] / cover.max)
    senescence.vec[nm.day] <- d * f.sec * (1 - water.avi.norm) #* cover.pred.vec[nm.day-1] / cover.max
    
    cover.pred.vec[nm.day] <- cover.pred.vec[nm.day-1] + growth.vec[nm.day] - senescence.vec[nm.day]                 
    # give min
    cover.pred.vec[nm.day] <- max(0,min(cover.pred.vec[nm.day],cover.max))
    
    # calculate swc
    evap.vec[nm.day] <- (1 - cover.pred.vec[nm.day-1]) * (water.avi.norm)^2 * et[nm.day]
    transp.vec[nm.day] <- f.extract * water.avi.norm * cover.pred.vec[nm.day] / cover.max
    
    swc.vec[nm.day] <- swc.vec[nm.day-1] + gcc.df$Rain[nm.day] - evap.vec[nm.day] - transp.vec[nm.day]
    
    swc.vec[nm.day] <- max(0,min(swc.capacity,swc.vec[nm.day]))
    
  }
  
  gcc.df$cover.hufken <- cover.pred.vec
  gcc.df$swc.hufken <- swc.vec
  
  out.df <- data.frame(gcc.df)
  out.df <- out.df[!is.na(out.df$cover),]
  
  return(gcc.df)
}
