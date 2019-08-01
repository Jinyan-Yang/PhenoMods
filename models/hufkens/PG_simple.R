# state var & initial condition
swc.vec <- c()
swc.vec[1] <- 50
cover.vec <- c()
cover.vec[1] <- 0

# drivers
days.vec <- 1:100 #number of days of simulation
ppt <- rep(0,length(days.vec))
ppt[7] <- 150
et <- rep(1,length(days.vec))

# vars
f.extract <- 0.7 #water extract fraction
f.sec <- 0.05 # senesence rate; fraction of cover d-1
f.growth <- 0.002 # growth rate; fraction of cover d-1 mm-1
lag.days <- 5 #the number of days takes for plant to respond to rainfall

# other var
water.avi <- c()
water.avi[1] <- 0
water.lag <- c()
water.lag[1:lag.days] <- 0

# fix par values
cover.max <- 0.9 
swc.capacity  <- 200
swc.wilt <- 50


# loop over the number of days - 1
for(nm.day in 2:length(days.vec)){
  
  water.avi[nm.day] <- max(0,swc.vec[nm.day-1]- swc.wilt)
  
  # change the lagged water avilibility after the initial days
  if(nm.day>lag.days){
    water.lag[nm.day] <- water.avi[nm.day-lag.days]
  }
  
  # check if senesence happen 
  if(water.lag[nm.day] <= water.lag[nm.day-1]){
    d = 1
  }else{
    d = 0
  }
  
  # plant
  cover.vec[nm.day] <- cover.vec[nm.day-1] + 
    f.growth * water.lag[nm.day-1] * (1 - cover.vec[nm.day-1] / cover.max)  -
    d * f.sec * (1 - cover.vec[nm.day-1]) * cover.vec[nm.day-1]
  
  cover.vec[nm.day] <- max(0,min(cover.max,cover.vec[nm.day]))
  
  # swc
  swc.vec[nm.day] <- swc.vec[nm.day-1] + ppt[nm.day-1] - 
    (1 - cover.vec[nm.day-1]) * 
    (water.avi[nm.day-1]/(swc.capacity - swc.wilt))^2 * et[nm.day] -
    f.extract * water.lag[nm.day-1] * cover.vec[nm.day-1]
  
  swc.vec[nm.day] <- max(swc.wilt,min(swc.capacity,swc.vec[nm.day]))
}
