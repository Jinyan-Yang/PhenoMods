rm(list=ls())

# Implement Hufkens model
# state variables & initial condition
swc.vec <- c()      # SWC in mm
swc.vec[1] <- 50
cover.vec <- c()    # Fractional cover
cover.vec[1] <- 0

# drivers
days.vec <- 1:100   # number of days of simulation
ppt <- rep(0,length(days.vec))    # Precipitation
ppt[7] <- 150                     # 150 mm on 7th day
et <- rep(1,length(days.vec))     # Is this PET? 1 mm d-1

# parameters
f.extract <- 0.07                  # Water extract fraction; 
f.sec <- 0.05                     # Senesencence rate; fraction of cover d-1
f.growth <- 0.002                 # growth rate; fraction of cover d-1 mm-1
lag.days <- 5                     # the number of days takes for plant to respond to rainfall

# Intermediate variables? 
water.avi <- c()                  # Available water? (mm)
water.avi[1] <- 0
water.lag <- c()                  # Lagged available water (mm)
water.lag[1:lag.days] <- 0

# more intermediate variables
growth.vec <- loss.vec <- evap.vec <- transp.vec <- c()
growth.vec[1] <- loss.vec[1] <- evap.vec[1] <- transp.vec[1] <- 0

# more parameter values
cover.max <- 0.9                  # Maximum cover fraction
swc.capacity  <- 100              # Soil water capacity
swc.wilt <- 50                    # Wilting point


# loop over the number of days - 1
for(nm.day in 2:length(days.vec)){
  
  water.avi[nm.day] <- max(0,swc.vec[nm.day-1]- swc.wilt)    # Available water = SWC - wilting point
  
  # change the lagged water avilibility after the initial days
  if(nm.day>lag.days){
    water.lag[nm.day] <- water.avi[nm.day-lag.days]
  }
  
  # check if senesence happen - occurs if lagged soil water content is not increasing
  if (water.lag[nm.day] <= water.lag[nm.day-1]) {
    d = 1
  } else {
    d = 0
  }
  
  # plant fractional cover
  growth.vec[nm.day] <- f.growth * water.lag[nm.day-1] * (1 - cover.vec[nm.day-1] / cover.max)
  loss.vec[nm.day] <- d * f.sec * (1 - cover.vec[nm.day-1]) * cover.vec[nm.day-1]
  cover.vec[nm.day] <- cover.vec[nm.day-1] + growth.vec[nm.day]  -  loss.vec[nm.day]                 
  
  cover.vec[nm.day] <- max(0,min(cover.max,cover.vec[nm.day]))
  
  # swc
  evap.vec[nm.day] <- (1 - cover.vec[nm.day-1]) * (water.avi[nm.day-1]/(swc.capacity - swc.wilt))^2 * et[nm.day]
  transp.vec[nm.day] <- f.extract * water.lag[nm.day-1] * cover.vec[nm.day-1]
  swc.vec[nm.day] <- swc.vec[nm.day-1] + ppt[nm.day-1] - 
    evap.vec[nm.day] - transp.vec[nm.day]
  
  swc.vec[nm.day] <- max(swc.wilt,min(swc.capacity,swc.vec[nm.day]))
}

# Make plots
plot(cover.vec,type="s")
plot(growth.vec,type="l",col="red")
plot(loss.vec,type="l",col="blue")

plot(swc.vec,type="s")
plot(evap.vec,type="s",col="red")
plot(transp.vec,type="l",col="blue")

