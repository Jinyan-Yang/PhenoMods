# # order of pars
# # f.h=200
# # f.t.opt=30
# # f.extract <- 0.1
# # f.sec <- 0.005
# # f.growth <- 0.002
# lower <- c(200, 10, 0.1,  0.001, 0.1) 
# upper <- c(200, 10, 0.9,  0.05,  0.4)

# install.packages("mvtnorm")
library("mvtnorm") # Creates candidate parameter vector as a multivariate normal jump away from the current candidate

par.df <- data.frame(f.h = c(200,220,240,NA,NA),
                     f.t.opt = c(19,20,21,NA,NA),
                     f.extract = c(0.05,0.175,0.3,NA,NA),
                     f.sec = c(0.05,0.175,0.3,NA,NA),
                     f.growth = c(0.1,0.3,0.5,NA,NA))
row.names(par.df) <- c('min','initial','max','fit','stdv')
# 
logLikelihood <- function (data,obs.sd,output){
  if(length(data) != length(output)) stop('data and output need to be of same length')
  # logLi <- c() # Initialising the logLi
  # for (i in 1:length(data)) {
  #   
  # logLi[i] = - 0.5*((output$Mleaf[i] - data$Mleaf[i])/data$Mleaf_SD[i])^2 - log(data$Mleaf_SD[i]) - log(2*pi)^0.5
  # }
  logLi = - 0.5*((output - data)/obs.sd)^2 - log(obs.sd) - log(2*pi)^0.5
  return(sum(logLi,na.rm=TRUE))
}
# 
pg.mcmc.func <- function(dat,
                         pars,
                         swc.wilt = 70,
                         swc.capacity = 150,
                         t.max = 35){
  pars <- unname(as.numeric(pars))
  f.h = pars[1]
  f.t.opt=pars[2]
  f.extract <- pars[3]
  f.sec <- pars[4]
  f.growth <- pars[5]
  
  sf.value <- scaling.f.func(mean(dat$map,na.rm=TRUE),f.h)
  # 
  dat$cover <- sf.value * dat$GCC.norm
  
  pred.df <- phenoGrass.func(dat,f.h,f.t.opt,f.extract,f.sec,f.growth,
                             swc.wilt = swc.wilt,
                             swc.capacity = swc.capacity,
                             t.max = t.max)
  
  dat$cover.pred <- pred.df$cover.pred.vec
  dat$swc.pred <- pred.df$swc.pred.vec
  
  return(dat)
}
pars <- unname(as.numeric(par.df['initial',]))
dat = gcc.met.pace.df.16
# dat$Species <- na.locf(dat$Species)
# dat$Species <- as.character(dat$Species)
# dat$GCC.norm.sd <- sd(dat$GCC.norm,na.rm=T)
# dat$vwc.sd <- sd(dat$vwc,na.rm=T)
# pg.mcmc.func(gcc.met.pace.df.16,par.df['initial',],70,150,45)

# Assign inputs for MCMC
chainLength = 3500 # Setting the length of the Markov Chain to be generated
bunr_in = chainLength * 0.1 # Discard the first 10% iterations for Burn-IN in MCMC (According to Oijen, 2008)
no.var = ncol(par.df) # variables to be modelled are: k,Y,af,as,sf

# Assign pot volumes and number of parameters per varible in temporal scale
# vol = c(5) # test run
# no.treat.par.var = c(1,2,3) # test run
# GPP.dat.raw = read.csv("rawdata/GPP.csv") # Units gC d-1
vol = 1#unique(dat$Species)[order(unique(dat$Species))] # Assign all treatment pot volumes
# no.treat.par.var = c(1,2,3,4,5,6,9) # temporal parameter count per variable

# par.mean = data.frame(matrix(ncol = no.var, nrow = length(no.treat.par.var)*length(vol)))
# names(param.mean) = c("k","Y","af","as","ar","sf")
# aic.bic = data.frame(matrix(ncol = 4, nrow = no.var))
time = data.frame(no.treat=colnames(par.df),
                  start.time=numeric(no.var*length(vol)),
                  end.time=numeric(no.var*length(vol)),
                  time.taken=numeric(no.var*length(vol)))

# q = 0 # Indicates the iteration number
# 
# # Initialize few output dat files
# q = q + 1
# time$start.time[q] <- Sys.time()
# param.vary = nrow(dat) # How many days the parameter set remain unchanged (weekly = 7; monthly = 30; just one parameter = nrow(dat))
no.treat = 1# ceiling(nrow(dat)/param.vary) # number of parameter set for the whole duration of experiment (121 days)

# 
pMinima <- par.df['min',]
pMaxima <- par.df['max',]
pInitial <- par.df['initial',] # Starting point of the chain
pChain <- matrix(0, nrow=chainLength, ncol = no.treat*no.var+1) # Initialising the chain

# Defining the variance-covariance matrix for proposal generation
vcov = (0.01*(pMaxima-pMinima))^2
vcovProposal =  vcov[1,] # The higher the coefficient, the higher the deviations in parameter time series

# Find the Prior probability density
prior.dist = vector("list", no.var)
for (i in 1:no.var) {
  prior.dist[i] = list(log(dunif(pInitial[ , i], pMinima[ , i], pMaxima[ , i])))
}
logPrior0 <- sum(unlist(prior.dist))

# Calculating model outputs for the starting point of the chain
output =  pg.mcmc.func(dat,pInitial,70,150,45)
logL0 <- logLikelihood(output$cover,output$GCC.norm.sd,output$cover.pred) # Calculate log likelihood of starting point of the chain
pChain[1,] <- c(as.numeric(pInitial),logL0) # Assign the first parameter set with log likelihood

# Calculating the next candidate parameter vector, as a multivariate normal jump away from the current point
for (c.num in (2 : chainLength)) {
  candidatepValues = matrix(ncol = no.var, nrow = no.treat)
  for (i in 1:no.var) {
    candidatepValues[ , i] = rmvnorm(n=1, mean=pInitial[ , i],
                                     sigma=diag(vcovProposal[i],no.treat))
    
  }
  candidatepValues = data.frame(candidatepValues)
  names(candidatepValues) <- names(par.df)
  
  
  # Reflected back to generate another candidate value
  reflectionFromMin = pmin(0, as.numeric(candidatepValues-pMinima))
  reflectionFromMax = pmax(0, as.numeric(candidatepValues-pMaxima))
  candidatepValues = candidatepValues - 2 * reflectionFromMin - 2 * reflectionFromMax 

  # Calculating the prior probability density for the candidate parameter vector
  if (all(candidatepValues>pMinima) && all(candidatepValues<pMaxima)){
    uni.dist = vector("list", no.var)
    for (i in 1:no.var) {
      uni.dist[i] = list(log(dunif(candidatepValues[ , i], pMinima[ , i], pMaxima[ , i])))
    }
    logPrior1 <- sum(unlist(uni.dist))
    Prior1 = 1
  } else {
    Prior1 <- 0
  }
  
  # Calculating the outputs for the candidate parameter vector and then log likelihood
  if (Prior1 == 1){
    out.cand = pg.mcmc.func(dat,candidatepValues,70,150,45)
    logL1 <- logLikelihood(out.cand$cover,out.cand$GCC.norm.sd,out.cand$cover.pred) # Calculate log likelihood
    
    
    # Calculating the logarithm of the Metropolis ratio
    logalpha <- (logPrior1+logL1) - (logPrior0+logL0) 
    # Accepting or rejecting the candidate vector
    if (log(runif(1, min = 0, max =1)) < logalpha) {
      # if ( log(runif(1, min = 0, max =1)) < logalpha ) { 
      pInitial <- candidatepValues
      logPrior0 <- logPrior1
      logL0 <- logL1
    }
  }
  pChain[c.num,] <- c(as.numeric(pInitial),logL0)
}
# Discard the first 500 iterations for Burn-IN in MCMC
pChain <- pChain[(bunr_in + 1):nrow(pChain),]

# Store the final parameter set values
par.df['fit',] = colMeans(pChain[ , 1:(no.treat*no.var)])
par.df['stdv',] = apply(pChain[ , 1:(no.treat*no.var)], 2, sd)

# Calculate final output set from the predicted parameter set
output.final = pg.mcmc.func(dat,par.df['fit',],70,150,45)

plot(cover~Date,data = output.final)
points(cover.pred~Date,data = output.final,type='l')
par(new=T)
plot(vwc~Date,data = output.final,type='l',col='blue',ann=F,axes=F,ylim=c(0.03,0.13))
axis(4,at=seq(0,0.12,by=0.03),labels = seq(0,0.12,by=0.03))
