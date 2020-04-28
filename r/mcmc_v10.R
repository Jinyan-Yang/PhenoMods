source('models/hufkens/pG_v10.R')
day.lag <- 3
source('r/pace_data_process.R')
# 
gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
                                    species.in = 'Luc',
                                    prep.in = 'Control',
                                    temp.in ='Ambient')
gcc.met.pace.df.16 <- gcc.met.pace.df.16[gcc.met.pace.df.16$Date<as.Date('2019-11-26'),]
gcc.met.pace.df.16$map <- 760
# functions#####
# function to get log likelihood
logLikelihood.func <- function (model.out){
  
  obs <- model.out$cover
  mod <- model.out$cover.hufken
  obs.sd <- model.out$GCC.norm.sd * 0.8
  
  logLi <-  - (0.5 * ((mod - obs)/obs.sd)^2 + log(obs.sd) + 0.5*log(2*pi))
  
  # return(sum(logLi,na.rm=TRUE))
  return(sum(logLi,na.rm=T))
}

# The code below modifed from 
# https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/

# prior probability
prior.func <- function(par.df){
  aprior <- c()
  for(i in 1:ncol(par.df)){
    aprior[i] = dunif(par.df["initial",i], min=par.df["min",i], max=par.df["max",i], log = T)
  }
  return(sum(aprior))
}

# posterios probability
posterior.func <- function(prior.prob,bucket.size,FUN,...){
  
  model.pred <- FUN(...,
                    bucket.size = bucket.size,
                    swc.wilt = 0.05 * bucket.size,
                    swc.capacity = 0.13 * bucket.size)
  
  return (logLikelihood.func(model.pred) + prior.prob)
}

######## Metropolis algorithm ################

# function to generate a radom par value from a normal distribution based on mean and sd
proposal.func <- function(param,par.df){
  
  prop.vec <- c()
  
  for(i in 1:ncol(par.df)){
    prop.vec[i] <- rnorm(1,mean = as.numeric(param[i]), sd= par.df['stdv',i])
  }
  return(abs(prop.vec))
}

# prior.prob,data,data.sd,model.form,pars.ls
mh.MCMC.func <- function(iterations,par.df,gcc.met.pace.df.16,bucket.size = 300){
  
  # get prior 
  prior.prob <- prior.func(par.df)
  
  # start MC ####
  # intial
  chain = array(dim = c(iterations+1,ncol(par.df)))
  chain[1,] = as.numeric(par.df['initial',])
  
  # chain move on
  for (i in 1:iterations){
    proposal = proposal.func(chain[i,],par.df)
    
    # prior.prob,data,data.sd,bucket.size = 300,...
    probab = exp(posterior.func(prior.prob,FUN = phenoGrass.func.v10,
                                gcc.df = gcc.met.pace.df.16,
                                f.h = 222,
                                f.t.opt = proposal[1],
                                f.extract = proposal[2],
                                f.sec = proposal[3],
                                f.growth = proposal[4] ,
                                t.max = 45,
                                bucket.size = bucket.size) - 
                   posterior.func(prior.prob,FUN = phenoGrass.func.v10,
                                  gcc.df = gcc.met.pace.df.16,
                                  f.h = 222,
                                  f.t.opt = chain[i,1],
                                  f.extract = chain[i,2],
                                  f.sec = chain[i,3],
                                  f.growth = chain[i,4] ,
                                  t.max = 45,
                                  bucket.size = bucket.size))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

# packages
library(doBy)
library(zoo)

# para values####
par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
  f.t.opt = c(17,20,23,NA,NA,NA),
  f.extract = c(0.3,0.5,0.7,NA,NA,NA),
  f.sec = c(0.02,0.075,0.1,NA,NA,NA),
  f.growth = c(0.001,0.002,0.003,NA,NA,NA))
row.names(par.df) <- c('min','initial','max','fit','stdv','prop')

# this assume 100% of the data falls into the max min range 
# in a normal distribution for proposal.func
par.df['stdv',] <- ((par.df['max',] - par.df['min',])/10)

gcc.met.pace.df.16 <- gcc.met.pace.df.16[(gcc.met.pace.df.16$Date) < as.Date('2018-9-1'),]
# start mcmc fiting######
v10chain = mh.MCMC.func(10000,par.df,gcc.met.pace.df.16, bucket.size = 300)

# 
hist(v10chain[0:1000,4])
hist(v10chain[5000:10000,1])

saveRDS(v10chain,'cache/v10chain.rds')
v10chain <- readRDS('cache/v10chain.rds')
# check acceptance so that the 
burnIn = 1000
acceptance = 1-mean(duplicated(v10chain[-(1:burnIn),])) #should be >20% but <60%; 20-25% were suggested

# see how it works#####
par.df["fit",] <- colMeans(v10chain[burnIn:nrow(v10chain),])
bucket.size <- 300
hufken.pace.pred <- phenoGrass.func.v10(gcc.met.pace.df.16,
                                        f.h = 222,
                                        f.t.opt = par.df["fit",1],
                                        f.extract = par.df["fit",2],
                                        f.sec= par.df["fit",3],
                                        f.growth = par.df["fit",4],
                                        bucket.size = bucket.size,
                                        swc.wilt = 0.03 * bucket.size,
                                        swc.capacity = 0.13 * bucket.size,
                                        t.max = 45)


library(viridisLite)
palette(viridis(8))
par(mar=c(5,5,1,1))
plot( cover~Date,data = hufken.pace.pred,type='b',pch=16,
     xlab='2018',ylab=expression(f[cover]),ylim=c(0,0.8),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred,type='b',col=palette()[8],pch=16)

# par(new=T)
# 
# plot(vwc~Date,data = hufken.pace.pred,ann=F,axes=F,type='s',col=palette()[3])
# 
# par(new=T)
# 
# plot(swc.hufken~Date,data = hufken.pace.pred,ann=F,axes=F,type='l',lty='dashed',col=palette()[4])

legend('bottomright',legend = c('MOD','OBS'),pch=16,col=palette()[c(8,6)],bty='n',title = 'Plant cover')

# legend('bottom',legend = c('MOD','OBS'),lty=c('dashed','solid'),col=palette()[c(4,3)],bty='n',title = 'Soil moisture')

summary(lm(cover~cover.hufken,data = hufken.pace.pred))


# 
# use original hufkens with pace data. ####
hufken.pace.pred.ori <- phenoGrass.func.v10(gcc.met.pace.df.16,
                                            f.h = 222,
                                            f.t.opt =33,
                                            f.extract = 0.519,
                                            f.sec= 0.0755,
                                            f.growth = 0.00227,
                                            bucket.size = 1000,
                                            swc.wilt = 0.1245 * 1000,
                                            swc.capacity = 0.4 * 1000,
                                            t.max = 45,
                                            day.lay = 3
                                            )


library(viridisLite)
palette(viridis(8))
par(mar=c(5,5,1,1))
plot( cover~Date,data = hufken.pace.pred.ori,type='b',pch=16,
      xlab='2018',ylab=expression(f[cover]),ylim=c(0,0.8),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred.ori,type='b',col=palette()[8],pch=16)
legend('bottomright',legend = c('MOD','OBS'),pch=16,col=palette()[c(8,6)],bty='n',title = 'Plant cover')

# use original hufkens to pace data with altered wilting point and capacity. 
hufken.pace.pred.ori.1 <- phenoGrass.func.v10(gcc.met.pace.df.16,
                                            f.h = 222,
                                            f.t.opt =33,
                                            f.extract = 0.519,
                                            f.sec= 0.0755,
                                            f.growth = 0.00227,
                                            bucket.size = 300,
                                            swc.wilt = 0.03 * 300,
                                            swc.capacity = 0.25 * 300,
                                            t.max = 45,
                                            day.lay = day.lag
)


library(viridisLite)
palette(viridis(8))
par(mar=c(5,5,1,1))
plot( cover~Date,data = hufken.pace.pred.ori.1,type='b',pch=16,
      xlab='2018',ylab=expression(f[cover]),ylim=c(0,0.8),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred.ori.1,type='b',col=palette()[8],pch=16)

legend('bottomright',legend = c('MOD','OBS'),pch=16,col=palette()[c(8,6)],bty='n',title = 'Plant cover')

