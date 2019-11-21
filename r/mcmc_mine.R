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
posterior.func <- function(prior.prob,bucket.size = 200,FUN,...){
  
  model.pred <- FUN(...,
                    bucket.size = bucket.size,
                    swc.wilt = 0.03 * bucket.size,
                    swc.capacity = 0.12 * bucket.size)
  
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
mh.MCMC.func <- function(iterations,par.df,gcc.met.pace.df.16){
  
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
    probab = exp(posterior.func(prior.prob,FUN = phenoGrass.func.v12,
                                gcc.df = gcc.met.pace.df.16,
                                f.h = 222,
                                f.t.opt = proposal[1],
                                f.extract = proposal[2],
                                f.sec = proposal[3],
                                f.growth = proposal[4] ,
                                t.max = 45,
                                day.lay = 1) - 
                   posterior.func(prior.prob,FUN = phenoGrass.func.v12,
                                  gcc.df = gcc.met.pace.df.16,
                                  f.h = 222,
                                  f.t.opt = chain[i,1],
                                  f.extract = chain[i,2],
                                  f.sec = chain[i,3],
                                  f.growth = chain[i,4] ,
                                  t.max = 45,
                                  day.lay = 1))
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

# read data####
gcc.met.pace.df <- readRDS('cache/gcc.met.pace.df.processed.rds')

temp.df <- gcc.met.pace.df[gcc.met.pace.df$Species == 'Luc'&
                             gcc.met.pace.df$Precipitation == 'Control'&
                             gcc.met.pace.df$Temperature == 'Ambient',]

# temp.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P6A',]
library(doBy)
sd.df <- summaryBy(GCC.norm + vwc ~ Date,
                   data = temp.df,FUN=c(sd),na.rm=TRUE,keep.names = F)

mean.df <- summaryBy(.~ Date,
                     data = temp.df,FUN=c(mean),na.rm=TRUE,keep.names = T)

test.df <- merge(mean.df,sd.df)

test.df <- test.df[order(test.df$Date),]

test.df$GCC.norm <- na.locf(test.df$GCC.norm,fromLast=F)

# set the number of lageed days
day.lag <- 1
test.df$GCC.norm[1:day.lag] <- NA
test.df$GCC[1:day.lag] <- NA

start.date <- test.df$Date[min(which(!is.na(test.df$GCC.norm)))]

gcc.met.pace.df.na.rm <- test.df[!is.na(test.df$GCC) & test.df$Date > start.date,]

gcc.met.pace.df.16 <- rbind(gcc.met.pace.df.na.rm,
                            test.df[test.df$Date <= start.date &
                                      test.df$Date > (start.date- (day.lag + 1)),])

gcc.met.pace.df.16 <- gcc.met.pace.df.16[order(gcc.met.pace.df.16$Date),]

gcc.met.pace.df.16 <- gcc.met.pace.df.16[!is.na(gcc.met.pace.df.16$Date),]

gcc.met.pace.df.16$GCC.norm <- (gcc.met.pace.df.16$GCC - 0.328) /
  (max(gcc.met.pace.df.16$GCC,na.rm = T) - 0.328)

# para values####
par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
  f.t.opt = c(17,20,23,NA,NA,NA),
  f.extract = c(0.05,0.075,0.1,NA,NA,NA),
  f.sec = c(0.05,0.1,0.15,NA,NA,NA),
  f.growth = c(0.1,0.2,0.3,NA,NA,NA))
row.names(par.df) <- c('min','initial','max','fit','stdv','prop')

# this assume 100% of the data falls into the max min range 
# in a normal distribution for proposal.func
par.df['stdv',] <- ((par.df['max',] - par.df['min',])/10)

# start mcmc fiting######
chain = mh.MCMC.func(20000,par.df,gcc.met.pace.df.16)

saveRDS(chain,'cache/chain.rds')
chain <- readRDS('cache/chain.rds')
# check acceptance so that the 
burnIn = 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),])) #should be >20% but <60%; 20-25% were suggested

# see how it works#####
par.df["fit",] <- colMeans(chain[burnIn:nrow(chain),])
bucket.size <- 200
hufken.pace.pred <- phenoGrass.func.v12(gcc.met.pace.df.16,
                                        f.h = 222,
                                        f.t.opt = par.df["fit",1],
                                        f.extract = par.df["fit",2],
                                        f.sec= par.df["fit",3],
                                        f.growth = par.df["fit",4],
                                        bucket.size = bucket.size,
                                        swc.wilt = 0.03 * bucket.size,
                                        swc.capacity = 0.12 * bucket.size,
                                        t.max = 45,
                                        day.lay = 1)
library(viridisLite)
palette(viridis(8))
par(mar=c(5,5,1,1))
plot(cover~Date,data = hufken.pace.pred,type='b',pch=16,
     xlab='2018',ylab=expression(f[cover]),ylim=c(0,0.8),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred,type='b',col=palette()[8],pch=16)

par(new=T)

plot(vwc~Date,data = hufken.pace.pred,ann=F,axes=F,type='s',col=palette()[3])

par(new=T)

plot(swc.hufken~Date,data = hufken.pace.pred,ann=F,axes=F,type='l',lty='dashed',col=palette()[4])

legend('bottomright',legend = c('MOD','OBS'),pch=16,col=palette()[c(8,6)],bty='n',title = 'Plant cover')

legend('bottom',legend = c('MOD','OBS'),lty=c('dashed','solid'),col=palette()[c(4,3)],bty='n',title = 'Soil moisture')

summary(lm(cover~cover.hufken,data = hufken.pace.pred))

# plot((cover~cover.hufken,data = hufken.pace.pred))
hist(chain[10000:20001,2])



# 
library(viridisLite)
palette(viridis(8))
par(mar=c(3,5,1,1))
par(bg = rgb(240/255,241/255,211/255))
plot(cover~Date,data = hufken.pace.pred,type='b',pch=16,
     xlab=' ',ylab=expression('Plant cover'),ylim=c(0,0.8),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred,type='l',col=palette()[1],lwd=2)

legend('bottomright',legend = c('MOD','OBS'),lty='solid',col=palette()[c(1,6)],bty='n')

