source('r/function_hydro.R')
source('models/hufkens/pG_v10.R')
source('models/hufkens/hufkensV12.R')
source('models/hufkens/hufkensV11.R')
# functions#####
# function to get log likelihood
logLikelihood.func <- function (model.out){
  
  obs <- model.out$cover
  mod <- model.out$cover.hufken
  obs.sd <- model.out$GCC.norm.sd #* 0.8
  
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
posterior.func <- function(prior.prob,bucket.size,swc.wilt,swc.capacity,FUN,...){
  
  model.pred <- FUN(...,
                    bucket.size = bucket.size,
                    swc.wilt = swc.wilt,
                    swc.capacity = swc.capacity)
  
  return (logLikelihood.func(model.pred) + prior.prob)
}

######## Metropolis algorithm ################
# function to generate a radom par value from a normal distribution based on mean and sd
proposal.func <- function(param,par.df){
  
  prop.vec <- c()
  
  for(i in 1:ncol(par.df)){
    prop.vec[i] <- rnorm(1,mean = as.numeric(param[i]), sd = par.df['stdv',i])
  }
  return(abs(prop.vec))
}

# prior.prob,data,data.sd,model.form,pars.ls
mh.MCMC.func <- function(iterations,par.df,
                         gcc.met.pace.df.16,
                         bucket.size,swc.wilt ,
                         swc.capacity,day.lay,
                         my.fun,
                         use.smooth){
  
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
    probab = exp(posterior.func(prior.prob,FUN = my.fun,
                                gcc.df = gcc.met.pace.df.16,
                                f.h = 222,
                                f.t.opt = proposal[1],
                                f.extract = proposal[2],
                                f.sec = proposal[3],
                                f.growth = proposal[4] ,
                                t.max = 45,
                                day.lay = day.lay,
                                bucket.size = bucket.size,
                                swc.wilt = swc.wilt ,
                                swc.capacity = swc.capacity,
                                use.smooth = use.smooth) - 
                   posterior.func(prior.prob,FUN = my.fun,
                                  gcc.df = gcc.met.pace.df.16,
                                  f.h = 222,
                                  f.t.opt = chain[i,1],
                                  f.extract = chain[i,2],
                                  f.sec = chain[i,3],
                                  f.growth = chain[i,4] ,
                                  t.max = 45,
                                  day.lay = day.lay,
                                  swc.wilt = swc.wilt ,
                                  swc.capacity = swc.capacity,
                                  bucket.size = bucket.size,
                                  use.smooth = use.smooth))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
    
    print(paste0(i,' / ',iterations))
  }
  return(chain)
}