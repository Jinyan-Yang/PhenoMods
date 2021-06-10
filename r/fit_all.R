day.lag <- 3
source('r/pace_data_process.R')
source('r/ym_data_process.R')
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
source('r/process_paddock_gcc_met.R')
# devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)
library(foreach)
library(doParallel)
# 
ym.18.df <- get.ym.func(18)
gcc.met.con.df <- get.paddock.func('control')
# species.vec <- c('Bis','Dig','Luc','Fes','Rye','Kan','Rho','ym','flux')
species.vec <- c('Bis','Luc','Dig','Kan','Rho','Fes','Pha','Rye','ym','flux')
species.vec <- c('Rho','Pha','ym')
# species.vec <- c('Kan','ym','flux')
# species.vec <- c('Bis','Luc','Kan','Fes','Pha','ym','flux')
# species.vec <- c('Bis','Luc','Dig','Kan','Rho','Fes','Pha','Rye')

# species.vec <- c('Luc','Dig','Rho','Fes','Pha')
for (i in seq_along(species.vec)) {
  
  if(species.vec[i]!='ym'){
    df = gcc.met.pace.df
    swc.cap = 0.13
    swc.wilt = 0.05
    bucket.size=300
  }
  
  if(species.vec[i]=='ym'){
    df = ym.18.df
    swc.ym.con <- quantile(ym.18.df$vwc,na.rm=T,probs = c(0.01,0.99))
    swc.cap =swc.ym.con[[2]]
    swc.wilt = swc.ym.con[[1]]
    bucket.size=1000
  }
  
  if(species.vec[i]=='flux'){
    df = gcc.met.con.df
    swc.q.con <- quantile(gcc.met.con.df$vwc,na.rm=T,probs = c(0.01,0.99))
    swc.cap =  swc.q.con[[2]]
    swc.wilt = swc.q.con[[1]]
    bucket.size=1000
  }
  
  fit.mcmc.2q.func(df,
                   n.iter = 50000,
                   species.in=species.vec[i],
                   prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,
                   out.nm.note='v13.2q.', 
                   use.smooth = TRUE,cal.initial = TRUE,day.lag = 3,
                   swc.capacity = swc.cap,swc.wilt = swc.wilt,bucket.size = bucket.size)
  
}