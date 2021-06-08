day.lag <- 3
source('r/pace_data_process.R')
source('r/ym_data_process.R')
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
source('r/process_paddock_gcc_met.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)
source('r/plot.mcmc.r')

ym.18.df <- get.ym.func(18)
gcc.met.con.df <- get.paddock.func('control')
# species.vec <- c('Bis','Dig','Luc','Fes','Rye','Kan','Rho','ym','flux')
species.vec <- c('Bis','Luc','Dig','Kan','Rho','Fes','Pha','Rye','ym','flux')
# species.vec <- 'Kan'

pdf('figures/obs_pred.pdf',width = 8,height = 8*.618)
for (i in seq_along(species.vec)) {
  
  if(species.vec[i]!='ym'){
    df = gcc.met.pace.df
    swc.cap = 0.13
    swc.wilt = 0.05
    bucket.size=300
  }
  
  if(species.vec[i]=='ym'){
    df = ym.18.df
    swc.ym.con <- quantile(ym.18.df$vwc,na.rm=T,probs = c(0.001,0.999))
    swc.cap = swc.ym.con[[2]]
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
  
  plot.mcmc.func.2q(df,species.vec[i],'Control','Ambient',
                    my.fun = phenoGrass.func.v13,
                    nm.note='v13.2q.',use.smooth = TRUE,day.lag = 3,
                    swc.in.cap = swc.cap,swc.in.wilt = swc.wilt,bucket.size = bucket.size)

  plot.title.func(species.vec[i]) 
}
dev.off()


# 
pdf('figures/diag.pdf',width = 8,height = 8*.618)
for (i in seq_along(species.vec)) {
  
  fn <- sprintf('cache/smv13.2q.chain.%s.Control.Ambient.rds',species.vec[i])
  chain.3.ls = readRDS(fn)
  lapply(chain.3.ls, plot.check.mcmc.func,species.in=species.vec[i])
  
  par(mfrow=c(3,2),mar=c(5,5,1,1))
  for(par.num in 1:6){
    
    start.row <- nrow(chain.3.ls[[1]]) / 4*3
    
    plot.line.mcmc.func(chain.3.ls,par.num,range.iter =  round(start.row:nrow(chain.3.ls[[1]])))
    
  }
}
dev.off()

