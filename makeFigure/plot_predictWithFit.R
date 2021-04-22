day.lag <- 3
source('r/pace_data_process.R')
source('r/ym_data_process.R')
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)
source('r/plot.mcmc.r')

ym.18.df <- get.ym.func(18)
gcc.met.con.df <- get.paddock.func('control')
gcc.met.iri.df <- get.paddock.func('irrigated')
species.vec <- c('Bis','Dig','Luc','Fes','Rye','Kan','Rho','ym','flux')


# pdf('figures/makepredictions.pdf',width = 8,height = 8*.618)
for (i in seq_along(species.vec)) {
  
  pred.con <- 'Control'
  
  if(species.vec[i]!='ym'){
    df = gcc.met.pace.df
    swc.cap = 0.13
    swc.wilt = 0.05
    bucket.size=300
  }
  
  if(species.vec[i]=='ym'){
    df = ym.18.df
    swc.cap = 0.3
    swc.wilt = 0.05
    bucket.size=1000
  }
  
  if(species.vec[i]=='flux'){
    df = gcc.met.iri.df
    swc.q.con <- quantile(gcc.met.con.df$vwc,na.rm=T,probs = c(0.01,0.99))
    swc.cap = swc.q.con[[2]]
    swc.wilt = swc.q.con[[1]]
    bucket.size=1000
    pred.con = 'Irrigated'
  }
  
  plot.mcmc.func.2q(df,species.vec[i],pred.con,'Ambient',
                    my.fun = phenoGrass.func.v13,
                    nm.note='v13.2q',use.smooth = TRUE,day.lag = 3,
                    swc.in.cap = swc.cap,swc.in.wilt = swc.wilt,bucket.size = bucket.size,do.predict = 'Control')
  plot.title.func(species.vec[i]) 
}
# dev.off()

# # ###########
# plot.mcmc.func.2q(gcc.met.iri.df,'flux','Irrigated','Ambient',
#                   my.fun = phenoGrass.func.v13,
#                   nm.note='v13.2q',use.smooth = TRUE,day.lag = 3,
#                   swc.in.cap =  swc.q.iri[[2]],swc.in.wilt =  swc.q.iri[[1]],bucket.size = bucket.size,do.predict = 'Control')
# plot.title.func('Flux_irrigated')
# 
# # #
# # fn <- sprintf('cache/smv13.2qchain.%s.Irrigated.Ambient.rds','flux')
# # chain.3.ls = readRDS(fn)
# # lapply(chain.3.ls, plot.check.mcmc.func,species.in='Flux-irrigated')
# # 
# # par(mfrow=c(3,2),mar=c(5,5,1,1))
# # for(par.num in 1:6){
# # 
# #   start.row <- nrow(chain.3.ls[[1]]) / 4*3
# # 
# #   plot.line.mcmc.func(chain.3.ls,par.num,range.iter =  round(start.row:nrow(chain.3.ls[[1]])))
# # 
# # }
# 
# dev.off()