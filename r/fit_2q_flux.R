##########################################################################################
day.lag <- 3
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)
library(foreach)
library(doParallel)

source('r/process_paddock_gcc_met.R')
gcc.met.con.df <- get.paddock.func('control')
# gcc.met.iri.df <- get.paddock.func('irrigated')
# range(gcc.met.con.df$vwc,na.rm=T)

swc.q.con <- quantile(gcc.met.con.df$vwc,na.rm=T,probs = c(0.01,0.99))
# swc.q.iri <- quantile(gcc.met.iri.df$vwc,na.rm=T,probs = c(0.01,0.99))

bucket.size <- 1000

fit.mcmc.2q.func(df=gcc.met.con.df,n.iter = 40000,
                 species.in='flux',prep.in = 'Control', temp.in ='Ambient',
                 my.fun = phenoGrass.func.v13,out.nm.note='v13.2q.',use.smooth = TRUE,
                 swc.capacity = swc.q.con[[2]],swc.wilt = swc.q.con[[1]],bucket.size = bucket.size,cal.initial = T,day.lag = 3)
# # 
# # fit.mcmc.2q.func(df=gcc.met.iri.df,n.iter = 40000,
# #                  species.in='flux',prep.in = 'Irrigated', temp.in ='Ambient',
# #                  my.fun = phenoGrass.func.v13,out.nm.note='v13.2q',use.smooth = TRUE,
# #                  swc.capacity = swc.q.iri[[2]],swc.wilt = swc.q.iri[[1]],bucket.size = 1000,cal.initial = T,day.lag = 3)
# 
# 
# source('r/plot.mcmc.r')
# pdf('figures/obs_pred_flux.pdf',width = 8,height = 8*.618)
# # 
#     # swc.cap = 0.3
#     # swc.wilt = 0.05
#     # bucket.size=500
# 
#   plot.mcmc.func.2q(gcc.met.con.df,'flux','Control','Ambient',
#                     my.fun = phenoGrass.func.v13,
#                     nm.note='v13.2q',use.smooth = TRUE,day.lag = 3,
#                     swc.in.cap =  swc.q.con[[2]],swc.in.wilt = swc.q.con[[1]],bucket.size = bucket.size,burn.proportion = 0.75)
#   plot.title.func('Flux_control') 
#   
#   # 
#   fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds','flux')
#   chain.3.ls = readRDS(fn)
#   lapply(chain.3.ls, plot.check.mcmc.func,species.in='Flux-control')
#   
#   par(mfrow=c(3,2),mar=c(5,5,1,1))
#   for(par.num in 1:6){
#     
#     start.row <- nrow(chain.3.ls[[1]]) / 4*3
#     
#     plot.line.mcmc.func(chain.3.ls,par.num,range.iter =  round(start.row:nrow(chain.3.ls[[1]])))
#     
#   }
#   
#   # # ###########
#   # plot.mcmc.func.2q(gcc.met.iri.df,'flux','Irrigated','Ambient',
#   #                   my.fun = phenoGrass.func.v13,
#   #                   nm.note='v13.2q',use.smooth = TRUE,day.lag = 3,
#   #                   swc.in.cap =  swc.q.iri[[2]],swc.in.wilt =  swc.q.iri[[1]],bucket.size = bucket.size,do.predict = 'Control')
#   # plot.title.func('Flux_irrigated') 
#   # 
#   # # 
#   # fn <- sprintf('cache/smv13.2qchain.%s.Irrigated.Ambient.rds','flux')
#   # chain.3.ls = readRDS(fn)
#   # lapply(chain.3.ls, plot.check.mcmc.func,species.in='Flux-irrigated')
#   # 
#   # par(mfrow=c(3,2),mar=c(5,5,1,1))
#   # for(par.num in 1:6){
#   #   
#   #   start.row <- nrow(chain.3.ls[[1]]) / 4*3
#   #   
#   #   plot.line.mcmc.func(chain.3.ls,par.num,range.iter =  round(start.row:nrow(chain.3.ls[[1]])))
#   #   
#   # }
# 
# dev.off()