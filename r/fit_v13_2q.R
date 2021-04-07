day.lag <- 3
source('r/pace_data_process.R')
source('r/ym_data_process.R')
source('r/process_cw_gcc.R')
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)
library(foreach)
library(doParallel)
#
# fit pace####
species.vec <- c('Luc')
                 # 'Dig', 'DigBis', 'Kan', 
                 # 'KanWal','Pha', 'PhaSub', 
                 # 'Rho',  'Wal')
# unique(gcc.met.pace.df$Species)
# seq_along(species.vec)
for(i in seq_along(species.vec)){
  fit.mcmc.2q.func(df = gcc.met.pace.df,
                   n.iter = 40000,
                     species.in=species.vec[i],prep.in = 'Control', temp.in ='Ambient',
                     my.fun = phenoGrass.func.v13,
                   out.nm.note='v13.2q', use.smooth = TRUE,cal.initial = F,day.lag = 3)
  
}

for(i in seq_along(species.vec)){
  fit.mcmc.2q.func(df = gcc.met.pace.df,
                   n.iter = 40000,
                   species.in=species.vec[i],prep.in = 'Drought', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,
                   out.nm.note='v13.2q', use.smooth = TRUE,cal.initial = F,day.lag = 3)
  
}


# fit YM####
ym.18.df <- get.ym.func(18)
fit.mcmc.2q.func(df=ym.18.df,n.iter = 40000,
                   species.in='ym',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13.2q',use.smooth = TRUE,
                   swc.capacity = 0.3,swc.wilt = 0.05,bucket.size = 1000,cal.initial = F,day.lag = 3)
# 
ym.drought.df <- get.ym.func('Drought')
fit.mcmc.2q.func(df=ym.drought.df,n.iter = 40000,
                 species.in='ym',prep.in = 'Drought', temp.in ='Ambient',
                 my.fun = phenoGrass.func.v13,out.nm.note='v13.2q',use.smooth = TRUE,
                 swc.capacity = 0.3,swc.wilt = 0.05,bucket.size = 1000,cal.initial = F,day.lag = 3)


# plot(cover~Date,data = pred.ym.df)
# plot(GCC.norm~Date,data = pred.ym.df)
# plot(GCC.norm.smooth~Date,data = pred.ym.df)
# 
# fit.mcmc.2q.func(df=ym.18.df,n.iter = 1000,
#                  species.in='ym',prep.in = 'Control', temp.in ='Ambient',
#                  my.fun = phenoGrass.func.v13,out.nm.note='test',use.smooth = TRUE,
#                  swc.capacity = 0.3,swc.wilt = 0.05,day.lag=1,bucket.size = 1000)

# fit to cw sites####
gcc.met.cw.df <- readRDS('cache/gcc.met.cw.df.rds')
for (i in seq_along(site.vec)){
  fit.mcmc.2q.func(df=gcc.met.cw.df,
                   species.in=site.vec[i],prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13.2q',use.smooth = FALSE,
                   swc.capacity = 0.3,swc.wilt = 0.05,n.iter = 10000,bucket.size = 1000)
}

# 
# fit to modis sites####

source('r/proces_modis_sites.R')
# seq_along(modis.sites.vec)
for (i in c(14)){
  
  limits.vec <- limit.ls[[i]]
  fit.mcmc.2q.func(df=gcc.met.modis.df,
                   species.in=modis.sites.vec[i],prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13.2q.rooting.oldproposal.newNDVI.',use.smooth = FALSE,
                   swc.capacity = 0.45,swc.wilt = 0.05,n.iter = 20000,bucket.size = 1000,
                   norm.min.max = limits.vec,cal.initial = FALSE,day.lag = 3)
}

# make plots####
source('r/plot.mcmc.r')
# make PDF plots#####
pdf('figures/v13.2q.pace.pdf',width = 8,height = 8*0.618)

# ym
plot.mcmc.func.2q(ym.18.df,species.in='ym',prep.in = 'Control','Ambient',
                  my.fun = phenoGrass.func.v13,
                  nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)
plot.title.func('YM') 


plot.mcmc.func.2q(ym.drought.df,species.in='ym',prep.in = 'Drought','Ambient',
                  my.fun = phenoGrass.func.v13,
                  nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)
plot.title.func('YM') 
# pace
c(1,2,3,4,6,10)
for (i in seq_along(species.vec)) {
  plot.mcmc.func.2q(gcc.met.pace.df,species.vec[i],'Control','Ambient',
                    my.fun = phenoGrass.func.v13,
                    nm.note='v13.2q',use.smooth = TRUE,day.lag = 3)
  plot.title.func(species.vec[i]) 

}
for (i in seq_along(species.vec)) {
  plot.mcmc.func.2q(gcc.met.pace.df,species.vec[i],'Drought','Ambient',
                    my.fun = phenoGrass.func.v13,
                    nm.note='v13.2q',use.smooth = TRUE,day.lag = 3)
  plot.title.func(paste0(species.vec[i],'-Drought')) 
  
}

# # cw
# for(i in seq_along(site.vec[1:7])){
#   fn <- sprintf('cache/smv13chain.%s.Control.Ambient.rds',site.vec[i])
#   if(file.exists(fn)){
# 
#     plot.mcmc.func.2q(gcc.met.cw.df,site.vec[i],'Control','Ambient'
#                       ,subplot = NULL,nm.note = 'v13.2q',use.smooth = TRUE,my.fun =phenoGrass.func.v13,
#                    swc.in.wilt = 0.05,swc.in.cap = 0.3,bucket.size=1000)
#     plot.title.func(site.vec[i])
#   }
# }

dev.off()

# # plot modis

pdf('figures/v13.2q.modis.pdf',width = 8,height = 8*0.618)

# modis sites
for (i in c(12)) {
  limits.vec <- limit.ls[[i]]
  plot.mcmc.func.2q.modis(df=gcc.met.modis.df,#[year(gcc.met.modis.df$Date)==2014,]
                          species.in=modis.sites.vec[i],
                          prep.in='Control',
                          temp.in='Ambient',
                          my.fun = phenoGrass.func.v13,
                          nm.note='v13.2q.rooting.oldproposal.newNDVI.',use.smooth = FALSE,norm.min.max = limits.vec,
                          swc.in.cap = 0.45,swc.in.wilt = 0,bucket.size = 1000)
  # 
  plot.title.func(modis.sites.vec[i])
  
  # modis
  fn <- sprintf('cache/v13.2q.rooting.oldproposal.newNDVI.chain.%s.Control.Ambient.rds',modis.sites.vec[i])
  chain.3.ls = readRDS(fn)
  lapply(chain.3.ls, plot.check.mcmc.func,species.in=modis.sites.vec[i])
  
  # 
  par(mfrow=c(3,2),mar=c(5,5,1,1))
  for(par.num in 1:6){
    
    plot.line.mcmc.func(chain.3.ls,par.num,range.iter =  round(3*nrow(chain.3.ls[[1]])/4):nrow(chain.3.ls[[1]]))
    
  }
  
}


dev.off()

# # plot ym only
# pdf('figures/v13.ym.2q.pdf',width = 8,height = 8*0.618)
# plot.mcmc.func.2q(ym.18.df,'ym','Control','Ambient',
#                   my.fun = phenoGrass.func.v13,
#                   nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)
# 
# dev.off()


# #############
pdf('figures/v13.2q.diag.pdf',width = 8,height = 8*0.618)
for(i in  c(1,2,3,4,6,10)){
  fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds',species.vec[i])
  chain.3.ls = readRDS(fn)
  lapply(chain.3.ls, plot.check.mcmc.func,species.in=species.vec[i])
  
  par(mfrow=c(3,2),mar=c(5,5,1,1))
  for(par.num in 1:6){
    
    plot.line.mcmc.func(chain.3.ls,par.num,range.iter =  round(30000:nrow(chain.3.ls[[1]])))
    
  }
}

# 


# ym
fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds','ym')
chain.3.ls = readRDS(fn)
lapply(chain.3.ls, plot.check.mcmc.func,species.in='ym')

par(mfrow=c(3,2),mar=c(5,5,1,1))
for(i in 1:6){
  
  plot.line.mcmc.func(chain.3.ls,i)
  
}

# modis
fn <- c('cache/v13.2q.rooting.oldproposal.newNDVI.chain.tussock_600.Control.Ambient.rds')
chain.3.ls = readRDS(fn)
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Pasture<600')

# 
par(mfrow=c(3,2),mar=c(5,5,1,1))
for(i in 1:6){
  
  plot.line.mcmc.func(chain.3.ls,i)
  
}

# 
# summary(lm(chain.3.ls[[1]][,4]~chain.3.ls[[1]][,6]))

dev.off()

# 
fn <- c('cache/smtestchain.ym.Control.Ambient.rds')
chain.3.ls = readRDS(fn)
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Pasture<1000')

par(mfrow=c(3,2),mar=c(5,5,1,1))
for(i in 1:6){
  
  plot.line.mcmc.func(chain.3.ls,i,range.iter = 10000:20000)
  
}
# pdf('figures/v13.ym.2q.diag.pdf',width = 8,height = 8*0.618)
# fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds','ym')
# chain.3.ls = readRDS(fn)
# lapply(chain.3.ls, plot.check.mcmc.func,species.in='ym')
# dev.off()
