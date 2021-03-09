day.lag <- 1
source('r/pace_data_process.R')
source('r/ym_data_process.R')
source('r/process_cw_gcc.R')
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)
library(foreach)
library(doParallel)
# library(mvtnorm)

# fit pace####
species.vec <- c('Luc','Fes','Rye',
                 'Dig', 'DigBis', 'Kan', 
                 'KanWal','Pha', 'PhaSub', 
                 'Rho',  'Wal')
# seq_along(species.vec)
for(i in c(2,3,4,6,10)){
  fit.mcmc.2q.func(df = gcc.met.pace.df,
                   n.iter = 20000,
                     species.in=species.vec[i],prep.in = 'Control', temp.in ='Ambient',
                     my.fun = phenoGrass.func.v13,
                   out.nm.note='v13.2q', use.smooth = TRUE)
  
}

# fit YM####
ym.18.df <- get.ym.func(18)
fit.mcmc.2q.func(df=ym.18.df,n.iter = 20000,
                   species.in='ym',prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13.2q',use.smooth = TRUE,
                   swc.capacity = 0.3,swc.wilt = 0.05,day.lag=1,bucket.size = 1000)
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
for (i in c(5)){
  fit.mcmc.2q.func(df=gcc.met.modis.df,
                   species.in=modis.sites.vec[i],prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,out.nm.note='v13.2q.rooting.oldproposal.',use.smooth = FALSE,
                   swc.capacity = 0.3,swc.wilt = 0.05,n.iter = 15000,bucket.size = 1000,
                   norm.min.max = limits.vec)
}


# make plots####
source('r/plot.mcmc.r')
# make PDF plots#####
pdf('figures/v13.2q.pdf',width = 8,height = 8*0.618)

# ym
plot.mcmc.func.2q(ym.18.df,'ym','Control','Ambient',
                  my.fun = phenoGrass.func.v13,
                  nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)
plot.title.func('YM') 
# pace
for (i in c(1,2,3,4,6,10)) {
  plot.mcmc.func.2q(gcc.met.pace.df,species.vec[i],'Control','Ambient',
                    my.fun = phenoGrass.func.v13,
                    nm.note='v13.2q',use.smooth = TRUE)
  plot.title.func(species.vec[i]) 

}
# cw
for(i in seq_along(site.vec[1:7])){
  fn <- sprintf('cache/smv13chain.%s.Control.Ambient.rds',site.vec[i])
  if(file.exists(fn)){

    plot.mcmc.func.2q(gcc.met.cw.df,site.vec[i],'Control','Ambient'
                      ,subplot = NULL,nm.note = 'v13.2q',use.smooth = TRUE,my.fun =phenoGrass.func.v13,
                   swc.in.wilt = 0.05,swc.in.cap = 0.3,bucket.size=1000)
    plot.title.func(site.vec[i])
  }
}

dev.off()

# # plot modis
pdf('figures/v13.2q.modis.pdf',width = 8,height = 8*0.618)

gcc.met.modis.df <- readRDS('cache/modis/modis.ndvi.met.rds')
limits.vec <- as.vector(quantile(gcc.met.modis.df$GCC,na.rm=T,probs = c(.01,.99)))
gcc.met.modis.df <- gcc.met.modis.df[gcc.met.modis.df$Species != 'tussock<1100',]

gcc.met.modis.df$Species <- gsub('<','_',gcc.met.modis.df$Species)
modis.sites.vec <- unique(gcc.met.modis.df$Species)

modis.sites.vec <- modis.sites.vec[c(4:9,14:19)]

# modis sites
for (i in c(1,11,15,7)) {
  plot.mcmc.func.2q.modis(df=gcc.met.modis.df,
                    species.in=modis.sites.vec[i],
                    prep.in='Control',
                    temp.in='Ambient',
                    my.fun = phenoGrass.func.v13,
                    nm.note='v13.2q.rooting.oldproporal..',use.smooth = TRUE,norm.min.max = limits.vec,
                    swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)
  
  plot.title.func(modis.sites.vec[i])
  
  # 
#   combine_pdfs = function(path, output_pdf) {
#     system(sprintf("pdftk %s/*pdf cat output %s"), path, output_pdf)
#   }
}

dev.off()

# # plot ym only
# pdf('figures/v13.ym.2q.pdf',width = 8,height = 8*0.618)
# plot.mcmc.func.2q(ym.18.df,'ym','Control','Ambient',
#                   my.fun = phenoGrass.func.v13,
#                   nm.note='v13.2q',use.smooth = TRUE,swc.in.cap = 0.3,swc.in.wilt = 0.05,bucket.size = 1000)
# 
# dev.off()

plot.check.mcmc.func=function(chain.in,species.in='',nm.vec = c('Topt','f.extract','senescence','growth','q','qs')){
  
  burnIn = round(nrow(chain.in) / 2)
  
  par(mfrow=c(3,2),mar=c(5,5,1,1))
  
  for(i in 1:ncol(chain.in)){
    hist(chain.in[burnIn:nrow(chain.in),i],xlab = nm.vec[i],
         main='')
    
  }
  plot.title.func(species.in = species.in)
}

# 
acceptance.func <- function(vec){
  sum(!duplicated(vec)) / length(vec)
}

# 
plot.line.mcmc.func <- function(chain.3.ls,val.nm,
                                nm.vec = c('Topt','f.extract','senescence','growth','q','qs'),
                                range.iter = NULL){
  
  # 
  n.iter <- nrow(chain.3.ls[[1]])
  
  if(is.null(range.iter)){
    range.iter <- round(0.75*n.iter):n.iter
  }
  
  # 
  min.val <- min(min(chain.3.ls[[1]][range.iter,val.nm]),min(chain.3.ls[[3]][range.iter,val.nm]),min(chain.3.ls[[2]][range.iter,val.nm]))
  
  max.val <- max(max(chain.3.ls[[1]][range.iter,val.nm]),max(chain.3.ls[[3]][range.iter,val.nm]),max(chain.3.ls[[2]][range.iter,val.nm]))
# 
  plot(chain.3.ls[[1]][range.iter,val.nm],pch=16,ylim=c(min.val,max.val),ylab=nm.vec[val.nm],xlab='Iteration')
  print(acceptance.func(chain.3.ls[[1]][range.iter,val.nm]))
  for (i in 2:3) {
    points(chain.3.ls[[i]][range.iter,val.nm],pch=16,col=i)
  }
}

pdf('figures/v13.2q.diag.pdf',width = 8,height = 8*0.618)
for(i in  c(1,2,3,4,6,10)){
  fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds',species.vec[i])
  chain.3.ls = readRDS(fn)
  lapply(chain.3.ls, plot.check.mcmc.func,species.in=species.vec[i])
}

# ym
fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds','ym')
chain.3.ls = readRDS(fn)
lapply(chain.3.ls, plot.check.mcmc.func,species.in='ym')

# modis
fn <- c('cache/smv13.2q.rooting.oldproporal..chain.pasture_500.Control.Ambient.rds')
chain.3.ls = readRDS(fn)
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Pasture<300')

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
  
  plot.line.mcmc.func(chain.3.ls,i,range.iter = 1:100)
  
}
# pdf('figures/v13.ym.2q.diag.pdf',width = 8,height = 8*0.618)
# fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds','ym')
# chain.3.ls = readRDS(fn)
# lapply(chain.3.ls, plot.check.mcmc.func,species.in='ym')
# dev.off()
