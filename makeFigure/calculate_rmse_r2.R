library(doBy)
library(lubridate)
species.vec <- c('Bis','Luc','Dig','Kan','Rho','Fes','Pha','Rye','ym','flux')

# loop through all spcies/site
predic.ls <- list()
for (i in seq_along(species.vec)){
  species.in <- species.vec[i]
  out.pred.nm <- paste0('tmp/pred.smv13.2q.07072021.chain.',
                        species.in,
                        '.Control.Ambient.rds')
  # out.pred.nm <- 'pred.smv13.2q.07072021.chain.flux.Control.Ambient.rds'
  tmp.df <- readRDS(out.pred.nm)
  tmp.df <- tmp.df[,c('Date','GCC.norm','cover','cover.hufken','harvest')]
  tmp.df$spc <- species.in
  
  predic.ls[[i]] <- tmp.df
  
  
}
predic.df <- do.call(rbind,predic.ls)


fit.out <- summary(lm(GCC.norm~cover.hufken,data = predic.df[predic.df$harvest!=1,]))
fit.out$r.squared

rmse.func = function(m, o){
  sqrt(mean((m - o)^2,na.rm=T))
}
predic.df.sub <- predic.df[predic.df$harvest!=1,]
rmse.func(predic.df.sub$cover.hufken,predic.df.sub$GCC.norm)
# same but under drought
# loop through all spcies/site
species.vec <- c('Bis','Luc','Dig','Kan','Rho','Fes','Pha','Rye','ym')
predic.ls <- list()
for (i in seq_along(species.vec)){
  species.in <- species.vec[i]
  out.pred.nm <- paste0('tmp/pred.smv13.2q.07072021.chain.',
                        species.in,
                        '.Control.predict.Ambient.rds')
  # out.pred.nm <- 'tmp/pred.smv13.2q.07072021.chain.Bis.Control.predict.Ambient.rds'
  # pred.smv13.2q.07072021.chain.Bis.Control.predict.Ambient.rds
  tmp.df <- readRDS(out.pred.nm)
  tmp.df <- tmp.df[,c('Date','GCC.norm','cover','cover.hufken','harvest')]
  tmp.df$spc <- species.in
  
  predic.ls[[i]] <- tmp.df
  
  
}
predic.df <- do.call(rbind,predic.ls)


fit.out <- summary(lm(GCC.norm~cover.hufken,data = predic.df[predic.df$harvest!=1,]))
fit.out$r.squared

rmse.func = function(m, o){
  sqrt(mean((m - o)^2,na.rm=T))
}
predic.df.sub <- predic.df[predic.df$harvest!=1,]
rmse.func(predic.df.sub$cover.hufken,predic.df.sub$GCC.norm)
