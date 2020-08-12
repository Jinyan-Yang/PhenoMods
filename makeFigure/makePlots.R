source('r/functions_mcmc_v12.r')
source('models/hufkens/pG_v10.R')
source('r/plot.mcmc.r')
day.lag <- 3
source('r/pace_data_process.R')
# packages
library(doBy)
library(zoo)

# functions
plot.title.func=function(species.in){
  par(mfrow=c(1,1),new=T,mar=c(1,1,1,1))
  plot(0,ann=F,axes=F,pch=' ')
  title(main = species.in,line = 0)
}

plot.check.mcmc.func=function(chain.in,burnIn =3000,species.in = ''){
  par(mfrow=c(2,2),mar=c(5,5,2,1))
  for(i in 1:ncol(chain.in)){
    hist(chain.in[burnIn:nrow(chain.in),i],xlab = c('Topt','f.extract','senescence','growth')[i],
         main = '')
    
  }
  plot.title.func(species.in = species.in)
}


# plots for v10####
# output
pdf('figures/plot.v10.pdf',width =10,height = 10*0.618)
plot.mcmc.func('Luc','Control','Ambient',subplot = NULL,nm.note = 'v10',use.smooth = TRUE,my.fun =phenoGrass.func.v10 )
plot.title.func('Luc')

plot.mcmc.func('Fes','Control','Ambient',subplot = NULL,nm.note = 'v10',use.smooth = TRUE,my.fun =phenoGrass.func.v10 )
plot.title.func('Fes')

plot.mcmc.func('Rye','Control','Ambient',subplot = NULL,nm.note = 'v10',use.smooth = TRUE,my.fun =phenoGrass.func.v10 )
plot.title.func('Rye')
dev.off()



# fitted pars
pdf('figures/plot.diag.v10.pdf',width = 6,height = 6*0.618)

chain.3.ls = readRDS('cache/smv10chain.Luc.Control.Ambient.rds')
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Luc')

chain.3.ls = readRDS('cache/smv10chain.Fes.Control.Ambient.rds')
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Fes')

chain.3.ls = readRDS('cache/smv10chain.Rye.Control.Ambient.rds')
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Rye')

dev.off()


# plots for V11####
# output
pdf('figures/plot.v11.pdf',width =10,height = 10*0.618)
plot.mcmc.func('Luc','Control','Ambient',subplot = NULL,nm.note = 'v11',use.smooth = TRUE,my.fun =phenoGrass.func.v11 )
plot.title.func('Luc')

plot.mcmc.func('Fes','Control','Ambient',subplot = NULL,nm.note = 'v11',use.smooth = TRUE,my.fun =phenoGrass.func.v11 )
plot.title.func('Fes')

plot.mcmc.func('Rye','Control','Ambient',subplot = NULL,nm.note = 'v11',use.smooth = TRUE,my.fun =phenoGrass.func.v11 )
plot.title.func('Rye')
dev.off()

# fitted pars
pdf('figures/plot.diag.v11.pdf',width = 6,height = 6*0.618)

chain.3.ls = readRDS('cache/smv11chain.Luc.Control.Ambient.rds')
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Luc')

chain.3.ls = readRDS('cache/smv11chain.Fes.Control.Ambient.rds')
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Fes')

chain.3.ls = readRDS('cache/smv11chain.Rye.Control.Ambient.rds')
lapply(chain.3.ls, plot.check.mcmc.func,species.in='Rye')

dev.off()





