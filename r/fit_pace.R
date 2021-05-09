day.lag <- 3
source('r/pace_data_process.R')
# source('r/ym_data_process.R')
# source('r/process_cw_gcc.R')
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)
library(foreach)
library(doParallel)
#
# fit pace####
species.vec <- as.character(unique(gcc.met.pace.df$Species)[c(1,2,4,5,7,8,10,11)])
# species.vec <- c('Bis','Luc','Rye',"Pha")
# species.vec <- c('Rho')
# c('Luc')
#                # 'Dig', 'DigBis', 'Kan', 
#                # 'KanWal','Pha', 'PhaSub', 
#                # 'Rho',  'Wal')
# 
# seq_along(species.vec)
# select.spc <- 
# species.vec <- 'Kan'
for(i in seq_along(species.vec)){
  fit.mcmc.2q.func(df = gcc.met.pace.df,
                   n.iter = 40000,
                   species.in=species.vec[i],prep.in = 'Control', temp.in ='Ambient',
                   my.fun = phenoGrass.func.v13,
                   out.nm.note='v13.2q.', 
                   use.smooth = TRUE,cal.initial = TRUE,day.lag = 3,
                   swc.capacity = 0.13,swc.wilt = 0.05,bucket.size = 300)
  
}

