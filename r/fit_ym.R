day.lag <- 3
source('r/pace_data_process.R')
source('r/ym_data_process.R')
# source('r/process_cw_gcc.R')
source('r/v13_common_fun.R')
source('models/hufkens/hufkensV13.R')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
library(zoo)
library(foreach)
library(doParallel)

# fit YM####
ym.18.df <- get.ym.func(18)
fit.mcmc.2q.func(df=ym.18.df,n.iter = 40000,
                 species.in='ym',prep.in = 'Control', temp.in ='Ambient',
                 my.fun = phenoGrass.func.v13,out.nm.note='v13.2q.noT.',use.smooth = TRUE,
                 swc.capacity = 0.3,swc.wilt = 0.05,bucket.size = 1000,cal.initial = T,day.lag = 3)
