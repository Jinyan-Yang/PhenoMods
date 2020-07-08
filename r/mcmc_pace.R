source('r/functions_mcmc_v12.r')
day.lag <- 2
source('r/pace_data_process.R')
# packages
library(doBy)
library(zoo)

fit.mcmc.pace.func <- function(species.in = 'Luc',prep.in = 'Control', 
                               temp.in ='Ambient',subplot =NA,
                               my.fun = phenoGrass.func.v11,
                               out.nm.note = '',use.smooth = FALSE){
  gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
                                      species.in =species.in,
                                      prep.in = prep.in,
                                      temp.in =temp.in,
                                      subplot = subplot)
  gcc.met.pace.df.16 <- gcc.met.pace.df.16[gcc.met.pace.df.16$Date<as.Date('2019-09-01'),]
  gcc.met.pace.df.16$map <- 760
  # para values####
  par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
    f.t.opt = c(10,15,20,NA,NA,NA),
    f.extract = c(0.05,0.075,0.1,NA,NA,NA),
    f.sec = c(0.05,0.1,0.15,NA,NA,NA),
    f.growth = c(0.1,0.2,0.3,NA,NA,NA))
  row.names(par.df) <- c('min','initial','max','fit','stdv','prop')
  
  # this assume 100% of the data falls into the max min range 
  # in a normal distribution for proposal.func
  par.df['stdv',] <- ((par.df['max',] - par.df['min',])/10)
  
  # start mcmc fiting######
  bucket.size <- 300
  # soil.water.var <- quantile(gcc.met.pace.df.16$vwc,c(.1,.99))
  chain.fes = mh.MCMC.func(30000,
                           par.df,
                           gcc.met.pace.df.16,
                           bucket.size = bucket.size,
                           day.lay = day.lag,
                           swc.capacity = 0.13,
                           swc.wilt = 0.05,
                           my.fun = my.fun,
                           use.smooth = use.smooth)
  
  # chain.INGE = mh.MCMC.func(20000,par.df,gcc.met.df,
  #                           bucket.size = bucket.size,
  #                           day.lay = 2,
  #                           swc.capacity = 0.38,
  #                           swc.wilt = 0.111)
  if(use.smooth==TRUE){
    smooth.nm='sm'
  }else{
    smooth.nm=''
  }
  
  if(is.na(subplot)){
    out.name <- sprintf('cache/%s%schain.%s.%s.%s.rds',smooth.nm,out.nm.note,species.in,prep.in,temp.in)
  }else{
    out.name <- sprintf('cache/%schain.%s.rds',out.nm.note,subplot)
  }
 
  saveRDS(chain.fes,out.name)
}
# $$$$####
fit.mcmc.pace.func(subplot = 'S3P3B')

fit.mcmc.pace.func(species.in='Luc',prep.in = 'Control', temp.in ='Ambient')

fit.mcmc.pace.func(species.in='Fes',prep.in = 'Control', temp.in ='Ambient')
fit.mcmc.pace.func(species.in='Rye',prep.in = 'Control', temp.in ='Ambient')

fit.mcmc.pace.func(species.in='Luc',prep.in = 'Drought', temp.in ='Ambient')
fit.mcmc.pace.func(species.in='Fes',prep.in = 'Drought', temp.in ='Ambient')
# 
# get fit by species but with original hufkens 
fit.mcmc.pace.func(species.in='Luc',prep.in = 'Control', temp.in ='Ambient',my.fun = phenoGrass.func.v11,out.nm.note='v10')
fit.mcmc.pace.func(species.in='Luc',prep.in = 'Control', temp.in ='Ambient',my.fun = phenoGrass.func.v11,out.nm.note='v10',use.smooth = TRUE)

# make the plots
# fit.mcmc.pace.func(subplot = 'S3P3B',my.fun = phenoGrass.func.v11,out.nm.note='v10')
luc.c.a.v10.df= readRDS('cache/v10chain.Luc.Control.Ambient.rds')


pdf('umsm.v10.pdf',width = 6,height = 3*6*0.618)
plot.mcmc.func('Luc','Control','Ambient',subplot = NULL,nm.note = 'v10')
dev.off()
# 
# luc.c.a.v10.sm.df= readRDS('cache/smv10chain.Luc.Control.Ambient.rds')
pdf('sm.v10.pdf',width = 6,height = 3*6*0.618)
plot.mcmc.func('Luc','Control','Ambient',subplot = NULL,nm.note = 'v10',use.smooth = TRUE)
dev.off()

hist(s3p3b.v10.df[10000:30000,4])

































# # ######
luc.d.a.df= readRDS('cache/chain.Luc.Control.Ambient.rds')
gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
                                    species.in = 'Fes',
                                    prep.in = 'Drought',
                                    temp.in ='Ambient')
min(gcc.met.pace.df.16$GCC,na.rm=T)
gcc.met.pace.df.16 <- gcc.met.pace.df.16[(gcc.met.pace.df.16$Date) < as.Date('2018-9-1'),]
gcc.met.pace.df.16$map <- 760
# gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
#                                     species.in = 'Luc',
#                                     prep.in = 'Control',
#                                     temp.in ='Ambient')
# gcc.met.pace.df.16 <- gcc.met.pace.df.16[gcc.met.pace.df.16$Date<as.Date('2019-11-26'),]
# gcc.met.pace.df.16$map <- 760
# # quantile(gcc.met.pace.df.16$vwc,c(.1,.99))
# # range(gcc.met.pace.df.16$vwc)

# # para values####
# par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
#   f.t.opt = c(10,15,20,NA,NA,NA),
#   f.extract = c(0.05,0.075,0.1,NA,NA,NA),
#   f.sec = c(0.05,0.1,0.15,NA,NA,NA),
#   f.growth = c(0.1,0.2,0.3,NA,NA,NA))
# row.names(par.df) <- c('min','initial','max','fit','stdv','prop')
# 
# # this assume 100% of the data falls into the max min range 
# # in a normal distribution for proposal.func
# par.df['stdv',] <- ((par.df['max',] - par.df['min',])/10)
# 
# # start mcmc fiting######
# bucket.size <- 300
# # soil.water.var <- quantile(gcc.met.pace.df.16$vwc,c(.1,.99))
# gcc.met.pace.df.16 <- gcc.met.pace.df.16[(gcc.met.pace.df.16$Date) < as.Date('2018-9-1'),]
# chain.fes = mh.MCMC.func(10000,
#                          par.df,
#                          gcc.met.pace.df.16,
#                          bucket.size = bucket.size,
#                          day.lay = day.lag,
#                          swc.capacity = 0.13,
#                          swc.wilt = 0.05)
#  
# # chain.INGE = mh.MCMC.func(20000,par.df,gcc.met.df,
# #                           bucket.size = bucket.size,
# #                           day.lay = 2,
# #                           swc.capacity = 0.38,
# #                           swc.wilt = 0.111)
# 
# saveRDS(chain.fes,'cache/chain.luc2018.rds')
# # saveRDS(chain.fes,'cache/chain.fes.rds')
gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
                                    species.in = 'Rye',
                                    prep.in = 'Control',
                                    temp.in ='Ambient')
gcc.met.pace.df.16 <- gcc.met.pace.df.16[(gcc.met.pace.df.16$Date) < as.Date('2018-9-1'),]
gcc.met.pace.df.16$map <- 760
chain.fes <- readRDS('cache/chain.Rye.Control.Ambient.rds')

# 
# # check acceptance so that the 
burnIn = 10000
acceptance = 1-mean(duplicated(chain.fes[-(1:burnIn),])) #should be >20% but <60%; 20-25% were suggested
# 
# # 
# hist(chain.fes[8000:30000,1])
# plot(chain.fes[,1])
# plot(chain.fes[,2])
# plot(chain.fes[,3])
# plot(chain.fes[,4])
# 
# # see how it works#####
par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
  f.t.opt = c(10,15,20,NA,NA,NA),
  f.extract = c(0.05,0.075,0.1,NA,NA,NA),
  f.sec = c(0.05,0.1,0.15,NA,NA,NA),
  f.growth = c(0.1,0.2,0.3,NA,NA,NA))
row.names(par.df) <- c('min','initial','max','fit','stdv','prop')
par.df["fit",] <- colMeans(chain.fes[burnIn:nrow(chain.fes),])
par.df["fit",] <- colMeans(luc.d.a.df[burnIn:nrow(luc.d.a.df),])
# 
bucket.size = 300
hufken.pace.pred <- phenoGrass.func.v12(gcc.met.pace.df.16,
                                        f.h = 222,
                                        f.t.opt = par.df["fit",1],
                                        f.extract = par.df["fit",2],
                                        f.sec= par.df["fit",3],
                                        f.growth = par.df["fit",4],
                                        bucket.size = bucket.size,
                                        swc.wilt = 0.05 ,
                                        swc.capacity = 0.13 ,
                                        t.max = 45,
                                        day.lay = day.lag)
# hufken.pace.pred$water.norm <- hufken.pace.pred$water.avi / (0.13-0.05)/300
library(viridisLite)
palette(viridis(8))
par(mar=c(5,5,1,1))
plot(cover~Date,data = hufken.pace.pred,type='b',pch=16,
     xlab=' ',ylab=expression(f[cover]),ylim=c(0,0.8),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred,type='b',col=palette()[8],pch=16)


# 
# 
# par(new=T)
# 
# plot(vwc~Date,data = hufken.pace.pred,ann=F,axes=F,type='s',col=palette()[3])
# 
# par(new=T)
# 
# plot(swc.hufken~Date,data = hufken.pace.pred,ann=F,axes=F,type='l',lty='dashed',col=palette()[4])
# 
# par(new=T)
# 
# plot(Rain~Date,data = hufken.pace.pred,ann=F,axes=F,type='l',col=palette()[4])
# 
# legend('bottomright',legend = c('MOD','OBS'),pch=16,col=palette()[c(8,6)],bty='n',title = 'Plant cover')
# 
# legend('bottom',legend = c('MOD','OBS'),lty=c('dashed','solid'),col=palette()[c(4,3)],bty='n',title = 'Soil moisture')
# 
# summary(lm(cover~cover.hufken,data = hufken.pace.pred))
# 
# plot(swc.hufken~vwc,data = hufken.pace.pred,type='p',lty='dashed',col=palette()[4])
