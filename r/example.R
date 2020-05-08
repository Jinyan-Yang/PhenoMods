#####load required stuff
source('r/functions_mcmc_v12.r')
day.lag <- 1#this is a left over from prevous model; not use at this moment
source('r/pace_data_process.R')
# packages
library(doBy)
library(zoo)

# subset input data####
# species.in = 'Luc'
# prep.in = 'Control'
# temp.in ='Ambient'
# gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
#                                     species.in =species.in,
#                                     prep.in = prep.in,
#                                     temp.in =temp.in)

gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
                                    subplot = 'S3P3B')

# check soil moisture capacity
swv.sat.obs = quantile(gcc.met.pace.df.16$vwc,.99,na.rm=T)
# max(gcc.met.pace.df.16$vwc,na.rm=T)
# min(gcc.met.pace.df.16$vwc,na.rm=T)

# subset by date
gcc.met.pace.df.16 <- gcc.met.pace.df.16[gcc.met.pace.df.16$Date<as.Date('2019-6-01')&
                                           gcc.met.pace.df.16$Date>as.Date('2019-05-01'),]
gcc.met.pace.df.16$map <- 760

# para values####
par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
  f.t.opt = c(10,15,20,NA,NA,NA),
  f.extract = c(0.1,0.5,0.9,NA,NA,NA),
  f.sec = c(0.05,0.1,0.15,NA,NA,NA),
  f.growth = c(0.1,0.2,0.3,NA,NA,NA))
row.names(par.df) <- c('min','initial','max','fit','stdv','prop')

# this assume 100% of the data falls into the max min range 
# in a normal distribution for proposal.func
par.df['stdv',] <- ((par.df['max',] - par.df['min',])/10)

bucket.size <- 300


# predict ####
chain.read <- readRDS('cache/chain.Luc.Control.Ambient.rds')
burnIn = 1000
par.df["fit",] <- colMeans(chain.read[burnIn:nrow(chain.read),])
hufken.pace.pred <- phenoGrass.func.v12(gcc.met.pace.df.16,
                                        f.h = 222,
                                        f.t.opt = par.df["fit",1],
                                        f.extract = par.df["fit",2],
                                        f.sec= par.df["fit",3],
                                        f.growth = par.df["fit",4],
                                        bucket.size = bucket.size,
                                        swc.wilt = 0.05 ,
                                        swc.capacity = swv.sat.obs ,
                                        t.max = 45,
                                        day.lay = day.lag)
#plot fitting # ####
library(viridisLite)
palette(viridis(8))
par(mar=c(5,5,1,1))
plot(cover~Date,data = hufken.pace.pred,type='l',#pch=16,
     xlab=' ',ylab=expression(f[cover]),ylim=c(0,0.9),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred,type='l',col=palette()[8])
# 
plot(vwc~Date,data = hufken.pace.pred,type='p',pch=16,
     xlab=' ',ylab=expression(VWC~('%')),ylim=c(0.03,swv.sat.obs),col = palette()[6])

points(vwc.hufken~Date,data = hufken.pace.pred,type='l',col=palette()[8])

# 
# legend('topleft',legend = paste0(species.in,prep.in,temp.in),bty='n')

# 
# start mcmc fiting######
chain.test = mh.MCMC.func(iterations = 10000,
                         par.df,
                         gcc.met.pace.df.16,
                         bucket.size = bucket.size,
                         day.lay = day.lag,
                         swc.capacity = swv.sat.obs,
                         swc.wilt = 0.05)
