source('r/functions_mcmc_v12.r')
day.lag <- 1
source('r/pace_data_process.R')
# packages
library(doBy)
library(zoo)

# # read data####
# gcc.met.pace.df <- readRDS('cache/gcc.met.pace.df.processed.rds')
# 
# temp.df <- gcc.met.pace.df[gcc.met.pace.df$Species == 'Luc'&
#                              gcc.met.pace.df$Precipitation == 'Control'&
#                              gcc.met.pace.df$Temperature == 'Ambient',]
# 
# # temp.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P6A',]
# library(doBy)
# sd.df <- summaryBy(GCC.norm + vwc ~ Date,
#                    data = temp.df,FUN=c(sd),na.rm=TRUE,keep.names = F)
# 
# mean.df <- summaryBy(.~ Date,
#                      data = temp.df,FUN=c(mean),na.rm=TRUE,keep.names = T)
# 
# test.df <- merge(mean.df,sd.df)
# 
# test.df <- test.df[order(test.df$Date),]
# 
# test.df$GCC.norm <- na.locf(test.df$GCC.norm,fromLast=F)
# 
# # set the number of lageed days
# day.lag <- 1
# test.df$GCC.norm[1:day.lag] <- NA
# test.df$GCC[1:day.lag] <- NA
# 
# start.date <- test.df$Date[min(which(!is.na(test.df$GCC.norm)))]
# 
# gcc.met.pace.df.na.rm <- test.df[!is.na(test.df$GCC) & test.df$Date > start.date,]
# 
# gcc.met.pace.df.16 <- rbind(gcc.met.pace.df.na.rm,
#                             test.df[test.df$Date <= start.date &
#                                       test.df$Date > (start.date- (day.lag + 1)),])
# 
# gcc.met.pace.df.16 <- gcc.met.pace.df.16[order(gcc.met.pace.df.16$Date),]
# 
# gcc.met.pace.df.16 <- gcc.met.pace.df.16[!is.na(gcc.met.pace.df.16$Date),]
# 
# gcc.met.pace.df.16$GCC.norm <- (gcc.met.pace.df.16$GCC - 0.328) /
#   (max(gcc.met.pace.df.16$GCC,na.rm = T) - 0.328)

# para values####
par.df <- data.frame(#f.h = c(200,220,240,NA,NA),
  f.t.opt = c(17,20,23,NA,NA,NA),
  f.extract = c(0.05,0.075,0.1,NA,NA,NA),
  f.sec = c(0.05,0.1,0.15,NA,NA,NA),
  f.growth = c(0.1,0.2,0.3,NA,NA,NA))
row.names(par.df) <- c('min','initial','max','fit','stdv','prop')

# this assume 100% of the data falls into the max min range 
# in a normal distribution for proposal.func
par.df['stdv',] <- ((par.df['max',] - par.df['min',])/10)


# start mcmc fiting######
bucket.size <- 400
chain = mh.MCMC.func(20000,par.df,gcc.met.pace.df.16,
                     bucket.size = bucket.size,
                     day.lay = 1,
                     swc.capacity = 0.43,
                     swc.wilt = 0.03)
# 
# chain.INGE = mh.MCMC.func(20000,par.df,gcc.met.df,
#                           bucket.size = bucket.size,
#                           day.lay = 2,
#                           swc.capacity = 0.38,
#                           swc.wilt = 0.111)

saveRDS(chain,'cache/chain.rds')
chain <- readRDS('cache/chain.rds')
# check acceptance so that the 
burnIn = 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),])) #should be >20% but <60%; 20-25% were suggested

# see how it works#####
par.df["fit",] <- colMeans(chain[burnIn:nrow(chain),])

hufken.pace.pred <- phenoGrass.func.v12(gcc.met.pace.df.16,
                                        f.h = 222,
                                        f.t.opt = par.df["fit",1],
                                        f.extract = par.df["fit",2],
                                        f.sec= par.df["fit",3],
                                        f.growth = par.df["fit",4],
                                        bucket.size = bucket.size,
                                        swc.wilt = 0.03 * bucket.size,
                                        swc.capacity = 0.12 * bucket.size,
                                        t.max = 45,
                                        day.lay = 1)
library(viridisLite)
palette(viridis(8))
par(mar=c(5,5,1,1))
plot(cover~Date,data = hufken.pace.pred,type='b',pch=16,
     xlab='2018',ylab=expression(f[cover]),ylim=c(0,0.8),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred,type='b',col=palette()[8],pch=16)

par(new=T)

plot(vwc~Date,data = hufken.pace.pred,ann=F,axes=F,type='s',col=palette()[3])

par(new=T)

plot(swc.hufken~Date,data = hufken.pace.pred,ann=F,axes=F,type='l',lty='dashed',col=palette()[4])

legend('bottomright',legend = c('MOD','OBS'),pch=16,col=palette()[c(8,6)],bty='n',title = 'Plant cover')

legend('bottom',legend = c('MOD','OBS'),lty=c('dashed','solid'),col=palette()[c(4,3)],bty='n',title = 'Soil moisture')

summary(lm(cover~cover.hufken,data = hufken.pace.pred))

# plot((cover~cover.hufken,data = hufken.pace.pred))
hist(chain[10000:20001,2])



# 
pdf('Modified Huf LUC CWAT.pdf',width = 8,height = 8*0.618)
library(viridisLite)
palette(viridis(8))
par(mar=c(3,5,1,1))
# par(bg = rgb(240/255,241/255,211/255))
plot(cover~Date,data = hufken.pace.pred,type='b',pch=16,
     xlab=' ',ylab=expression('Plant cover'),ylim=c(0,0.8),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred,type='l',col=palette()[1],lwd=2)

legend('bottomright',legend = c('MOD','OBS'),lty='solid',col=palette()[c(1,6)],bty='n')

legend('topleft',legend = 'LUC Control Ambient',bty='n')

dev.off()