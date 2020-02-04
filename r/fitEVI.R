evi.ym.df <- readRDS('cache/ym20012016.rds')
# ####
met <- readRDS('cache/met.ym20012016.rds')
met$rad[met$rad<0] <- NA
met$rad <- na.locf(met$rad)
met$PPFD <- met$rad * 10^6 * 4.57 / 3600 / 24

gcc.met.pace.df.16 <- merge(evi.ym.df[,c('Date','evi')],met,all=T)

# gcc.met.pace.df.16$evi <- na.fill(gcc.met.pace.df.16$evi,fill="extend")

gcc.met.pace.df.16$GCC.norm <- (gcc.met.pace.df.16$evi - min(gcc.met.pace.df.16$evi,na.rm = T)) / 
  (max(gcc.met.pace.df.16$evi,na.rm = T) - min(gcc.met.pace.df.16$evi,na.rm = T))

gcc.met.pace.df.16$evi[1:15] <- NA
gcc.met.pace.df.16$vwc <- 0.15
# gcc.met.pace.df.16$vwc[1:17] <- 0.15
gcc.met.pace.df.16$map=800
gcc.met.pace.df.16$Tair = gcc.met.pace.df.16$tmean
gcc.met.pace.df.16$Tmax = gcc.met.pace.df.16$tmax
gcc.met.pace.df.16$Tmin = gcc.met.pace.df.16$tmin
gcc.met.pace.df.16$u2=2
gcc.met.pace.df.16$Rain <- gcc.met.pace.df.16$rain

gcc.met.pace.df.16$RHmax <- 95
# With 
b=17.625  
c=243.04
gcc.met.pace.df.16$RHmin <- 100 * exp(c*b*(gcc.met.pace.df.16$tmin - gcc.met.pace.df.16$tmax) /
                                        (c+gcc.met.pace.df.16$tmax) / (c + gcc.met.pace.df.16$tmin))

gcc.met.pace.df.16$PPFD <- gcc.met.pace.df.16$rad

gcc.met.pace.df.16$GCC.norm.sd <- sd(gcc.met.pace.df.16$GCC.norm,na.rm=TRUE)


# # para values####
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
EVIchain = mh.MCMC.func(10000,par.df,gcc.met.pace.df.16)

saveRDS(EVIchain,'cache/EVIchain.rds')
EVIchain <- readRDS('cache/EVIchain.rds')

# check fitting
hist(EVIchain[1000:10000,4])

# see how it works#####
burnIn = 1000
par.df["fit",] <- colMeans(EVIchain[burnIn:nrow(EVIchain),])
bucket.size <- 200
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
     xlab=' ',ylab=expression(f[cover]),ylim=c(0,0.8),col = palette()[6])

points(cover.hufken~Date,data = hufken.pace.pred,type='b',col=palette()[8],pch=16)

par(new=T)

plot(vwc~Date,data = hufken.pace.pred,ann=F,axes=F,type='s',col=palette()[3])

par(new=T)

plot(swc.hufken~Date,data = hufken.pace.pred,ann=F,axes=F,type='l',lty='dashed',col=palette()[4])

legend('bottomright',legend = c('MOD','OBS'),pch=16,col=palette()[c(8,6)],bty='n',title = 'Plant cover')

legend('bottom',legend = c('MOD','OBS'),lty=c('dashed','solid'),col=palette()[c(4,3)],bty='n',title = 'Soil moisture')


r.value <- summary(lm(cover~cover.hufken,data = hufken.pace.pred))$r.squared


