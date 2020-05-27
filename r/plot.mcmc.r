plot.mcmc.func = function(species.in,prep.in,temp.in){
  gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
                                      species.in = species.in,
                                      prep.in = prep.in,
                                      temp.in =temp.in)
  gcc.met.pace.df.16 <- gcc.met.pace.df.16[(gcc.met.pace.df.16$Date) < as.Date('2019-11-26'),]
  gcc.met.pace.df.16$map <- 760
  fn=paste0('cache/chain.',species.in,'.',prep.in,'.',temp.in,'.rds')
  # chain.fes <- readRDS('cache/chain.Rye.Control.Ambient.rds')
  # 
  chain.fes <- readRDS(fn)
  # # check acceptance so that the 
  burnIn = 10000
  # acceptance = 1-mean(duplicated(chain.fes[-(1:burnIn),])) #should be >20% but <60%; 20-25% were suggested
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
  # par.df["fit",] <- colMeans(luc.d.a.df[burnIn:nrow(luc.d.a.df),])
  # 
  bucket.size = 300
  hufken.pace.pred <- phenoGrass.func.v11(gcc.met.pace.df.16,
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
  par(mfrow=c(1,2))
  plot(cover~Date,data = hufken.pace.pred,type='l',#pch=16,
       xlab=' ',ylab=expression(f[cover]),ylim=c(0,0.8),col = palette()[6])
  
  points(cover.hufken~Date,data = hufken.pace.pred,type='l',col=palette()[8])
  # 
  legend('topleft',legend = paste0(species.in,prep.in,temp.in),bty='n')
  
  plot(cover~cover.hufken,data = hufken.pace.pred,pch=16,col='grey',
       xlab='MOD',ylab='OBS')
  abline(a=0,b=1)
}

pdf('PACE.V11.pdf',width = 8,height =  8*0.618)

# plot.mcmc.func('Rye','Control','Ambient')
plot.mcmc.func('Luc','Control','Ambient')
plot.mcmc.func('Fes','Control','Ambient')

# plot.mcmc.func('Luc','Drought','Ambient')
# plot.mcmc.func('Fes','Drought','Ambient')

dev.off()

