day.lag <- 3
source('r/pace_data_process.R')
source('r/ym_data_process.R')



# process ym data

ym.18.df <- get.ym.func(18)
ym.con.df.sum <- get.pace.func(ym.18.df,
                               species.in='ym',
                               prep.in = 'Control',
                               temp.in ='Ambient',
                               subplot = NA,
                               norm.min.max = NULL)
ym.con.df.sum <- ym.con.df.sum[,c('Date','GCC.norm.smooth',
                                  'GCC.norm.sd','vwc')]

names(ym.con.df.sum) <- c("Date","GCC.mean.con",
                          'GCC.sd.con',"vwc.mean.con")


ym.drought.df <- get.ym.func('Drought')
ym.drought.df.sum <- get.pace.func(ym.drought.df,
                                   species.in='ym',
                                   prep.in = 'Drought',
                                   temp.in ='Ambient',
                                   subplot = NA,
                                   norm.min.max = NULL)
ym.drought.df.sum <- ym.drought.df.sum[,c('Date','GCC.norm.smooth',
                                  'GCC.norm.sd','vwc')]
names(ym.drought.df.sum) <- c("Date","GCC.mean.drt",
                              'GCC.sd.drt',"vwc.mean.drt")

ym.both.df <- merge(ym.con.df.sum,ym.drought.df.sum,
                    by = c('Date'))
# 
plot(GCC.mean.drt~Date,data = ym.both.df,type='l',col='red')
points(GCC.mean.con~Date,data = ym.both.df,type='l',col='navy')
# 
plot(vwc.mean.drt~Date,data = ym.both.df,type='l',col='red')
points(vwc.mean.con~Date,data = ym.both.df,type='l',col='navy')
# 
daily.reduction <- with(ym.both.df,GCC.mean.drt/GCC.mean.con)
hist(daily.reduction,freq = F)
# 
mean.annual.reduction <- with(ym.both.df,sum(GCC.mean.drt) / sum(GCC.mean.con))
mean.annual.reduction.vwc <- with(ym.both.df,sum(vwc.mean.drt) / sum(vwc.mean.con))
# pace data
pace.ls <- list()
species.vec <- c('Bis','Luc','Dig','Kan','Rho','Fes','Rye','YM')
for (i in seq_along(species.vec)) {
  fn.con <- sprintf('tmp/pred.smv13.2qchain.%s.Control.Ambient.rds',
                    species.vec[i])
  # dat.con <- get.pace.func(gcc.met.pace.df,
  #                          species.in=species.vec[i],
  #                          prep.in = 'Control',
  #                          temp.in ='Ambient',
  #                          subplot = NA,
  #                          norm.min.max = NULL)
  
  dat.con <- readRDS(fn.con)
  dat.con <- dat.con[,c('Date','GCC.norm.smooth',
                        'GCC.norm.sd','vwc','cover.hufken','Rain')]
  names(dat.con) <- c("Date","GCC.mean.con",
                      'GCC.sd.con',"vwc.mean.con",'cover.pred.con','rain.con')
  
  # dat.drought <- get.pace.func(gcc.met.pace.df,
  #                              species.in=species.vec[i],
  #                              prep.in = 'Drought',
  #                              temp.in ='Ambient',
  #                              subplot = NA,
  #                              norm.min.max = NULL)
  fn.drt <- sprintf('tmp/pred.smv13.2qchain.%s.Control.predict.Ambient.rds',
                    species.vec[i])
  dat.drought <- readRDS(fn.drt)
  dat.drought <- dat.drought[,c('Date','GCC.norm.smooth',
                        'GCC.norm.sd','vwc','cover.hufken','Rain')]
  names(dat.drought) <- c("Date","GCC.mean.drt",
                      'GCC.sd.drt',"vwc.mean.drt",'cover.pred.drt','rain.drt')
  
  dat.both.df <- merge(dat.con,dat.drought,
                      by = c('Date'))
  
  dat.both.df$daily.reduction.obs <- with(dat.both.df,
                                          GCC.mean.drt/GCC.mean.con)
  dat.both.df$daily.reduction.pred <- with(dat.both.df,
                                           cover.pred.drt/cover.pred.con)
  
  dat.both.df$mean.annual.reduction.obs <- with(dat.both.df,
                                                sum(GCC.mean.drt,na.rm=T) /
                                                  sum(GCC.mean.con,na.rm=T))
  dat.both.df$mean.annual.reduction.ped <- with(dat.both.df,
                                                sum(cover.pred.drt,na.rm=T) / 
                                                  sum(cover.pred.con,na.rm=T))
  dat.both.df$mean.annual.reduction.rain <- with(dat.both.df,
                                                sum(rain.drt,na.rm=T) / 
                                                  sum(rain.con,na.rm=T))
  
  dat.both.df$spc <- species.vec[i] 
  
  pace.ls[[i]] <-  dat.both.df

}

reduction.obs <- reduction.peds <- reduction.rain  <- c()
for (i in 1:9) {
reduction.obs[i] <- (unique(pace.ls[[i]]$mean.annual.reduction.obs))  
reduction.peds[i] <-(unique(pace.ls[[i]]$mean.annual.reduction.ped))  
reduction.rain[i] <- (unique(pace.ls[[i]]$mean.annual.reduction.rain))  

}
reduction.vwc <- c()
for (i in 1:8) {
 tmp.df <-pace.ls[[i]] 
 reduction.vwc[i] <- with(tmp.df,sum(vwc.mean.drt,na.rm=T) / sum(vwc.mean.con))
}

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END
png('figures/mod.obs.drought.png',width =600,height = 600*.618)
par(mar=c(5,5,1,1))
barplot(reduction.obs,col=t_col('black',50),border = F,
        names.arg = species.vec,
        ylab='Annual reduction in cover')

barplot(reduction.peds,add=T,col=t_col('pink',50),border = F)

legend('topleft',legend = c('OBS','MOD'),bty='n',col=c(t_col('black',50),
                                               t_col('pink',50)),
       pch=15)
dev.off()

