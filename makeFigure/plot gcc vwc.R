find.past.ppt.func <- function(ng.day.df,days.past){
  ng.day.df$ppt.past <- NA
  for(i in 1:nrow(ng.day.df)){
    
    ng.day.df$ppt.past[i] <- sum(ng.day.df$irrig.tot[ng.day.df$Date %in% 
                                                         seq(ng.day.df$Date[i] - days.past,ng.day.df$Date[i],by='day')],
                                 na.rm=TRUE)
  }
  return(ng.day.df$ppt.past)
}

days.rained.func <- function(plot.df){
  
  raindates <- plot.df$Date[plot.df$irrig.tot > 0]
  
  days.since.rain <- c()
  for(i in 1:nrow(plot.df)){
    
    days.since.rain[i] <- as.numeric(min(abs(plot.df$Date[i] - raindates)))
  }
  return(days.since.rain)
}

gcc.met.pace.df.in <- readRDS('cache/gcc.met.pace.df.rds')
gcc.met.pace.df <- 
  gcc.met.pace.df.in[with(gcc.met.pace.df.in,order(Species,Precipitation,Temperature)),]

tmp.df = readRDS('cache/pace.gcc.2018.rds')
tmp.df.1 = readRDS('cache/pace.gcc.2019.rds')
gcc.met.pace.df <- rbind(tmp.df,tmp.df.1)
gcc.met.pace.df$GCC <- gcc.met.pace.df$G / (gcc.met.pace.df$R + gcc.met.pace.df$G + gcc.met.pace.df$B)
gcc.met.pace.df$Date <- gcc.met.pace.df$DateTime
# gcc.met.pace.df$SubplotID <- droplevels(gcc.met.pace.df$SubplotID)
# gcc.met.pace.df$SubplotID <- as.character(gcc.met.pace.df$SubplotID )

subplot.vec <- unique(gcc.met.pace.df$SubplotID)

# # gcc.met.pace.df
# plot(Tair~Date,data = gcc.met.pace.df)
# 
# plot(Tmax~Date,data = gcc.met.pace.df)
# 
# plot(Tmin~Date,data = gcc.met.pace.df)
# plot(par~Date,data = gcc.met.pace.df)

library(tidyr)
# library(WaveletComp)
library(lubridate)

# plot each plot####
pdf('pace_gcc_vwc.pdf',width = 12,height = 12*0.618)
par(mfrow = c(2,2))
for(i in seq_along(subplot.vec)){

  plot.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == subplot.vec[i],]
  
  plot.df <- complete(plot.df,GCC)
  plot.df <- plot.df[order(plot.df$Date),]
  plot.df$Species <- droplevels(plot.df$Species)
  
  wave.fit <- analyze.wavelet(plot.df,my.series = 'GCC',loess.span = 0.2)
  
  plot(GCC~Date,data = plot.df,pch=16,col = 'darkseagreen')
  points(wave.fit$series$GCC.trend~plot.df$Date,type='l',col='green',lwd=3)
  
  if(!all(is.na(plot.df$vwc))){
    par(new=TRUE)
    plot(vwc~Date,data = plot.df,type='s',col='lightskyblue',ann=FALSE,axes=F)
    axis(4,at = seq(0,0.2,by=0.05),labels = seq(0,0.2,by=0.05))
    mtext('VWC',side = 4,line = -1,col='lightskyblue')
  }else{
    par(new=TRUE)
    plot(irrigsum~Date,data = plot.df,type='s',col='lightskyblue',ann=FALSE,axes=F)
    axis(4,at = seq(0,20,by=5),labels = seq(0,20,by=5))
    mtext('Rain (mm/day)',side = 4,line = -1,col='lightskyblue')
  }
  
  legend('topleft',legend = paste0(subplot.vec[i],'-',levels(plot.df$Species),
                                   '-',unique(plot.df$Precipitation),'-',unique(plot.df$Temperature)),bty='n')
  
}

dev.off()
# plot diff####

pdf('pace_gcc_diff_vwc.pdf',width = 8,height = 2*8*0.618)
par(mfrow = c(2,1))
# for(i in seq_along(subplot.vec)){
  
  # plot.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == subplot.vec[i],]
  plot.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S5P4D',]
  # plot.df <- plot.df[plot.df$Location == 'Lower',]

  plot.df <- complete(plot.df,GCC)
  plot.df <- plot.df[order(plot.df$Date),]
  plot.df$Species <- droplevels(plot.df$Species)
  plot.df <- plot.df[complete.cases(plot.df),]
  plot.df$doy <- yday(plot.df$Date)
  plot.df$growth <- c(0,diff(plot.df$GCC))
  
  plot.df$rain.past <- find.past.ppt.func(plot.df,10)
  plot.df$day.since.rain <- days.rained.func(plot.df)
  # gam.fit <- gam(growth~s(doy,k=100),data = plot.df)
  
  # plot(gam.fit$fitted.values)
 
  
  plot.df$g.d <- NA
  plot.df$g.d[plot.df$growth>0] <- 1
  plot.df$g.d[plot.df$growth<=0] <- 0
  palette(c('red','darkseagreen'))
  plot(growth~vwc,data = plot.df[plot.df$g.d == 1,],pch=16,col = 2,ylim=c(-0.02,0.02))
  points(growth~vwc,data = plot.df[plot.df$g.d == 0,],pch=16,col = 1,ylim=c(-0.02,0.02))
  
  # abline(v=plot.df$Date[plot.df$rain.past>0],lty='dashed',col='grey')
  # # points(wave.fit$series$GCC.trend~plot.df$Date,type='l',col='green',lwd=3)
  # points(wave.fit$series$GCC.trend~plot.df$Date,type='l',col='green',lwd=3)
  # wave.fit$fitted.values
  
  # summary(lm(growth~rain.past,data = plot.df))
  
  # plot(growth~rain.past,data = plot.df[plot.df$g.d == 1,],pch=16,col = 2,ylim=c(-0.019,0.019))
  # points(growth~rain.past,data = plot.df[plot.df$g.d == 0,],pch=16,col = 1,ylim=c(-0.019,0.019))
  
  plot(growth~day.since.rain,data = plot.df[plot.df$g.d == 1,],pch=16,col = 2,ylim=c(-0.02,0.02))
  points(growth~day.since.rain,data = plot.df[plot.df$g.d == 0,],pch=16,col = 1,ylim=c(-0.02,0.02))
  
  
  # plot(growth~Date,data = plot.df[plot.df$g.d == 1,],pch=16,col = 2,ylim=c(-0.013,0.013))
  # points(growth~Date,data = plot.df[plot.df$g.d == 0,],pch=16,col = 1,ylim=c(-0.013,0.013))
  
  # abline(v=plot.df$Date[plot.df$irrig.tot>0],lty='dashed',col='grey')
#   if(!all(is.na(plot.df$vwc))){
#     par(new=TRUE)
#     plot(vwc~Date,data = plot.df,type='s',col='lightskyblue',ann=FALSE,axes=F)
#     axis(4,at = seq(0,0.2,by=0.05),labels = seq(0,0.2,by=0.05))
#     mtext('VWC',side = 4,line = -1,col='lightskyblue')
#   }else{
#     par(new=TRUE)
#     plot(irrigsum~Date,data = plot.df,type='s',col='lightskyblue',ann=FALSE,axes=F)
#     axis(4,at = seq(0,20,by=5),labels = seq(0,20,by=5))
#     mtext('Rain (mm/day)',side = 4,line = -1,col='lightskyblue')
#   }
#   
#   legend('topleft',legend = paste0(subplot.vec[i],'-',levels(plot.df$Species),
#                                    '-',unique(plot.df$Precipitation),'-',unique(plot.df$Temperature)),bty='n')
#   
# }

dev.off()
# #################################################################
library(doBy)
gcc.met.pace.df.species <- summaryBy(GCC + vwc + irrigsum ~ Species + Date + Precipitation + Temperature,
                                     data = gcc.met.pace.df,FUN=c(mean),na.rm=TRUE,keep.names = T)

gcc.met.pace.df.species <- gcc.met.pace.df.species[!is.na(gcc.met.pace.df.species$Species),]

species.vec <- unique(gcc.met.pace.df.species$Species)

plot.func <- function(cWaT.df){
  
  wave.fit.cWaT <- analyze.wavelet(cWaT.df,my.series = 'GCC',loess.span = 0.2)
  
  plot(GCC~Date,data = cWaT.df,pch=16,col = 'grey',xlab='2018')
  points(wave.fit.cWaT$series$GCC.trend~cWaT.df$Date,type='l',col='black',lwd=3)
  
  if(!all(is.na(cWaT.df$vwc))){
    par(new=TRUE)
    plot(vwc~Date,data = cWaT.df,type='s',col='lightskyblue',ann=FALSE,axes=F)
    axis(4,at = seq(0,0.2,by=0.05),labels = seq(0,0.2,by=0.05))
    mtext('VWC',side = 4,line = -1,col='lightskyblue')
  }else{
    par(new=TRUE)
    plot(irrigsum~Date,data = cWaT.df,type='s',col='lightskyblue',ann=FALSE,axes=F)
    axis(4,at = seq(0,20,by=5),labels = seq(0,20,by=5))
    mtext('Rain (mm/day)',side = 4,line = -1,col='lightskyblue')
  }
}


pdf('pace_treat.pdf',width = 12,height = 12*0.618)

for(i in seq_along(species.vec)){
  
  plot.df <- gcc.met.pace.df.species[gcc.met.pace.df.species$Species == species.vec[i],]
  
  plot.df <- complete(plot.df,GCC)
  plot.df <- plot.df[order(plot.df$Date),]
  plot.df$Species <- droplevels(plot.df$Species)
  
  cWaT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Ambient',]
  dWaT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Ambient',]
  
  cWeT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Elevated',]
  dWeT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Elevated',]
  
  # wave.fit.cWaT <- analyze.wavelet(cWaT.df,my.series = 'GCC.mean',loess.span = 0.2)
  # wave.fit.dWaT <- analyze.wavelet(dWaT.df,my.series = 'GCC.mean',loess.span = 0.2)
  
  par(mfrow = c(2,2))
  
  plot.func(cWaT.df)
  legend('topleft',legend = 'cWaT',bty='n')
  plot.func(dWaT.df)
  legend('topleft',legend = 'dWaT',bty='n')
  
  if(nrow(cWeT.df) > 0){
    plot.func(cWeT.df)
    legend('topleft',legend = 'cWeT',bty='n')
    plot.func(dWeT.df)
    legend('topleft',legend = 'dWeT',bty='n')
    
  }else{
    plot(1,col='white',ann=F,axes = F)
    plot(1,col='white',ann=F,axes = F)
  }
  par(mfrow=c(1,1))
  par(new=TRUE)
  plot(1,col='white',ann=F,axes = F)
  title(species.vec[i])
  
  
  
  
  # points(wave.fit.cWaT$series$GCC.mean.trend~cWaT.df$Date,type='l',col='navy',lwd=3)
  # points(wave.fit.dWaT$series$GCC.mean.trend~dWaT.df$Date,type='l',col='gold',lwd=3)
  # 
  # if(nrow(cWeT.df) !=0){
  #   wave.fit.cWeT <- analyze.wavelet(cWeT.df,my.series = 'GCC.mean',loess.span = 0.2)
  #   wave.fit.dWeT <- analyze.wavelet(dWeT.df,my.series = 'GCC.mean',loess.span = 0.2)
  #   # darkseagreenCC
  #   points(GCC.mean~Date,data = cWeT.df,pch=15,col = 'darkseagreen')
  #   points(GCC.mean~Date,data = dWeT.df,pch=15,col = 'red')
  #   
  #   points(wave.fit.cWeT$series$GCC.mean.trend~cWeT.df$Date,type='l',col='green',lwd=3,lty='dashed')
  #   points(wave.fit.dWeT$series$GCC.mean.trend~dWeT.df$Date,type='l',col='coral',lwd=3,lty='dashed')
  # }
  # 
  # legend('topleft',legend = c('cWaT','dWaT','cWeT','dWeT'),pch=c(16,16,15,15),
  #        col=c('navy','gold','green','red'))
  # 
  # # title( species.vec[i])

}
  
dev.off()

# plot by spec and treat#########################################################################
library(doBy)
library(mgcv)
library(lubridate)
gcc.met.pace.df.st <- summaryBy(GCC ~ Species + Date + Precipitation + Temperature + SubplotID,
                                data = gcc.met.pace.df,FUN=c(mean),na.rm=TRUE,keep.names = T)
gcc.met.pace.df.st=gcc.met.pace.df.st[!is.na(gcc.met.pace.df.st$Date),]
gcc.met.pace.df.st$plot.factor <- as.factor(gcc.met.pace.df.st$SubplotID )
species.vec <- unique(gcc.met.pace.df.st$Species)

plot.st.func <- function(cWaT.df){
  
  # wave.fit.cWaT <- gam(GCC~s(yday(Date),k=20),data = cWaT.df)
  # plot(GCC~Date,data = cWaT.df,col = 'grey',xlab='2018',pch=paste0(facto.vec[plot.factor]))
  # points(wave.fit.cWaT$fitted.values~cWaT.df$Date,type='l',col='black',lwd=3)
  cWaT.df = cWaT.df[!is.na(cWaT.df$GCC),]
  cWaT.df$plot.factor = droplevels(cWaT.df$plot.factor)
  plots.vec <- unique(cWaT.df$plot.factor)
  y.min <- floor(min(cWaT.df$GCC,na.rm=T)*1000)/1000
  y.max <- ceiling(max(cWaT.df$GCC,na.rm=T)*1000)/1000
  
  for(plot.num in seq_along(plots.vec)){
    plot.df <- cWaT.df[cWaT.df$plot.factor == plots.vec[plot.num],]
    plot.df = plot.df[!is.na(plot.df$GCC),]
    plot.df=plot.df[order(plot.df$Date),]
    plot.df$time.d = as.numeric(row.names(plot.df))
    wave.fit.cWaT <- gam(GCC~s(time.d,k=nrow(plot.df)/3),data = plot.df)
    if(plot.num == 1){
      
      plot(GCC~Date,data = plot.df,col = plot.num,xlab=' ',
           pch=paste0(facto.vec[plot.factor]),
           cex=0.7,ylim=c(y.min,y.max))
      points(wave.fit.cWaT$fitted.values~plot.df$Date,type='l',col=plot.num,lwd=2)
   
       }else{
      points(GCC~Date,data = plot.df,col = plot.num,pch=paste0(facto.vec[plot.factor]),cex=0.7)
      points(wave.fit.cWaT$fitted.values~plot.df$Date,type='l',col=plot.num,lwd=2)
    }
  }
  legend('topleft',legend = paste0(unique(cWaT.df$Species),'-',
                                   unique(cWaT.df$Precipitation),'-',
                                   unique(cWaT.df$Temperature)),bty='n')
}

facto.vec <- rep(1:6)

pdf('figures/pace_gcc_species_treat.pdf',width = 12,height = 12*0.618)
library("viridisLite") 
library(survival)
palette(viridis(6))
par(mar=c(3,5,1,1))

for(i in seq_along(species.vec)){
  
  plot.df <- gcc.met.pace.df.st[gcc.met.pace.df.st$Species == species.vec[i],]
  
  # plot.df <- complete(plot.df,GCC)
  plot.df <- plot.df[order(plot.df$Date),]
  plot.df$Species <- droplevels(plot.df$Species)
  
  cWaT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Ambient',]
  dWaT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Ambient',]
  
  cWeT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Elevated',]
  dWeT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Elevated',]
  
  par(mfrow = c(2,2))
  
  plot.st.func(cWaT.df)
  # legend('topleft',legend = 'cWaT',bty='n')
  plot.st.func(dWaT.df)
  # legend('topleft',legend = 'dWaT',bty='n')
  
  if(nrow(cWeT.df) > 0){
    plot.st.func(cWeT.df)
    # legend('topleft',legend = 'cWeT',bty='n')
    plot.st.func(dWeT.df)
    # legend('topleft',legend = 'dWeT',bty='n')
    
  }else{
    plot(1,col='white',ann=F,axes = F)
    plot(1,col='white',ann=F,axes = F)
  }
}

dev.off()

# #####################
# vwc.df <- gcc.met.pace.df.st[]
pdf('pace_vwc_species_treat.pdf',width = 12,height = 12*0.618)
library("viridisLite")  
palette(viridis(6))
par(mar=c(3,5,1,1))

plot(GCC~Date,data = cWaT.df[cWaT.df$SubplotID == 'S3P3B' &
                               month(cWaT.df$Date) %in% 2:6,])

abline(v = as.Date('2019-4-5'),col='lightgrey')
for(i in seq_along(species.vec)){
  
  plot.df <- gcc.met.pace.df.st[gcc.met.pace.df.st$Species == species.vec[i],]
  
  # plot.df <- complete(plot.df,GCC)
  plot.df <- plot.df[order(plot.df$Date),]
  plot.df$Species <- droplevels(plot.df$Species)
  
  cWaT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Ambient',]
  dWaT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Ambient',]
  
  cWeT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Elevated',]
  dWeT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Elevated',]
  
  par(mfrow = c(2,2))
  
  plot.st.func(cWaT.df)
  # legend('topleft',legend = 'cWaT',bty='n')
  plot.st.func(dWaT.df)
  # legend('topleft',legend = 'dWaT',bty='n')
  
  if(nrow(cWeT.df) > 0){
    plot.st.func(cWeT.df)
    # legend('topleft',legend = 'cWeT',bty='n')
    plot.st.func(dWeT.df)
    # legend('topleft',legend = 'dWeT',bty='n')
    
  }else{
    plot(1,col='white',ann=F,axes = F)
    plot(1,col='white',ann=F,axes = F)
  }
}

dev.off()


# by treat with color by cover #################################################################
library(doBy)
gcc.met.pace.df.species <- summaryBy(GCC.norm + vwc + irrigsum ~ Species + Date + Precipitation + Temperature,
                                     data = gcc.met.pace.df,FUN=c(mean),na.rm=TRUE,keep.names = T)
gcc.met.pace.df.species$gcc.factor <- cut(gcc.met.pace.df.species$GCC.norm,breaks = seq(0,1,by=0.2),
                                          labels = paste0('<',seq(0.2,1,by=0.2)))
gcc.met.pace.df.species <- gcc.met.pace.df.species[!is.na(gcc.met.pace.df.species$Species),]

species.vec <- unique(gcc.met.pace.df.species$Species)

plot.func <- function(cWaT.df){
  
  # wave.fit.cWaT <- analyze.wavelet(cWaT.df,my.series = 'GCC',loess.span = 0.2)
  # plot(GCC.norm~Date,data = cWaT.df,type='p',col = gcc.factor,xlab=' ')
  plot(gcc.change~Date,data = cWaT.df,type='p',pch=16,col = gcc.factor,xlab=' ')
  abline(h=0,col='grey',lty='dashed')
  # points(wave.fit.cWaT$series$GCC.trend~cWaT.df$Date,type='l',col='black',lwd=3)
  legend('top',legend = levels(cWaT.df$gcc.factor),pch=16,col=palette(),bty='n',horiz = T)
  if(!all(is.na(cWaT.df$vwc))){
    par(new=TRUE)
    plot(vwc~Date,data = cWaT.df,type='s',col='lightskyblue',ann=FALSE,axes=F)
    axis(4,at = seq(0,0.2,by=0.05),labels = seq(0,0.2,by=0.05))
    mtext('VWC',side = 4,line = -1,col='lightskyblue')
  }else{
    par(new=TRUE)
    plot(irrigsum~Date,data = cWaT.df,type='s',col='lightskyblue',ann=FALSE,axes=F)
    axis(4,at = seq(0,20,by=5),labels = seq(0,20,by=5))
    mtext('Rain (mm/day)',side = 4,line = -1,col='lightskyblue')
  }
}


pdf('pace_treat_coverColor.pdf',width = 12,height = 12*0.618)
# library("viridisLite")  
# palette(viridis(5))
palette(c('brown','coral','green','lightseagreen','darkseagreen'))
for(i in seq_along(species.vec)){
  
  plot.df <- gcc.met.pace.df.species[gcc.met.pace.df.species$Species == species.vec[i],]
  
  # plot.df <- complete(plot.df,GCC.norm)
  plot.df <- plot.df[order(plot.df$Date),]
  plot.df$Species <- droplevels(plot.df$Species)
  
  cWaT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Ambient',]
  cWaT.df$gcc.change <- c(0,diff(cWaT.df$GCC.norm))
  dWaT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Ambient',]
  dWaT.df$gcc.change <- c(0,diff(dWaT.df$GCC.norm))
  cWeT.df <- plot.df[plot.df$Precipitation == 'Control' & plot.df$Temperature == 'Elevated',]
  if(nrow(cWeT.df)>0){cWeT.df$gcc.change <- c(0,diff(cWeT.df$GCC.norm))}
  dWeT.df <- plot.df[plot.df$Precipitation == 'Drought' & plot.df$Temperature == 'Elevated',]
  if(nrow(dWeT.df)>0){dWeT.df$gcc.change <- c(0,diff(dWeT.df$GCC.norm))}
  # wave.fit.cWaT <- analyze.wavelet(cWaT.df,my.series = 'GCC.mean',loess.span = 0.2)
  # wave.fit.dWaT <- analyze.wavelet(dWaT.df,my.series = 'GCC.mean',loess.span = 0.2)
  
  par(mfrow = c(2,2))
  
  plot.func(cWaT.df)
  legend('topleft',legend = 'cWaT',bty='n')
  plot.func(dWaT.df)
  legend('topleft',legend = 'dWaT',bty='n')
  
  if(nrow(cWeT.df) > 0){
    plot.func(cWeT.df)
    legend('topleft',legend = 'cWeT',bty='n')
    plot.func(dWeT.df)
    legend('topleft',legend = 'dWeT',bty='n')
    
  }else{
    plot(1,col='white',ann=F,axes = F)
    plot(1,col='white',ann=F,axes = F)
  }
  par(mfrow=c(1,1))
  par(new=TRUE)
  plot(1,col='white',ann=F,axes = F)
  title(species.vec[i])
  
}

dev.off()

# check swc and irrig####


pdf('pace_rain_vwc_subplot.pdf',width = 12,height = 12*0.618)
par(mfrow = c(1,1))
palette(c('red','navy'))
for(i in seq_along(subplot.vec)){
  
  plot.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == subplot.vec[i],]
  
  if(sum(plot.df$vwc,na.rm=T) > 0){
  plot.df <- plot.df[order(plot.df$Date),]
  plot.df$Species <- droplevels(plot.df$Species)
  # 
  
  plot(vwc~Date,data = plot.df,pch=16,col = Location)
  
  par(new=TRUE)
  plot(irrigsum~Date,data = plot.df,type='s',col='lightskyblue',ann=FALSE,axes=F)
  axis(4,at = seq(0,20,by=5),labels = seq(0,20,by=5))
  mtext('Rain (mm/day)',side = 4,line = -1,col='lightskyblue')
  
  legend('topleft',legend = paste0(subplot.vec[i],'-',levels(plot.df$Species),
                                   '-',unique(plot.df$Precipitation),'-',unique(plot.df$Temperature)),bty='n')
  
  abline(v = plot.df$Date[plot.df$irrigsum > 0],lty='dashed',col='grey')
  }
}

dev.off()
