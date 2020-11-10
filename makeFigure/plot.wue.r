# read data sets
day.lag <- 1
source('r/pace_data_process.R')
source('r/ym_data_process.R')
ym.18.df <- get.ym.func(18)

# function to get wue
# assumes 300mm rooting depth
# uses monthly change in swc and smoothed and normalised GCC
get.wue.func <- function(gcc.met.pace.df.luc){
  library(doBy)
  gcc.met.pace.df.luc <- gcc.met.pace.df.luc[order(gcc.met.pace.df.luc$Date),]
  gcc.met.pace.df.luc$delta.gcc <- c(0,diff(gcc.met.pace.df.luc$GCC.norm.smooth))
  gcc.met.pace.df.luc$delta.vwc <- c(0,diff(gcc.met.pace.df.luc$vwc))
  
  # sum by month
  gcc.met.pace.df.luc$mon.year <- factor(format(gcc.met.pace.df.luc$Date,'%Y-%m'))
  
  gcc.met.pace.df.luc.sum <- summaryBy(delta.gcc + delta.vwc + Rain~mon.year,data = gcc.met.pace.df.luc,
                                       FUN=sum,na.rm=T,keep.names = T)
  # get water use
  gcc.met.pace.df.luc.sum$water.use <- -(gcc.met.pace.df.luc.sum$delta.vwc * 300 - gcc.met.pace.df.luc.sum$Rain)
  # get wue
  gcc.met.pace.df.luc.sum$wue <- gcc.met.pace.df.luc.sum$delta.gcc / gcc.met.pace.df.luc.sum$water.use
  
  return(gcc.met.pace.df.luc.sum)
}

# get it for luc
gcc.met.pace.df.luc <- get.pace.func(gcc.met.pace.df,
                                    species.in ='Luc',
                                    prep.in = 'Control',
                                    temp.in ='Ambient',
                                    subplot = NA)
gcc.met.pace.df.luc.sum <- get.wue.func(gcc.met.pace.df.luc)

gcc.met.pace.df.rye <- get.pace.func(gcc.met.pace.df,
                                     species.in ='Rye',
                                     prep.in = 'Control',
                                     temp.in ='Ambient',
                                     subplot = NA)
gcc.met.pace.df.rye.sum <- get.wue.func(gcc.met.pace.df.rye)

# get ym 18 
gcc.met.pace.df.ym18 <- get.pace.func(ym.18.df,
                                     species.in ='ym',
                                     prep.in = 'Control',
                                     temp.in ='Ambient',
                                     subplot = NA)
gcc.met.pace.df.ym18.sum <- get.wue.func(gcc.met.pace.df.ym18)

pdf('figures/wue.pdf',width = 6,height = 6*3*.617)
par(mfrow=c(3,1),mar=c(3,5,3,1))
plot(wue~mon.year,data = gcc.met.pace.df.luc.sum,main='Luc',xlab='')
abline(h=0,lty='dashed',col='grey',lwd=3)

plot(wue~mon.year,data = gcc.met.pace.df.rye.sum,main='Rye',xlab='')
abline(h=0,lty='dashed',col='grey',lwd=3)

plot(wue~mon.year,data = gcc.met.pace.df.ym18.sum,main='YM 18 control',xlab='')
abline(h=0,lty='dashed',col='grey',lwd=3)
dev.off()
# 
# 
# gcc.met.pace.df.luc
# gcc.met.pace.df.ym18 <- get.pace.func(ym.18.df,
#                                      species.in ='ym',
#                                      prep.in = 'Control',
#                                      temp.in ='Ambient',
#                                      subplot = NA)
# gcc.met.pace.df.ym18$delta.gcc <- c(0,diff(gcc.met.pace.df.ym18$GCC.norm.smooth))
# gcc.met.pace.df.ym18$delta.swc <- c(0,diff(gcc.met.pace.df.ym18$vwc))
# gcc.met.pace.df.ym18$wue <- gcc.met.pace.df.ym18$delta.gcc / gcc.met.pace.df.ym18$delta.swc

plot(wue~mon.year,data = gcc.met.pace.df.luc.sum)
abline(h=0,lty='dashed',col='grey',lwd=3)
