library(zoo)
library(mgcv)
ym.18.df.smoothed <-  readRDS('tmp/pred.smv13.2qchain.Luc.Control.Ambient.rds')
ym.18.df.smoothed$gcc.change <- c(0,diff(ym.18.df.smoothed$GCC.norm.smooth))
ym.18.df.smoothed$week <- format(ym.18.df.smoothed$Date,'%Y-%W')

ym.18.df.smoothed$swc
library(doBy)



# 

ym.18.df.smoothed$fornight <- NA
for(i in 15:nrow(ym.18.df.smoothed)){
  # ym.18.df.smoothed$swc.past15[i] <- mean(ym.18.df.smoothed$swc[(i-14):i],na.rm=TRUE)
  
  ym.18.df.smoothed$fornight[i] <- floor(i / 14)
}

# ym.18.df.smoothed$swc.past15 <- NA
# ym.18.df.smoothed$fornight <- NA
# for(i in unique(ym.18.df.smoothed$fornight)){
#   # ym.18.df.smoothed$swc.past15[i] <- mean(ym.18.df.smoothed$swc[(i-14):i],na.rm=TRUE)
#   
#   ym.18.df.smoothed$fornight[i] <- floor(i / 14)
# }

# library(data.table)
# 
# sum.df <- data.table(ym.18.df.smoothed)[,list(gcc.c = sum(gcc.change,na.rm=T),
#                                               swc.15 = mean(ym.18.df.smoothed$swc[(i-14):i],na.rm=TRUE)),
#                                         by = fornight]
ym.18.df.smoothed$swc <- ym.18.df.smoothed$vwc
sum.df <- summaryBy(swc + gcc.change ~ fornight, data = ym.18.df.smoothed,FUN=c(mean,sum),na.rm=T)
# plot(gcc.change.sum~swc.mean,data = sum.df)

plot(sum.df$gcc.change.sum[2:nrow(sum.df)]~sum.df$swc.mean[1:nrow(sum.df)-1])
abline(h=0,col='grey')
