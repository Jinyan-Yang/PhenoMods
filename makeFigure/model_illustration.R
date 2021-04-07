# code to show how the model works#######################################################
#########################################################################################

# not to use for now
# ym.18.df <- get.ym.func(18)
# with(ym.18.df,plot(swc~Date))
# tmp.df <- get.pace.func(ym.18.df,
#                         species.in ='ym',
#                         prep.in = 'Control', temp.in ='Ambient',
                        # subplot = NA)

# load functions and packages
source('models/hufkens/hufkensV13.R')

# contructe a df to plot ###########
tmp.df <- data.frame(Date = seq(as.Date('2019-6-1'),
                                as.Date('2019-11-1'),by='day'),
                     Rain = 0,
                     u2 =  2,
                     PPFD =  0.02,
                     Tair = 30,
                     Tmax = 35,
                     Tmin = 25,
                     RHmax = 100,
                     RHmin = 50,
                     harvest = 0,
                     GCC.norm = 1)

# set drought scenario
tmp.df$vwc <- 0.05
tmp.df$GCC.norm.smooth <- 0.9

optimal.drought.pred.df <- phenoGrass.func.v13(tmp.df,
                                               f.h = 200,
                                               f.t.opt = 30,
                                               f.extract = 0.0287,
                                               f.sec =0.25,
                                               f.growth = 0.095,
                                               swc.wilt =0.05 ,
                                               swc.capacity = 0.3,
                                               bucket.size=1000,
                                               t.max = 45,
                                               day.lay=1,
                                               use.smooth=TRUE,
                                               q = 0.05,
                                               q.s=1.5)
# max(optimal.drought.pred.df$senescence,na.rm=T)
plot(growth~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5],ylim=c(0,0.25))
points(senescence~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[1])

plot((growth - senescence)~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5])
abline(h=0)

plot(cover.hufken~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5])


# set rain scenario
tmp.df$vwc <- 0.3
tmp.df$GCC.norm.smooth <- 0


optimal.drought.pred.df <- phenoGrass.func.v13(tmp.df,
                                               f.h = 200,
                                               f.t.opt = 30,
                                               f.extract = 0.0287,
                                               f.sec =0.25,
                                               f.growth = 0.095,
                                               swc.wilt =0.05 ,
                                               swc.capacity = 0.3,
                                               bucket.size=1000,
                                               t.max = 45,
                                               day.lay = 1,
                                               use.smooth=TRUE,
                                               q = 0.05,
                                               q.s = 1.5)

plot(growth~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5])
points(senescence~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[1])

plot((growth - senescence)~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5])
abline(h=0)

plot(cover.hufken~vwc.hufken,data = optimal.drought.pred.df,type='b',pch=16,col=col.df$flower[5])

# calculate threshold with distribution####
ym.fit.df <- readRDS('tmp/pred.smv13.2qchain.ym.Control.Ambient.rds')

plot((growth - senescence)~vwc.hufken,data = ym.fit.df)
abline(h=0)

# 
sen.df <- ym.fit.df[(ym.fit.df$growth - ym.fit.df$senescence)<0 &
                      (ym.fit.df$growth - ym.fit.df$senescence)>-0.001 ,]

hist(sen.df$vwc.hufken)

hist(sen.df$swc)

quantile(sen.df$vwc.hufken,na.rm=T,probs = 0.1)
index.sen.vec <- c()
index.gro.vec <- c()
for (i in 2:nrow(ym.fit.df)) {
  
  index.sen.vec[i-1] <- ym.fit.df$net.change[i] < 0 &
    ym.fit.df$net.change[i-1] > 0
  
  index.gro.vec[i-1] <- ym.fit.df$net.change[i] > 0 &
    ym.fit.df$net.change[i-1] < 0
  
  
}

hist(ym.fit.df$swc[which(index.sen.vec)-1])
hist(ym.fit.df$swc[which(index.gro.vec)+1])

# 
ym.fit.df$net.change <- c(0,diff(ym.fit.df$cover.hufken))
plot(net.change~vwc.hufken,data = ym.fit.df)

# calculate threshold as max sen####
get.threshold.func <- function(gcc.vec = seq(0.1,1,by=0.01),
                               f.sec =0.25,
                               f.growth = 0.095,
                               swc.wilt =0.05 ,
                               swc.capacity = 0.3,
                               bucket.size=1000,
                               q = 0.05,
                               q.s = 1.5){
  # gcc.vec <- seq(0.1,1,by=0.01)
  # swc.vec <- seq(0.05,0.3,by=0.01)
  pred.ls <- list()
  break.points <- c()
  
  for (i in seq_along(gcc.vec)) {
    
    # set rain scenario
    tmp.df$vwc <- 0.3
    tmp.df$GCC.norm.smooth <- gcc.vec[i]
    
    pred.out <- phenoGrass.func.v13(tmp.df,
                                    f.h = 200,
                                    f.t.opt = 30,
                                    f.extract = 0.0287,
                                    f.sec =f.sec,
                                    f.growth = f.growth,
                                    swc.wilt =swc.wilt ,
                                    swc.capacity = swc.capacity,
                                    bucket.size=bucket.size,
                                    t.max = 45,
                                    day.lay = 1,
                                    use.smooth=TRUE,
                                    q = q,
                                    q.s = q.s)
    pred.out <- pred.out[complete.cases(pred.out),]
    pred.out$net.change <- pred.out$growth - pred.out$senescence
    
    # library(mgcv)
    # 
    # fit.gam <- gam(vwc.hufken~s(net.change,k=4),data = pred.out)
    # 
    # predict(fit.gam,newdata = data.frame(net.change=0))
    
    break.points[i] <- pred.out$vwc.hufken[abs(pred.out$net.change)<0.05]
    
    # break.points[i] <- pred.out$vwc.hufken[pred.out$senescence ==
    #                                          max(pred.out$senescence,
    #                                              na.rm=T)]
  }
  
  threshold.df <- data.frame(gcc = gcc.vec,
                             threshold = break.points) 
  
  return(threshold.df)
}


high.threshold.df <- get.threshold.func(q.s = 1)
low.qs.threshold.df <- get.threshold.func(q.s = 0.05)
dig.threshold.df <- get.threshold.func(f.sec=0.19,
                                       f.growth = 0.15,
                                       q = 1.9,q.s = 0.2,
                                       swc.wilt =0.05 ,
                                       swc.capacity = 0.13,
                                       bucket.size=300)

ym.threshold.df <- get.threshold.func(f.sec=0.125,
                                       f.growth = 0.055,
                                       q = 0.01,q.s = 3.6)

ym.threshold.df <- get.threshold.func(f.sec=0.01465686,
                                      f.growth = 0.07186675,
                                      q = 0.2184495,q.s = 0.6646073,
                                      swc.wilt =0.05 ,
                                      swc.capacity = 0.3,
                                      bucket.size=1000,)
# 
plot(threshold~gcc,data = high.threshold.df,type='l',
     xlab='Initial cover',
     ylab='SWC threshold',ylim=c(0,0.13))
points(threshold~gcc,data = low.qs.threshold.df,type='l',col='red')
abline(h=0.05,lty='dashed')
legend('topleft',legend = c('q.s = 0.05','q.s = 1'),
       lty='solid',col=c('red','black'))

# same plot with fitted values from ym and pace
plot(threshold~gcc,data = ym.threshold.df,type='l',
     xlab='Initial cover',
     ylab='SWC threshold',ylim=c(0,0.3))
points(threshold~gcc,data = dig.threshold.df,type='l',col='red')
abline(h=0.05,lty='dashed')
legend('topleft',legend = c('Digit','YM'),
       lty='solid',col=c('red','black'))


# solve for threshold with only sen and growth fucntions##########
pdf('figures/threshold.pdf',width = 8,height = 8*.618)
# function to get the intersect of growth vs sensescence
solve.intersect.func <- function(vwc.in){
  swc.norm <-  (vwc.in- swc.wilt)/ (swc.capacity - swc.wilt)
  loss.f <- swc.norm^q
  loss.f.s <- (1-swc.norm)^q.s
  
  growth.vec <- 
    loss.f *
    (1 - cover.in / cover.max)
  
  senescence.vec <- (loss.f.s) *
    # (1 - cover.pred.vec[nm.day-1])*
    cover.in
  
  return(growth.vec - senescence.vec)
}

# first try one set of parameter
swc.capacity <- 0.3
swc.wilt <- 0.05
q <- 2
q.s <- 0.5
cover.max <- 1
cover.in <- 0.5

cover.vec <- seq(0,1,by=0.01)

# calculate threshold
threshold.vec.tmp <- c()

for (i in seq_along(cover.vec)){
  cover.in <- cover.vec[i]
  
  threshold.vec.tmp[i] <- uniroot(solve.intersect.func,interval = c(0.05,0.3))$root
}
# try another set of sensitivity
q <- 0.5
q.s <- 2
cover.vec <- seq(0,1,by=0.01)
threshold.vec.tmp.2 <- c()

for (i in seq_along(cover.vec)){
  cover.in <- cover.vec[i]
  
  threshold.vec.tmp.2[i] <- uniroot(solve.intersect.func,interval = c(0.05,0.3))$root
}
# make plot
# load color
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
# 
plot(threshold.vec.tmp.2~cover.vec,ylim=c(0.05,0.3),
     xlab = 'Initial cover',ylab = 'SWC threshold',
     type='l',lwd=2,col = 'coral')
points(threshold.vec.tmp~cover.vec,
     type='l',lwd=2,col = 'grey10')

abline(v=0.2,lty=3,col='grey')
abline(v=0.4,lty=3,col='grey')
abline(v=0.6,lty=3,col='grey')
abline(v=0.8,lty=3,col='grey')

abline(h=0.1,lty=3,col='grey')
abline(h=0.15,lty=3,col='grey')
abline(h=0.2,lty=3,col='grey')
abline(h=0.25,lty=3,col='grey')

legend('topleft',legend = c('q = 0.5; q.s = 2','q = 2;    q.s = 0.5'),lty=1,col=c('coral','grey10'),lwd=2)

# plot q value
beta.func <- function(x,a=0.05,b=0.3,q=5,deficit =FALSE){
  if(deficit){
    (1 - (x - a) / (b - a))^q 
  }else{
    ((x - a) / (b - a))^q 
  }
 
}

swc.vec <- seq(0.05,0.3,by=0.01)

beta.q.0.5 = beta.func(x=swc.vec,q=0.5)
beta.qs.2 = beta.func(x=swc.vec,q=2,deficit=T)

beta.q.2 = beta.func(x=swc.vec,q=2)
beta.qs.0.5 = beta.func(x=swc.vec,q=0.5,deficit=T)

# 
plot(beta.q.0.5~swc.vec,type='l',col=col.df$iris[5],
     xlab='SWC',ylab=expression(beta),lty=1,lwd=3)

points(beta.qs.2~swc.vec,type='l',col=col.df$iris[1],lty=1,lwd=3)

points(beta.q.2~swc.vec,type='l',col=col.df$iris[5],lty=2,lwd=3)

points(beta.qs.0.5~swc.vec,type='l',col=col.df$iris[1],lty=2,lwd=3)

# 
abline(v=0.1,lty=3,col='grey')
abline(v=0.15,lty=3,col='grey')
abline(v=0.2,lty=3,col='grey')
abline(v=0.25,lty=3,col='grey')

abline(h=0.2,lty=3,col='grey')
abline(h=0.4,lty=3,col='grey')
abline(h=0.6,lty=3,col='grey')
abline(h=0.8,lty=3,col='grey')

legend('top',legend = c("q = 0.5","q = 2","q.s = 0.5","q.s = 2"),
       lty=c(1,2,1,2),col=(col.df$iris[c(5,5,1,1)]),
       bty='n',lwd=3)

dev.off()
# 
# vwc.vec <- seq(0.05,0.3,by=0.01)
# 
# swc.norm <-  (vwc.vec- swc.wilt)/ (swc.capacity - swc.wilt)
# loss.f <- swc.norm^q
# loss.f.s <- (1-swc.norm)^q.s
# 
# growth.vec <- 
#   loss.f *
#   (1 - cover.in / cover.max)
# 
# senescence.vec <- (loss.f.s) *
#   # (1 - cover.pred.vec[nm.day-1])*
#   cover.in
# swc.norm <-  (vwc.vec- swc.wilt)/ (swc.capacity - swc.wilt)
# loss.f <- swc.norm^q
# loss.f.s <- (1-swc.norm)^q.s
# 
# growth.vec <- 
#   loss.f *
#   (1 - cover.in / cover.max)
# 
# senescence.vec <- (loss.f.s) *
#   # (1 - cover.pred.vec[nm.day-1])*
#   cover.in
# 
# 
# plot(growth.vec~vwc.vec,type='l')
# points(senescence.vec~vwc.vec,col='red',type='l')
# 
# threshold.root <- uniroot(solve.intersect.func,interval = c(0.05,0.3))
# abline(v = threshold.root$root)
# 
# # plot(net.change~vwc.hufken,data = pred.out)
# # abline(h=0)
# # 
# # contour(x = ym.fit.df$cover.hufken,
# #         y = ym.fit.df$swc,
# #         z = ym.fit.df$net.change)
# # 
# # 
# # for (i in 1:length(pred.ls)){
# #   
# # }