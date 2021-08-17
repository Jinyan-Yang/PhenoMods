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
    cover.in / cover.max
  
  return(growth.vec - senescence.vec)
}
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

pdf('figures/theory_demo.pdf',width = 8,height = 2*8*.62)
#####
par(mar=c(5,5,1,1))
par(mfrow=c(2,1))


# 
plot.shape.func <- function(x,a,b,c=3,d=15){
  return(
    ((b-x) / (b-a))^c * exp(c/d*(1-((b-x)/(b-a))^d))
    )
}

x.vec <- seq(0.01,22,by=0.1)
# 
y.vec.ll <- plot.shape.func(x.vec,
                a = 2,b=10)
# which.min(abs(y.vec.ll - y.vec.hh))

plot(y.vec.ll~x.vec,ylim=c(0.1,1),type='l',lwd=2,col=1,
     xlim=c(0.7,8),
     xaxt='n',yaxt='n',xlab='Time',ylab = 'Cover')
# 
y.vec.hh <- plot.shape.func(x.vec,
                         a = 5,b=10,
                         c=3,d=2)

points(y.vec.hh~x.vec,ylim=c(0,1),type='l',lwd=2,col=2)

# 
points(x.vec[which.max(y.vec.ll)],y.vec.ll[which.max(y.vec.ll)],cex=2,pch=3,col='grey')
points(x.vec[which.max(y.vec.hh)],y.vec.hh[which.max(y.vec.hh)],cex=2,pch=3,col='grey')

# 
legend('bottom',legend = c('Onset of Brown-down',
                             'Low growth sensitivity; Low senescence sensitivity', 
                             'High growth sensitivity; High senescence sensitivity'),bty='n',
       pch=c(3,rep(NA,2)),
       lty = 1,lwd=3,
       col = c('grey',palette()))
# abline(v=hufken.pace.pred.lgls$Date[20],lty='dashed',col='navy')
legend('topleft',legend = '(a)',bty='n')
# b#########
# first try one set of parameter
swc.capacity <- 0.3
swc.wilt <- 0.05
cover.max <- 1
cover.in <- NA
cover.vec <- seq(0,1,by=0.01)
#ll
q <- 0.6
q.s <- 4

# calculate threshold
threshold.vec.ll<- c()

for (i in seq_along(cover.vec)){
  cover.in <- cover.vec[i]
  
  threshold.vec.ll[i] <- uniroot(solve.intersect.func,interval = c(0.05,0.3))$root
}
#
q <- 0.6
q.s <- 0.1

# calculate threshold
threshold.vec.lh<- c()

for (i in seq_along(cover.vec)){
  cover.in <- cover.vec[i]
  
  threshold.vec.lh[i] <- uniroot(solve.intersect.func,interval = c(0.05,0.3))$root
}
#
q <- 1
q.s <- 4

# calculate threshold
threshold.vec.hl<- c()

for (i in seq_along(cover.vec)){
  cover.in <- cover.vec[i]
  
  threshold.vec.hl[i] <- uniroot(solve.intersect.func,interval = c(0.05,0.3))$root
}
#
q <- 1
q.s <- 0.1

# calculate threshold
threshold.vec.hh<- c()

for (i in seq_along(cover.vec)){
  cover.in <- cover.vec[i]
  
  threshold.vec.hh[i] <- uniroot(solve.intersect.func,interval = c(0.05,0.3))$root
}

# # try another set of sensitivity
# q <- 0.5
# q.s <- 2
# cover.vec <- seq(0,1,by=0.01)
# threshold.vec.tmp.2 <- c()
# 
# for (i in seq_along(cover.vec)){
#   cover.in <- cover.vec[i]
#   
#   threshold.vec.tmp.2[i] <- uniroot(solve.intersect.func,interval = c(0.05,0.3))$root
# }
# # make plot
# # load color

# 
plot(threshold.vec.ll~cover.vec,ylim=c(0.05,0.3),
     xlab = 'Cover',ylab = 'Soil moisture',
     type='l',lwd=2,col = 1)
# points(threshold.vec.lh~cover.vec,
#        type='l',lwd=2,col = 2)
# points(threshold.vec.hl~cover.vec,
#        type='l',lwd=2,col = 3)
points(threshold.vec.hh~cover.vec,
       type='l',lwd=2,col = 4)
legend('topleft',legend = '(b)',bty='n')

dev.off()