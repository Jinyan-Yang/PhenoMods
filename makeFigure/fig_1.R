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
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

pdf('figures/fig1.pdf',width = 8,height = 2*8*.62)
par(mar=c(5,5,1,1))
par(mfrow=c(2,1))
# a#########
#a first try one set of parameter
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

# 
plot(threshold.vec.tmp.2~cover.vec,ylim=c(0.05,0.3),
     xlab = 'Initial cover',ylab = 'Soil moisture threshold',
     type='l',lwd=2,col = 'coral')
points(threshold.vec.tmp~cover.vec,
       type='l',lwd=2,col = 'grey10')
legend('topleft',legend = '(a)',bty='n')

# b#############

f.vec <- seq(0.3,2,length.out = 20)
f.vec <- f.vec + 0.1*rnorm(20)

fs.vec <- seq(2,0.01,length.out = 20)
fs.vec <- fs.vec + 0.1*rnorm(20)

plot.df <- data.frame(f = f.vec,
                      f.s = fs.vec,
                      map = rep(seq(100,1000,length.out=10),each=2))
plot.df$map <- plot.df$map + 50*rnorm(20)

# 

plot(f~map,data = plot.df,pch=16,col=col.df$iris[5],ylab='Sensitivity to drought',xlab='MAP (mm/yr)',axes = F)
points(f.s~map,data = plot.df,pch=16,col=col.df$iris[1])

axis(1,at = c(200,1000),labels = c('Low','High'))
axis(2,at = c(0.5,2),labels = c('Low','High'))

legend('top',legend = c('Growth','Senescence'),bty='n',
       pch=16,col=col.df$iris[c(5,1)])

legend('topleft',legend = '(b)',bty='n')

dev.off()



