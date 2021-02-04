beta.func <- function(x,a=0.05,b=0.3,q=5){
  ((x - a) / (b - a))^q 
}

swc.vec <- seq(0.05,0.3,by=0.001)

beta.luc = beta.func(x=swc.vec,q=5)
beta.fes = beta.func(swc.vec,q=1)
beta.SCOT = beta.func(swc.vec,q=0.5)

png('figures/theoreticalQ.png',width = 600,height = 600*0.618)

plot(beta.luc~swc.vec,type='l',col='coral',
     xlab='VWC',ylab=expression(f[w]),lwd=3)

points(beta.fes~swc.vec,type='l',col='grey',lwd=3)
points(beta.SCOT~swc.vec,type='l',col='lightskyblue',lwd=3)

legend('topleft',legend = c("q = 5","q = 1","q = 0.5"),
       lty=1,col=c("coral","grey","lightskyblue"),
       bty='n')
dev.off()
