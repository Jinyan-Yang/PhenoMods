
species.vec <- c('Luc','Fes','Rye','Kan',
                 'YM')

out.df <- data.frame(spc = species.vec,
                     f.t.opt =NA,
                     f.extract =NA,
                     f.sec =NA,
                     f.growth = NA,
                     q = NA,
                     q.s = NA)
for(i in seq_along(species.vec)){
  fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds',species.vec[i])
  
  chain.3.ls = readRDS(fn)
  
  chain.3.ls.new = lapply(chain.3.ls,function(m.in)m.in[round(2*nrow(m.in)/3):nrow(m.in),])
  chain.fes <- do.call(rbind,chain.3.ls.new)
  
  
  fitted.val <- colMeans(chain.fes[15000:nrow(chain.fes),])
  
  out.df[i,2:7] <- fitted.val
  
}


beta.func <- function(x,a=0.05,b=0.3,q=5,is.q.s = FALSE){
 if(is.q.s){
   (1-(x - a) / (b - a))^q 
 }else{
   ((x - a) / (b - a))^q 
 }
}

swc.vec <- seq(0.05,0.3,by=0.001)
beta.growth.ls <- beta.sene.ls <- list()
for (i.nm in seq_along(species.vec)) {
  beta.growth.ls[[i.nm]] <- beta.func(x=swc.vec,q=out.df$q[i.nm])
  beta.sene.ls[[i.nm]] <- beta.func(x=swc.vec,q=out.df$q.s[i.nm],is.q.s = TRUE)
}

palette(col.df$auLandscape)

png('figures/fig3_q.png',width = 600,height = 2*600*0.618)
par(mar=c(5,5,1,1),mfrow=c(2,1))
plot( beta.growth.ls[[1]]~swc.vec,type='l',col=1,
     xlab='VWC',ylab=expression(f[w]),lwd=3)

points(beta.growth.ls[[2]]~swc.vec,type='l',col=2,lwd=3)
points(beta.growth.ls[[3]]~swc.vec,type='l',col=3,lwd=3)
points(beta.growth.ls[[4]]~swc.vec,type='l',col=4,lwd=3)
points(beta.growth.ls[[5]]~swc.vec,type='l',col=5,lwd=3)
legend('topleft',legend = '(a)',bty='n')
legend('bottomright',legend = out.df$spc,
       lty='solid',col=palette())

# 
plot( beta.sene.ls[[1]]~swc.vec,type='l',col=1,
      xlab='VWC',ylab=expression(f[w*','*s]),lwd=3,lty='dashed')

points(beta.sene.ls[[2]]~swc.vec,type='l',col=2,lwd=3,lty='dashed')
points(beta.sene.ls[[3]]~swc.vec,type='l',col=3,lwd=3,lty='dashed')
points(beta.sene.ls[[4]]~swc.vec,type='l',col=4,lwd=3,lty='dashed')
points(beta.sene.ls[[5]]~swc.vec,type='l',col=5,lwd=3,lty='dashed')
legend('topleft',legend = '(b)',bty='n')
dev.off()
