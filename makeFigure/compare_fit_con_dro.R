
species.vec <- c('Luc','Fes','Rye','Kan','YM')

out.df.drought <- data.frame(spc = species.vec,
                     f.t.opt =NA,
                     f.extract =NA,
                     f.sec =NA,
                     f.growth = NA,
                     q = NA,
                     q.s = NA)
for(i in seq_along(species.vec)){
  fn <- sprintf('cache/smv13.2qchain.%s.Drought.Ambient.rds',species.vec[i])
  
  chain.3.ls = readRDS(fn)
  
  chain.3.ls.new = lapply(chain.3.ls,function(m.in)m.in[round(2*nrow(m.in)/3):nrow(m.in),])
  chain.fes <- do.call(rbind,chain.3.ls.new)
  
  
  fitted.val <- colMeans(chain.fes[15000:nrow(chain.fes),])
  
  out.df.drought[i,2:7] <- fitted.val
  
}


out.df.con <- data.frame(spc = species.vec,
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
  
  out.df.con[i,2:7] <- fitted.val
  
}

beta.func <- function(x,a=0.05,b=0.3,q=5,is.q.s = FALSE){
  if(is.q.s){
    (1-(x - a) / (b - a))^q 
  }else{
    ((x - a) / (b - a))^q 
  }
}

swc.vec <- seq(0.05,0.3,by=0.001)
beta.growth.ls.con <- beta.growth.ls.dro <- beta.sene.ls.con <- beta.sene.ls.dro <- list()
for (i.nm in seq_along(species.vec)) {
  beta.growth.ls.con[[i.nm]] <- beta.func(x=swc.vec,q=out.df.con$q[i.nm])
  beta.growth.ls.dro[[i.nm]] <- beta.func(x=swc.vec,q=out.df.drought$q[i.nm])
  beta.sene.ls.con[[i.nm]] <- beta.func(x=swc.vec,q=out.df.con$q.s[i.nm],is.q.s = TRUE)
  beta.sene.ls.dro[[i.nm]] <- beta.func(x=swc.vec,q=out.df.drought$q.s[i.nm],is.q.s = TRUE)
}

palette(col.df$auLandscape)

pdf('figures/compare_fit_con_dro.pdf',width = 8,height = 2*8*0.618)
par(mar=c(5,5,1,1),mfrow=c(3,2))

for (i in seq_along(species.vec)) {
  plot( beta.growth.ls.con[[i]]~swc.vec,type='l',
        xlab='Soil moisture',ylab=expression(f[w]),lwd=3)
  points(beta.growth.ls.dro[[i]]~swc.vec,type='l',lwd=3,lty='dashed')
  
  legend('topleft',legend = paste0('(',letters[i],') ', species.vec[i]),
         col=palette(),bty='n')
  
  if(i==1){
    legend('bottomright',legend = c('Control','Drought'),
           lty=c('solid','dashed'),bty='')
  }
}

par(mar=c(5,5,1,1),mfrow=c(3,2))

for (i in seq_along(species.vec)) {
  plot( beta.sene.ls.con[[i]]~swc.vec,type='l',
        xlab='Soil moisture',ylab=expression(f[w*','*s]),lwd=3,
        ylim=c(0,1.1))
  points(beta.sene.ls.dro[[i]]~swc.vec,type='l',lwd=3,lty='dashed')
  
  legend('topleft',legend = paste0('(',letters[i+5],') ', species.vec[i]),
         col=palette(),bty='n')
  
  if(i==1){
    legend('bottomleft',legend = c('Control','Drought'),
           lty=c('solid','dashed'),bty='')
  }
}
dev.off()

