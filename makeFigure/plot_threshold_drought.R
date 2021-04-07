source('r/plot.mcmc.r')
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")


# pdf('figures/v13.2q.diag.pdf',width = 8,height = 8*0.618)
# fit pace####
species.vec <- c('Luc','Fes','Rye','Kan','YM')

out.df <- data.frame(spc = species.vec,
                     f.t.opt =NA,
                     f.extract =NA,
                     f.sec =NA,
                     f.growth = NA,
                     q = NA,
                     q.s = NA)
for(i in  seq_along(species.vec)){
  fn <- sprintf('cache/smv13.2qchain.%s.Drought.Ambient.rds',species.vec[i])
  
  chain.3.ls = readRDS(fn)
  
  chain.3.ls.new = lapply(chain.3.ls,function(m.in)m.in[round(2*nrow(m.in)/3):nrow(m.in),])
  chain.fes <- do.call(rbind,chain.3.ls.new)
  
  
  fitted.val <- colMeans(chain.fes[15000:nrow(chain.fes),])
  
  out.df[i,2:7] <- fitted.val
  
  # lapply(chain.3.ls, plot.check.mcmc.func,species.in=species.vec[i])
  
  # par(mfrow=c(3,2),mar=c(5,5,1,1))
  # for(par.num in 1:6){
  #   
  #   plot.line.mcmc.func(chain.3.ls,par.num,range.iter =  round(15000:nrow(chain.3.ls[[1]])))
  #   
  # }
}


# 
solve.intersect.func <- function(vwc.in){
  swc.norm <-  (vwc.in- swc.wilt)/ (swc.capacity - swc.wilt)
  loss.f <- swc.norm^q
  loss.f.s <- f.growth*(1-swc.norm)^q.s
  
  growth.vec <- f.sen*
    loss.f *
    (1 - cover.in / cover.max)
  
  senescence.vec <- (loss.f.s) *
    # (1 - cover.pred.vec[nm.day-1])*
    cover.in
  
  return(growth.vec - senescence.vec)
}

# 

pred.ls <- list()
for (iter.nm in 1:nrow(out.df)) {
  
  swc.capacity <- 0.3
  swc.wilt <- 0.05
  q <- out.df$q[iter.nm]
  q.s <- out.df$q.s[iter.nm]
  cover.max <- 1
  cover.in <- 0.5
  
  cover.vec <- seq(0,1,by=0.01)
  
  # calculate threshold
  threshold.vec.tmp <- c()
  
  for (i in seq_along(cover.vec)){
    cover.in <- cover.vec[i]
    
    threshold.vec.tmp[i] <- uniroot(solve.intersect.func,interval = c(swc.wilt,swc.capacity))$root
  }
  
  
  pred.ls[[iter.nm]] <- threshold.vec.tmp
}

# 
palette(col.df$auLandscape)

png('figures/threshold_drought.png',width = 600,height = 600*0.618)

plot(pred.ls[[1]]~cover.vec,ylim=c(0.05,0.3),
     xlab = 'Initial cover',ylab = 'Soil moisture threshold',
     type='l',lwd=2,col = 1,lty='dashed')
points(pred.ls[[2]]~cover.vec,type='l',col=2,lwd=2,lty='dashed')
points(pred.ls[[3]]~cover.vec,type='l',col=3,lwd=2,lty='dashed')
points(pred.ls[[4]]~cover.vec,type='l',col=4,lwd=2,lty='dashed')
points(pred.ls[[5]]~cover.vec,type='l',col=5,lwd=2,lty='dashed')
legend('topleft',legend = '(b)',bty='n')
legend('bottomright',legend = out.df$spc,
      col=palette(),lwd=2,lty='dashed')

dev.off()