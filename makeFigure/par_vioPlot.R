get.fit.value.func <- function(fn,burin.frac=0.75){
  in.chain =  readRDS(fn)
  burnIn = 1
  chain.3.ls.new = lapply(in.chain,function(m.in)m.in[round(nrow(m.in)* (1-burin.frac)):nrow(m.in),])
  
  chain.fes <- do.call(rbind,chain.3.ls.new)
  
  return(colMeans(chain.fes[burnIn:nrow(chain.fes),]))
}

get.fit.ci.func <- function(fn,burin.frac=0.75){
  in.chain =  readRDS(fn)
  burnIn = 1
  chain.3.ls.new = lapply(in.chain,function(m.in)m.in[round(nrow(m.in)* (1-burin.frac)):nrow(m.in),])
  
  chain.fes <- do.call(rbind,chain.3.ls.new)
  
  out.df <- data.frame(f.t.opt = quantile(chain.fes[,1],probs = c(0.05,0.95)),
                       f.extract = quantile(chain.fes[,2],probs = c(0.05,0.95)),
                       f.sec = quantile(chain.fes[,3],probs = c(0.05,0.95)),
                       f.growth= quantile(chain.fes[,4],probs = c(0.05,0.95)),
                       q = quantile(chain.fes[,5],probs = c(0.05,0.95)),
                       q.s = quantile(chain.fes[,6],probs = c(0.05,0.95))
  )
  
  return(out.df)
}



tmp.ls <- list()

spc.vec <-c('Bis','Luc','Dig','Kan','Rho','Fes','Pha','Rye','YM','Flux')

for (spc.i in seq_along(spc.vec)) {
  fn <- sprintf('cache/smv13.2q.chain.%s.Control.Ambient.rds',spc.vec[spc.i])
  
  v13.chain <- get.fit.value.func(fn)
  
  v13.chain.ci <- get.fit.ci.func(fn)
  
  tmp.ls[[spc.i]] <- data.frame(model='v1.1',
                                site = spc.vec[spc.i],
                                
                                f.t.opt = v13.chain[1],
                                f.t.opt.05 = v13.chain.ci[1,1],
                                f.t.opt.95 = v13.chain.ci[2,1],
                                
                                f.extract = v13.chain[2] ,
                                f.extract.05 = v13.chain.ci[1,2],
                                f.extract.95 = v13.chain.ci[2,2],
                                
                                
                                f.sec = v13.chain[3] ,
                                f.sec.05 = v13.chain.ci[1,3],
                                f.sec.95 = v13.chain.ci[2,3],
                                
                                f.growth= v13.chain[4],
                                f.growth.05 = v13.chain.ci[1,4],
                                f.growth.95 = v13.chain.ci[2,4],
                                
                                q = v13.chain[5],
                                q.05 = v13.chain.ci[1,5],
                                q.95 = v13.chain.ci[2,5],
                                
                                q.s =v13.chain[6],
                                q.s.05 = v13.chain.ci[1,6],
                                q.s.95 = v13.chain.ci[2,6]
                                
                                
  )
}




out.df = do.call(rbind,tmp.ls)
# 
library(vioplot)
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
palette(c(col.df$iris))
col.nm.vec <- c(1,1,2,2,2,3,3,3,4,4)

plot.box.func <- function(spc.vec,col2plot,burin.frac=0.75,y.nm){
  
  tmp.ls <- list()
  for (i in seq_along(spc.vec)) {
    fn <- sprintf('cache/smv13.2q.chain.%s.Control.Ambient.rds',spc.vec[i])
    in.chain =  readRDS(fn)
    burnIn = 1
    chain.3.ls.new = lapply(in.chain,function(m.in)m.in[round(nrow(m.in)* (1-burin.frac)):nrow(m.in),])
    
    chain.fes <- do.call(rbind,chain.3.ls.new)
    
    tmp.ls[[i]] <- data.frame(spc = spc.vec[i],
                              par.val = chain.fes[,col2plot])
    
  }
 
  plot.df <- do.call(rbind,tmp.ls)
  plot.df$spc <- factor(plot.df$spc,levels=spc.vec)

  vioplot(par.val~spc,plot.df,col=col.nm.vec,
          xlab='',ylab=y.nm)
}

pdf('figures/par.vioplot.pdf',width = 4*2,height = 4*.618*3)
par(mfrow=c(3,2))
par(mar=c(5,5,1,1))

# var.nm.vec <- c('f.t.opt', 'f.extract',
#                 'f.sec','f.growth', 
#                 'q','q.s')

y.nm.vec <- c(expression('Opt. T '*(degree*C)),
              expression('Max. tran. rate '*(mm~d^-1)),
              expression('Turnover rate '*(cover~d^-1)),
              expression('Growth rate '*(cover~d^-1)),
              expression('q (growth)'),
              expression(q[s]~(turnover)))

for (plot.var.nm in c(1,2,3,4,6,5)) {
  
  
  plot.box.func(spc.vec,
                plot.var.nm,
                y.nm = y.nm.vec[plot.var.nm])
  
}
dev.off()



