get.fit.value.func <- function(fn,burin.frac=0.75){
  in.chain =  readRDS(fn)
  burnIn = 1
  chain.3.ls.new = lapply(in.chain,function(m.in)m.in[round(nrow(m.in)* (1-burin.frac)):nrow(m.in),])
  
  chain.fes <- do.call(rbind,chain.3.ls.new)
  
  return(colMeans(chain.fes[burnIn:nrow(chain.fes),]))
}

tmp.ls <- list()

spc.vec <- c('Bis','Dig','Luc','Fes','Rye','Kan','Rho','ym','flux')

for (spc.i in seq_along(spc.vec)) {
  fn <- sprintf('cache/smv13.2qchain.%s.Control.Ambient.rds',spc.vec[spc.i])
  
  v13.chain <- get.fit.value.func(fn)
  
  tmp.ls[[spc.i]] <- data.frame(model='v1.1',
                                site = spc.vec[spc.i],
                                f.t.opt = v13.chain[1],
                                f.extract = v13.chain[2] ,
                                f.sec = v13.chain[3] ,
                                f.growth= v13.chain[4],
                                q = v13.chain[5],
                                q.s =v13.chain[6])
}




out.df = do.call(rbind,tmp.ls)

write.csv(out.df,'cache/fittedParValue.csv',row.names = F)


