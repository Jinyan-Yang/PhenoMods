library(coda)

# this function uses the coda package to test gelman and rubin########
# potential scale reduction factor####################################
test.converge.func <- function(spc.in,var.in){
  # inputs are species name and variable name
  # examples:
  # spc.in='ym'
  # var.in=2

  # read chain 
  fn <- sprintf('cache/smv13.2q.chain.%s.Control.Ambient.rds',spc.in)
  chain.3.ls = readRDS(fn)
  
  start.row <- nrow(chain.3.ls[[1]]) / 4*3
  end.row <- nrow(chain.3.ls[[1]])
  combinedchains <- mcmc.list(mcmc(chain.3.ls[[1]][start.row:end.row,var.in]),
                              mcmc(chain.3.ls[[2]][start.row:end.row,var.in]),
                              mcmc(chain.3.ls[[3]][start.row:end.row,var.in]))
  out.info<- gelman.diag(combinedchains)
  return(out.info$psrf[1])
}
# 
# 
# combinedchains <- mcmc.list(mcmc(Wal.chain[[1]][45000:50000,5]),
#                             mcmc(Wal.chain[[2]][45000:50000,5]),
#                             mcmc(Wal.chain[[3]][45000:50000,5]))
# gelman.diag(combinedchains)
# gelman.plot(combinedchains)
# test.converge.func('flux',6)


species.vec <- c('Bis','Luc','Dig','Wal','Rho','Fes','Pha','Rye','ym','flux')

par.ls <- list()
for (i in 1:6){
  par.ls[[i]] <- sapply(species.vec, test.converge.func,var.in=i)
}
par.df <- do.call(rbind,par.ls)

row.names(par.df) <- c(expression('Opt. T '*(degree*C)),
                       expression('Max. tran. rate '*(mm~d^-1)),
                       expression('Senescence rate '*(cover~d^-1)),
                       expression('Growth rate '*(cover~d^-1)),
                       expression('q (growth)'),
                       expression(q[s]~(Senescence)))

saveRDS(par.df,'cache/converge_test.rds')
