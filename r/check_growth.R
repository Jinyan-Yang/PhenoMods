#####load required stuff
day.lag <- 2 #this is a left over from previous model; not use at this moment
source('r/pace_data_process.R')
# packages
library(doBy)
library(zoo)

plot.growth.func=function(plot.in){
  # gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
  #                                     subplot = 'S3P3B')
  gcc.met.pace.df.16 <- get.pace.func(gcc.met.pace.df,
                                      subplot = plot.in)
  # gcc.met.pace.df$Rain = gcc.met.pace.df$irrig.tot
  # gcc.met.pace.df.16 <- gcc.met.pace.df
  
  gcc.met.pace.df.16 = gcc.met.pace.df.16[gcc.met.pace.df.16$Date < as.Date('2019-10-1'),]
  # plot(Rain~Date,data =gcc.met.pace.df.16[gcc.met.pace.df.16$Date <as.Date('2018-5-1'),])
  
  index.vec= which(gcc.met.pace.df.16$Rain > 0)
  
  tmp.ls = list()
  for(i in seq_along(index.vec)){
    tmp.ls[[i]] = gcc.met.pace.df.16[gcc.met.pace.df.16$Date >= gcc.met.pace.df.16$Date[index.vec[i]] &
                                       gcc.met.pace.df.16$Date <= (gcc.met.pace.df.16$Date[index.vec[i]] + 5),]
    
    tmp.ls[[i]]$days.rained = tmp.ls[[i]]$Date - gcc.met.pace.df.16$Date[index.vec[i]]
    
    tmp.ls[[i]]$gcc.change = c(0,diff(tmp.ls[[i]]$GCC))
  }
  
  # plot(gcc.change~days.rained,data = tmp.ls[[1]],ylim=c(-0.10,0.1),
  #      pch=16,col='grey')
  # 
  # for(i in 1:length(tmp.ls)){
  #   points(gcc.change~days.rained,data = tmp.ls[[i]])
  # }
  
  growth.df = do.call(rbind,tmp.ls)
  

  
  # boxplot(gcc.change~days.rained,data = growth.df,notch=TRUE,ylim=c(-0.01,0.01))
  # abline(h=0,lty='dotted')
  
  return(growth.df)
  
}

plot.spec.func = function(spc.in){
  sub.vec = unique(gcc.met.pace.df$SubplotID[gcc.met.pace.df$Species == spc.in& 
                                               gcc.met.pace.df$Temperature == 'Ambient' ])
  sub.vec = sub.vec[!is.na(sub.vec)]
  
  grow.ls = lapply(sub.vec,plot.growth.func)
  grow.df = do.call(rbind,grow.ls)
  boxplot(gcc.change~days.rained,data = grow.df,notch=TRUE,ylim=c(-0.01,0.01))
  abline(h=0,lty='dotted')
}

pdf('growth.pdf',width = 10,height = 10*0.618)

# sub.vec = unique(gcc.met.pace.df$SubplotID[gcc.met.pace.df$Species == 'Luc'& 
#                                              gcc.met.pace.df$Temperature == 'Ambient' ])
# sub.vec = sub.vec[!is.na(sub.vec)]
# 
# grow.ls = lapply(sub.vec,plot.growth.func)
# grow.df = do.call(rbind,grow.ls)
# boxplot(gcc.change~days.rained,data = grow.df,notch=TRUE,ylim=c(-0.01,0.01))
# abline(h=0,lty='dotted')

plot.spec.func('Luc')
plot.spec.func('Fes')
plot.spec.func('Rho')
plot.spec.func('Rye')
dev.off()