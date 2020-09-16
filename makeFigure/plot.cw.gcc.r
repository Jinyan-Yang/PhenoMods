gcc.met.cw.df <- readRDS('cache/gcc.met.cw.df.rds')

site.vec <- as.character(unique(gcc.met.cw.df$Code))

pdf('figures/cw.gcc.plot.pdf',width = 8,height = 4*0.618*3)
par(mfrow=c(3,2))
for (i in seq_along(site.vec)) {
  plot.df <- gcc.met.cw.df[gcc.met.cw.df$Code == site.vec[i],]

 plot(GCC~Date,data = plot.df,
         pch=16,col='darkseagreen',xlab='',ylab='GCC',main =  site.vec[i],
      ylim=c(0.3,0.4),xaxt='n')
 
 date.range = range(plot.df$Date,na.rm=T)
 mons.vec =  seq(date.range[1],date.range[2],by='mon')
 
 mon.c <- format(mons.vec,'%m')
 axis(1,at = mons.vec,labels = mon.c)
 # mtext('2018',side = 1,adj=0,line = 3)
 # mtext('2019',side = 1,adj=0.5,line = 3)
 yr.vec <- unique(year(plot.df$Date))
 where.c <-which(mon.c =='01') / length(mon.c)
 num.yr <- length(where.c)
 mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  
}
dev.off()
