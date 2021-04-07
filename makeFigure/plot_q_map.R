pasture.gps.df <- read.csv('e:/repo/get_evi/cache/chosen pasture sites.csv')
names(pasture.gps.df)[3] <- 'type' 
pasture.gps.df <- pasture.gps.df[,c("type" ,"x" ,  "y" ,  "map" , "map.level","poly")]
pasture.gps.df$species <- 'pasture'
tossuck.gps.df <- read.csv('e:/repo/get_evi/cache/chosen sites.csv')
tossuck.gps.df$species <- 'tussock'
both.df <- rbind(tossuck.gps.df,pasture.gps.df)
both.df$site <- paste0(both.df$species,gsub('<','_',both.df$map.level))
# 
get.q.func <- function(site.nm){
  
  fn <- sprintf('cache/v13.2q.rooting.oldproposal.newNDVI.chain.%s.Control.Ambient.rds',site.nm)
  print(fn)
  in.chain =  readRDS(fn)

    # assuming 1/3 burn in
    chain.3.ls.new = lapply(in.chain,function(m.in)m.in[round(3*nrow(m.in)/4):nrow(m.in),])
    
    chain.fes <- do.call(rbind,chain.3.ls.new)
 
    fit.vec.q <- quantile(chain.fes[,5],probs = c(0.05,0.5,0.95))
    fit.vec.qs <- quantile(chain.fes[,6],probs = c(0.05,0.5,0.95))
    
    return(data.frame(varible = c('q','qs'),
                      quantile_5 = c(fit.vec.q[1],fit.vec.qs[1]),
                      quantile_50 = c(fit.vec.q[2],fit.vec.qs[2]),
                      quantile_95 = c(fit.vec.q[3],fit.vec.qs[3]),
                      site = site.nm)
           )
}

site.vec <- paste0(both.df$species,gsub('<','_',both.df$map.level))

source('r/proces_modis_sites.R')
q.ls <- lapply(modis.sites.vec[c(1,4,6,11,13,15)], get.q.func)

q.qs.df <- do.call(rbind,q.ls)

q.qs.map.df <- merge(q.qs.df,both.df,all.x=T,by = 'site')
q.qs.map.df$type <- as.factor(q.qs.map.df$type)
q.qs.map.df <- q.qs.map.df[order(q.qs.map.df$map),]


pdf('figures/q_map.pdf',width = 6,height = 2*6*.618)
par(mar=c(5,5,1,1),mfrow=c(2,1))
palette(col.df$flower)
plot(quantile_50~map,data = q.qs.map.df[q.qs.map.df$varible =='q' &
                                          q.qs.map.df$type=='9',],pch=16,col=type,ylab = 'Mediam',xlab='MAP (mm/yr)',ylim=c(0,8.5),type='b',
     xlim=c(300,1000),main='Growth')
points(quantile_50~map,data = q.qs.map.df[q.qs.map.df$varible =='q' &
                                            q.qs.map.df$type=='19',],pch=3,col=type,cex=1.3,type='b',lty='dotted')
legend('topright',legend = c('pasture','tussock'),pch=c(16,3),col=palette(),bty='n')

# 
plot(quantile_50~map,data = q.qs.map.df[q.qs.map.df$varible =='qs' &
                                          q.qs.map.df$type=='9',],pch=16,col=type,ylab = 'Mediam',xlab='MAP (mm/yr)',ylim=c(0,8.5),type='b',
     xlim=c(300,1000),main='Senescence')
points(quantile_50~map,data = q.qs.map.df[q.qs.map.df$varible =='qs' &
                                            q.qs.map.df$type=='19',],pch=3,col=type,cex=1.3,type='b',lty='dotted')

legend('topright',legend = c('pasture','tussock'),pch=c(16,3),col=palette(),bty='n')

dev.off()