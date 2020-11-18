day.lag <- 1
source('r/process_cw_gcc.R')
source('r/v13_common_fun.R')

gcc.met.cw.df <- readRDS('cache/gcc.met.cw.df.rds')
site.vec <- unique(gcc.met.cw.df$Code)
site.vec <- as.character(site.vec)

plot.nm.vec <- paste0(site.vec,rep(2014:2015,each=12))

pdf('figures/cor.fc.evi.gcc.pdf',width = 6,height = 6)

for(i in seq_along(site.vec)){
  cw.site.df <- gcc.met.cw.df[gcc.met.cw.df$Code == site.vec[i],]
  # 
  cw.tmp.df <- try(get.pace.func(cw.site.df,
                                 species.in = site.vec[i],
                                 prep.in = 'Control',
                                 temp.in ='Ambient',
                                 subplot = NA))
  
  if(class(cw.tmp.df) != 'try-error'){
    # 
    fn1 <- paste0('cache/modisFC/',site.vec[i],2014,'.rds')
    tmp.df1 <- readRDS(fn1)
    
    fn2 <- paste0('cache/modisFC/',site.vec[i],2015,'.rds')
    tmp.df2 <- readRDS(fn2)
    
    tmp.df.fc <- rbind(tmp.df1,tmp.df2)
    # 
    fn1.evi <- paste0('cache/evi.', site.vec[i],'20012016','.rds')
    tmp.df.evi <- readRDS(fn1.evi)
    
    tmp.sat.df <- merge(tmp.df.fc,tmp.df.evi,by=c('Date'),all.x=T)
    
   
    # 
    cw.sat.gcc.df <- merge(tmp.sat.df,cw.tmp.df)
    
    pairs(cw.sat.gcc.df[,c('FC','GCC.smooth','evi')],upper.panel=NULL,
          panel=function(x,y){
      points(x,y)
      abline(lm(y~x), col='red')
      text(mean(x,na.rm=T),quantile(y,probs = 0.9,na.rm = T),labels = paste('R=',round((cor(x,y,use ='na.or.complete')),2)) ,col='red' )
    })
    
    title(site.vec[i])
    
    # plot.gcc.fc.func(tmp.df,cw.tmp.df,site.vec[i])
  }
  
  
}

dev.off()
