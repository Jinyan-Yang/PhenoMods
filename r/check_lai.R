# reead pace lai from Amy
pace.lai.df <- read.csv('cache/pace_lai.csv',stringsAsFactors = F)
pace.lai.df$SubplotID <- as.character(pace.lai.df$SubPlotID)
pace.lai.df$Date <- as.Date(pace.lai.df$ï..Date)

# remove spad and height for now
pace.lai.df <- pace.lai.df[,c('Date','SubplotID','LAI')]

# merge lai and gcc
gcc.met.pace.df <- readRDS('cache/gcc.met.pace.df.rds')

gcc.met.lai.df <- merge(gcc.met.pace.df,pace.lai.df,all.x = T,
                        by = c('Date','SubplotID'))

gcc.met.lai.df <- gcc.met.lai.df[!is.na(gcc.met.lai.df$SubplotID),]
gcc.met.lai.df <- gcc.met.lai.df[!is.na(gcc.met.lai.df$LAI),]

# check for gcc and lai
plot(GCC~LAI,data = gcc.met.lai.df)

# it seems there's too many plots with nothing
# select only high lai plots
high.vec <-
  as.character(unique(gcc.met.lai.df$Species[gcc.met.lai.df$LAI>1]))

# plot high lai points and color by species
for (i in seq_along(high.vec)){

  if(i ==1){
    plot.df <- gcc.met.lai.df[gcc.met.lai.df$Species == high.vec[i]&
                                gcc.met.lai.df$Precipitation == 'Control'&
                                gcc.met.lai.df$Temperature == 'Ambient',]
    
    plot.df$col.fac <- as.factor(plot.df$SubplotID)
    
    # palette()
    
    plot(GCC~LAI,data = plot.df,
         pch=16,col=col.fac,
         xlim=c(0,3),ylim=c(0.3,0.42))
  }else{
    points(GCC~LAI,data = gcc.met.lai.df[gcc.met.lai.df$Species == high.vec[i],],
         pch=16,col=i)
  }
 
 
}
legend('bottomright',legend = high.vec,col = palette(),pch=16)

# plot only luc
plot(GCC~Date,data = gcc.met.lai.df[gcc.met.lai.df$Species=='Luc' &
                                      gcc.met.lai.df$Precipitation == 'Control'&
                                      gcc.met.lai.df$Temperature == 'Ambient'&
                                      gcc.met.lai.df$Date > as.Date('2019-8-1'),]
     ,pch=16,col='blue')

par(new=T)
plot(LAI~Date,data = gcc.met.lai.df[gcc.met.lai.df$Species=='Luc' &
                                      gcc.met.lai.df$Precipitation == 'Control'&
                                      gcc.met.lai.df$Temperature == 'Ambient'&
                                      gcc.met.lai.df$Date > as.Date('2019-8-1'),]
     ,pch=16,col='red',ann=F,axes=F)


