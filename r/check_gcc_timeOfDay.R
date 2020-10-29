cw.df <- read.csv('data/GrasslandGCC_12Sites_Fulldataset.csv')
cw.df$date <- as.Date(as.character(cw.df$daterep),'%d/%m/%Y')
plot(GCC1~timerep3,data = cw.df,pch=16,col=Code)

code.vec <- levels(cw.df$Code)


pdf('figures/cw.gcc.time.pdf',width = 10,height = 10*.618)

par(mfrow = c(2,2))

for(i in seq_along(code.vec)){
  plot(GCC9~date,data = cw.df[cw.df$Code == code.vec[i],],pch=16,col=timerep3,
       xlab= code.vec[i],ylim=c(0.28,0.45))
  if(i %in% c(1,5,9)){
    legend('top',legend = unique(cw.df$timerep3),pch=16,col=palette(),horiz = T)
  }
}

par(mfrow = c(2,2))

for(i in seq_along(code.vec)){
  plot(GCC8~date,data = cw.df[cw.df$Code == code.vec[i],],pch=16,col=timerep3,
       xlab= code.vec[i],ylim=c(0.28,0.45))
  if(i %in% c(1,5,9)){
    legend('top',legend = unique(cw.df$timerep3),pch=16,col=palette(),horiz = T)
  }
}

par(mfrow = c(2,2))

for(i in seq_along(code.vec)){
  plot(GCC7~date,data = cw.df[cw.df$Code == code.vec[i],],pch=16,col=timerep3,
       xlab= code.vec[i],ylim=c(0.28,0.45))
  if(i %in% c(1,5,9)){
    legend('top',legend = unique(cw.df$timerep3),pch=16,col=palette(),horiz = T)
  }
}

dev.off()