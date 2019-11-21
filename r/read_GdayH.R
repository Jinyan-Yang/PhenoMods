hufkens.df <- read.csv('E:/PTP/PTP/hufkens/outPutHufken.csv',skip=1)
hufkens.df$Date <- seq(as.Date('2018-1-1'),as.Date('2018-12-31'),by='day')
plot(lai~Date,data = hufkens.df)
par(new=T)
plot(wtfac_root~Date,data = hufkens.df,col='navy',ann=F,axes=F,pch=16)

plot(lai~Date,data = hufkens.df[hufkens.df$Date %in% temp.df$Date,],pch=16,col='grey')
par(new=T)
plot(GCC~Date,data = temp.df,col='red')