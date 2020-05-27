# gcc.pace.df <- read.csv('download/ColourCoordinates2018.csv')
# gcc.pace.df$GCC <- gcc.pace.df$G / (gcc.pace.df$R + gcc.pace.df$G + gcc.pace.df$B)
# gcc.pace.df$date <- as.Date(as.character(gcc.pace.df$ï..DateTime),'%d/%m/%Y')
# 
# gcc.pace.df.daily <- doBy::summaryBy(GCC~  date + Cam + Subplot,
#                                      FUN = mean, na.rm=T,keep.names = T,
#                                      data = gcc.pace.df)
# 
# gcc.pace.df.daily$Shelter <- ceiling(gcc.pace.df.daily$Cam / 8)
# 
# gcc.pace.df.daily$Plot <- gcc.pace.df.daily$Cam %% 8
# gcc.pace.df.daily$Plot[gcc.pace.df.daily$Plot == 0] <- 8
# # names(gcc.pace.df.daily) <- c('Date','Cam','Subplot','RCC','GCC','BCC','Shelter','Plot')


gcc.pace.df.daily <- readRDS('cache/gcc.met.pace.df.processed.rds')

# plot
gcc.pace.df.daily$days <- as.numeric(gcc.pace.df.daily$Date - min(gcc.pace.df.daily$Date))
# 
# plot(GCC~date,data = gcc.pace.df.daily[gcc.pace.df.daily$Cam == 1 & gcc.pace.df.daily$Subplot == 'A',])
# 
# plot(GCC~days,data = gcc.pace.df.daily[gcc.pace.df.daily$Cam == 1 & gcc.pace.df.daily$Subplot == 'A',])



# library(mgcv)
# 
# fit.gam <- gam(GCC~s(days),
#                data = gcc.pace.df.daily[gcc.pace.df.daily$Cam == 1 & gcc.pace.df.daily$Subplot == 'A',])
# 
# plot.df <- data.frame(days = gcc.pace.df.daily$days[gcc.pace.df.daily$Cam == 1 & gcc.pace.df.daily$Subplot == 'A'],
#                       gcc.pred = predict(fit.gam))
# 
# # points(gcc.pred~days,data = plot.df,type='l',col='grey')




s1p3c.df <- gcc.pace.df.daily[gcc.pace.df.daily$SubplotID == 'S1P3C',]
s1p3c.df$days <- as.numeric(s1p3c.df$Date - min(s1p3c.df$Date)) + 1
s1p3c.df <- s1p3c.df[order(s1p3c.df$days),]
s1p3c.df <- s1p3c.df[!is.na(s1p3c.df$GCC),]

# s1p3c.df.sub <- s1p3c.df[seq(1, nrow(s1p3c.df), 7), ]
# s1p3c.df.sub <- s1p3c.df.sub[s1p3c.df.sub$date != as.Date('2018-07-09'),]
library(mgcv)
fit.gam <- gam(GCC~s(days,k=10),
               data = s1p3c.df)
# 
# plot.df <- data.frame(days = fit.gam$coefficients,
#                       gcc.pred = fit.gam$fitted.values)

par(mar=c(5,5,5,5))
plot(GCC~Date,data = s1p3c.df,pch=16,col='darkseagreen')
points(fit.gam$fitted.values~s1p3c.df$Date,type='l',col='grey80',lwd=3)
par(new=T)
plot(irrig.tot~Date,data = s1p3c.df,type='s',col='blue',ann=F,axes=F,xlab='',ylab='')
axis(4,at=seq(0,15,by=5),labels = seq(0,15,by=5))
mtext('Irrigation (mm/d)',side = 4,line=2,col='blue')





file.dir.nm <- 'download/pic/P3'

pic.vec <- file.path(file.dir.nm,list.files(file.dir.nm,pattern=".jpg"))

# list.files(file.dir.nm,pattern=".jpg")

library('imager')


date.vec <- unique(s1p3c.df.sub$date)

for(i in seq_along(date.vec)){
  
  # 
  img.num <- grep(format(date.vec[i],'%Y%m%d'),pic.vec)
  if(length(img.num)>=1){
    
    jpeg(filename = paste0('s1p3c/',i,'.jpg'),
         width = 1200, height = 1200*0.618, units = "px", pointsize = 12,
         quality = 75,
         bg = "white")
    par(mar=c(5,5,1,5))
    par(fig = c(0,1,0,1))
    plot(GCC~days,data = s1p3c.df,pch=16,col='grey',xaxt='n',xlab='',ylab='Greenness')
    axis(1,at = s1p3c.df.sub$days[i],labels = s1p3c.df.sub$date[i])
    points(gcc.pred~days,data = plot.df,type='l',col='grey',lwd = 3)
    abline(v = s1p3c.df.sub$days[i],lwd = 2,col='darkseagreen')
    
    # 
    test1 <- load.image(pic.vec[img.num])
    par(fig = c(0.45,0.85, 0.5, 1), new = T)  
    # par(mar=c(5,5,1,1))
    plot(test1,ann=F,axes=F,xlim=c(650,1150),ylim=c(300,800), asp = 1)
    # 650	1150	300	800
    polygon(c(650,650,1150,1150),c(300,800,800,300),border='red',lwd=3)
    
    
    dev.off()
  }
}

