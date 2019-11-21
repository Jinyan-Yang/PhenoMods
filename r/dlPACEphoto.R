library(HIEv)
setToPath('download/pic')
setToken()


fn.vec <- sprintf('PACE_AUTO_S1_PHENOCAM_R_%s.ZIP',format(s1p3c.df.sub$Date,'%Y%m%d'))

mapply(function(x){try(downloadHIEv(searchHIEv(x)))},
       fn.vec)
# ip.file.extract(file, zipname = fn.vec[1], unzip = getOption("unzip"))
# 
# mapply(function(x){},
#        fn.vec)


extract.pic.fun <- function(fn.nm){
  zip.fn <- file.path('download/pic',fn.nm)
  try(zipped_pic_names <- grep('PACECAM03', unzip(zip.fn, list=TRUE)$Name, 
                           ignore.case=FALSE, value=TRUE))
  try(unzip(zip.fn, files=zipped_pic_names[1],exdir = 'download/pic/P3'))
}

mapply(extract.pic.fun,
       fn.vec)


# 1700	2200	1300	1800
# Rye	Drought	Ambient


pic.vec <- file.path('download/pic',list.files('download/pic',pattern=".jpg"))

list.files('download/pic',pattern=".jpg")

library('imager')
test1 <- load.image(pic.vec[1])

par(mar=c(5,5,1,1))
plot(test1,xlim=c(1700,2200),ylim=c(1300,1800))

polygon(c(1700,1700,2200,2200),c(1300,1800,1800,1300),border='red',lwd=3)

test1.crop <- test1[1700:2200,1300:1800,]


par(fig = c(0,1, 0.5, 1), new = T)  
par(mar=c(5,5,1,1))
i=1
plot(G~days,data = s1p1a.df,pch=16,col='grey',xaxt='n',xlab='',ylab='Greenness')
axis(1,at = s1p1a.df.sub$days[i],labels = s1p1a.df.sub$date[i])
points(gcc.pred~days,data = plot.df,type='l',col='grey',lwd = 3)

abline(v = s1p1a.df.sub$days[i],lwd = 2,col='darkseagreen') 


# rn.func <- function(old.nm){
#   # paste0('download/pic/',str_sub(old.nm,1,10),str_sub(old.nm ,16,23),'.jpg')
#   
#   a <- format(file.info(old.nm)$ctime,'%Y%m%d')
#   b <- 'cam1'
#   return(paste0(b,a,'.jpg'))
# }
# 
# format(file.path('download/pic',list.files('download/pic',pattern=".jpg"))[1]$ctime,'%Y%m%d')
# 
# new.nm.vec <- unname(mapply(rn.func,file.path('download/pic',list.files('download/pic',pattern=".jpg"))))
# 
# 
# 
# file.rename(file.path('download/pic',list.files('download/pic',pattern=".jpg")), new.nm.vec)
