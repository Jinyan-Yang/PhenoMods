gcc.df.19 <- readRDS('cache/gcc.pace.daily.2019.rds')
# gcc.df.19 <- gcc.df.19[,c("Date","SubplotID","Species","Precipitation","Temperature","GCC")]
gcc.df.18 <- readRDS('cache/gcc.pace.daily.2018.rds')
# gcc.df.18$SubplotID <- paste0('S',gcc.df.18$Shelter,'P',gcc.df.18$Plot,gcc.df.18$Subplot)
gcc.df <- rbind(gcc.df.19,gcc.df.18)
gcc.df$Shelter <- substr(gcc.df$SubplotID ,2,2)
gcc.df$Plot <- substr(gcc.df$SubplotID ,4,4)
gcc.df$Subplot <- substr(gcc.df$SubplotID ,5,5)
# 
har.flie.vec <- list.files('data/harvest/',full.names = T)

library("readxl")
har.ls <- lapply(har.flie.vec,read_excel,sheet = 'TotalHarvest')

for (i in seq_along(har.ls)) {
  
  if(length(grep('SubPlotID',names(har.ls[[i]])))>0){
    temp.df <- har.ls[[i]]
    
    temp.df$Shelter <- substr(temp.df$SubPlotID,2,2)
    temp.df$Plot <- substr(temp.df$SubPlotID,4,4)
    temp.df$Subplot <- substr(temp.df$SubPlotID,5,5)
    
    har.ls[[i]] <- temp.df
    rm(temp.df)
  }
  
  
  
  har.ls[[i]] <- har.ls[[i]][,c( "Date","Shelter","Plot", "Subplot", "Species","TotalFM" )]
  
}

har.df <- do.call(rbind,har.ls)


har.df <- har.df[!is.na(har.df$TotalFM),]
har.df  <- har.df[har.df$TotalFM >0,]
har.df <- har.df[!duplicated(har.df),]
# 
# pace.gcc.df <- readRDS('cache/pace.met.gcc.df.rds')

pace.gcc.df.har <- merge(har.df,gcc.df,by=c( "Date","Shelter","Plot", "Subplot", "Species" ),all.y=T)

pace.gcc.df.har$Date <- as.Date(pace.gcc.df.har$Date)
# 

get.change.gcc.func <- function(index.nm){
  
  # index.nm <- 25642
  tmp.df <- pace.gcc.df.har[index.nm,]
  # print(tmp.df)
  
  tmp.df$past.day.gcc <- mean(pace.gcc.df.har[pace.gcc.df.har$Date >= tmp.df$Date - 3 &
                                           pace.gcc.df.har$Date <= tmp.df$Date -1 &
                                pace.gcc.df.har$Shelter == tmp.df$Shelter &
                                pace.gcc.df.har$Plot == tmp.df$Plot &
                                pace.gcc.df.har$Subplot == tmp.df$Subplot,
                                'GCC'],na.rm=T)
  
# check.df <- pace.gcc.df.har[
#                                            pace.gcc.df.har$Shelter == tmp.df$Shelter &
#                                            pace.gcc.df.har$Plot == tmp.df$Plot &
#                                            pace.gcc.df.har$Subplot == tmp.df$Subplot,]
  
  tmp.df$next.day.gcc <- mean(pace.gcc.df.har[pace.gcc.df.har$Date <= tmp.df$Date + 3 &
                                                pace.gcc.df.har$Date >= tmp.df$Date +1 &
                                                pace.gcc.df.har$Shelter == tmp.df$Shelter &
                                                pace.gcc.df.har$Plot == tmp.df$Plot &
                                                pace.gcc.df.har$Subplot == tmp.df$Subplot,
                                              'GCC'],na.rm=T)
  
  # 
  
  return(tmp.df)
}


index.vec <- which(!is.na(pace.gcc.df.har$TotalFM))


har.gcc.ls <- lapply(index.vec, get.change.gcc.func)

har.gcc.df <- do.call(rbind,har.gcc.ls)
har.gcc.df$gcc.change <- har.gcc.df$past.day.gcc - har.gcc.df$next.day.gcc

har.gcc.df$Species <- as.factor(har.gcc.df$Species)

write.csv(har.gcc.df,'cache/pace_gcc_harvest.csv',row.names = F)


# make plots####
pdf('figures/gcc_harvest.pdf',width = 4*4,height = 4*3*.618)
# plot gcc past and harv
par(mfrow=c(4,3))

for (spc.i in seq_along(levels(har.gcc.df$Species))) {
  plot.df <- har.gcc.df[har.gcc.df$Species == levels(har.gcc.df$Species)[spc.i],]
  
  with(plot.df,plot(TotalFM~past.day.gcc,pch=LETTERS[spc.i],col='grey'))
  fit.lm <- summary(with(plot.df,lm(TotalFM~gcc.change)))
  r.sqrt <- format(fit.lm$r.squared,digits = 2)
  legend('topleft',legend = paste0('(',letters[spc.i],') ',
                                   levels(har.gcc.df$Species)[spc.i],
                                   ' R2 = ',
                                   r.sqrt),bty='n')
 }
# plot gcc after 
par(mfrow=c(4,3))

for (spc.i in seq_along(levels(har.gcc.df$Species))) {
  plot.df <- har.gcc.df[har.gcc.df$Species == levels(har.gcc.df$Species)[spc.i],]
  
  with(plot.df,plot(TotalFM~next.day.gcc,pch=LETTERS[spc.i],col='grey'))
  fit.lm <- summary(with(plot.df,lm(TotalFM~gcc.change)))
  r.sqrt <- format(fit.lm$r.squared,digits = 2)
  legend('topleft',legend = paste0('(',letters[spc.i],') ',
                                   levels(har.gcc.df$Species)[spc.i],
                                   ' R2 = ',
                                   r.sqrt),bty='n')
}


# plot change of gcc and biomass 
par(mfrow=c(4,3))

for (spc.i in seq_along(levels(har.gcc.df$Species))) {
plot.df <- har.gcc.df[har.gcc.df$Species == levels(har.gcc.df$Species)[spc.i],]
  
  with(plot.df,plot(TotalFM~gcc.change,pch=LETTERS[spc.i],col='grey'))
  fit.lm <- summary(with(plot.df,lm(TotalFM~gcc.change)))
  r.sqrt <- format(fit.lm$r.squared,digits = 2)
  legend('topleft',legend = paste0('(',letters[spc.i],') ',
                                   levels(har.gcc.df$Species)[spc.i],
                                   ' R2 = ',
                                   r.sqrt),bty='n')
  # legend('topleft',legend = levels(har.gcc.df$Species),col=palette(),pch=LETTERS[seq_along(levels(har.gcc.df$Species))],bty='n')
}
dev.off()
# # 
# with(har.gcc.df,plot(TotalFM~gcc.change,pch=LETTERS[Species],col=Species))
# legend('topleft',legend = levels(har.gcc.df$Species),col=palette(),pch=LETTERS[seq_along(levels(har.gcc.df$Species))],bty='n')
# 
# summary(with(har.gcc.df,lm(TotalFM~gcc.change)))

