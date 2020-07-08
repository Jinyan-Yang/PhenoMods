gcc.met.pace.df <- readRDS('cache/gcc.met.pace.df.rds')
# with(gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P1A',],plot(GCC~Date))

# 
get.norm.gcc.func <- function(df){
  quantiles.5.95 <- quantile(df$GCC[!is.na(df$GCC)],
                             c(.05,.95),an.rm=T)
  quantiles.5.95[1] = 0.3197
  df$GCC.norm <- (df$GCC - quantiles.5.95[1]) /
    (quantiles.5.95[2] - quantiles.5.95[1])
  
  return(df)
}

# 
get.smooth.gcc.func = function(Date.vec,gcc.vec){
  library(mgcv)
  library(lubridate)
  gam.frdm = round(length(Date.vec)/3)
  
  gam.in.df = data.frame(x = as.numeric(Date.vec),
                         y = gcc.vec)
  fit.gam <- gam(y~s(x,k = gam.frdm),data = gam.in.df)
  
  out.df = predict(fit.gam,gam.in.df)
  return(out.df)
}

# get pace data 
get.pace.func <- function(gcc.met.pace.df,
                          species.in,
                          prep.in,
                          temp.in,subplot = NA){
  # temp.df <- gcc.met.pace.df[gcc.met.pace.df$Species == 'Luc'&
  #                              gcc.met.pace.df$Precipitation == 'Control'&
  #                              gcc.met.pace.df$Temperature == 'Ambient',]
  if(is.na(subplot)){
    temp.df <- gcc.met.pace.df[gcc.met.pace.df$Species == species.in&
                                 gcc.met.pace.df$Precipitation == prep.in&
                                 gcc.met.pace.df$Temperature == temp.in,]
  }else{
    temp.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == subplot,]
  }

  temp.df <- temp.df[!is.na(temp.df$Date),]
  
  if(nrow(temp.df)<2)stop('species/treament incorrect')
  
  # temp.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P6A',]
  
  subplot.vec <- unique(temp.df$SubplotID)
  
  temp.ls <- split(temp.df,temp.df$SubplotID)
  
  result.ls <- lapply(temp.ls,get.norm.gcc.func)
  
  # quantiles.5.95 <- quantile(temp.df$GCC[!is.na(temp.df$GCC)],
  #                            c(.05,.95),an.rm=T)
  # temp.df$GCC.norm <- (temp.df$GCC - quantiles.5.95[1]) /
  #   (quantiles.5.95[2] - quantiles.5.95[1])
  temp.norm.df <- do.call(rbind,result.ls)
  
  library(doBy)
  sd.df <- summaryBy(GCC.norm + vwc ~ Date,
                     data = temp.norm.df,FUN=c(sd),na.rm=TRUE,keep.names = F)
  
  mean.df <- summaryBy(.~ Date,
                       data = temp.norm.df,FUN=c(mean),na.rm=TRUE,keep.names = T)
  
  test.df <- merge(mean.df,sd.df)
  
  # test.df <- test.df[order(test.df$Date),]
  
 
  
  # set the number of lageed days
  test.df$GCC[1:day.lag] <- NA
  
  # test.df$GCC[1:day.lag] <- NA
  
  start.date <- test.df$Date[min(which(!is.na(test.df$GCC.norm)))]
  
  gcc.met.pace.df.na.rm <- test.df[test.df$Date > start.date,]
  
  gcc.met.pace.df.na.rm$GCC.norm <- na.locf(gcc.met.pace.df.na.rm$GCC.norm)
  
  gcc.met.pace.df.16 <- rbind(gcc.met.pace.df.na.rm,
                              test.df[test.df$Date <= start.date &
                                        test.df$Date > (start.date- (day.lag + 1)),])
  
  gcc.met.pace.df.16 <- gcc.met.pace.df.16[order(gcc.met.pace.df.16$Date),]
  
  gcc.met.pace.df.16 <- gcc.met.pace.df.16[!is.na(gcc.met.pace.df.16$Date),]
  
  # the minimum seems to be an outliner
  # use the second lowest instead
  # low.2nd <- sort(gcc.met.pace.df.16$GCC[!is.na(gcc.met.pace.df.16$GCC)])[2]

  # 
  gcc.met.pace.df.16$PPFD <- na.locf(gcc.met.pace.df.16$PAR.ros)
  gcc.met.pace.df.16$u2 <- na.locf(gcc.met.pace.df.16$WS_ms_Avg)
  gcc.met.pace.df.16$Rain <- na.locf(gcc.met.pace.df.16$irrig.tot)
  gcc.met.pace.df.16$Tair <- na.locf(gcc.met.pace.df.16$Tair)
  gcc.met.pace.df.16$RHmax <- na.locf(gcc.met.pace.df.16$RHmax)
  gcc.met.pace.df.16$RHmin <- na.locf(gcc.met.pace.df.16$RHmin)
  gcc.met.pace.df.16$Tmax <- na.locf(gcc.met.pace.df.16$Tmax)
  gcc.met.pace.df.16$Tmin <- na.locf(gcc.met.pace.df.16$Tmin)
  
  
  gcc.met.pace.df.16$GCC.smooth = get.smooth.gcc.func(gcc.met.pace.df.16$Date, 
                                                      gcc.met.pace.df.16$GCC)
  
  gcc.met.pace.df.16$GCC.norm.smooth = get.smooth.gcc.func(gcc.met.pace.df.16$Date, 
                                                      gcc.met.pace.df.16$GCC.norm)
    
  # tmp.df = get.smooth.gcc.func(y, gcc.met.pace.df.16$GCC)
  # 
  # out.df = merge(gcc.met.pace.df.16,tmp.df,by='y',all.x=T)
  
  return(gcc.met.pace.df.16)
}

