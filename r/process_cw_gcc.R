cw.gcc.df <- read.csv('download/GrasslandGCC_12Sites_Fulldataset.csv')
cw.gcc.df$Date <- as.Date(as.character(cw.gcc.df$daterep),'%d/%m/%Y')

library(doBy)
cw.gcc.df.cln <- summaryBy(GCCmean~Date + Location + Code + PS + Native,
                           data = cw.gcc.df,FUN=mean,na.rm=TRUE,
                           keep.names = TRUE)

site.vec <- unique(cw.gcc.df.cln$Code)
site.vec <- as.character(site.vec)
# i=7
bucket.size = 300
tmp.ls <- list()
for (i in seq_along(site.vec)) {
  gcc.tmp.df <- cw.gcc.df.cln[cw.gcc.df.cln$Code == site.vec[i],]
  
  tmp.met.df <- readRDS(sprintf('cache/met.%s20012016.rds',site.vec[i]))
  # 
  gcc.met.df <- merge(gcc.tmp.df,tmp.met.df,all.x=TRUE)
  colnames(gcc.met.df) <- c("Date", "Location","Code" ,
                            "PS" , "Native","GCC", 
                            "lat","lon", "Rain","Tmax","Tmin",
                            "Tair" ,"PPFD","vp","RHmin","RHmax")
  gcc.met.df$RHmax[gcc.met.df$RHmax>100] <- 100
  gcc.met.df$RH <- (gcc.met.df$RHmax + gcc.met.df$RHmin)/2
  gcc.met.df$u2 <- 2
  
  # gcc.max <- max(gcc.met.df$GCCmean,na.rm = T)
  # gcc.min <- min(gcc.met.df$GCCmean,na.rm = T)
  # gcc.met.df$GCC.norm <- (gcc.met.df$GCCmean - gcc.min) / (gcc.max - gcc.min)
  
  # gcc.met.df$GCC.norm[1:2] <- NA
  # gcc.met.df$GCC.norm.sd <- gcc.met.df$GCC.norm * 0.4
  gcc.met.df$map <- mean(tmp.met.df$Rain,na.rm = T)*365.25
  gcc.met.df$vwc <- 0.08
  
  gcc.met.df$Species <- site.vec[i]
  
  tmp.ls[[i]] <- gcc.met.df
}

gcc.met.cw.df <- do.call(rbind,tmp.ls)

gcc.met.cw.df$Precipitation <- 'Control'
gcc.met.cw.df$Temperature <- 'Ambient'
gcc.met.cw.df$SubplotID <- 1

gcc.met.cw.df$PAR.ros <- gcc.met.cw.df$PPFD
gcc.met.cw.df$WS_ms_Avg <- gcc.met.cw.df$u2
gcc.met.cw.df$irrig.tot <- gcc.met.cw.df$Rain
gcc.met.cw.df$rh <- gcc.met.cw.df$RH
# gcc.met.cw.df$harvest <- 0

saveRDS(gcc.met.cw.df,'cache/gcc.met.cw.df.rds')
