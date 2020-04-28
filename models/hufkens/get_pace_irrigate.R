# irrigation/rain
# PACE_AUTO_ALL_IRRIG_R_20190228.dat


# wind speed
# PACE_AUTO_ALL_WINDSPEED_R_20190228.dat
download.path <- file.path("download/")
setToPath(download.path)
startDate = '2018-01-01'
endDate = '2019-12-31'

library(HIEv)
irig.df <- downloadTOA5('PACE_AUTO_ALL_IRRIG_R_', 
                      maxnfiles = 100, 
                      rowbind=TRUE,
                      startDate = startDate,
                      endDate = endDate)  
ws.df <- downloadTOA5('PACE_AUTO_ALL_WINDSPEED_R', 
                        maxnfiles = 100, 
                        rowbind=TRUE,
                        startDate = startDate,
                        endDate = endDate)    
# plot(WS_ms_Avg~DateTime,data = ws.df[ws.df$Date == as.Date('2018-05-10'),])
# daily windspeed avg really is not ideal!
ws.daily.df <- summaryBy(WS_ms_Avg~Date,data = ws.df,FUN=mean,na.rm=TRUE,keep.names = TRUE)

irig.df$currentplot <- as.character(irig.df$currentplot)
# irig.df$Shelter <- grep(irig.df$currentplot,'S[:digit:]-')

sub.nm.df <- as.data.frame(do.call(rbind,strsplit(irig.df$currentplot,split = c('-'))))
names(sub.nm.df) <- c('Shelter','Plot')

irig.df$Shelter <- as.numeric(sub.nm.df$Shelter)

irig.df$Plot <- as.numeric(as.character(sub.nm.df$Plot))

irig.df <- irig.df[,c('Date','irrigsum','Shelter','Plot')]
library(doBy)

irig.df <- summaryBy(irrigsum~Date + Shelter + Plot,FUN=sum,na.rm=T,
                     data = irig.df,keep.names = T)

# 
saveRDS(irig.df,'cache/irig.rds')
saveRDS(ws.daily.df,'cache/ws.daily.rds')

irrig.wd.df <- merge(irig.df,ws.daily.df,all=T)
saveRDS(irrig.wd.df,'cache/irrig.wd.df.rds')
# saveRDS(irig.df,'cache/irig2019.rds')
# gcc.swc.irg.df <- merge(gcc.swc.df,irig.df,
#                         all.x=TRUE,all.y=FALSE,
#                         by=c('Date','Shelter','Plot'))
# 
# 
# 
# gcc.swc.irg.ws.df <- merge(gcc.swc.irg.df,
#                            ws.daily.df,
#                            all.x=TRUE,all.y=FALSE,
#                            by=c('Date'))

# see.df <- gcc.swc.irg.ws.df[gcc.swc.irg.ws.df$SubplotID == 'S1P8B',]
