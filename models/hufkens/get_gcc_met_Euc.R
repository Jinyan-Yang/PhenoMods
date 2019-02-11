library(HIEv)
# get gcc from hiev
green.df <- downloadCSV("FACE_P0037_RA_CANOPYGREENNESS-FULL_OPEN_L2.dat")
names(green.df) <- c("DateTime","Date","Ring","angle","Green")
# average by ring and date
green.sum.df <- doBy::summaryBy(Green~Date,
                                data = green.df,FUN = mean,na.rm=TRUE,keep.names = TRUE)
green.sum.df$Date <- as.Date(green.sum.df$Date)
# get met from ros
download.path <- file.path("download/")
setToPath(download.path)
startDate <- '2014-10-01'
endDate <- '2019-02-01'
ros15 <- downloadTOA5("ROS_WS_Table15",
                      startDate = startDate,
                      endDate = endDate,
                      maxnfiles = 500)
ros05 <- downloadTOA5("ROS_WS_Table05",
                      startDate = startDate,
                      endDate = endDate,
                      maxnfiles = 500)

library(data.table)

ros05_30 <- data.table(ros05)[,list(PPFD=mean(PPFD_Avg, na.rm=TRUE),
                                    Tair=mean(AirTC_Avg, na.rm=TRUE),
                                    Tmax = max(AirTC_Avg, na.rm=TRUE),
                                    Tmin = min(AirTC_Avg, na.rm=TRUE),
                                    RH=mean(RH, na.rm=TRUE),
                                    RHmax=max(RH, na.rm=TRUE),
                                    RHmin=min(RH, na.rm=TRUE),
                                    u2 = mean(WS_ms_Avg,na.rm=TRUE)
                                    ),by = 'Date']


ros15_30 <- data.table(ros15)[,list(Rain=sum(Rain_mm_Tot, na.rm=TRUE)),
                              by = 'Date']

ros.1 <- merge(ros15_30, ros05_30)

# combine gcc with net
gcc.met.df <- merge(green.sum.df,ros.1,all=TRUE)

# need to get 16 dates of met before gcc
start.date <- gcc.met.df$Date[min(which(!is.na(gcc.met.df$Green)))]

gcc.met.df <- gcc.met.df[gcc.met.df$Date >= (start.date - 16),]

gcc.met.df <- gcc.met.df[!is.na(gcc.met.df$PPFD),]

tmp.df <- gcc.met.df[gcc.met.df$Date <= start.date,]

gcc.met.df <- gcc.met.df[!is.na(gcc.met.df$Green),]

gcc.met.df <- rbind(tmp.df,gcc.met.df)

# normalis GCC
gcc.met.df$GCC.norm <- (gcc.met.df$Green - min(gcc.met.df$Green,na.rm=T)) / 
  (-min(gcc.met.df$Green,na.rm=T) + max(gcc.met.df$Green,na.rm=T))

gcc.met.df$map <- 830

gcc.met.df <- gcc.met.df[gcc.met.df$PPFD>0,]

saveRDS(gcc.met.df,'gcc.met.df.rds')