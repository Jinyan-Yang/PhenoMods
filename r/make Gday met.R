pace.met.df <- readRDS('cache/pace.met.rds')
irrig.df <- readRDS('cache/irig.rds')

# read data####
gcc.met.pace.df <- readRDS('cache/gcc.met.pace.df.processed.rds')

temp.df <- gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P6A',]


# 
startDate = as.Date('2013-01-01')
endDate = as.Date('2014-12-31')
n.days <- as.numeric(endDate - startDate)
# get ros par to replace the bad data in pace####
library(HIEv)
download.path <- file.path("download/")
setToPath(download.path)
setToken()
ros15 <- downloadTOA5("ROS_WS_Table15",
                      startDate = startDate,
                      endDate = endDate,
                      maxnfiles = 500)
ros.rain.day.df <- doBy::summaryBy(Rain_mm_Tot~Date,data = ros15,
                                   FUN=sum,keep.names = T,na.rm=T)
# ros.rain.day.df <- (irrig.df[irrig.df$Shelter == 1 & irrig.df$Plot == 6,])
# library(doBy)
# 
# ros.rain.day.df <- summaryBy(irrigsum~Date + Shelter + Plot,FUN=sum,na.rm=T,
#                      data = ros.rain.day.df,keep.names = T)

ros05 <- downloadTOA5("ROS_WS_Table05",
                      startDate = startDate,
                      endDate = endDate,
                      maxnfiles = 500)

ros05$PPFD[ros05$PPFD_Avg < 0] <- 0
ros05$par <- 5*60 * ros05$PPFD_Avg * 10^-6 / 4.57

library(data.table)
ros.day.1.df <- data.table(ros05)[,list(PAR = sum(par, na.rm=TRUE),
                                        Tair=mean(AirTC_Avg, na.rm=TRUE),
                                        Tmax = max(AirTC_Avg, na.rm=TRUE),
                                        Tmin = min(AirTC_Avg, na.rm=TRUE),
                                        RH=mean(RH, na.rm=TRUE),
                                        RHmax=max(RH, na.rm=TRUE),
                                        RHmin=min(RH, na.rm=TRUE),
                                        u2 = mean(WS_ms_Avg,na.rm=TRUE)
                                        
),by = 'Date']

# ros.day.1.df$Date <- as.Date(ros.day.1.df$Date,tz = 'UTC')
# ros.day.1.df <- ros.day.1.df[order(ros.day.1.df$Date),]
# ros.rain.day.df$Date <- as.Date(ros.rain.day.df$Date,tz = 'UTC')
# ros.rain.day.df <- ros.rain.day.df[order(ros.rain.day.df$Date),]

ros.day.df <- merge(ros.day.1.df,ros.rain.day.df[,c('Date','Rain_mm_Tot')],by='Date',all.x=T)

# ros.day.df$Rain_mm_Tot <- ros.day.df$irrigsum
ros.day.df$Rain_mm_Tot[is.na(ros.day.df$Rain_mm_Tot)] <- 0
# 
# setdiff(ros.day.df$Date,ros.day.1.df$Date)
# unique(ros.day.1.df$Date[!ros.day.1.df$Date %in% ros.rain.day.df$Date])
# ros.day.df[ros.day.df$Date %in% ros.day.1.df$Date[duplicated(ros.day.df$Date)],]
# ros.day.df[duplicated(ros.day.df$Date),]
# ros.day.df$Date[1:365] - ros.day.1.df$Date
  
met.fake.df <- data.frame(year = year(ros.day.df$Date),
                          doy = yday(ros.day.df$Date),
                          tair = ros.day.df$Tair,
                          rain = ros.day.df$Rain_mm_Tot,
                          tsoil = 20,
                          tam = ros.day.df$Tair,
                          tpm = ros.day.df$Tair,
                          tmin = ros.day.df$Tmin,
                          tmax = ros.day.df$Tmax,
                          tday = ros.day.df$Tair+1,
                          vpd_am = 1.5,
                          vpd_pm = 2.0,
                          co2 = 400,
                          ndep = 0,
                          nfix = 0,
                          wind = ros.day.df$u2,
                          pres = 101,
                          wind_am = ros.day.df$u2,
                          wind_pm = ros.day.df$u2,
                          par_am = ros.day.df$PAR/2,
                          par_pm= ros.day.df$PAR/2)

write.csv(met.fake.df,'met.csv',col.names = NA,row.names = F)

# 
library(data.table)
ros.day.1.df <- data.table(ros05)[,list(PAR = sum(par, na.rm=TRUE),
                                        Tair=mean(AirTC_Avg, na.rm=TRUE),
                                        Tmax = max(AirTC_Avg, na.rm=TRUE),
                                        Tmin = min(AirTC_Avg, na.rm=TRUE),
                                        RH=mean(RH, na.rm=TRUE),
                                        RHmax=max(RH, na.rm=TRUE),
                                        RHmin=min(RH, na.rm=TRUE),
                                        u2 = mean(WS_ms_Avg,na.rm=TRUE)
                                        
),by = 'Date']

ros.day.df.sgs <- ros.day.df

ros.day.df.sgs$vpd <- (1-ros.day.df.sgs$RH/100)*0.61375*exp(17.502*ros.day.df.sgs$Tair/(240.97+ros.day.df.sgs$Tair))

write.csv(ros.day.df.sgs[,c('Date','Rain_mm_Tot','Tmin','Tmax','PAR','vpd','u2')],
          'ros.csv',row.names = F)

ros.day.df.sgs <- read.csv('ros.csv')
