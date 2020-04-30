
# read data
header.df <- read.csv('download/SubPlotTreatmentsMaster 2019-02-13.csv')
header.df <- subset(header.df,select = -c(SubPlotID,x1,x2,y1,y2))

# gcc.df <- readRDS('cache/gcc.pace.daily.rds')
gcc.df.19 <- readRDS('cache/gcc.pace.daily.2019.rds')
# gcc.df.19 <- gcc.df.19[,c("Date","SubplotID","Species","Precipitation","Temperature","GCC")]
gcc.df.18 <- readRDS('cache/gcc.pace.daily.2018.rds')
# gcc.df.18$SubplotID <- paste0('S',gcc.df.18$Shelter,'P',gcc.df.18$Plot,gcc.df.18$Subplot)

gcc.df <- rbind(gcc.df.19,gcc.df.18)
met.df <- readRDS('cache/pace.met.rds')
irrig.df <- readRDS('cache/irig.rds')
ws.df <- readRDS('cache/ws.daily.rds')
swc.df <- readRDS('cache/swc.rds')


# merge data
met.ws.df <- merge(met.df,ws.df,by='Date',all=T)
met.ws.ir.df <- merge(met.ws.df,irrig.df,by=c('Date','Shelter'),all=T)
# temp.df <- met.ws.ir.df[,c('Date','Shelter','Plot')]
# temp.df <- temp.df[with(temp.df, order(Date, Shelter,Plot)),]
# 
# temp.2.df <- temp.df[rep(seq_len(nrow(temp.df)), each=4),]
# temp.2.df$Subplot <- rep(LETTERS[1:4],nrow(temp.df))
s.date <- min(met.ws.ir.df$Date,na.rm=T)
e.date <- max(met.ws.ir.df$Date,na.rm=T)
date.vec <- seq(s.date,e.date,by='day')
temp.df <- data.frame(Date = rep(date.vec,each = 6 * 8 * 4),
                      Shelter = rep(1:6, each= 8 * 4),
                      Plot = rep(rep(1:8,each = 4),6 * length(date.vec)),
                      Subplot = rep(rep(LETTERS[1:4],8), 6 * length(date.vec)))

nm.info.df <- merge(temp.df,header.df,by = c('Shelter','Plot','Subplot'),all.x=T)

met.nm.df <- merge(nm.info.df,met.ws.ir.df,all.x=T,by=c('Date','Shelter','Plot'))
met.nm.df$SubplotID <- as.character(met.nm.df$SubplotID)
gcc.df$SubplotID <- as.character(gcc.df$SubplotID)
met.gcc.df <- merge(met.nm.df,gcc.df,all.x=T)

swc.df$SubplotID <- as.character(swc.df$SubPlotID)
met.gcc.swc.df <-  merge(met.gcc.df,subset(swc.df,select = c(Date,vwc,Location,SubplotID)),
                         all.x=T,by=c('Date','SubplotID'))

saveRDS(met.gcc.swc.df,'cache/gcc.met.pace.df.auto.rds')


# add hand water
met.gcc.swc.df <- readRDS('cache/gcc.met.pace.df.auto.rds')
hand.water.df <- read.csv('data/Added irrigation for June 2018-May 2019.csv')

library(tidyr)
hand.water.df.long <- gather(hand.water.df,key = 'Date',value = 'hand.water',5:54)
hand.water.df.long <- hand.water.df.long[!is.na(hand.water.df.long$SubplotID),]
hand.water.df.long$Date <- gsub('X','',hand.water.df.long$Date)
hand.water.df.long$Date <- as.Date(hand.water.df.long$Date,'%d.%m.%Y')
hand.water.df.long$hand.water <- as.numeric(hand.water.df.long$hand.water)
hand.water.df.long$SubplotID <- as.character(hand.water.df.long$SubplotID)



met.gcc.swc.df.add <- merge(met.gcc.swc.df,
                            hand.water.df.long[,c('Date','SubplotID','hand.water')],
                            by = c('Date','SubplotID'),all=T)
names(met.gcc.swc.df.add)[names(met.gcc.swc.df.add) == 'irrigsum'] <- 'irrig.auto'

met.gcc.swc.df.add$hand.water[is.na(met.gcc.swc.df.add$hand.water)] <-  0

met.gcc.swc.df.add$irrig.tot <- met.gcc.swc.df.add$hand.water + met.gcc.swc.df.add$irrig.auto
# add harvest
# get harveast date
library(XLConnect)
event.wb <- loadWorkbook('data/PACE Events log - Start 2017.xlsx')
event.df <- readWorksheet(event.wb,sheet = 'PACE')
event.df$Date <- as.Date(event.df$Event.Date)
hav.df = event.df[event.df$Item..keyword. == 'Harvest',]
# fix date
hav.df$Date = as.Date(NA)

hav.df$Date[nchar(hav.df$Event.Date) == 19] = 
  as.Date(hav.df$Event.Date[nchar(hav.df$Event.Date) == 19])

hav.df$Date[nchar(hav.df$Event.Date) < 19] = 
  as.Date(hav.df$Event.Date[nchar(hav.df$Event.Date) < 19],'%d-%m-%Y')

# get species
tmp.ls = list()

spc.vec = c('Bis','Dig','DigBis','Fes',
            'Kan','KanWal','Luc','Pha',
            'PhaSub','Rho','Rye','Wal')

for (i in seq_along(spc.vec)) {
  
  if(!spc.vec[i] %in% c('PhaSub','Pha')){
    
    tmp.ls[[i]]=data.frame(Date = hav.df$Date[grep(spc.vec[i],
                                                   hav.df$Activity...Description,
                                                   ignore.case = T)],
                           Species = spc.vec[i])
    
  }
  
  if(spc.vec[i]=='PhaSub'){
    v1 = hav.df$Date[grep('PhaSub',
                          hav.df$Activity...Description,ignore.case = T)]
    v2 = hav.df$Date[grep('Pha/',
                          hav.df$Activity...Description,ignore.case = T)] 
    v3 = hav.df$Date[grep('Phal/',
                          hav.df$Activity...Description,ignore.case = T)]
    
    tmp.ls[[i]]=data.frame(Date = c(v1,v2,v3),
                           Species = spc.vec[i])
  }
  
  
  if(spc.vec[i]=='Pha'){
    v1 = hav.df$Date[grep('Pha ',
                          hav.df$Activity...Description,ignore.case = T)]
    v2 = hav.df$Date[grep('Pha,',
                          hav.df$Activity...Description,ignore.case = T)] 
    v3 = hav.df$Date[grep(' Pha',
                          hav.df$Activity...Description,ignore.case = T)]
    
    tmp.ls[[i]]=data.frame(Date = c(v1,v2,v3),
                           Species = spc.vec[i])
    
  }
  
  
  
  
}

hav.date.df=do.call(rbind,tmp.ls)
hav.date.df$harvest = 1
write.csv(hav.date.df,'cache/pace.harvest.date.csv',row.names = F)
met.gcc.swc.df.add.har = merge(met.gcc.swc.df.add,hav.date.df,by=c('Date','Species'),
                               all.x=T)
met.gcc.swc.df.add.har$harvest[is.na(met.gcc.swc.df.add.har$harvest)] = 0
# save results
saveRDS(met.gcc.swc.df.add.har,'cache/gcc.met.pace.df.rds')
write.csv(met.gcc.swc.df.add,'gcc.met.pace.df.csv',row.names = F)
sum(met.gcc.swc.df.add$irrig.tot[met.gcc.swc.df.add$SubplotID == 'S1P1A'],na.rm = T)
