# par RH and tair
# PACE_AUTO_S6_ABVGRND_R_20190228.dat


download.path <- file.path("download/")
setToPath(download.path)
startDate = '2018-04-01'
endDate = '2018-08-30'

shelter.vec <- 1:6
met.ls <- list()
for (shelter.num in shelter.vec){
  fn <- sprintf('PACE_AUTO_S%s_ABVGRND_R_',shelter.num)
  library(HIEv)
  met.df <- downloadTOA5(fn, 
                        maxnfiles = 100, 
                        rowbind=TRUE,
                        startDate = startDate,
                        endDate = endDate)      
  met.df$RH_Avg[met.df$RH_Avg > 100] <- 100
  met.df$RH_Avg[met.df$RH_Avg < 0] <- 0

  library(data.table)
  met.ls[[shelter.num]] <- data.table(met.df)[,list(Tair = mean(AirT2_Avg,na.rm=TRUE),
                                            Tmin = min(AirT2_Avg,na.rm=TRUE),
                                            Tmax = max(AirT2_Avg,na.rm=TRUE),
                                            par = mean(PAR_Avg,na.rm=TRUE),
                                            rh = mean(RH_Avg, na.rm=TRUE),
                                            RHmax = max(RH_Avg, na.rm=TRUE),
                                            RHmin = min(RH_Avg, na.rm=TRUE)),
                                      by=Date]
  
  met.ls[[shelter.num]]$Shelter <- shelter.num
}

met.df <- do.call(rbind,met.ls)

gcc.swc.irg.ws.met.df <- merge(gcc.swc.irg.ws.df,met.df,by=c('Date','Shelter'),all=T)

gcc.swc.irg.ws.met.df <- gcc.swc.irg.ws.met.df

gcc.swc.irg.ws.met.df$irrigsum[is.na(gcc.swc.irg.ws.met.df$irrigsum)] <- 0

saveRDS(gcc.swc.irg.ws.met.df,'pace_gcc_met.rds')
