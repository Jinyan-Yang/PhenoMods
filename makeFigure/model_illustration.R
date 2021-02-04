# ym.18.df <- get.ym.func(18)
# with(ym.18.df,plot(swc~Date))
# tmp.df <- get.pace.func(ym.18.df,
#                         species.in ='ym',
#                         prep.in = 'Control', temp.in ='Ambient',
                        # subplot = NA)

# contructe a df to plot ###########
tmp.df <- data.frame(Date = seq(as.Date('2019-6-1'),
                                as.Date('2020-6-1'),by='day'),
                     Rain = 0,
                     u2 =  2,
                     PPFD =  0.02,
                     Tair = 30,
                     Tmax = 35,
                     Tmin = 25,
                     RHmax = 100,
                     RHmin = 50,
                     harvest = 0,
                     GCC.norm = 1)

# set drought scenario
tmp.df$vwc <- 0.05
tmp.df$GCC.norm.smooth <- 0.9

optimal.drought.pred.df <- phenoGrass.func.v13(tmp.df,
                                               f.h = 200,
                                               f.t.opt = 30,
                                               f.extract = 0.0287,
                                               f.sec =0.25,
                                               f.growth = 0.095,
                                               swc.wilt =0.05 ,
                                               swc.capacity = 0.3,
                                               bucket.size=1000,
                                               t.max = 45,
                                               day.lay=1,
                                               use.smooth=TRUE,
                                               q = 0.05,
                                               q.s=1.5)
# max(optimal.drought.pred.df$senescence,na.rm=T)
plot(growth~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5],ylim=c(0,0.25))
points(senescence~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[1])

plot((growth - senescence)~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5])
abline(h=0)

plot(cover.hufken~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5])


# set raib scenario
tmp.df$vwc <- 0.3
tmp.df$GCC.norm.smooth <- 0


optimal.drought.pred.df <- phenoGrass.func.v13(tmp.df,
                                               f.h = 200,
                                               f.t.opt = 30,
                                               f.extract = 0.0287,
                                               f.sec =0.25,
                                               f.growth = 0.095,
                                               swc.wilt =0.05 ,
                                               swc.capacity = 0.3,
                                               bucket.size=1000,
                                               t.max = 45,
                                               day.lay = 1,
                                               use.smooth=TRUE,
                                               q = 0.05,
                                               q.s = 1.5)

plot(growth~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5])
points(senescence~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[1])

plot((growth - senescence)~vwc.hufken,data = optimal.drought.pred.df,pch=16,col=col.df$flower[5])
abline(h=0)

plot(cover.hufken~vwc.hufken,data = optimal.drought.pred.df,type='b',pch=16,col=col.df$flower[5])

