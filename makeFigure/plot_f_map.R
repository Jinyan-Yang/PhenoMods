devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

f.vec <- seq(0.3,2,length.out = 20)
f.vec <- f.vec + 0.1*rnorm(20)

fs.vec <- seq(2,0.01,length.out = 20)
fs.vec <- fs.vec + 0.1*rnorm(20)

plot.df <- data.frame(f = f.vec,
                      f.s = fs.vec,
                      map = rep(seq(100,1000,length.out=10),each=2))
plot.df$map <- plot.df$map + 50*rnorm(20)

# 
pdf('figures/concept.pdf',width = 8,height = 8*.62)
par(mar=c(5,5,1,1,))
plot(f~map,data = plot.df,pch=16,col=col.df$iris[5],ylab='Sensitivity to drought',xlab='MAP (mm/yr)',axes = F)
points(f.s~map,data = plot.df,pch=16,col=col.df$iris[1])

axis(1,at = c(200,1000),labels = c('Low','High'))
axis(2,at = c(0.5,2),labels = c('Low','High'))

legend('top',legend = c('Growth','Senescence'),bty='n',
       pch=16,col=col.df$iris[c(5,1)])

dev.off()
