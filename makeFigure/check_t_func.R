t.func <- function(t.mean,f.t.opt,t.max){
  
  gvalue <- (t.max-t.mean)/(t.max-f.t.opt)*(t.mean/f.t.opt)^(f.t.opt/(t.max-f.t.opt))
  return(max(0,gvalue))
}
t.vec <- 0:50
pred.t.20 <- sapply(t.vec,t.func,20,45)
pred.t.5 <- sapply(t.vec,t.func,1,45)
pred.t.35 <- sapply(t.vec,t.func,35,45)

plot(pred.t.20~t.vec,col='grey',pch=16)

points(pred.t.5~t.vec,col='navy',pch=16)
points(pred.t.35~t.vec,col='red',pch=16)

