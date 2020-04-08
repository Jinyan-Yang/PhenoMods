# 
# swc.sat <- 0.4
# swc <- seq(0.01,swc.sat,0.001)
# 
# k.s <- k.sat * (psi.e/psi.soil)^(2+3/b)

k.soil.func <- function(swc,
                        swc.sat = 0.4,
                        psi.e = -0.35,#KPa
                        b = 4.3, 
                        k.sat = 80){
  
  if(swc>swc.sat){
    warning(paste0('Given SWC ',swc,' is larger than satureate ',swc.sat,
                   '/n A max of ',k.sat,' is given'))
    k.s = k.sat
  }else{
    psi.soil <- psi.e * (swc / swc.sat)^-b
    
    k.s <- k.sat * (psi.e/psi.soil)^(2+3/b)
  }
  
  return(k.s)
}

# k.soil.func(1)
