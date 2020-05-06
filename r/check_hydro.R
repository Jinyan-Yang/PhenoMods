source('r/function_hydro.R')
# creat swc vector
swc.vec <- seq(0.01,0.13,by = 0.001)
# get psi
psi.vec <- psi.s.func(swc.vec,
                      psi.e = -0.03e-3,#KPa
                      b = 4.26, 
                      swc.sat = 0.13)
plot(psi.vec~swc.vec)
k.soil = k.soil.func(swc.vec,b = 4.26, swc.sat = 0.13,k.sat = 80)
plot(k.soil~swc.vec)
# get fraction of max E
fract.vec <- sapply(swc.vec,e.frac.func,
                    lai=4,
                    k.plant = 0.08,
                    psi.e = -0.03e-3,#MPa
                    b = 4.26, 
                    swc.sat = 0.13,
                    psi.min = -2,
                    k.sat = 80)
plot(fract.vec~swc.vec)
# change min plant psi
fract.vec.dry <- sapply(swc.vec,e.frac.func,
                        lai=4,
                        k.plant = 0.08,
                        psi.e = -0.03e-3,#KPa
                        b = 4.26, 
                        swc.sat = 0.13,
                        psi.min = -8,
                        k.sat = 80)

# plot
plot(fract.vec~swc.vec,type='l',lwd=3,col='grey',
     xlab=expression(theta[soil]),
     ylab=expression(f[water.avi]))
points(fract.vec.dry~swc.vec,type='l',lwd=3,col='red')

