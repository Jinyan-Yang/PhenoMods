# return function
r.func <- function(e,sla,k,t){

  return(e*sla / k * (1-exp(-k*t)))
}


return.func(50,0.08,10)

re.vec <- r.func(10,50,0.1,100) - r.func(10,50,0.1,1:100) 

plot(re.vec,type='l')

# life age function
ll.func <- function(k,r,e,sla){
  -(1/k) * log((1-r*k/(e*sla)))
}

ll.func(0.1,100,3,0.1)
ll.func(-0.1,100,3,0.2)

ll.vec <- ll.func(0.1,0.5,10,10:200)

plot(ll.vec~c(10:200),type='l')

# test e reduces with time
# a=E,b =sla,c=water use rate,d=swc, k=discount rate
e.reduce.func <- function(a,b,c,d,k,t){
  a * b * ((c*k*t - d*k + c) * exp(-k*t) / (k^2) +
             (d*k-c)/(k^2)) / d
  
  # a*b*c (1/k^2 - (k*t+1)*exp(-k*t)/k^2) / d
}

# initial value
a = 0.1
b = 50
c = 1
d = 200
k=0.08
value.initial <- e.reduce.func(a,b,c,d,k,300)
re.vec <- (value.initial-e.reduce.func(a,b,c,d,k,1:300)) / value.initial

plot(re.vec,type='l')


# test incorperating lai in the calculation
# unit.leaf.sw.func <- function(sw,lai){
#   sw / lai
# }

sw.func <- function(sw,e,a,t){
  
}

r.func <- function(e=.1,sla=5,k=0.08,a=2.0,sw = 150,lai,t){
  # e is mg c output per unit time per mg dry mass
  # sla is mm2 mg-1
  # k is per time unit
  # a is mm water per mg c  
  # sw is soil water bucket in mm 
  # lai is m2 m-2
  # t is leaf life sapn in days
  
  e*sla * exp(-k*t) * (1-a*e*sla*t/(sw / (lai * 1e6 / sla))) # c export unit drymass of plant
  
  export.rate.sat <- e / lai #mg C m-2 ground
  
  
  
  
}

r.whole.plant <- function(lai,e=.1,sla=5,k=0.08,a=2.0,sw = 150,t=100){
  # t = 600
  # sla =500
  
  # get intergal
  intergal.vec = c()
  for(i in 1:6){
    intergal.vec[i] =  t/6 * (r.func(lai=lai,t=(i-1)*t/6,e=e,sla=sla,k=k,a=a,sw = sw) + 
                                r.func(lai=lai,t=i*t/6,e=e,sla=sla,k=k,a=a,sw = sw)) / 2
  }
  
  intergal.sum = sum(intergal.vec)
  
  out.value = intergal.sum * lai * 1e6 / sla
   
   return(out.value)
}

# r.func(lai=1:10,t=100)

# r.whole.plant(1:10)

r.vec <- c()
for (i in 1:500){
  r.vec[i] = r.whole.plant(lai=i/100)
}

plot(r.vec)

optimise(r.whole.plant,interval = c(0.001,8),sla = 6,sw = 350,e = 0.001,t=30,maximum=TRUE)


# curve(r.whole.plant,from = 0,to = 15)

