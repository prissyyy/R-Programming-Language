RNG_multi<-function(a,z0,m)
{
  
z<-rep(0,20)

for(i in 1:20)
{
z[i]<-(a*z0) %% m
z0<-z[i]
}

print(z)

}

RNG_multi(197,12357,1387)
