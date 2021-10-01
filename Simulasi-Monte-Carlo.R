RNG_add<-function(a,z0,c,m)
{
z<-rep(0,20)
R<-rep(0,20)

for(i in 1:20)
{
z[i]<-((a*z0)+c)%% m
z0<-z[i]
R[i]<-z[i]/m
}
print(z)
cat("nilai Random Number : \n",R)

}
RNG_add(13,12357,29,23)
