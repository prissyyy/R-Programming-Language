#Import Library
library(car)
library(rgdal)
library(maptools)
library(gstat)
library(sp)
library(spdep)
library(lattice)
library(randtests)
library(stats)

#Import Data
library(readxl)
Data_InterpolasiCO <- read_excel("F:/Akademik/Semester 7/Analisa Spasial/Data_InterpolasiCO.xlsx")
View(Data_InterpolasiCO)
data_co <- Data_InterpolasiCO
head(data_co)

#Karakteristik Dataset
str(data_co)

#Statistik Deskriptif
sd(data_co$`Kadar CO`)
var(data_co$`Kadar CO`)
summary(data_co)

#Uji Normalitas
shapiro.test(data_co$`Kadar CO`)

#Uji Stasioneritas
plot(data_co$Y,data_co$`Kadar CO`)
plot(data_co$X,data_co$`Kadar CO`)

#Semivariogram
Data_InterpolasiEstimasiTitikCO <- read_excel("F:/Akademik/Semester 7/Analisa Spasial/Data_InterpolasiEstimasiTitikCO.xlsx")
View(Data_InterpolasiEstimasiTitikCO)
new <- Data_InterpolasiEstimasiTitikCO
head(new)

coordinates(data_co)<-~X+Y
coordinates(new)<-~X+Y
vario<-variogram(data_co$`Kadar CO`~1,data=data_co)
plot(vario,pl=T)
print(vario)

#Eksponensial
var1<-variogram(data_co$`Kadar CO`~1,data=data_co)
head(var1)
w1<-fit.variogram(vario,vgm(1,"Exp",2285,1))
w1
plot(var1,w1,pl=T)
m1<-vgm(10.48712,"Exp",4549,0.740627)
m1
p1<-krige(data_co$`Kadar CO`~1, data_co, new, model=m1)
print(p1)

#Gaussian
var2<-variogram(data_co$`Kadar CO`~1,data=data_co)
head(var2)
w2<-fit.variogram(vario,vgm(1,"Gau",2285,1))
w2
plot(var2,w2,pl=T)
m2<-vgm(10.48712,"Gau",4549,0.740627)
m2
p2<-krige(data_co$`Kadar CO`~1, data_co, new, model=m2)
print(p2)

#SPherical
var3<-variogram(data_co$`Kadar CO`~1,data=data_co)
head(var3)
w3<-fit.variogram(vario,vgm(1,"Sph",2285,1))
w3
plot(var3,w3,pl=T)
m3<-vgm(10.48712,"Sph",4549,0.740627)
m3
p3<-krige(data_co$`Kadar CO`~1, data_co, new, model=m3)
print(p3)

#Nilai Error
mse1=(sum((data_co$`Kadar CO`-p1$var1.pred)^2))/20
mse1
mse2=(sum((data_co$`Kadar CO`-p2$var1.pred)^2))/20
mse2
mse3=(sum((data_co$`Kadar CO`-p3$var1.pred)^2))/20
mse3
