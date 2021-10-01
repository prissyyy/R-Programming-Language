install.packages("fpp2")
library(tidyverse)
library(fpp2)

library(readxl)
hujan <- read_excel("F:/Akademik/Semester 6/ARW/hujan.xlsx")
hujan
View(hujan)
hujan.ts <- ts(hujan, start=c(1813))
plot(hujan.ts)

#Single exp. Smoothing (alpha = 0,25)
ses1 = HoltWinters(hujan.ts,alpha = 0.25,beta=F,gamma=F)
ses1$SSE
ses1$fitted

plot(ses1,xlab="Tahun",ylab="Curah Hujan",lty=1,col="black")

#evaluasi
ses<- cbind(hujan.ts,ses1$fitted[,1])
SSE <- sum((hujan.ts-ses1$fitted[,1])^2,na.rm=T)
MSE <- mean((hujan.ts-ses1$fitted[,1])^2,na.rm=T)
MAPE <- mean(abs((hujan.ts-ses1$fitted[,1])/hujan.ts),na.rm=T)
SSE
MSE
MAPE
predict(ses1, n.ahead = 5)