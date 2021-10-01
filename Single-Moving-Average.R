install.packages("forecast")
library(forecast)
library(TTR)
library(graphics)

library(readxl)
hujan <- read_excel("F:/Akademik/Semester 6/ARW/hujan.xlsx")
hujan
View(hujan)
hujan.ts <- ts(hujan, start=c(1813))
plot(hujan.ts)

#Single Moving Average
hujan.sma <- SMA(hujan.ts,3)
cbind(hujan.ts,hujan.sma)
hujan.sma

#Plot
plot(hujan.ts,xlab="Tahun",ylab="Curah Hujan",lty=1,col="black")
points(hujan.ts)
lines(hujan.sma, col = "red")

#Prediksi
phujan.sma <- lag(hujan.sma,-1)
phujan.sma
sma <- cbind(hujan.ts,hujan.sma,phujan.sma)
sma

#Evaluasi
SSE <- sum((phujan.sma-hujan.ts)^2,na.rm=T)
SSE
MSE <- mean((phujan.sma-hujan.ts)^2,na.rm=T)
MSE
MAPE <- mean(abs((hujan.ts-phujan.sma)/hujan.ts),na.rm=T)
MAPE