#Langkah 1 (Mengaktifkan Package)
library(tseries)
library(EnvStats)
library(forecast)
library(lmtest)
library(FitAR)
library(stats)

#Langkah 2 (Memanggil Data)
library(readxl)
inflowbali <- read_excel("F:/Akademik/Semester 6/ARW/inflowbali.xlsx")
View(inflowbali)
inflowbali

#Langkah 3 (Plot Time Series Data)
inflowbali.ts <- ts(inflowbali, start=c(1))
plot(inflowbali.ts)

#Langkah 4 (Stasioneritas Data)
#Stasioner varians
inflow <- BoxCox.lambda(inflowbali.ts)
inflow
transinflow=log(inflowbali.ts)
transinflow
inflow1 <- BoxCox.lambda(transinflow)
inflow1

#Stasioner mean
adf.test(transinflow)
inflowdif=diff(transinflow)
inflowdif
adf.test(inflowdif)

#Langkah 5 (Plot ACF dan PACF)
acf(inflowdif)
pacf(inflowdif)

#Langkah 6 (Estimasi Parameter)
fit1=arima(inflowdif,order=c(1,1,1))
coeftest(fit1)
fit1

fit2=arima(inflowdif,order=c(1,1,2))
coeftest(fit2)
fit2

fit3=arima(inflowdif,order=c(2,1,1))
coeftest(fit3)
fit3

fit4=arima(inflowdif,order=c(2,1,2))
coeftest(fit4)
fit4

#Langkah 7 (Uji Normalitas)
qqnorm(fit3$residuals)
qqline(fit3$residuals)
shapiro.test(fit3$residuals)

#Langkah 8 (Uji White Noise)
Box.test(fit3$residuals,type="Ljung-Box")
#karena lebih dari alfa maka kecukupan model terpenuhi

#Langkah 9 (Prediksi Inflow)
prediksi<-forecast(fit3,h=12)
plot(prediksi)
prediksi