library(car)
library(forecast)
library(lmtest)
library(tseries)
library(e1071)

#Memanggil Data
library(readxl)
data1 <- read_excel("F:/Akademik/Magang-PKL/data1.xlsx")
data1

#Statistika Deskriptif
attach(data1)
mean(Jumlah_Penumpang)
median(Jumlah_Penumpang)
sd(Jumlah_Penumpang)
min(Jumlah_Penumpang)
max(Jumlah_Penumpang)
skewness(Jumlah_Penumpang)

#Identifikasi Plot Data Series
plot.ts(Jumlah_Penumpang)
points(Jumlah_Penumpang)

#Regresi Trend Deterministik
regresi<-lm(Jumlah_Penumpang~t,data1=data1)
summary(regresi)

#Regresi Dummy
regresidummy<-lm(Jumlah_Penumpang~D1+D2,data1=data1)
summary(regresidummy)

#Estimasi Ulang Regresi Dummy
regresidummy<-lm(Jumlah_Penumpang~D1,data1=data1)
summary(regresidummy)

#Regresi Gabungan
modelregresi<-lm(Jumlah_Penumpang~D1+t,data1=data1)
summary(modelregresi)

#Asumsi Klasik Model Regresi
res<-residuals(modelregresi)
#Uji Heteroskedastisitas
bptest(modelregresi)
#Uji Autokorelasi
dwtest(modelregresi)
#Uji Multikolinieritas
vif(modelregresi)
#Uji Normalitas
ks.test(res,"pnorm")

#PROSES ARIMA
#Stasioneritas
#Stasioneritas terhadap ragam
lambda=BoxCox.lambda(res)
lambda
#Transformasi data kedalam bentuk ln
datatransformasi=log(abs(res))
lambda2=BoxCox.lambda(datatransformasi)
lambda2
#stasioner terhadap rata-rata
adf.test(datatransformasi)

#Pendugaan Parameter dengan melihat plot ACF dan PACF
par(mfrow=c(1,1))
acf(datatransformasi, lag.max = 100)
pacf(datatransformasi, lag.max = 100)

#Pemilihan model ARIMA
model1<-arima(datatransformasi, order = c(0,0,1))
model1
model2<-arima(datatransformasi, order = c(0,0,2))
model2
model3<-arima(datatransformasi, order = c(1,0,0))
model3
model4<-arima(datatransformasi, order = c(1,0,1))
model4
model5<-arima(datatransformasi, order = c(1,0,2))
model5
model6<-arima(datatransformasi, order = c(2,0,0))
model6
model7<-arima(datatransformasi, order = c(2,0,1))
model7
model8<-arima(datatransformasi, order = c(2,0,2))
model8

#Pemilihan Model yang Signifikan
coeftest(model1)
coeftest(model2)
coeftest(model3)
coeftest(model4)
coeftest(model5)
coeftest(model6)
coeftest(model7)
coeftest(model8)

#Model yang signifikan adalah model 1,3,7
#Uji Normalitas model yang signifikan menggunakan kolmogorov-smirnov test
residual1=resid(model1)
ks.test(residual1,"pnorm",mean(residual1),sd(residual1))
residual3=resid(model3)
ks.test(residual3,"pnorm",mean(residual3),sd(residual3))
residual7=resid(model7)
ks.test(residual7,"pnorm",mean(residual7),sd(residual7))

#Uji White Noise model yang signifikan dengan Ljung-Box
tsdiag(model1)
Box.test(model1$residuals,type = "Ljung-Box")
tsdiag(model3)
Box.test(model3$residuals,type = "Ljung-Box")
tsdiag(model7)
Box.test(model7$residuals,type = "Ljung-Box")

#Yang memenuhi semua asumsi adalah model3 atau ARIMA(1,0,0)
#Pemodelan ARIMAX
d1<-data1$D1
d1
t<-data1$t
t
modelarimax=arima(data1$Jumlah_Penumpang, xreg=data.frame(d1,t), order=c(1,0,0), method="ML")
modelarimax

#Diagnostic Model ARIMAX
#Uji White Noise
tsdiag(modelarimax)
Box.test(modelarimax$residuals,type="Ljung-Box")

#Uji Kenormalan Residual
residual=resid(modelarimax)
ks.test(residual,"pnorm",mean(residual),sd(residual))

#Peramalan
data2 <- read_excel("F:/Akademik/Magang-PKL/data2.xlsx")
data2
D1<-data2$D1
D1
T<-data2$T
T
predicted<-predict(modelarimax,newxreg=data.frame(D1,T),n.ahead=24)
predicted

accuracy(modelarimax)
