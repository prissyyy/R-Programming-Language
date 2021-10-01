library(car)
library(forecast)
library(lmtest)
library(tseries)
library(e1071)

#Memanggil Data
library(readxl)
data1 <- read_excel("F:/Akademik/SKRIPSI/data1.xlsx")
data1

#Statistika Deskriptif
attach(data1)
mean(Nilai_Ekspor)
median(Nilai_Ekspor)
sd(Nilai_Ekspor)
min(Nilai_Ekspor)
max(Nilai_Ekspor)
skewness(Nilai_Ekspor)

#Identifikasi Plot Data Series
plot.ts(Nilai_Ekspor)

#Regresi Trend Deterministik
regresi<-lm(Nilai_Ekspor~t,data1=data1)
summary(regresi)

#Regresi Dummy
regresidummy<-lm(Nilai_Ekspor~D1+D2,data1=data1)
summary(regresidummy)

#Estimasi Ulang Regresi Dummy
regresidummy<-lm(Nilai_Ekspor~D1,data1=data1)
summary(regresidummy)

#Regresi Gabungan
modelregresi<-lm(Nilai_Ekspor~D1+t,data1=data1)
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
#stasioner terhadap rata-rata
adf.test(res)
#differencing
Data=diff(res, differences = 1)
adf.test(Data)
plot.ts(Data)

#Pendugaan Parameter dengan melihat plot ACF dan PACF
par(mfrow=c(1,1))
acf(Data, lag.max = 100)
pacf(Data, lag.max = 100)

#Pemilihan model ARIMA
model1<-arima(Data, order = c(0,1,1))
model1
model2<-arima(Data, order = c(0,1,2))
model2
model3<-arima(Data, order = c(0,1,4))
model3
model4<-arima(Data, order = c(1,1,0))
model4
model5<-arima(Data, order = c(1,1,1))
model5
model6<-arima(Data, order = c(1,1,2))
model6
model7<-arima(Data, order = c(2,1,0))
model7
model8<-arima(Data, order = c(2,1,1))
model8
model9<-arima(Data, order = c(2,1,2))
model9
model10<-arima(Data, order = c(3,1,0))
model10
model11<-arima(Data, order = c(3,1,1))
model11
model12<-arima(Data, order = c(3,1,2))
model12
model13<-arima(Data, order = c(4,1,0))
model13
model14<-arima(Data, order = c(4,1,1))
model14
model15<-arima(Data, order = c(4,1,2))
model15
model16<-arima(Data, order = c(6,1,0))
model16
model17<-arima(Data, order = c(6,1,1))
model17
model18<-arima(Data, order = c(6,1,2))
model18

#Pemilihan Model yang Signifikan
coeftest(model1)
coeftest(model2)
coeftest(model3)
coeftest(model4)
coeftest(model5)
coeftest(model6)
coeftest(model7)
coeftest(model8)
coeftest(model9)
coeftest(model10)
coeftest(model11)
coeftest(model12)
coeftest(model13)
coeftest(model14)
coeftest(model15)
coeftest(model16)
coeftest(model17)
coeftest(model18)

#Model yang signifikan adalah model 1,2,4,5,7,8,10,13,16
#Uji Normalitas model yang signifikan menggunakan kolmogorov-smirnov test
residual1=resid(model1)
ks.test(residual1,"pnorm",mean(residual1),sd(residual1))
residual2=resid(model2)
ks.test(residual2,"pnorm",mean(residual2),sd(residual2))
residual4=resid(model4)
ks.test(residual4,"pnorm",mean(residual4),sd(residual4))
residual5=resid(model5)
ks.test(residual5,"pnorm",mean(residual5),sd(residual5))
residual7=resid(model7)
ks.test(residual7,"pnorm",mean(residual7),sd(residual7))
residual8=resid(model8)
ks.test(residual8,"pnorm",mean(residual8),sd(residual8))
residual10=resid(model10)
ks.test(residual10,"pnorm",mean(residual10),sd(residual10))
residual13=resid(model13)
ks.test(residual13,"pnorm",mean(residual13),sd(residual13))
residual16=resid(model16)
ks.test(residual16,"pnorm",mean(residual16),sd(residual16))

#Uji White Noise model yang signifikan dengan Ljung-Box
tsdiag(model1)
Box.test(model1$residuals,type = "Ljung-Box")
tsdiag(model2)
Box.test(model2$residuals,type = "Ljung-Box")
tsdiag(model4)
Box.test(model4$residuals,type = "Ljung-Box")
tsdiag(model5)
Box.test(model5$residuals,type = "Ljung-Box")
tsdiag(model7)
Box.test(model7$residuals,type = "Ljung-Box")
tsdiag(model8)
Box.test(model8$residuals,type = "Ljung-Box")
tsdiag(model10)
Box.test(model10$residuals,type = "Ljung-Box")
tsdiag(model13)
Box.test(model13$residuals,type = "Ljung-Box")
tsdiag(model16)
Box.test(model16$residuals,type = "Ljung-Box")

#Yang memenuhi semua asumsi adalah model13 atau ARIMA(4,1,0)
#Pemodelan ARIMAX
d1<-data1$D1
d1
t<-data1$t
t
modelarimax=arima(data1$Nilai_Ekspor, xreg=data.frame(d1,t), order=c(4,1,0), method="ML")
modelarimax

#Diagnostic Model ARIMAX
#Uji White Noise
tsdiag(modelarimax)
Box.test(modelarimax$residuals,type="Ljung-Box")

#Uji Kenormalan Residual
residual=resid(modelarimax)
ks.test(residual,"pnorm",mean(residual),sd(residual))
#Karena Tidak Normal, maka dilakukan transformasi kedalam bentuk Akar
Tmodelarimax=sqrt(abs(residual))
ks.test(Tmodelarimax,"pnorm",mean(Tmodelarimax),sd(Tmodelarimax))

#Peramalan
data2 <- read_excel("F:/Akademik/SKRIPSI/data2.xlsx")
data2
D1<-data2$D1
D1
T<-data2$T
T
predicted<-predict(modelarimax,newxreg=data.frame(D1,T),n.ahead=24)
predicted

accuracy(modelarimax)
