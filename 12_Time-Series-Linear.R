#Install library yang dibutuhkan
install.packages(c("lmtest", "forecast", "tseries"))

#Import Library 
library(lmtest)
library(forecast)
library(tseries)

#Import dataset
data("AirPassengers")
AirPassengers
str(AirPassengers)

#Membagi data menjadi train dan test
train_df <- AirPassengers[1:108]
train_df
test_df <- AirPassengers[109:144]
test_df

#Eksplorasi data train
train_df <- ts(train_df, start = 1949)
plot(train_df,
     col = "navyblue",
     main = "Annual flow of the Air Passengers")

#ACF-PACF
par(mfrow = c(1,2))
acf(train_df, lag.max = 10, main = "ACF")
axis(1, at = 1:15, labels = 1:15)

pacf(train_df, lag.max = 10, main = "PACF")
axis(1, at = 1:15, labels = 1:15)

#Fitting Model ARIMA
#AR(1)
arima_model <- Arima(train_df, order = c(1,0,0), method = c("ML"))
arima_model
arima_pred <- arima_model$fitted
arima_pred

plot(train_df,
     col = "navyblue",
     type = "l",
     main = "Plot Aktual vs Prediksi")
lines(arima_pred,
      col = "red")
summary(arima_model)
coeftest(arima_model)
coef(arima_model)

#Analisis Sisaan
#1. Sisaan Menyebar Normal
sisaan <- arima_model$residuals
sisaan

#Secara Eksploratif
par(mfrow = c(1,2))
hist(sisaan, col = "yellow")
qqnorm(sisaan, col = "navyblue")
qqline(sisaan, col = "red")

#Uji Formal Jarque Bera
jarque.bera.test(sisaan)

#2. Ragam Sisaan Homogen
plot(x = as.numeric(arima_pred), y = as.numeric(sisaan),
     col = "blue", 
     main = "Fitted vs Residual",
     xlab = "Fitted Value",
     ylab = "Residual")

abline(h = 0,
       col = "red",
       lty = 2)

#3. Antar Sisaan Saling Bebas
plot(sisaan,
     col = "navyblue",
     main = "Residual vs Order")
abline(h = 0,
       col = "red",
       lty = 2)

#Overfitting
#AR(2)
arima_model_2 <- Arima(train_df, order = c(2,0,0), method = "ML")
summary(arima_model_2)
coef(arima_model_2)

#Forecasting
forecast_arima <- forecast(arima_model, 20)
forecast_arima
accuracy(forecast_arima)

#Penerapan pada Semua Data ARIMA(1,0,0)
AirPassengers_arima <- Arima(AirPassengers, order = c(1,0,0), method = "ML")
forecast_AirPassengers <- forecast(AirPassengers_arima, 20)
plot(forecast_AirPassengers)
accuracy(forecast_AirPassengers)
