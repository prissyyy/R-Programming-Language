library(ResourceSelection)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(ROSE)
library(car)
library(stats)
library(lmtest)
library(modEvA)
library(foreign)
library(nnet)
library(ordinal)
library(MASS)
library(pscl)
library(zoo)

## REGRESI LINIER BERGANDA
#Import Dataset
install.packages("readr")
berganda <- read.csv("F:/Akademik/Semester 7/Data Mining/Data_Regresi-Linier-Berganda.csv")
View(berganda)
berganda
dim(berganda)

#Model Regresi
model <- lm(Y~X1+X2+X3, data=berganda)
model

#Asumsi Regresi
#Normalitas
ks.test(model$residuals, ecdf(model$residuals))
#Multikolinieritas
vif(model)
#Autokorelasi
dwtest(model)
#Heterokesdastisitas
bptest(model)

#Regresi
model2 <- lm(Y~X2+X3, data=berganda)
model2
summary(model2)

## REGRESI LOGISTIK BINER
#Input Data
biner <- read.csv("F:/Akademik/Semester 7/Data Mining/Data_Regresi-Logistik-Biner.csv")
View(biner)
biner
head(biner)
str(biner)

#Deskriptif Data
summary(biner)
sapply(biner,sd)

#Asumsi Regresi
#Multikolinieritas
model=glm(Y~X1+X2+X3+X4, family = "binomial", data=biner)
model
vif(model)

#Tabel Kontingensi
xtabs(~Y+X1, data=biner)
xtabs(~Y+X2, data=biner)
xtabs(~Y+X3, data=biner)
xtabs(~Y+X4, data=biner)

#Merubah Variabel menjadi faktor/kategori
biner$Y<-as.factor(biner$Y)
biner$X1<-as.factor(biner$X1)
biner$X2<-as.factor(biner$X2)
biner$X3<-as.factor(biner$X3)
biner$X4<-as.factor(biner$X4)
str(biner)

#Uji Signifikansi PARSIAL
#Model Regresi
model=glm(Y~X1+X2+X3+X4, family = "binomial", data=biner)
model
summary(model)
anova(model, test ="Chisq")

#Uji SIgnifikansi SERENTAK
#Pseudo R2
RsqGLM(model)
pR2(model)

#Goodness of fit
hoslem.test(model$y, fitted(model))

#Odds ratio dan 95% CI
exp(cbind(OR=coef(model),confint(model)))

#Prediksi
prob.prediksi<-predict(model,biner,type = "response")
prob.prediksi
prediksi <- ifelse(prob.prediksi>0.5,1,0)
prediksi
pred<-factor(prediksi,levels = c(0,1))
pred
actual <- biner$Y
actual
mean(pred==actual)
table(predicted=prediksi, actual=actual)
