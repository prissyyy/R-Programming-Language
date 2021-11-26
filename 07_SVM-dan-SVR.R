##SUPPORT VECTOR MACHINE##
#Import data
data<-read.csv('F:/Akademik/Semester 7/Data Mining/Data-SVM_heart.csv')
View(data)
str(data)

#Grafik pada 2 atribut
library(ggplot2)
qplot(trestbps,chol, data=data, color=target)

#Cek nilai yang hilang pada data
anyNA(data)
summary(data)

##Modelling
#Mengambil nama variabel
heart<-names(data[,1:13])
heart

#Concatenate strings
d<-paste(heart,collapse='+')
d<-paste('target~',d)

#Convert to formula
d <- as.formula(d)
d

#Contoh plotting menggunakan 2 atribut
library(e1071)
mymodel<-svm(d, data=data, kernel="radial", type="C-classification")
plot(mymodel, data, chol~trestbps)

#Membuat data training dan data testing
n<-round(nrow(data)*0.70)
set.seed(30)
samp=sample(1:nrow(data),n)
head(samp)

train<-data[samp,]
test<-data[-samp,]

library(e1071)
mod.svm1<-svm(d,train,kernel="linear", type="C-classification")
mod.svm1
mod.svm2<-svm(d,train,kernel="radial", type="C-classification")
mod.svm2
mod.svm3<-svm(d,train,kernel="polynomial", type="C-classification")
mod.svm3

library(caret)
pred.svm1<-predict(mod.svm1,newdata=test[,1:13])
pred.svm2<-predict(mod.svm2,newdata=test[,1:13])
pred.svm3<-predict(mod.svm3,newdata=test[,1:13])
hasil=data.frame(test$target, pred.svm1, pred.svm2, pred.svm3)

confusionMatrix(table(pred.svm1,test$target))
confusionMatrix(table(pred.svm2,test$target))
confusionMatrix(table(pred.svm3,test$target))

##SUPPORT VECTOR REGRESSION##
library(e1071)
library(caret)
library(dplyr)
library(forecast)
library(TTR)

##DATA
boston = MASS ::Boston
View(boston)
set.seed(123)
indexes = createDataPartition(boston$medv, p = .85, list = F)
train = boston[indexes, ]
train
test = boston[-indexes, ]
test

##MENCARI NILAI ERROR TERBAIK
tunning_h <- tune.rpart(medv~., data = train, minsplit = c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000, 10000))
summary(tunning_h)

##MODEL
tc <- tune.control(cross = 10)
tc
model_reg1 = svm(medv~., data = train, type = 'eps-regression', kernel = 'linear', trainControl= tc)
model_reg1
tc <- tune.control(cross = 10)
model_reg2 = svm(medv~., data = train, type = 'eps-regression', kernel = 'polynomial', trainControl= tc)
model_reg2
tc <-tune.control(cross = 10)
model_reg3 = svm(medv~., data = train, type = 'eps-regression', kernel = 'radial', trainControl= tc)
model_reg3 

###Predict
pred1 = predict(model_reg1, test)
View(pred1)
pred2 = predict(model_reg2, test)
View(pred2)
pred3 = predict(model_reg3, test)
View(pred3)
x = 1:length(test$medv)
x
plot(x, test$medv, pch=18, col="red")
lines(x, pred1, lwd="1", col = "blue")
lines(x, pred2, lwd="1", col = "black")
lines(x, pred3, lwd="1", col = "green")

##AKURASI
##pred1
mae = MAE(test$medv, pred1)
rmse = RMSE(test$medv, pred1)
r1 = R2(test$medv, pred1, form = "traditional")
cat("MAE:",mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r1)
##pred2'
mae = MAE(test$medv, pred2)
rmse = RMSE(test$medv, pred2)
r2 = R2(test$medv, pred2, form = "traditional")
cat("MAE:",mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r2)
##pred3
mae = MAE(test$medv, pred3)
rmse = RMSE(test$medv, pred3)
r3 = R2(test$medv, pred3, form = "traditional")
cat("MAE:",mae, "\n", "RMSE:", rmse, "\n", "R-squared:", r3)
