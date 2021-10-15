#install packages
install.packages("e1071")
install.packages("caret")

#Libraries
library(e1071)
library(caret)

#Data
data=iris
View(iris)
iris

#Membagi data menjadi 2 
sampel=sample(1:nrow(data),0.75*nrow(data),replace = TRUE)
sampel
training=data.frame(data)[sampel,]
training
testing=data.frame(data)[-sampel,]
testing

#Membuat Model Naive Bayes
modelNB=naiveBayes(Species~.,data = training)
modelNB

#Prediksi data baru
prediksi=predict(modelNB,newdata=testing)
prediksi

#Confusion Matriks untuk melihat akurasi prediksi
hasil=confusionMatrix(table(prediksi,testing$Species))
hasil