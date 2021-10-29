#Install Package
install.packages("party")

##Decision Tree
#Memanggil data di R Data Sets
data(iris)
iris
str(iris)
summary(iris)

#Separate training and test datasets
set.seed(123)
ind <- sample(2, nrow(iris), 
              replace=TRUE, prob=c(0.7, 0.3))
ind

trainData <- iris[ind==1,]
trainData
testData <- iris[ind==2,]
testData

#MODEL
library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
myFormula

#cek prediksi model
iris_ctree <- ctree(myFormula, data=trainData)
iris_ctree
table(predict(iris_ctree), trainData$Species)

#buat alur pohon
plot(iris_ctree)

testPred <- predict(iris_ctree, newdata = testData)
testPred
table(testPred, testData$Species)

#Cek akurasi
accuracy <- sum(diag(testPred))
print(paste('Accuracy for test',accuracy))

##Random Forest
install.packages("randomForest")

#Membagi data menjadi data testing & training
ind <- sample(2, nrow(iris), 
              replace=TRUE, 
              prob=c(0.7, 0.3))
ind

training <- iris[ind==1,]
training
testing <- iris[ind==2,]
testing

#Membuat model prediksi
library(randomForest)
rf <- randomForest(
  Species ~ .,
  data=training
)
rf

table(predict(rf), training$Species)
print(rf)

#Menguji Model dengan data testing
Prediksi <- predict(rf, newdata=testing)
table(Prediksi, testing$Species)

#Akurasi
accuracyR <- sum(diag(Prediksi))
print(paste('Accuracy for test',accuracyR))

#Komponen output RF
plot(rf)
importance(rf)
varImpPlot(rf)