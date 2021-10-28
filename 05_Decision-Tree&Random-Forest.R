#Install Package
install.packages("party")

##Decision Tree
#Memanggil data
data("bodyfat", package="TH.data")
bodyfat
str(bodyfat)
summary(bodyfat)

#Separate training and test datasets
set.seed(123)
ind <- sample(2, nrow(bodyfat), 
              replace=TRUE, prob=c(0.8, 0.2))
ind

trainData <- bodyfat[ind==1,]
trainData
testData <- bodyfat[ind==2,]
testData

#MODEL
library(party)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4
myFormula

#cek prediksi model
bodyfat_ctree <- ctree(myFormula, data=trainData)
bodyfat_ctree
table(predict(bodyfat_ctree), trainData$DEXfat)

#buat alur pohon
plot(bodyfat_ctree)

testPred <- predict(bodyfat_ctree, newdata = testData)
testPred
table(testPred, testData$DEXfat)

#Cek akurasi
accuracy <- sum(diag(testPred))
accuracy
print(paste('Accuracy for test',accuracy))

##Random Forest
install.packages("randomForest")

#Data testing dan training
ind <- sample(2, nrow(bodyfat), 
              replace=TRUE, 
              prob=c(0.7, 0.3))
ind

training <- bodyfat[ind==1,]
training
testing <- bodyfat[ind==2,]
testing

#Membuat model prediksi
library(randomForest)
rf <- randomForest(
  DEXfat ~ .,
  data=training
)
rf

table(predict(rf), training$DEXfat)

#Prediksi Model dengan data testing
Prediksi <- predict(rf, newdata=testing)
table(Prediksi, testing$DEXfat)

#Akurasi
accuracyR <- sum(diag(Prediksi))
print(paste('Accuracy for test',accuracyR))

#Komponen output RF
plot(rf)
importance(rf)
varImpPlot(rf)