#Install Packages
#install.packages("class")
#Load required libraries
library(class)

#Loading data
dim(ChickWeight)
ChickWeight

#Data Summarization
summary(ChickWeight)

#Pembuatan KNN
indexes = sample(1:nrow(ChickWeight), size = 0.2*nrow(ChickWeight))
indexes

#Test
test = ChickWeight[indexes,]
test
head(test)
#Train
train = ChickWeight[-indexes,]
train
head(train)

#KNN
ChickWeight.KNN=knn(train[,-4],test[,-4],train[,4],k=5)
ChickWeight.KNN

#Cros Validasi
(table(ChickWeight.KNN,test[,4]))

#Visualisasi Pairs Plot
pairs(test[,1:3],pch=as.character(test[,4],col=c(1,4))[(test$Diet!=ChickWeight.KNN)+1])
