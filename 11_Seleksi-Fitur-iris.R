#Libraries
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

#Data
data("iris")
View(iris)
str(iris)

#Feature Selection
set.seed(111)
boruta<-Boruta(Species~.,data=iris,doTrace=2,maxRuns=100)
print(boruta)
plot(boruta, las=2, cex.axis=0.7)
plotImpHistory(boruta)
getNonRejectedFormula(boruta)
getConfirmedFormula(boruta)

#Tentative Fix
bor<-TentativeRoughFix(boruta)
print(bor)
attStats(boruta)

#Data Partition
set.seed(222)
ind<-sample(2,nrow(iris),replace=T,prob = c(0.6,0.4))
train<-iris[ind==1,]
View(train)
test<-iris[ind==2,]
View(test)

#Random Forest Model
set.seed(333)
rf60<-randomForest(Species~.,data=train)
rf60
rf41<-randomForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,  
                   data=train)
rf41
rf33<-randomForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                   data=train)
rf33

#Prediction &Confussion Matrix-Test
p<-predict(rf60,test)
confusionMatrix(p,test$Species)
p<-predict(rf41,test)
confusionMatrix(p,test$Species)
p<-predict(rf33,test)
confusionMatrix(p,test$Species)

