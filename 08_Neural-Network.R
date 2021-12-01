#Import Library
install.packages("KMsurv")
library(survival)

#Memanggi Data di library Survival
data(diabetic)
View(diabetic)
str(diabetic)

#Mengubah data teks menjadi integer
diabetic2=diabetic
diabetic2$laser=factor(diabetic2$laser,
                       levels=c('xenon','argon'),
                       labels=c(1,2))
diabetic2$eye=factor(diabetic2$eye,
                     levels=c('left','right'),
                     labels=c(1,2))
diabetic2$laser=as.numeric(diabetic2$laser)
diabetic2$eye=as.numeric(diabetic2$eye)
View(diabetic2)
str(diabetic2)

#Standarisasi Data
diabetic3=diabetic2
for(i in 1:8){
  diabetic3[,i]<-(diabetic3[,i]-min(diabetic3[,i]))/(max(diabetic3[,i])-min(diabetic3[,i]))
}
diabetic3
View(diabetic3)

#Membagi data menjadi training & testing
n<-round(nrow(diabetic3)*0.75);n

set.seed(12);samp=sample(1:nrow(diabetic3),n)
samp

train=diabetic3[samp,]
View(train)
str(train)

test=diabetic3[-samp,]
View(test)
str(test)

#Pembuatan Model
library(neuralnet)

nn<-neuralnet(status~., data = train, hidden = 3)
plot(nn)

#Pengujian model
output<-compute(nn,test[,-8])
prediction<-ifelse(output$net.result > 0.5,1,0)
prediction

#Membuat prediksi dan akurasi dari data testing
conf_matrix=table(prediction,test$status);conf_matrix
accuracy = (conf_matrix[1,1]+conf_matrix[2,2])/sum(conf_matrix)
accuracy