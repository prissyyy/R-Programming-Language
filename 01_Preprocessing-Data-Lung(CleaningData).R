library(survival)
rawdata<-lung
lung
View(rawdata)

#CLEANING DATA
#cleaning data dengan menggunakan mean
data=rawdata
for (i in 1:ncol(data))
{
  data[is.na(data[,i]),i]<-mean(data[,1],na.rm=TRUE)
}
print(i)
data
View(data)

#cleaning data dengan menghilangkan NA
data2=data
data2<-na.omit(data)
data2
View(data2)

#pengkategorian data
data$status=factor(data$status,
                levels=c('2','1'),
                labels=c(0,1))

data$sex=factor(data$sex,
                levels=c('1','2'),
                labels=c(0,1))

str(data)

data$status=as.numeric(data$status)
data$inst=as.numeric(data$inst)
data$sex=as.numeric(data$sex)

str(data)

#REDUCTION DATA

#reducing dengan dimensionality reduction

#standardisasi data
data_standardized<-scale(x=data)
data_standardized

#menghitung matriks ragam per ragam
data_covariance<-cov(data_standardized)
data_covariance

#menghitung vektor eigen
data_eigen<-eigen(data_covariance)
data_eigen

#melakukan PCA
data_PCA<-prcomp(x=data,scale=TRUE,center=TRUE)
data_PCA
names(data_PCA)
summary(data_PCA)
#dipilih variabel dg nilai PCA terakhir mencapai 0,8
data_PCA$rotation
data_PCA2<-head(data_PCA$x[,1:7])  
data_PCA2

data3<-data[,1:7]
data3
View(data3)

#menghapus outliers
num_col<-unlist(lapply(data3,is.numeric))
num_col
data_num<-data3[,num_col]
data_num
boxplot(data_num)
boxplot

data_outlier<-function(x,na.rm=FALSE){
  qs=quantile(x,probs=c(0.25,0.75,na.rm=na.rm))
  
  lowerq<-qs[1]
  upperq<-qs[2]
  iqr=upperq-lowerq
  
  extreme.threshold.upper=(iqr*3)+upperq
  extreme.threshold.lower=lowerq-(iqr*3)
  
  x>extreme.threshold.upper|x<extreme.threshold.lower
}

remove_outliers<-function(data3,cols=names(data)){
  for(col in cols){
    cat("Removing outliers in column:",col,"\n")
    data3<-data3[!data_outlier(data3[[col]]),]
  }
  data3
}

#Variabel yg mengalami outliers 
vars_of_interest<-c("time") 

data_filtered<-remove_outliers(data3,vars_of_interest)
data_filtered

boxplot(data_filtered)

#TRANSFORMATION DATA

library(caTools)
set.seed(123)
split=sample.split(data_filtered$ph.ecog,SplitRatio = 0.8)
print(split)
training_set=subset(data_filtered,split==TRUE)
training_set
test_set=subset(data_filtered,split==FALSE)
test_set

#normalisasi dengan scaling
training_set[,1:7]=scale(training_set[,1:7])
test_set[,1:7]=scale(test_set[,1:7])
training_set
test_set