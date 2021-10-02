#Install packages stats, dplyr, dan RColorBrewer

#Load required libraries
library(stats)
library(dplyr)
library(RColorBrewer)

#Menampilkan Struktur Data
mydata=iris
iris
View(iris)
str(mydata)

#Data Summarization
SL = mydata$Sepal.Length

#statistika deskriptif
summary(mydata)

#Mode Function
Mode=function(x){
  ta=table(x)
  tan=max(ta)
  if(all(ta == tan))
    mod=NA
  else
    if(is.numeric(x))
      mod=as.numeric(names(ta)[ta == tan])
  else
    mod=names(ta)[ta == tan]
  return(mod)
}

#calculate Mode=Modus
Mode(SL)

#calculate standard Deviation
sd(SL)

#calculate Variance
var(SL)

#calculate IQR
quantile(SL)

#Data Visualization
Species=mydata$Species

#Numeric Data Histogram
hist(iris$Sepal.Length, col="hot pink", border = "black")
hist

#categorical Data Barplot
barplot(table(Species), main = "Barplot Jumlah Masing^2 Species", col="purple")
barplot

#Data Normalization
#select all data numeric variables in the dataset
mydata_numeric=select(mydata,c(1,2,3,4))
mydata_numeric

#Normalize Dataset (Z-Score) All Variableon the same scale
Zscore_mydata=scale(mydata_numeric)

#check all the variabel converted to Z-Score
Zscore_mydata
View(Zscore_mydata)
