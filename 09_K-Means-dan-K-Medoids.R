###SYNTAX K-MEANS###

#Loading Packages#
install.packages("cluster.datasets")
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")

#Import Library
#Dataset
library(cluster.datasets)
#Data Manipulation
library(tidyverse)
#Clustering Algorithms
library(cluster)
#Clustering Algorithms & Visualization
library(factoextra)      

#Data
library(readxl)
Data_KMeans <- read_excel("F:/Akademik/Semester 7/Data Mining/Data_KMeans.xlsx")
View(Data_KMeans)
dataclus <- Data_KMeans
head(dataclus)
str(dataclus)

#Menghilangkan Variabel Teks/Label           
dataclus$Provinsi<-NULL    
head(dataclus)

#Menghilangkan Data Missing
dataclus1 <- na.omit(dataclus) 
summary(dataclus1)

#Standarisasi Data
datafix=scale(dataclus1)  
head(datafix)

#Menghitung Jarak
distance <- get_dist(datafix,method='euclidean')
distance

#Mencari K Optimal# 
#1. Metode Elbow
set.seed(123)
fviz_nbclust(datafix, kmeans, method = "wss") 

#2. Metode Silhouette
set.seed(123)
fviz_nbclust(datafix, kmeans, method = "silhouette")

#3. Gap Statistics
set.seed(123)
gap_stat <- clusGap(datafix, FUN = kmeans, nstart = 25,
                    K.max = 5, B = 150)
gap_stat
fviz_gap_stat(gap_stat)

#Eksekusi K-Means
set.seed(123)
final <- kmeans(datafix, 3, nstart = 25)
print(final)

fviz_cluster(final, data = datafix)

#Karakteristik Cluster
dataclus %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#---------------------------------------#
  
###SYNTAX K-MEDOIDS###

#Loading Packages#
install.packages("fpc")

#Import Library
library(fpc)
#Data Manipulation
library(tidyverse)
#Clustering Algorithms
library(cluster) 
#Clustering Algorithms & Visualization
library(factoextra) 

#Impor Data
library(readxl)
Data_KMedoids <- read_excel("F:/Akademik/Semester 7/Data Mining/Data_KMedoids.xlsx")
head(Data_KMedoids)
View(Data_KMedoids)
data <- Data_KMedoids

#Melihat Tipe Data
str(data)  

#Menghilangkan Variabel Teks           
data$Provinsi<- NULL
head(data)

#Mendeteksi Missing Value & Outlier#
#Menghitung Missing Value
summary(is.na(data))  
#Menghilangkan Missing Value
na.omit(data)   
#Membuat Boxplot
boxplot(data)           

#Cluster dengan Fungsi PAMK#
pamk.hasil <-pamk(data)
pamk.hasil
#Menampilkan Jumlah Cluster yang Terbentuk
pamk.hasil$nc          

#Cluster dengan Fungsi PAM# 
fviz_nbclust(data, pam, method = "silhouette")

pam.hasil  <- pam(data, 2)
pam.hasil
summary(pam.hasil)

#Melihat Komponen (Contoh: nama.cluster$nama.komponen)
pam.hasil$medoids
pam.hasil$silinfo

#Eksekusi K-Medoids
data.frame(Data_KMedoids$Provinsi,pam.hasil$clustering)

fviz_cluster(pam.hasil)

#Karakteristik Cluster
data%>%
  mutate(Cluster = pam.hasil$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
