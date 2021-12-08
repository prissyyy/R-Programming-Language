## DENSITY BASED CLUSTERING With DBSCAN ##

#Import Library
library(fpc)
library(dbscan)
if(!require(devtools))install.packages("devtools")
library(factoextra)

#Import dataset
data("multishapes")
head(multishapes)

#Melihat Struktur Data
str(multishapes)

#Menghapus variabel shapes
new<-multishapes[,-3]
head(new)

#obtaining optimal eps value
kNNdistplot(new, k=2)
abline(h=0.45, col="red", lty=2)

#density-based clustering with fpc&dbscan
set.seed(123)
f<-fpc::dbscan(new,eps=0.45,MinPts = 4)
f
d<-dbscan::dbscan(new,0.45,4)
d

#Cluster Visualization
fviz_cluster(f,new,geom="point")
fviz_cluster(d,new,geom="point")

#---------------------------#

## HIERARCHICAL CLUSTERING ##

library(factoextra)
data("multishapes")
View(multishapes)
summary(multishapes[,1:2])

#uji asumsi multikolinearitas
#Jika nilai korelasi antar variabel tidak ada yang melebihi 0.7, artinya TIDAK ADA MULTIKOLINIERITAS
install.packages("car")
library("car")
multikol<-cor(multishapes[,1:2])
multikol

#perhitungan jarak antar satu data ke data lainnya
jarak=dist(multishapes[,1:2])
jarak

##analisis cluster dengan metode average
hierarkiave<-hclust(dist(scale(multishapes[,1:2])), method="ave")
hierarkiave
plot(hierarkiave, multishapes$shape)
#DENDOGRAM
rect.hclust(hierarkiave,3)
anggotaave<-data.frame(id=multishapes$shape, cutree(hierarkiave,k=3))
View(anggotaave)
cophenetic(hierarkiave)
#korelasi cophenetic 
d1 <- dist(multishapes[,1:2])
hc <- hclust(d1,"ave")
hc
d2 <- cophenetic(hc)
corave=cor(d1,d2)
corave

##analisis cluster dengan metode Complete
hierarkicomp<-hclust(dist(scale(multishapes[,1:2])), method = "complete")
hierarkicomp
plot(hierarkicomp, multishapes$shape)
#dendogram
rect.hclust(hierarkicomp,3)
anggotacomp<-data.frame(id=multishapes$shape, cutree(hierarkicomp,k=3))
View(anggotacomp)
cophenetic(hierarkicomp)
#korelasi cophenetic
d1 <- dist(multishapes[,1:2])
hc <- hclust(d1, "complete")
d2 <- cophenetic(hc)
corcomp=cor(d1,d2)
corcomp

##analisis cluster dengan metode Single
hierarkising <- hclust(dist(scale(multishapes[,1:2])),method = "single")
hierarkising
plot(hierarkising, multishapes$shape)
#DENDROGAM
rect.hclust(hierarkising,3)
anggotasing <- data.frame(id=multishapes$shape, cutree(hierarkising,k=3))
View(anggotasing)
cophenetic(hierarkising)
#korelasi cophenetic
d1 <- dist(multishapes[,1:2])
hc <- hclust(d1,"single")
d2 <- cophenetic(hc)
corsing=cor(d1,d2)
corsing

##analisis cluster dengan metode Ward
hierarkiward <- hclust(dist(scale(multishapes[,1:2])),method = "ward.D")
hierarkiward
plot(hierarkiward, multishapes$shape)
#DENDROGAM
rect.hclust(hierarkiward,3)
anggotaward <- data.frame(id=multishapes$shape, cutree(hierarkiward,k=3))
View(anggotaward)
cophenetic(hierarkiward)
#korelasi cophenetic
d1 <- dist(multishapes[,1:2])
hc <- hclust(d1,"ward.D")
d2 <- cophenetic(hc)
corward=cor(d1,d2)
corward

##analisis cluster dengan metode centroid
hierarkicent <- hclust(dist(scale(multishapes[,1:2])),method = "centroid")
hierarkicent
plot(hierarkicent, multishapes$shape)
#DENDROGAM
rect.hclust(hierarkicent,3)
anggotacent <- data.frame(id=multishapes$shape, cutree(hierarkicent,k=3))
View(anggotacent)
cophenetic(hierarkicent)
#korelasi cophenetic
d1 <- dist(multishapes[,1:2])
hc <- hclust(d1,"centroid")
d2 <- cophenetic(hc)
corcent=cor(d1,d2)
corcent

#Pemilihan Model Terbaik
#semakin mendekati angka 1 maka solusi yang dihasilkan oleh proses klustering semakin baik.
model.terbaik<-data.frame(corave, corcomp, corsing, corward, corcent)
model.terbaik
