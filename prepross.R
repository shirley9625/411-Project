library(sf)
library(data.table)
library(birk)
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(RgoogleMaps)
library(beepr)
#install.packages("beepr")
dataset<-fread("training.csv")

#缺失值处理,没有na的值
n = sum(is.na(dataset))

raw_dataset = dataset[,3:1922]
#PCA = princomp(raw_dataset,cor = T)


data.prcomp = prcomp(raw_dataset,center = TRUE,scale = TRUE)
plot(data.prcomp$x, main = "after PCA")
#pca_eigen_values <- data.prcomp$sdev^2
#data.prcomp$sdev
screeplot(data.prcomp,npcs = 146, type = "lines")


#summary(data.prcomp)
pc <- data.prcomp$rotation[,1:75]
ra <- as.matrix(raw_dataset)
pc_m <- as.matrix(pc)
trainingset <-ra%*%pc_m 

index <- as.matrix(dataset[,1:2])
trainingsetF <-cbind(index,trainingset)
trainingsetF <- as.data.frame(trainingsetF)
write.csv(trainingsetF,"./trainingset75.csv",row.names = FALSE)


#--------------------------------------------------
testraw<-fread("testing.csv")
raw_dataset = testraw[,3:1922]
#PCA = princomp(raw_dataset,cor = T)


#data.prcomp = prcomp(raw_dataset,center = TRUE,scale = TRUE)
#plot(data.prcomp$x, main = "after PCA")
#pca_eigen_values <- data.prcomp$sdev^2
#data.prcomp$sdev
#screeplot(data.prcomp,npcs = 146, type = "lines")


#summary(data.prcomp)
pc <- data.prcomp$rotation[,1:75]
ra <- as.matrix(raw_dataset)
pc_m <- as.matrix(pc)
#crossprod(pc_m,ra)
trainingset <-ra%*%pc_m 

index <- as.matrix(testraw[,1:2])
trainingsetF <-cbind(index,trainingset)
trainingsetF <- as.data.frame(trainingsetF)



write.csv(trainingsetF,"./testingset75.csv",row.names = FALSE)


#------------------------------------
dataset1<-fread("trainingset75.csv")
dataset1<-fread("testingset110.csv")


shell("cmd/c C:/Windows/Media/tada.wav")

