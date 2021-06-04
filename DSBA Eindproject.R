# preparation

rm(list = ls())

library('FactoMineR')
library('gmodels')
library('class')
library('caret')
library("tidyverse")
library("kernlab")
library("randomForest")
library("ranger")

df1<-readRDS("sample_def.RDs")
df1<-df1[2:4]

# question 1 KNN RFM


set.seed(123)
SSW<-NULL			# maak een lege SSW vector
for (k in 1:10)	 				
{k.out<-kmeans(df1,k,nstart=100)
SSW <-rbind(SSW,k.out$tot.withinss)}
plot(SSW, type="b")


# Calinski-Harabasz
set.seed(1)
CH<-NULL			
for (k in (1:10)){k.out<-kmeans(df1,k,nstart=100);
CH<-rbind(CH,(nrow(df1)-k)*k.out$betweenss/((k-1)*k.out$tot.withinss))}
plot(CH, type="b")

set.seed(1)
K=3
k.out<-kmeans(df1,K,nstart=100)
table(df$country,k.out$cluster)
par(mfrow=c(2,2))
boxplot(df1[k.out$cluster==1,])
boxplot(df1[k.out$cluster==2,])
boxplot(df1[k.out$cluster==3,])


# Om inzicht te krijgen in de verdelingen per cluster kunnen we de data per cluster selecteren:
C1<-df1[k.out$cluster==1,] 
C2<-df1[k.out$cluster==2,]
C3<-df1[k.out$cluster==3,]

# We kunnen ook de gemiddelden van alle variabelen bekijken en die in een plaatje zetten:
C1Mean<-colMeans(C1)
C2Mean<-colMeans(C2)
C3Mean<-colMeans(C3)


ClusMeans<-cbind(C1Mean,C2Mean,C3Mean)
matplot(ClusMeans, ylab="Means",xaxt="n", type="l", lwd = 4)
legend("bottomright", inset=.01, 
       legend=c("C1", "C2", "C3"), pch="-", cex=0.5, col=c(1:3))
axis(1, at= 1:21, labels=names(df1), cex.axis=0.6,las=2)


# Wat te denken van PCA:
cntr<-as.character(k.out$cluster)
cntr2<-as.character(df$country)
pca.out<-princomp(df1)
biplot(pca.out,xlabs=cntr,ylabs=names(df1), col=c("grey","red"))
biplot(pca.out,xlabs=cntr2,ylabs=names(df1), col=c("grey","red"))
plot(pca.out)

