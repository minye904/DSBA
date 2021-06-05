# preparation k-means ====

rm(list = ls())

library('FactoMineR')
library('gmodels')
library('class')
library('caret')
library("tidyverse")
library("kernlab")
library("randomForest")
library("ranger")

# df0 = orginal data
df0<-readRDS("sample_def.RDs")

# df1 = orginal data with only RFM variables
df1<-df0[2:4]


# Standardize train and test using means & sd's

means<-colMeans(df1)
sds<-apply(df1, 2, sd)
df2<-scale(df1,means,sds)

# df3 = standardized data with only RFM variables
df3<-as_tibble(df2)

# cluster selection with SSW & CH ====

# SSW
set.seed(2021)
SSW<-NULL			# maak een lege SSW vector
for (k in 1:10)	 				
{k.out<-kmeans(df3,k,nstart=100)
SSW <-rbind(SSW,k.out$tot.withinss)}
plot(SSW, type="b")


# Calinski-Harabasz
set.seed(2021)
CH<-NULL			
for (k in (1:10)){k.out<-kmeans(df3,k,nstart=100);
CH<-rbind(CH,(nrow(df1)-k)*k.out$betweenss/((k-1)*k.out$tot.withinss))}
plot(CH, type="b")



# k-means and data onderzoek ====

# k-means using the right K cluster derived from SSW & Calinski-Harabasz plots
set.seed(2021)
K=4 # select the right cluster 
k.out<-kmeans(df3,K,nstart=100)


# heavy scatter plot 
plot1<-plot(df3,col=k.out$cluster)
plot1


# boxplot standardized values with ylim 0-20
par(mfrow=c(2,2))
boxplot(df3[k.out$cluster==1,], ylim = c(0, 20))
boxplot(df3[k.out$cluster==2,],ylim = c(0, 20))
boxplot(df3[k.out$cluster==3,],ylim = c(0, 20))
boxplot(df3[k.out$cluster==4,],ylim = c(0, 20))


# Om inzicht te krijgen in de verdelingen per cluster kunnen we de data per cluster selecteren:
C1<-df1[k.out$cluster==1,] 
C2<-df1[k.out$cluster==2,]
C3<-df1[k.out$cluster==3,]
C4<-df1[k.out$cluster==4,]

# boxplot original values with ylim 0-4000
par(mfrow=c(2,2))
boxplot(df1[k.out$cluster==1,],ylim = c(0, 4000))
boxplot(df1[k.out$cluster==2,],ylim = c(0, 4000))
boxplot(df1[k.out$cluster==3,],ylim = c(0, 4000))
boxplot(df1[k.out$cluster==4,],ylim = c(0, 4000))


# summary of the statistics
summary(C1)
summary(C2)
summary(C3)
summary(C4)

# revenue statistics
sum(C1$revenue)/sum(df1$revenue)
sum(C2$revenue)/sum(df1$revenue)
sum(C3$revenue)/sum(df1$revenue)
sum(C4$revenue)/sum(df1$revenue)

# kobo vs a la carte statistics

CC1<-df0[k.out$cluster==1,] 
CC2<-df0[k.out$cluster==2,]
CC3<-df0[k.out$cluster==3,]
CC4<-df0[k.out$cluster==4,]

sum(CC1$indKP)
sum(CC2$indKP)
sum(CC3$indKP)
sum(CC4$indKP)

summary(CC1)
summary(CC2)
summary(CC3)
summary(CC4)

# k-means and data visualisatie ====
# kobo plus in binary
# df4 = original data with Kobo as binary variable

df4<-df0
df4$Koboplus <-0
df4$Koboplus [df4$indKP>0] <-1
sum(df4$Koboplus)
sum(df4$indKP)

# toevoegen van clusters aan de originele dataset

df4$cluster<-NULL
df4$cluster<-k.out$cluster


# individuele dataset maken voor clusters
CCC1<-df4[k.out$cluster==1,] 
CCC2<-df4[k.out$cluster==2,]
CCC3<-df4[k.out$cluster==3,]
CCC4<-df4[k.out$cluster==4,]

boxplot(CCC1[2:4])
boxplot(CCC2[2:4])
boxplot(CCC3[2:4])
boxplot(CCC4[2:4])


# data categorisatie op basis van interpretatie

df4$cluster[df4$cluster==1]<-"Sleepy"
df4$cluster[df4$cluster==2]<-"Better"
df4$cluster[df4$cluster==3]<-"Best"
df4$cluster[df4$cluster==4]<-"Good"

df4$Koboplus[df4$Koboplus==1]<-"Kobo plus klanten"
df4$Koboplus[df4$Koboplus==0]<-"a la carte klanten"



# tabel met kern informatie

df4_mutated <-df4%>%
  group_by(cluster)%>%
  summarize(aantal_klanten = n(), 
            percentage_klanten = n()/nrow(df4),
            omzet=sum(revenue),
            omzet_percentage= sum(revenue)/sum(df4$revenue),
            gemiddelde_omzet= mean(revenue), 
            gemiddelde_recency=mean(recency), 
            gemiddelde_frequency=mean(frequency), 
            kobo_plus_binary=mean(Koboplus),
            kobo_plus_fractie=mean(indKP))


# voorbereiding plots
df4_plot<-df4 %>%
  mutate(cluster=as.factor(cluster)) %>%
  mutate(cluster=fct_relevel(cluster,"Best", "Better", "Good","Sleepy"))%>%
  mutate(Koboplus=as.factor(Koboplus))


# RFM plot voor 4 clusters
ggplot(df4_plot, aes(y=recency, fill=cluster))+geom_boxplot()
ggplot(df4_plot, aes(y=frequency, fill=cluster))+geom_boxplot()
ggplot(df4_plot, aes(y=revenue, fill=cluster))+geom_boxplot()


# RFM plot voor 4 clusters en Kobo vs a la carte

ggplot(df4_plot, aes(y=recency, fill=cluster, color=Koboplus))+geom_boxplot()+
  scale_color_manual(values = c("black", "dark red"))+
  scale_fill_manual(values = c("light blue", "yellow", "light green", "grey"))

ggplot(df4_plot, aes(y=frequency, fill=cluster, color=Koboplus))+geom_boxplot()+
  scale_color_manual(values = c("black", "dark red"))+
  scale_fill_manual(values = c("light blue", "yellow", "light green", "grey"))

ggplot(df4_plot, aes(y=revenue, fill=cluster, color=Koboplus))+geom_boxplot()+
  scale_color_manual(values = c("black", "dark red"))+
  scale_fill_manual(values = c("light blue", "yellow", "light green", "grey"))


# moeten we Kobo plus recency & frequency op basis van borrow data berekenen?
# we nemen nu Kobo plus abonnement betaling als uitgangspunt, waardoor Kobo plus klanten automatisch
# elke maand, bij doorlopende abonnement een hoge recency & frequency heeft


# plot Kobo vs a la carte
df4_plot2<-df4 %>%
  mutate(Koboplus=as.factor(Koboplus))

ggplot(df4_plot2, aes(y=recency, fill=Koboplus))+geom_boxplot()
ggplot(df4_plot2, aes(y=frequency, fill=Koboplus))+geom_boxplot()
ggplot(df4_plot2, aes(y=revenue, fill=Koboplus))+geom_boxplot()
