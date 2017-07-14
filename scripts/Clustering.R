require(mclust)
library(NbClust)
bdata <- read.table("..//dataset/Sikora_Mcorr_(1).txt",header = T)
write.csv(bdata,"Sikora_Mcorr_1.csv",row.names = F)
bdata <-bdata[-74,]

bdata2 <- bdata[,c(3,4,5)]
chart.Correlation(bdata2, histogram=TRUE, pch=19)


nb <- NbClust(bdata2 , diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=5, method = "average", 
              index = "all", alphaBeale = 0.1)


clusters <- hclust(dist(bdata2), method = 'average')
clusterCut <- cutree(clusters, 2)
plot(bdata2,col=clusterCut)

plot(bdata2[,1:2],col=clusterCut)

x<- bdata$Log_L
y<- bdata$LogLRc
fit<-lm(y~x) 
summary(fit)

x2<- log10(10^bdata$LogLRc + 10^bdata$Log_L)
y2<- bdata$LogLRc
fit2<-lm(y2~x2) 
summary(fit2)


b0<-bdata$Log_L
b0[b0 > 0]<- 1


ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green'))