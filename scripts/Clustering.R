require(mclust)
require(magrittr)
require(dplyr)
require(corrplot)

# First data Sikora

Sikora_data <-read.csv("..//dataset/Sikora_M.csv")


# Correlation between parameters
M <-cor(Sikora_data[,c(1:5)])
corrplot(M, method="number")

# Caso 1, includindo Log_L, LcorrM, LogRF

dc1 <- Mclust(Sikora_data[,c("Log_L","LcorrM","LogRF")],modelName = "VVV")
plot(dc1)

# Second data

Kell_data <-read.csv("..//dataset/Kell_table.csv")

# Correlation between parameters
Kell_data <- Kell_data %>% select(.,-2) %>%
  mutate(.,VLA_6cm = log(VLA_6cm))

M2 <-cor(Kell_data[,-2])
corrplot(M2, method="number")

# Caso 1, includindo Log_L, LcorrM, LogRF

dc2 <- Mclust(Kell_data[,2:3],modelName = "VVV")
plot(dc2)

outlier <- boxplot.stats(Kell_data$I_Band)$out

nb <- NbClust(Sikora_data_cut , diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=5, method = "average", 
              index = "all", alphaBeale = 0.1)


clusters <- hclust(dist(Sikora_data_cut), method = 'average')
clusterCut <- cutree(clusters, 2)
plot(Sikora_data_cut,col=clusterCut)

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