#### script MLG USP 01-11-2017 - sikora
rm(list=ls())


#1) analise fator R>1:

#leitura
#load("~/WKSPCE_sikora.RData")
sikora.loc<-file.choose()
sikora.df <- read.table(sikora.loc, header=TRUE)
#str(sikora.df)

# criando fator
sikora.df$R1 <- (sikora.df$R >= 1)
sikora.df$R1 <- ifelse(sikora.df$R1 == TRUE, "R>1", "R<1")


#plotando
#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

comb = combn(ncol(sikora.df[,1:4]),2)
p = list()
#i = 1

#x = comb[,1]
# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes <- colnames(sikora.df[1:4])

comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))
comb_nomes <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes[comb[i,j]]
  })
})

p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(sikora.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = R1), size = 1.5)+
      ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#ok!

grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)
#p[[1]]; p[[2]]



# # ou .. melhor para controlar o título:
# p1 <- ggplot(sikora.df, aes(Core, Lobe))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p1
# 
# p2 <- ggplot(sikora.df, aes(Core, B_Band))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p2
# 
# p3 <- ggplot(sikora.df, aes(Core, Mass))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p3
# 
# p4 <- ggplot(sikora.df, aes(Lobe, B_Band))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p4
# 
# p5 <- ggplot(sikora.df, aes(Lobe, Mass))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p5
# 
# p6 <- ggplot(sikora.df, aes(Mass, B_Band))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p6
# 
# x11()
# grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3)
# str(p1)

#rodando MClust

### G irrestrito
library(mclust)
library(factoextra)

out1 <- Mclust(sikora.df[,1:4])
str(out1)
summary(out1$BIC)
fviz_mclust_bic(out1)

# levando a classificação para os dados originais
sikora.df$cluster1 <- as.factor(out1$classification)

p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(sikora.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster1, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

#fviz_mclust(out1, "classification", geom = "point")


### G=2

out2 <- Mclust(sikora.df[,1:4], G=2)
str(out2)
#smy2<-summary(out2$BIC)
#fviz_mclust_bic(out2)

# levando a classificação para os dados originais
sikora.df$cluster2 <- as.factor(out2$classification)

p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(sikora.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster2, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

fviz_mclust(out2, "classification", geom = "point")

#PCA:
pca<-princomp(sikora.df[,1:4])
summary(pca)
pca$loadings
#install.packages("ggfortify")
#http://rpubs.com/sinhrks/plot_pca

library(ggfortify)
autoplot(pca)


biplot(pca)

#tourr
library(tourr)

animate(sikora.df[,1:4],
        grand_tour(d = 2), display = display_xy())
animate(sikora.df[,1:4],
        grand_tour(d = 3), display = display_depth())
animate(sikora.df[,1:4],
        grand_tour(d = 4), display = display_pcp())

biplot(pca)



# animação GIF
#install.packages("animation")
library(animation)
help("animation")

ani.options(interval=0.08)
saveGIF(animate(sikora.df[,1:4],
                grand_tour(d = 2), display = display_xy()),
        movie.name = "sikora.gif")




# teste específico para a combinação de duas variáveis que aprece na diagonal:
# fazer para G irrestrito e G=2 (ainda mantendo o shape dos R >< 1)
# O gráfico Core vs B_Band eh o que tem o formato mais parecido
#... com a diagonal:

### G irrestrito (I_BAND vs REDSHIFT)

out3 <- Mclust(sikora.df[,c("Core", "B_Band")])
str(out3)
smy3<-summary(out3$BIC)
fviz_mclust_bic(out3)

# levando a classificação para os dados originais
sikora.df$cluster3 <- as.factor(out3$classification)

p <- ggplot(sikora.df, aes(Core, B_Band))+
  geom_point(aes(colour = cluster3, shape=R1),size = 1.5)+
  ggtitle("Teste Clustering Específico para 2 var")+
  theme(plot.title = element_text(hjust=0.5));p

### G=2 (Core vs B_Band)

out4 <- Mclust(sikora.df[,c("Core", "B_Band")], G=2)
#str(out4)
#summary(out4$BIC)
#fviz_mclust_bic(out2)

# levando a classificação para os dados originais
sikora.df$cluster4 <- as.factor(out4$classification)

p <- ggplot(sikora.df, aes(Core, B_Band))+
  geom_point(aes(colour = cluster4, shape=R1),size = 1.5)+
  ggtitle("Teste Clustering Específico para 2 var")+
  theme(plot.title = element_text(hjust=0.5));p
