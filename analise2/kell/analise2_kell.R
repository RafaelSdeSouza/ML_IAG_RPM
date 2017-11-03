#### script MLG USP 01-11-2017 - kell
rm(list=ls())


#1) analise fator R>1:

#leitura
load("~/WKSPCE_analise2_kell.RData")
kell.loc<-file.choose()
kell.df <- read.table(kell.loc, header=TRUE)
str(kell.df)


head(kell.df)

# eliminando a coluna de REDSHIFT
kell.df <- kell.df[,-2]


# tirando as obs com zeros:
#kell.df[kell.df==0,]

kell.df <- kell.df[-which(kell.df$delta==0),]


# transformando as variaveis Ratio e VLA_6cm para log
library(tidyr)
kell.df[,c("VLA_6cm", "Ratio")] <- log(kell.df[,c("VLA_6cm", "Ratio")])

colnames(kell.df)[c(2,4)] <- c("Log_VLA_6cm", "Log_Ratio")


# # criando fator
# kell.df$R1 <- (kell.df$R >= 1)
# kell.df$R1 <- ifelse(kell.df$R1 == TRUE, "R>1", "R<1")


#plotando
#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

comb = combn(ncol(kell.df[,1:4]),2)
p = list()
#i = 1

#x = comb[,1]
# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes <- colnames(kell.df)

comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))
comb_nomes <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes[comb[i,j]]
  })
})

p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(kell.df, aes_string(x[1], x[2]))+
      geom_point(size = 1.5)+
      ggtitle("Comparações 2x2")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#ok!

# p <- sapply(1:length(comb), function(i){
#   apply(comb, 2, function(x){
#     p[i] <- ggplot(kell.df, aes(kell.df[,x[1]], kell.df[,x[2]]))+
#       geom_point(size = 1.5)+
#       ggtitle("Comparações 2x2")+
#       theme(plot.title = element_text(hjust=0.5))
#   })  
# })
p
#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)
#p[[1]]; p[[2]]



# # ou .. melhor para controlar o título:
# p1 <- ggplot(kell.df, aes(delta, REDSHIFT))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p1
# 
# p2 <- ggplot(kell.df, aes(delta, VLA_6cm))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p2
# 
# p3 <- ggplot(kell.df, aes(delta, I_Band))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p3
# 
# p4 <- ggplot(kell.df, aes(delta, Ratio))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p4
# 
# p5 <- ggplot(kell.df, aes(REDSHIFT, VLA_6cm))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p5
# 
# p6 <- ggplot(kell.df, aes(REDSHIFT, I_Band))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p6
# 
# p7 <- ggplot(kell.df, aes(REDSHIFT, Ratio))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p7
# 
# p8 <- ggplot(kell.df, aes(VLA_6cm, I_Band))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p8
# 
# p9 <- ggplot(kell.df, aes(VLA_6cm, Ratio))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p9
# 
# p10 <- ggplot(kell.df, aes(I_Band, Ratio))+
#   geom_point(size = 1.5)+
#   ggtitle("Comparações 2x2")+
#   theme(plot.title = element_text(hjust=0.5)); p10
# 
# x11()
# grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3)
# str(p1)

#rodando MClust

### G irrestrito
library(mclust)
library(factoextra)

out1 <- Mclust(kell.df[,1:4])
str(out1)
summary(out1$BIC)
fviz_mclust_bic(out1)

# levando a classificação para os dados originais
kell.df$cluster1 <- as.factor(out1$classification)


p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(kell.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster1),size = 1.5)+
      ggtitle("Comparações 2x2")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})



#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

fviz_mclust(out1, "classification", geom = "point")


### G=2

out2 <- Mclust(kell.df[,1:4], G=2)
#str(out2)
summary(out2$BIC)
#fviz_mclust_bic(out2)

# levando a classificação para os dados originais
kell.df$cluster2 <- as.factor(out2$classification)

p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(kell.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster2),size = 1.5)+
      ggtitle("Comparações 2x2")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

fviz_mclust(out1, "classification", geom = "point")


#PCA:
pca<-princomp(kell.df[,1:4])
summary(pca)
pca$loadings
# log_VLA e Ratio são os mais importantes para explicar a variabilidade dos dados

#install.packages("ggfortify")
#http://rpubs.com/sinhrks/plot_pca

library(ggfortify)
autoplot(pca)

# verificando quem eh o outlier:
biplot(pca)
# o objeto 18 aprece como um outlier


#tourr
library(tourr)

animate(kell.df[,1:4],
        grand_tour(d = 2), display = display_xy())
animate(kell.df[,1:4],
        grand_tour(d = 3), display = display_depth())
animate(kell.df[,1:4],
        grand_tour(d = 4), display = display_pcp())

biplot(pca)
# o objeto 18 aprece como um outlier

# animação GIF
#install.packages("animation")
library(animation)
help("animation")

ani.options(interval=0.08)
saveGIF(animate(kell.df[,1:4],
                grand_tour(d = 2), display = display_xy()),
        movie.name = "kell.gif")




# teste específico para a combinação de duas variáveis que aprece na diagonal:
# fazer para G irrestrito e G=2
# Nos parece que o gráfico I_Band vs Redshift eh o que tem o formato mais parecido
#... com a diagonal:

### G irrestrito (I_BAND vs REDSHIFT)
out3 <- Mclust(kell.df[,c("Log_VLA_6cm", "Log_Ratio")])
#str(out3)
summary(out3$BIC)
fviz_mclust_bic(out3)

# levando a classificação para os dados originais
kell.df$cluster3 <- as.factor(out3$classification)

p <- ggplot(kell.df, aes(Log_VLA_6cm, Log_Ratio))+
  geom_point(aes(colour = cluster3),size = 1.5)+
  ggtitle("Teste Clustering Específico para 2 var")+
  theme(plot.title = element_text(hjust=0.5));p


### G=2 (Log_VLA_6cm vs Log_Ratio)

out4 <- Mclust(kell.df[,c("Log_VLA_6cm", "Log_Ratio")], G=2)
#str(out4)
#summary(out4$BIC)
#fviz_mclust_bic(out2)

# levando a classificação para os dados originais
kell.df$cluster4 <- as.factor(out4$classification)

p <- ggplot(kell.df, aes(Log_VLA_6cm, Log_Ratio))+
  geom_point(aes(colour = cluster4),size = 1.5)+
  ggtitle("Teste Clustering Específico para 2 var")+
  theme(plot.title = element_text(hjust=0.5));p