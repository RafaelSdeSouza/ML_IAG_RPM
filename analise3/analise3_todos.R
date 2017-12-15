#### script MLG USP 14-12-2017 - todos os data sets
rm(list=ls())


################ ------------------- data 1) 
#analise fator R>1:
#leitura
#load("~/WKSPCE_data1.RData")
data1.loc<-file.choose()
data1.df <- read.table(data1.loc, header=TRUE)
#str(data1.df)

# criando fator
data1.df$R1 <- (data1.df$Rc_Index >= 1)
data1.df$R1 <- ifelse(data1.df$R1 == TRUE, "R>1", "R<1")


#plotando
#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

comb = combn(ncol(data1.df[,1:4]),2)
p = list()
#i = 1

#x = comb[,1]
# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes <- colnames(data1.df[1:4])

comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))
comb_nomes <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes[comb[i,j]]
  })
})

p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data1.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = R1), size = 1.5)+
      ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#ok!

grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)
#p[[1]]; p[[2]]



# # ou .. melhor para controlar o título:
# p1 <- ggplot(data1.df, aes(Core, Lobe))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p1
# 
# p2 <- ggplot(data1.df, aes(Core, B_Band))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p2
# 
# p3 <- ggplot(data1.df, aes(Core, Mass))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p3
# 
# p4 <- ggplot(data1.df, aes(Lobe, B_Band))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p4
# 
# p5 <- ggplot(data1.df, aes(Lobe, Mass))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p5
# 
# p6 <- ggplot(data1.df, aes(Mass, B_Band))+
#   geom_point(aes(colour = R1), size = 2)+
#   ggtitle("Comparação R>1 e R<1")+
#   theme(plot.title = element_text(hjust=0.5)); p6
# 
# x11()
# grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3)
# str(p1)

#rodando MClust
### G=2
library(mclust)
out2 <- Mclust(data1.df[,1:4], G=2)
str(out2)
#smy2<-summary(out2$BIC)
#fviz_mclust_bic(out2)

# levando a classificação para os dados originais
data1.df$cluster2 <- as.factor(out2$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data1.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster2, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

library(factoextra)
fviz_mclust(out2, "classification", geom = "point")

summary(out2$BIC)
fviz_mclust_bic(out2)

# quão significativo G=2 eh em relação a G=3 (ou vice versa)?:
### G=3
out3 <- Mclust(data1.df[,1:4], G=3)
#str(out4)

# levando a classificação para os dados originais
data1.df$cluster3 <- as.factor(out3$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data1.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster3, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

summary(out3$BIC)
fviz_mclust_bic(out3)


### G irrestrito (soh para pegar gráfico do AIC -- jah iria retornar 3 otimo?,
#.. senao fazia soh comoo otimo)
### NAO! --> retorna 4 como otimo no data1

out4 <- Mclust(data1.df[,1:4])
str(out4)

# levando a classificação para os dados originais
data1.df$cluster4 <- as.factor(out4$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data1.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster4, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

summary(out4$BIC)
fviz_mclust_bic(out4)

# não parece ser tão diferente em termos de BIC !


################ ------------------- data 2) 
#analise fator R>1:
#leitura
#load("~/WKSPCE_data1.RData")
data2.loc<-file.choose()
data2.df <- read.table(data2.loc, header=TRUE)
str(data2.df)

# criando fator
data2.df$R1 <- (data2.df$R_Index >= 1)
data2.df$R1 <- ifelse(data2.df$R1 == TRUE, "R>1", "R<1")


#plotando
#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

comb = combn(ncol(data2.df[,1:4]),2)
p = list()
#i = 1

#x = comb[,1]
# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes <- colnames(data2.df[1:4])

comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))
comb_nomes <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes[comb[i,j]]
  })
})

p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data2.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = R1), size = 1.5)+
      ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#ok!

grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)
#p[[1]]; p[[2]]

#rodando MClust
### G=2
library(mclust)
out2 <- Mclust(data2.df[,1:4], G=2)
str(out2)
#smy2<-summary(out2$BIC)
#fviz_mclust_bic(out2)

# levando a classificação para os dados originais
data2.df$cluster2 <- as.factor(out2$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data2.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster2, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

# NÃO HAH EVIDENCIAS MESMO OARA OS DADOS ORIGINAIS !!!


library(factoextra)
fviz_mclust(out2, "classification", geom = "point")

summary(out2$BIC)
fviz_mclust_bic(out2)

# quão significativo G=2 eh em relação a G=3 (ou vice versa)?:
### G=3
out3 <- Mclust(data2.df[,1:4], G=3)
#str(out4)

# levando a classificação para os dados originais
data2.df$cluster3 <- as.factor(out3$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data2.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster3, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

# SEM PADRÃO DIAGONAL

summary(out3$BIC)
fviz_mclust_bic(out3)


### G irrestrito (soh para pegar gráfico do AIC -- jah iria retornar 3 otimo?,
#.. senao fazia soh comoo otimo)
### NAO! --> retorna 4 como otimo no data2 TB!

out4 <- Mclust(data2.df[,1:4])
str(out4)

# levando a classificação para os dados originais
data2.df$cluster4 <- as.factor(out4$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data2.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster4, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

summary(out4$BIC)
fviz_mclust_bic(out4)

# a diferença tb não é tão relevante entre os BICs de out2, out3 e out4


################ ------------------- data 3) 
#analise fator R>1:
#leitura
#load("~/WKSPCE_data1.RData")
data3.loc<-file.choose()
data3.df <- read.table(data3.loc, header=TRUE)
str(data3.df)
which(data3.df$delta==0)
# Rafa jah retirou REDSHIFT e as obs com zeros

# criando fator
data3.df$R1 <- (data3.df$Ratio >= 1) # confirma se eh essa coluna mesmo
data3.df$R1 <- ifelse(data3.df$R1 == TRUE, "R>1", "R<1")


#plotando
#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

comb = combn(ncol(data3.df[,1:4]),2)
p = list()
#i = 1

#x = comb[,1]
# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes <- colnames(data3.df[1:4])

comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))
comb_nomes <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes[comb[i,j]]
  })
})

p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data3.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = R1), size = 1.5)+
      ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#ok!

grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)
#p[[1]]; p[[2]]

#rodando MClust
### G=2
library(mclust)
out2 <- Mclust(data3.df[,1:4], G=2)
str(out2)
#smy2<-summary(out2$BIC)
#fviz_mclust_bic(out2)

# levando a classificação para os dados originais
data3.df$cluster2 <- as.factor(out2$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data3.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster2, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

# NÃO HAH EVIDENCIAS MESMO OARA OS DADOS ORIGINAIS !!!


library(factoextra)
fviz_mclust(out2, "classification", geom = "point")

summary(out2$BIC)
fviz_mclust_bic(out2)

# quão significativo G=2 eh em relação a G=3 (ou vice versa)?:
### G=3
out3 <- Mclust(data3.df[,1:4], G=3)
#str(out4)

# levando a classificação para os dados originais
data3.df$cluster3 <- as.factor(out3$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data3.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster3, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

# SEM PADRÃO DIAGONAL

summary(out3$BIC)
fviz_mclust_bic(out3)


### G irrestrito (soh para pegar gráfico do AIC -- jah iria retornar 3 otimo?,
#.. senao fazia soh comoo otimo)
### NAO! --> retorna 4 como otimo no data3 TB!

out4 <- Mclust(data3.df[,1:4])
str(out4)

# levando a classificação para os dados originais
data3.df$cluster4 <- as.factor(out4$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data3.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster4, shape=R1), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

summary(out4$BIC)
fviz_mclust_bic(out4)

# NO 3 DATASET O IDEAL SÃO 3 CLUSTERS, COM MAIOR DEIFERENÇA PARA G=2


################ ------------------- data 4) 
#analise fator R>1:
#leitura
#load("~/WKSPCE_data1.RData")
data4.loc<-file.choose()
data4.df <- read.table(data4.loc, header=TRUE)
str(data4.df)

#NAO HAH ÍNDICE R NO DATASET4 -- nao teremos parametro shape nos plots 2 a 2!!!

#plotando
#install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

comb = combn(ncol(data4.df),2) # TODAS AS VARIAVEIS
p = list()
#i = 1

#x = comb[,1]
# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes <- colnames(data4.df)

comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))
comb_nomes <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes[comb[i,j]]
  })
})

p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data4.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = "red"), size = 1.5)+
      ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#ok!

grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)
#p[[1]]; p[[2]]

#rodando MClust
### G=2
library(mclust)
out2 <- Mclust(data4.df, G=2)
str(out2)
#smy2<-summary(out2$BIC)
#fviz_mclust_bic(out2)

# levando a classificação para os dados originais
data4.df$cluster2 <- as.factor(out2$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data4.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster2), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)


library(factoextra)
fviz_mclust(out2, "classification", geom = "point")

summary(out2$BIC)
fviz_mclust_bic(out2)

# quão significativo G=2 eh em relação a G=3 (ou vice versa)?:
### G=3
out3 <- Mclust(data4.df, G=3)
#str(out4)

# levando a classificação para os dados originais
data4.df$cluster3 <- as.factor(out3$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data4.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster3), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

# SEM PADRÃO DIAGONAL

summary(out3$BIC)
fviz_mclust_bic(out3)


### G irrestrito (soh para pegar gráfico do AIC -- jah iria retornar 3 otimo?,
#.. senao fazia soh comoo otimo)
### NAO! --> retorna 4 como otimo no data4 TB!

out4 <- Mclust(data4.df)
str(out4)

# levando a classificação para os dados originais
data4.df$cluster4 <- as.factor(out4$classification)

# plots 2 a 2:
p <- sapply(1:length(comb), function(i){
  apply(comb_nomes, 2, function(x){
    p[i] <- ggplot(data4.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster4), size = 1.5)+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#x11()
grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]], ncol=3)

# O MELHOR NO DATA SET4 SAO DOIS CLUSTERS !! 

summary(out4$BIC)
fviz_mclust_bic(out4)

