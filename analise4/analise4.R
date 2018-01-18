#### script MLG USP 14-12-2017 - todos os data sets
rm(list=ls())

### 1) GMM com todas as combinações de espaços de parâmetros (6C4 = 24):

## lendo todos os data sets
data1.loc<-file.choose()
data2.loc<-file.choose()
data3.loc<-file.choose()
data4.loc<-file.choose()

data1.df <- read.table(data1.loc, header=TRUE)
data2.df <- read.table(data2.loc, header=TRUE)
data3.df <- read.table(data3.loc, header=TRUE)
data4.df <- read.table(data4.loc, header=TRUE)

## criando fator R>1, R<1

# dataset1
data1.df$R1 <- (data1.df$Rc_Index >= 1)
data1.df$R1 <- ifelse(data1.df$R1 == TRUE, "R>1", "R<1")
# dataset2
data2.df$R1 <- (data2.df$R_Index >= 1)
data2.df$R1 <- ifelse(data2.df$R1 == TRUE, "R>1", "R<1")
# dataset3
data3.df$R1 <- (data3.df$Ratio >= 1) # confirmar se eh essa coluna mesmo
data3.df$R1 <- ifelse(data3.df$R1 == TRUE, "R>1", "R<1")
# dataset 4 ---> nao hah radio

## eliminando Speak do dataset4
library(magrittr)
data4.df <- data4.df %>% .[,-4]
data4.df <- data4.df[,-4]

## GMM com G=2 para todos
library(mclust)
# dataset1
set.seed(1984)
out1 <- Mclust(data1.df[,1:4], G=2)
data1.df$cluster <- as.factor(out1$classification)
# dataset2
set.seed(1984)
out2 <- Mclust(data2.df[,1:4], G=2)
data2.df$cluster <- as.factor(out2$classification)
# dataset3
set.seed(1984)
out3 <- Mclust(data3.df[,1:4], G=2)
data3.df$cluster <- as.factor(out3$classification)
# dataset4
set.seed(1984)
out4 <- Mclust(data4.df[,1:4], G=2)
data4.df$cluster <- as.factor(out4$classification)

## combinacoes para os plots
comb = combn(ncol(data1.df[,1:4]),2)
p = list()

# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes1 <- colnames(data1.df[,1:4])
nomes2 <- colnames(data2.df[,1:4])
nomes3 <- colnames(data3.df[,1:4])
nomes4 <- colnames(data4.df[,1:4])

comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))


comb_nomes1 <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes1[comb[i,j]]
  })
})
comb_nomes2 <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes2[comb[i,j]]
  })
})
comb_nomes3 <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes3[comb[i,j]]
  })
})
comb_nomes4 <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes4[comb[i,j]]
  })
})

## plots 2 a 2
# dataset1
p1 <- sapply(1:length(comb), function(i){
  apply(comb_nomes1, 2, function(x){
    p[i] <- ggplot(data1.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(3, 16))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset2
p2 <- sapply(1:length(comb), function(i){
  apply(comb_nomes2, 2, function(x){
    p[i] <- ggplot(data2.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(3, 16))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset3
p3 <- sapply(1:length(comb), function(i){
  apply(comb_nomes3, 2, function(x){
    p[i] <- ggplot(data3.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(3, 16))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset4
p4 <- sapply(1:length(comb), function(i){
  apply(comb_nomes4, 2, function(x){
    p[i] <- ggplot(data4.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster), size = 1.2)+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

# (grf <- paste0("p", rep(1:4, each=6), "[[", 1:6, "]]", ",")) 
# (grf <- paste0("p", rep(1:4, each=6), "[[", 1:6, "]]", collapse="," )) 
# 
# grf[length(grf)] <- sub(",", "", grf[length(grf)])
# 
# library(gridExtra)
# x11()
# grid.arrange( eval(parse(text=grf)), ncol=6 )
# 
# grid.arrange(eval(parse(text=grf)), ncol=3)

library(gridExtra)

help(grid.arrange)

grid.arrange(p1[[1]],p1[[2]],p1[[3]],p1[[4]],p1[[5]],p1[[6]],
             p2[[1]],p2[[2]],p2[[3]],p2[[4]],p2[[5]],p2[[6]],
             p3[[1]],p3[[2]],p3[[3]],p3[[4]],p3[[5]],p3[[6]],
             p4[[1]],p4[[2]],p4[[3]],p4[[4]],p4[[5]],p4[[6]], ncol=3)
x11()
grid.arrange(p1[[1]],p1[[2]],p1[[3]],p1[[4]],p1[[5]],p1[[6]],
             p2[[1]],p2[[2]],p2[[3]],p2[[4]],p2[[5]],p2[[6]],
             p3[[1]],p3[[2]],p3[[3]],p3[[4]],p3[[5]],p3[[6]],
             p4[[1]],p4[[2]],p4[[3]],p4[[4]],p4[[5]],p4[[6]], ncol=6,
             top = "Cluster G=2 vs R>1/R<1")

savePlot("fig1.PNG", type = "png",
         device = dev.cur())

### 2) BIC & ICL

## geral -- saida do MClust:
library(factoextra)
p1_bic <- fviz_mclust_bic(out1)
p2_bic <- fviz_mclust_bic(out2)
p3_bic <- fviz_mclust_bic(out3)
p4_bic <- fviz_mclust_bic(out4)

grid.arrange(p1_bic, p2_bic, p3_bic, p4_bic, ncol=2)


bic = -BIC(out1,out2,out3,out4)[,2]
ICL = c(icl(out1),icl(out2),icl(out3),icl(out4))
method = rep(c("-BIC","ICL"), each=4)
Dataset = paste0("dataset", 1:4)

df <- data.frame("value" = c(bic, ICL), method, Dataset)
str(df)

x11()
p_bic_icl <- ggplot(df, aes(x=Dataset, y=value, group=method))+
  geom_line(aes(color=method), size=1.2)+
  scale_color_manual(values=c('#E69F00', '#56B4E9'))+
  ggtitle("BIC vs ICL")+
  theme(plot.title = element_text(hjust=0.5))
p_bic_icl

savePlot("fig2.PNG", type = "png",
         device = dev.cur())

### 3) R vs optico com probabilidade (z scores) pertencimento
# help("Mclust")
# out1$z

require(hexbin)
# Default plot 
c + geom_hex()
# Change the number of bins
c + geom_hex(bins = 10)


require(hexbin)
# Default plot 
p1[[1]] + geom_hex()
# Change the number of bins
p1[[1]] + geom_hex(bins = 10)

length(out1$z)
p1[[1]] + stat_bin_hex()
p1[[1]] + stat_summary_hex(aes(z = out1$z[,1])) #  a sacada eh pegar soh uma das colunas,
#... pq a outra probabilidade serah 1-p

# dataset1
p1_p <- ggplot(data1.df, aes(Core, B_Band))+
  stat_summary_hex(aes(z = out1$z[,1]))+
  ggtitle("Dataset 1 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
p1_p

p2_p <- ggplot(data2.df, aes(Radio, B_Band))+
  stat_summary_hex(aes(z = out2$z[,1]))+
  ggtitle("Dataset 2 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability')
p2_p

p3_p <- ggplot(data3.df, aes(VLA_6cm, I_Band))+
  stat_summary_hex(aes(z = out3$z[,1]))+
  ggtitle("Dataset 3 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability')
p3_p

p4_p <- ggplot(data4.df, aes(Bmag, Sint))+
  stat_summary_hex(aes(z = out4$z[,1]))+
  ggtitle("Dataset 4 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability')
p4_p
## alterar eixos para o plot4 ? Bmag seria optico e deveria ir no eixo y?

grid.arrange(p1_p,p2_p,p3_p,p4_p, ncol=2)

#plot(out1, what = "density", type = "persp")

#plot(out2, what = "density", type = "persp", col=rainbow(100))

savePlot("fig3.PNG", type = "png",
         device = dev.cur())


### 4) Figura com divisão dos tipos (somente datasets 1 e 2) e R>1, R<1

p1_t <- ggplot(data1.df, aes(Core, B_Band))+
  geom_point(aes(colour = Type, shape=cluster), size = 1.8)+
  scale_shape_manual(values=c(3, 16))+
  #scale_color_manual(values=c('#999999', '#E69F00', '#56B4E9', 'green2', 'indianred4'))+
  ggtitle("Dataset 1 - Type vs Cluster")+
  theme(plot.title = element_text(hjust=0.5))
p1_t

p2_t <- ggplot(data2.df, aes(Radio, B_Band))+
  geom_point(aes(colour = Type, shape=cluster), size = 1.8)+
  scale_shape_manual(values=c(3, 16))+
  ggtitle("Dataset 2 - Type vs Cluster")+
  theme(plot.title = element_text(hjust=0.5))
p2_t

grid.arrange(p1_t,p2_t,ncol=2)


savePlot("fig4.PNG", type = "png",
         device = dev.cur())


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

