############## script MLG USP 23-01-2018 - todos os data sets
rm(list=ls())

#### A) SEM ELIMINAR OUTLIERS
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

## transformando Sint na escala log
data4.df[,4] <- log(data4.df[,4])

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


## PREPARANDO OS PLOTS 2a2 (SEM ELIPSES)
## combinacoes para os plots 
comb = combn(ncol(data1.df[,1:4]),2) # apenas a estrutura

# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes1 <- colnames(data1.df[,1:4])
nomes2 <- colnames(data2.df[,1:4])
nomes3 <- colnames(data3.df[,1:4])
nomes4 <- colnames(data4.df[,1:4])

#comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))
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

## plots 2 a 2 (SEM ELIPSES)
library(ggplot2)
p = list()
# dataset1
p1 <- sapply(1:length(comb), function(i){
  apply(comb_nomes1, 2, function(x){
    p[i] <- ggplot(data1.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#p1[[1]]
# dataset2
p2 <- sapply(1:length(comb), function(i){
  apply(comb_nomes2, 2, function(x){
    p[i] <- ggplot(data2.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
p2[[1]]
# dataset3
p3 <- sapply(1:length(comb), function(i){
  apply(comb_nomes3, 2, function(x){
    p[i] <- ggplot(data3.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
p3[[1]]
# dataset4
p4 <- sapply(1:length(comb), function(i){
  apply(comb_nomes4, 2, function(x){
    p[i] <- ggplot(data4.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster), size = 1.2, shape=1)+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
p4[[1]]
# obs: o warning diz respeito a como estamos preenchendo a lista que foi criada em branco

library(gridExtra)

help(grid.arrange)

x11()
grid.arrange(p1[[1]],p1[[2]],p1[[3]],p1[[4]],p1[[5]],p1[[6]],
             p2[[1]],p2[[2]],p2[[3]],p2[[4]],p2[[5]],p2[[6]],
             p3[[1]],p3[[2]],p3[[3]],p3[[4]],p3[[5]],p3[[6]],
             p4[[1]],p4[[2]],p4[[3]],p4[[4]],p4[[5]],p4[[6]], ncol=6,
             top = "Cluster G=2 vs R>1/R<1")

savePlot("fig1.PNG", type = "png",
         device = dev.cur())

### 2) QUAL DIMENSÃO APRESENTA MAIOR ICL ENTRE OS GRUPOS?

## a) todas as combinações 2 a 2:

# combinacoes para as colunas
comb2a2_col = combn(ncol(data1.df[,1:4]),2)

# A IDEIA EH USAR O INDICE DO MENOR ICL PARA INDEXARMOS A COLUNA QUE NOS RETORNARÁ OS NOMES DAS VARIAVEIS
#.. e fazermos uma tabela
# para pegar e passar os nomes das variaveis:
nomes1 <- colnames(data1.df[,1:4])
nomes2 <- colnames(data2.df[,1:4])
nomes3 <- colnames(data3.df[,1:4])
nomes4 <- colnames(data4.df[,1:4])

# matriz a ser preenchida:
comb_nomes2a2 <- matrix(NA, nrow=2, ncol=ncol(comb2a2_col))


comb_nomes1_2a2 <- sapply(1:ncol(comb2a2_col),function(j){
  sapply(1:2,function(i){
    comb_nomes2a2[i,j]<-nomes1[comb2a2_col[i,j]]
  })
})
comb_nomes2_2a2 <- sapply(1:ncol(comb2a2_col),function(j){
  sapply(1:2,function(i){
    comb_nomes2a2[i,j]<-nomes2[comb2a2_col[i,j]]
  })
})
comb_nomes3_2a2 <- sapply(1:ncol(comb2a2_col),function(j){
  sapply(1:2,function(i){
    comb_nomes2a2[i,j]<-nomes3[comb2a2_col[i,j]]
  })
})
comb_nomes4_2a2 <- sapply(1:ncol(comb2a2_col),function(j){
  sapply(1:2,function(i){
    comb_nomes2a2[i,j]<-nomes4[comb2a2_col[i,j]]
  })
})


#comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb2a2_col))

## GMM 2 a 2: usar a mesma ideia com sapply dos plots
library(mclust)
#dataset1
# infl_out2a2_1 <- list()
# rm(infl_out2a2_1)
infl_out2a2_dt1 <- apply(comb2a2_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data1.df[,c(x[1], x[2])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset2
infl_out2a2_dt2 <- apply(comb2a2_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data2.df[,c(x[1], x[2])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset3
infl_out2a2_dt3 <- apply(comb2a2_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data3.df[,c(x[1], x[2])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset4
infl_out2a2_dt4 <- apply(comb2a2_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data4.df[,c(x[1], x[2])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

rbind(comb_nomes1_2a2, trunc(infl_out2a2_dt1))
rbind(comb_nomes2_2a2, trunc(infl_out2a2_dt2))
rbind(comb_nomes3_2a2, trunc(infl_out2a2_dt3))
rbind(comb_nomes4_2a2, trunc(infl_out2a2_dt4))

# NOTAR QUE AS COMBINAÇÕES COM A INCLUSÃO DA MASSA GARANTEM OS MAIORES icl'S nos datasets 1 e 2 !!


## b) todas as combinacoes 3 a 3:
# combinacoes para as colunas
comb3a3_col = combn(ncol(data1.df[,1:4]),3)

# para pegar e passar os nomes das variaveis:
nomes1 <- colnames(data1.df[,1:4])
nomes2 <- colnames(data2.df[,1:4])
nomes3 <- colnames(data3.df[,1:4])
nomes4 <- colnames(data4.df[,1:4])

# matriz a ser preenchida:
comb_nomes3a3 <- matrix(NA, nrow=3, ncol=ncol(comb3a3_col))

comb_nomes1_3a3 <- sapply(1:ncol(comb3a3_col),function(j){
  sapply(1:3,function(i){
    comb_nomes3a3[i,j]<-nomes1[comb3a3_col[i,j]]
  })
})
comb_nomes2_3a3 <- sapply(1:ncol(comb3a3_col),function(j){
  sapply(1:3,function(i){
    comb_nomes3a3[i,j]<-nomes2[comb3a3_col[i,j]]
  })
})
comb_nomes3_3a3 <- sapply(1:ncol(comb3a3_col),function(j){
  sapply(1:3,function(i){
    comb_nomes3a3[i,j]<-nomes3[comb3a3_col[i,j]]
  })
})
comb_nomes4_3a3 <- sapply(1:ncol(comb3a3_col),function(j){
  sapply(1:3,function(i){
    comb_nomes3a3[i,j]<-nomes4[comb3a3_col[i,j]]
  })
})

# GMM 3 a 3: usar a mesma ideia com sapply dos plots
library(mclust)
#dataset1
infl_out3a3_dt1 <- apply(comb3a3_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data1.df[,c(x[1], x[2], x[3])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset2
infl_out3a3_dt2 <- apply(comb3a3_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data2.df[,c(x[1], x[2], x[3])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset3
infl_out3a3_dt3 <- apply(comb3a3_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data3.df[,c(x[1], x[2], x[3])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset4
infl_out3a3_dt4 <- apply(comb3a3_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data4.df[,c(x[1], x[2], x[3])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

rbind(comb_nomes1_3a3, trunc(infl_out3a3_dt1))
rbind(comb_nomes2_3a3, trunc(infl_out3a3_dt2))
rbind(comb_nomes3_3a3, trunc(infl_out3a3_dt3))
rbind(comb_nomes4_3a3, trunc(infl_out3a3_dt4))

## c) todos os 4 parametros:
# combinacoes para as colunas
comb4a4_col = combn(ncol(data1.df[,1:4]),4)

# para pegar e passar os nomes das variaveis:
nomes1 <- colnames(data1.df[,1:4])
nomes2 <- colnames(data2.df[,1:4])
nomes3 <- colnames(data3.df[,1:4])
nomes4 <- colnames(data4.df[,1:4])

# matriz a ser preenchida:
comb_nomes4a4 <- matrix(NA, nrow=4, ncol=ncol(comb4a4_col))

comb_nomes1_4a4 <- sapply(1:ncol(comb4a4_col),function(j){
  sapply(1:4,function(i){
    comb_nomes4a4[i,j]<-nomes1[comb4a4_col[i,j]]
  })
})
comb_nomes2_4a4 <- sapply(1:ncol(comb4a4_col),function(j){
  sapply(1:4,function(i){
    comb_nomes4a4[i,j]<-nomes2[comb4a4_col[i,j]]
  })
})
comb_nomes3_4a4 <- sapply(1:ncol(comb4a4_col),function(j){
  sapply(1:4,function(i){
    comb_nomes4a4[i,j]<-nomes3[comb4a4_col[i,j]]
  })
})
comb_nomes4_4a4 <- sapply(1:ncol(comb4a4_col),function(j){
  sapply(1:4,function(i){
    comb_nomes4a4[i,j]<-nomes4[comb4a4_col[i,j]]
  })
})

# GMM com as 4 colunas:
library(mclust)
#dataset1
infl_out4a4_dt1 <- apply(comb4a4_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data1.df[,c(x[1], x[2], x[3], x[4])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset2
infl_out4a4_dt2 <- apply(comb4a4_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data2.df[,c(x[1], x[2], x[3], x[4])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset3
infl_out4a4_dt3 <- apply(comb4a4_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data3.df[,c(x[1], x[2], x[3], x[4])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset4
infl_out4a4_dt4 <- apply(comb4a4_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data4.df[,c(x[1], x[2], x[3], x[4])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

rbind(comb_nomes1_4a4, trunc(infl_out4a4_dt1))
rbind(comb_nomes2_4a4, trunc(infl_out4a4_dt2))
rbind(comb_nomes3_4a4, trunc(infl_out4a4_dt3))
rbind(comb_nomes4_4a4, trunc(infl_out4a4_dt4))


# ## GMMS JAH FORMA RODADOS
# ## geral -- saida do MClust:
# library(factoextra)
# p1_bic <- fviz_mclust_bic(out1)
# p2_bic <- fviz_mclust_bic(out2)
# p3_bic <- fviz_mclust_bic(out3)
# p4_bic <- fviz_mclust_bic(out4)
# 
# grid.arrange(p1_bic, p2_bic, p3_bic, p4_bic, ncol=2)
# 
# 
# bic = -BIC(out1,out2,out3,out4)[,2]
# ICL4 = c(icl(out1),icl(out2),icl(out3),icl(out4))
# method = rep(c("-BIC","ICL"), each=4)
# Dataset = paste0("dataset", 1:4)
# 
# df <- data.frame("value" = c(bic, ICL4), method, Dataset)
# str(df)
# 
# 
# 
# ### 2) BIC & ICL
# 
# ## geral -- saida do MClust:
# library(factoextra)
# p1_bic <- fviz_mclust_bic(out1)
# p2_bic <- fviz_mclust_bic(out2)
# p3_bic <- fviz_mclust_bic(out3)
# p4_bic <- fviz_mclust_bic(out4)
# 
# grid.arrange(p1_bic, p2_bic, p3_bic, p4_bic, ncol=2)
# 
# 
# bic = -BIC(out1,out2,out3,out4)[,2]
# ICL = c(icl(out1),icl(out2),icl(out3),icl(out4))
# method = rep(c("-BIC","ICL"), each=4)
# Dataset = paste0("dataset", 1:4)
# 
# df <- data.frame("value" = c(bic, ICL), method, Dataset)
# str(df)
# 
# x11()
# p_bic_icl <- ggplot(df, aes(x=Dataset, y=value, group=method))+
#   geom_line(aes(color=method), size=1.2)+
#   scale_color_manual(values=c('#E69F00', '#56B4E9'))+
#   ggtitle("BIC vs ICL (G=2)")+
#   theme(plot.title = element_text(hjust=0.5))
# p_bic_icl
# 
# savePlot("fig2.PNG", type = "png",
#          device = dev.cur())

### 3) R vs optico com probabilidade (z scores) pertencimento

# require(hexbin)

# dataset1
x11()
p1_p <- ggplot(data1.df, aes(Core, B_Band))+
  stat_summary_hex(aes(z = out1$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 1 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
p1_p
# dataset2
p2_p <- ggplot(data2.df, aes(Radio, B_Band))+
  stat_summary_hex(aes(z = out2$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 2 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
p2_p
# datset3
p3_p <- ggplot(data3.df, aes(VLA_6cm, I_Band))+
  stat_summary_hex(aes(z = out3$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 3 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
p3_p
# datset4
p4_p <- ggplot(data4.df, aes(Bmag, Sint))+
  stat_summary_hex(aes(z = out4$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 3 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
p4_p

## alterar eixos para o plot4 ? Bmag seria optico e deveria ir no eixo y?
x11()
grid.arrange(p1_p,p2_p,p3_p,p4_p, ncol=2)

savePlot("fig3.PNG", type = "png",
         device = dev.cur())


### 4) gráficos do item 1 com ellipses:

## plots 2 a 2
# dataset1
library(ggplot2)

#data1.df$cluster <- as.factor(data1.df$cluster)
pe1 <- sapply(1:length(comb), function(i){
  apply(comb_nomes1, 2, function(x){
    p[i] <- ggplot(data1.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data1.df[,x[1]], data1.df[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
pe1[[1]]
# dataset2
pe2 <- sapply(1:length(comb), function(i){
  apply(comb_nomes2, 2, function(x){
    p[i] <- ggplot(data2.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data2.df[,x[1]], data2.df[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset3
pe3 <- sapply(1:length(comb), function(i){
  apply(comb_nomes3, 2, function(x){
    p[i] <- ggplot(data3.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data3.df[,x[1]], data3.df[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset4
pe4 <- sapply(1:length(comb), function(i){
  apply(comb_nomes4, 2, function(x){
    p[i] <- ggplot(data4.df, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster), size = 1.2, shape=1)+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data4.df[,x[1]], data4.df[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

library(gridExtra)

x11()
grid.arrange(pe1[[1]],pe1[[2]],pe1[[3]],pe1[[4]],pe1[[5]],pe1[[6]],
             pe2[[1]],pe2[[2]],pe2[[3]],pe2[[4]],pe2[[5]],pe2[[6]],
             pe3[[1]],pe3[[2]],pe3[[3]],pe3[[4]],pe3[[5]],pe3[[6]],
             pe4[[1]],pe4[[2]],pe4[[3]],pe4[[4]],pe4[[5]],pe4[[6]], ncol=6,
             top = "Cluster G=2 vs R>1/R<1")

savePlot("fig4_elipses.PNG", type = "png",
         device = dev.cur())


### 5) Figura com divisão dos tipos (somente datasets 1 e 2) e R>1, R<1

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


savePlot("fig5.PNG", type = "png",
         device = dev.cur())

########################### FIM PARTE A)

#### B) ANÁLISE COM ELIMINAÇÃO DE OUTLIERS
### 1) GMM com todas as combinações de espaços de parâmetros (6C4 = 24):

## lendo todos os data sets
# data1.loc<-file.choose()
# data2.loc<-file.choose()
# data3.loc<-file.choose()
# data4.loc<-file.choose()
# 
# data1.df <- read.table(data1.loc, header=TRUE)
# data2.df <- read.table(data2.loc, header=TRUE)
# data3.df <- read.table(data3.loc, header=TRUE)
# data4.df <- read.table(data4.loc, header=TRUE)
# 
# ## criando fator R>1, R<1
# 
# # dataset1
# data1.df$R1 <- (data1.df$Rc_Index >= 1)
# data1.df$R1 <- ifelse(data1.df$R1 == TRUE, "R>1", "R<1")
# # dataset2
# data2.df$R1 <- (data2.df$R_Index >= 1)
# data2.df$R1 <- ifelse(data2.df$R1 == TRUE, "R>1", "R<1")
# # dataset3
# data3.df$R1 <- (data3.df$Ratio >= 1) # confirmar se eh essa coluna mesmo
# data3.df$R1 <- ifelse(data3.df$R1 == TRUE, "R>1", "R<1")
# # dataset 4 ---> nao hah radio
# 
# ## eliminando Speak do dataset4
# library(magrittr)
# data4.df <- data4.df %>% .[,-4]
# data4.df <- data4.df[,-4]
# 
# ## transformando Sint na escala log
# data4.df[,4] <- log(data4.df[,4])

### 1) eliminando os outliers
# OBS: do pto de vista estatístico, não é muito recomendável que se remova os outliers
#.. a não ser que se tenha certeza que se trata de erros de medição, etc
data_list <- list(data1.df[,1:4], data2.df[,1:4], data3.df[,1:4], data4.df[,1:4])

# out_lista <- lapply(1:length(data_list), function(i){
#   sapply(1:ncol(data1.df), function(j){
#     out<-quantile(data_list[[i]][,j],c(0.01,0.99))
#     data_clean <- data_list[[i]][data_list[[i]][,j] > out[1] & data_list[[i]][,j] <= out[2], ]
#   })
# })

# # elimino com base em todas as colunas primeiro
# data_clean.list <- lapply(data_list, function(x){
#   lapply(1:ncol(x), function(j){
#     out<-quantile(x[,j],c(0.01,0.99))
#     x <- x[x[,j] >= out[1] & x[,j] <= out[2],]  
#   })
# })
# #ok! 2 lapply's ao inves de sapply + lapply


# dataset1
data1_clean <- data1.df[,1:4]
for(j in 1:ncol(data1_clean)){
  quant<-quantile(data1_clean[,j],c(0.01,0.99))
  data1_clean <- data1_clean[data1_clean[,j] >= quant[1] & data1_clean[,j] <= quant[2],]
}

# dataset2
data2_clean <- data2.df[,1:4]
for(j in 1:ncol(data2_clean)){
  quant<-quantile(data2_clean[,j],c(0.01,0.99))
  data2_clean <- data2_clean[data2_clean[,j] >= quant[1] & data2_clean[,j] <= quant[2],]
}

# dataset3
data3_clean <- data3.df[,1:4]
for(j in 1:ncol(data3_clean)){
  quant<-quantile(data3_clean[,j],c(0.01,0.99))
  data3_clean <- data3_clean[data3_clean[,j] >= quant[1] & data3_clean[,j] <= quant[2],]
}

# dataset4
#j=2
data4_clean <- data4.df[,1:4]
for(j in 1:ncol(data4_clean)){
  quant<-quantile(data4_clean[,j],c(0.01,0.99))
  data4_clean <- data4_clean[data4_clean[,j] >= quant[1] & data4_clean[,j] <= quant[2],]
}

ggplot(data4_clean, aes(Rmag, Bmag))+
  geom_point()

data4_clean[25,]

## GMM com G=2 para todos
library(mclust)
# dataset1
set.seed(1984)
outc1 <- Mclust(data1_clean, G=2)
data1_clean$cluster <- as.factor(outc1$classification)
# dataset2
set.seed(1984)
outc2 <- Mclust(data2_clean, G=2)
data2_clean$cluster <- as.factor(outc2$classification)
# dataset3
set.seed(1984)
outc3 <- Mclust(data3_clean, G=2)
data3_clean$cluster <- as.factor(outc3$classification)
# dataset4
set.seed(1984)
outc4 <- Mclust(data4_clean, G=2)
data4_clean$cluster <- as.factor(outc4$classification)



## PREPARANDO OS PLOTS 2a2 (SEM ELIPSES)
## combinacoes para os plots 
comb = combn(ncol(data1_clean[,1:4]),2) # apenas a estrutura

# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes1 <- colnames(data1_clean[,1:4])
nomes2 <- colnames(data2_clean[,1:4])
nomes3 <- colnames(data3_clean[,1:4])
nomes4 <- colnames(data4_clean[,1:4])

#comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))
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


## criando fator R>1, R<1

# dataset1
data1_clean$R1 <- (data1_clean$Rc_Index >= 1)
data1_clean$R1 <- ifelse(data1_clean$R1 == TRUE, "R>1", "R<1")
# dataset2
data2_clean$R1 <- (data2_clean$R_Index >= 1)
data2_clean$R1 <- ifelse(data2_clean$R1 == TRUE, "R>1", "R<1")
# dataset3
data3_clean$R1 <- (data3_clean$Ratio >= 1) # confirmar se eh essa coluna mesmo
data3_clean$R1 <- ifelse(data3_clean$R1 == TRUE, "R>1", "R<1")
# dataset 4 ---> nao hah radio

## plots 2 a 2 (SEM ELIPSES)
library(ggplot2)
pc = list()
# dataset1
pc1 <- sapply(1:length(comb), function(i){
  apply(comb_nomes1, 2, function(x){
    pc[i] <- ggplot(data1_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
#p1[[1]]
# dataset2
pc2 <- sapply(1:length(comb), function(i){
  apply(comb_nomes2, 2, function(x){
    pc[i] <- ggplot(data2_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# pc2[[1]]
# dataset3
pc3 <- sapply(1:length(comb), function(i){
  apply(comb_nomes3, 2, function(x){
    pc[i] <- ggplot(data3_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# pc3[[1]]
# dataset4
pc4 <- sapply(1:length(comb), function(i){
  apply(comb_nomes4, 2, function(x){
    pc[i] <- ggplot(data4_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster), size = 1.2, shape=1)+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# pc4[[1]]
# obs: o warning diz respeito a como estamos preenchendo a lista que foi criada em branco

library(gridExtra)

# help(grid.arrange)

x11()
grid.arrange(pc1[[1]],pc1[[2]],pc1[[3]],pc1[[4]],pc1[[5]],pc1[[6]],
             pc2[[1]],pc2[[2]],pc2[[3]],pc2[[4]],pc2[[5]],pc2[[6]],
             pc3[[1]],pc3[[2]],pc3[[3]],pc3[[4]],pc3[[5]],pc3[[6]],
             pc4[[1]],pc4[[2]],pc4[[3]],pc4[[4]],pc4[[5]],pc4[[6]], ncol=6,
             top = "Cluster G=2 vs R>1/R<1")

## ao eliminar os outliers, melhora a visualização do ultimo dataset
#... No entanto, "pioram" a divisão dos grupos nos primeiros datasets

savePlot("fig1.PNG", type = "png",
         device = dev.cur())

### 2) QUAL DIMENSÃO APRESENTA MAIOR ICL ENTRE OS GRUPOS?

## a) todas as combinações 2 a 2:

# combinacoes para as colunas
comb2a2_col = combn(ncol(data1_clean[,1:4]),2)

# A IDEIA EH USAR O INDICE DO MENOR ICL PARA INDEXARMOS A COLUNA QUE NOS RETORNARÁ OS NOMES DAS VARIAVEIS
#.. e fazermos uma tabela
# para pegar e passar os nomes das variaveis:
nomes1 <- colnames(data1_clean[,1:4])
nomes2 <- colnames(data2_clean[,1:4])
nomes3 <- colnames(data3_clean[,1:4])
nomes4 <- colnames(data4_clean[,1:4])

# matriz a ser preenchida:
comb_nomes2a2 <- matrix(NA, nrow=2, ncol=ncol(comb2a2_col))


comb_nomes1_2a2 <- sapply(1:ncol(comb2a2_col),function(j){
  sapply(1:2,function(i){
    comb_nomes2a2[i,j]<-nomes1[comb2a2_col[i,j]]
  })
})
comb_nomes2_2a2 <- sapply(1:ncol(comb2a2_col),function(j){
  sapply(1:2,function(i){
    comb_nomes2a2[i,j]<-nomes2[comb2a2_col[i,j]]
  })
})
comb_nomes3_2a2 <- sapply(1:ncol(comb2a2_col),function(j){
  sapply(1:2,function(i){
    comb_nomes2a2[i,j]<-nomes3[comb2a2_col[i,j]]
  })
})
comb_nomes4_2a2 <- sapply(1:ncol(comb2a2_col),function(j){
  sapply(1:2,function(i){
    comb_nomes2a2[i,j]<-nomes4[comb2a2_col[i,j]]
  })
})


#comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb2a2_col))

## GMM 2 a 2: usar a mesma ideia com sapply dos plots
library(mclust)
#dataset1
# infl_out2a2_1 <- list()
# rm(infl_out2a2_1)
infl_out2a2_dt1 <- apply(comb2a2_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data1_clean[,c(x[1], x[2])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset2
infl_out2a2_dt2 <- apply(comb2a2_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data2_clean[,c(x[1], x[2])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset3
infl_out2a2_dt3 <- apply(comb2a2_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data3_clean[,c(x[1], x[2])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset4
infl_out2a2_dt4 <- apply(comb2a2_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data4_clean[,c(x[1], x[2])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

rbind(comb_nomes1_2a2, trunc(infl_out2a2_dt1))
rbind(comb_nomes2_2a2, trunc(infl_out2a2_dt2))
rbind(comb_nomes3_2a2, trunc(infl_out2a2_dt3))
rbind(comb_nomes4_2a2, trunc(infl_out2a2_dt4))

# NOTAR QUE AS COMBINAÇÕES COM A INCLUSÃO DA MASSA GARANTEM OS MAIORES icl'S nos datasets 1 e 2 !!


## b) todas as combinacoes 3 a 3:
# combinacoes para as colunas
comb3a3_col = combn(ncol(data1_clean[,1:4]),3)

# para pegar e passar os nomes das variaveis:
nomes1 <- colnames(data1_clean[,1:4])
nomes2 <- colnames(data2_clean[,1:4])
nomes3 <- colnames(data3_clean[,1:4])
nomes4 <- colnames(data4_clean[,1:4])

# matriz a ser preenchida:
comb_nomes3a3 <- matrix(NA, nrow=3, ncol=ncol(comb3a3_col))

comb_nomes1_3a3 <- sapply(1:ncol(comb3a3_col),function(j){
  sapply(1:3,function(i){
    comb_nomes3a3[i,j]<-nomes1[comb3a3_col[i,j]]
  })
})
comb_nomes2_3a3 <- sapply(1:ncol(comb3a3_col),function(j){
  sapply(1:3,function(i){
    comb_nomes3a3[i,j]<-nomes2[comb3a3_col[i,j]]
  })
})
comb_nomes3_3a3 <- sapply(1:ncol(comb3a3_col),function(j){
  sapply(1:3,function(i){
    comb_nomes3a3[i,j]<-nomes3[comb3a3_col[i,j]]
  })
})
comb_nomes4_3a3 <- sapply(1:ncol(comb3a3_col),function(j){
  sapply(1:3,function(i){
    comb_nomes3a3[i,j]<-nomes4[comb3a3_col[i,j]]
  })
})

# GMM 3 a 3: usar a mesma ideia com sapply dos plots
library(mclust)
#dataset1
infl_out3a3_dt1 <- apply(comb3a3_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data1_clean[,c(x[1], x[2], x[3])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset2
infl_out3a3_dt2 <- apply(comb3a3_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data2_clean[,c(x[1], x[2], x[3])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset3
infl_out3a3_dt3 <- apply(comb3a3_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data3_clean[,c(x[1], x[2], x[3])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset4
infl_out3a3_dt4 <- apply(comb3a3_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data4_clean[,c(x[1], x[2], x[3])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

rbind(comb_nomes1_3a3, trunc(infl_out3a3_dt1))
rbind(comb_nomes2_3a3, trunc(infl_out3a3_dt2))
rbind(comb_nomes3_3a3, trunc(infl_out3a3_dt3))
rbind(comb_nomes4_3a3, trunc(infl_out3a3_dt4))

## c) todos os 4 parametros:
# combinacoes para as colunas
comb4a4_col = combn(ncol(data1_clean[,1:4]),4)

# para pegar e passar os nomes das variaveis:
nomes1 <- colnames(data1_clean[,1:4])
nomes2 <- colnames(data2_clean[,1:4])
nomes3 <- colnames(data3_clean[,1:4])
nomes4 <- colnames(data4_clean[,1:4])

# matriz a ser preenchida:
comb_nomes4a4 <- matrix(NA, nrow=4, ncol=ncol(comb4a4_col))

comb_nomes1_4a4 <- sapply(1:ncol(comb4a4_col),function(j){
  sapply(1:4,function(i){
    comb_nomes4a4[i,j]<-nomes1[comb4a4_col[i,j]]
  })
})
comb_nomes2_4a4 <- sapply(1:ncol(comb4a4_col),function(j){
  sapply(1:4,function(i){
    comb_nomes4a4[i,j]<-nomes2[comb4a4_col[i,j]]
  })
})
comb_nomes3_4a4 <- sapply(1:ncol(comb4a4_col),function(j){
  sapply(1:4,function(i){
    comb_nomes4a4[i,j]<-nomes3[comb4a4_col[i,j]]
  })
})
comb_nomes4_4a4 <- sapply(1:ncol(comb4a4_col),function(j){
  sapply(1:4,function(i){
    comb_nomes4a4[i,j]<-nomes4[comb4a4_col[i,j]]
  })
})

# GMM com as 4 colunas:
library(mclust)
#dataset1
infl_out4a4_dt1 <- apply(comb4a4_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data1_clean[,c(x[1], x[2], x[3], x[4])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset2
infl_out4a4_dt2 <- apply(comb4a4_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data2_clean[,c(x[1], x[2], x[3], x[4])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset3
infl_out4a4_dt3 <- apply(comb4a4_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data3_clean[,c(x[1], x[2], x[3], x[4])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

# dataset4
infl_out4a4_dt4 <- apply(comb4a4_col, 2, function(x){
  set.seed(1984)
  out <- Mclust(data4_clean[,c(x[1], x[2], x[3], x[4])], G=2)
  icl(out)
  #data1.df$cluster <- as.factor(out1$classification)
})

rbind(comb_nomes1_4a4, trunc(infl_out4a4_dt1))
rbind(comb_nomes2_4a4, trunc(infl_out4a4_dt2))
rbind(comb_nomes3_4a4, trunc(infl_out4a4_dt3))
rbind(comb_nomes4_4a4, trunc(infl_out4a4_dt4))


# ## GMMS JAH FORMA RODADOS
# ## geral -- saida do MClust:
# library(factoextra)
# p1_bic <- fviz_mclust_bic(out1)
# p2_bic <- fviz_mclust_bic(out2)
# p3_bic <- fviz_mclust_bic(out3)
# p4_bic <- fviz_mclust_bic(out4)
# 
# grid.arrange(p1_bic, p2_bic, p3_bic, p4_bic, ncol=2)
# 
# 
# bic = -BIC(out1,out2,out3,out4)[,2]
# ICL4 = c(icl(out1),icl(out2),icl(out3),icl(out4))
# method = rep(c("-BIC","ICL"), each=4)
# Dataset = paste0("dataset", 1:4)
# 
# df <- data.frame("value" = c(bic, ICL4), method, Dataset)
# str(df)
# 
# 
# 
# ### 2) BIC & ICL
# 
# ## geral -- saida do MClust:
# library(factoextra)
# p1_bic <- fviz_mclust_bic(out1)
# p2_bic <- fviz_mclust_bic(out2)
# p3_bic <- fviz_mclust_bic(out3)
# p4_bic <- fviz_mclust_bic(out4)
# 
# grid.arrange(p1_bic, p2_bic, p3_bic, p4_bic, ncol=2)
# 
# 
# bic = -BIC(out1,out2,out3,out4)[,2]
# ICL = c(icl(out1),icl(out2),icl(out3),icl(out4))
# method = rep(c("-BIC","ICL"), each=4)
# Dataset = paste0("dataset", 1:4)
# 
# df <- data.frame("value" = c(bic, ICL), method, Dataset)
# str(df)
# 
# x11()
# p_bic_icl <- ggplot(df, aes(x=Dataset, y=value, group=method))+
#   geom_line(aes(color=method), size=1.2)+
#   scale_color_manual(values=c('#E69F00', '#56B4E9'))+
#   ggtitle("BIC vs ICL (G=2)")+
#   theme(plot.title = element_text(hjust=0.5))
# p_bic_icl
# 
# savePlot("fig2.PNG", type = "png",
#          device = dev.cur())

### 3) R vs optico com probabilidade (z scores) pertencimento

# require(hexbin)

# dataset1
# x11()
p1_p <- ggplot(data1_clean, aes(Core, B_Band))+
  stat_summary_hex(aes(z = outc1$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 1 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
p1_p
# dataset2
p2_p <- ggplot(data2_clean, aes(Radio, B_Band))+
  stat_summary_hex(aes(z = outc2$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 2 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
p2_p
# datset3
p3_p <- ggplot(data3_clean, aes(VLA_6cm, I_Band))+
  stat_summary_hex(aes(z = outc3$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 3 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
p3_p
# datset4
p4_p <- ggplot(data4_clean, aes(Bmag, Sint))+
  stat_summary_hex(aes(z = outc4$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 3 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
p4_p

## alterar eixos para o plot4 ? Bmag seria optico e deveria ir no eixo y?
x11()
grid.arrange(p1_p,p2_p,p3_p,p4_p, ncol=2)

savePlot("fig3.PNG", type = "png",
         device = dev.cur())


### 4) gráficos do item 1 com ellipses:

## plots 2 a 2
# dataset1
library(ggplot2)

#data1.df$cluster <- as.factor(data1.df$cluster)
pe1 <- sapply(1:length(comb), function(i){
  apply(comb_nomes1, 2, function(x){
    p[i] <- ggplot(data1_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data1_clean[,x[1]], data1_clean[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
pe1[[1]]
# dataset2
pe2 <- sapply(1:length(comb), function(i){
  apply(comb_nomes2, 2, function(x){
    p[i] <- ggplot(data2_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data2_clean[,x[1]], data2_clean[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset3
pe3 <- sapply(1:length(comb), function(i){
  apply(comb_nomes3, 2, function(x){
    p[i] <- ggplot(data3_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data3_clean[,x[1]], data3_clean[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset4
pe4 <- sapply(1:length(comb), function(i){
  apply(comb_nomes4, 2, function(x){
    p[i] <- ggplot(data4_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster), size = 1.2, shape=1)+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data4_clean[,x[1]], data4_clean[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

library(gridExtra)

x11()
grid.arrange(pe1[[1]],pe1[[2]],pe1[[3]],pe1[[4]],pe1[[5]],pe1[[6]],
             pe2[[1]],pe2[[2]],pe2[[3]],pe2[[4]],pe2[[5]],pe2[[6]],
             pe3[[1]],pe3[[2]],pe3[[3]],pe3[[4]],pe3[[5]],pe3[[6]],
             pe4[[1]],pe4[[2]],pe4[[3]],pe4[[4]],pe4[[5]],pe4[[6]], ncol=6,
             top = "Cluster G=2 vs R>1/R<1")

savePlot("fig4_elipses.PNG", type = "png",
         device = dev.cur())


### 5) Figura com divisão dos tipos (somente datasets 1 e 2) e R>1, R<1

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


savePlot("fig5.PNG", type = "png",
         device = dev.cur())

### FIM