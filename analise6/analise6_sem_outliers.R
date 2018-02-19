################ ANALISE 6 (19-FEV-2018)
#### B) ANÁLISE COM ELIMINAÇÃO DE OUTLIERS
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

## eliminando Speak do dataset4
library(magrittr)
data4.df <- data4.df %>% .[,-4]
#data4.df <- data4.df[,-4]

## transformando Sint na escala log
data4.df[,4] <- log(data4.df[,4])

### 1) eliminando os outliers
# OBS: do pto de vista estatístico, não é muito recomendável que se remova os outliers
#.. a não ser que se tenha certeza que se trata de erros de medição, etc

# vamos eliminar todos os pontos em todas as colunas que ultrapassem o percentil 99% e fique aquem do peercentil 1%
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

savePlot("fig1_parte2.PNG", type = "png",
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


#sapply(comb_nomes1_2a2, function(x) Reduce(paste, x))

help(data.frame)
#dataset1
icl.df1 <- data.frame(matrix(NA, ncol=6, nrow=1))
colnames(icl.df1) <- apply(comb_nomes1_2a2, 2, function(x) paste(x[1], x[2], sep=".vs.") )
icl.df1[1,] <- trunc(infl_out2a2_dt1)
#dataset2
icl.df2 <- data.frame(matrix(NA, ncol=6, nrow=1))
colnames(icl.df2) <- apply(comb_nomes2_2a2, 2, function(x) paste(x[1], x[2], sep=".vs.") )
icl.df2[1,] <- trunc(infl_out2a2_dt2)
#dataset3
icl.df3 <- data.frame(matrix(NA, ncol=6, nrow=1))
colnames(icl.df3) <- apply(comb_nomes3_2a2, 2, function(x) paste(x[1], x[2], sep=".vs.") )
icl.df3[1,] <- trunc(infl_out2a2_dt3)
#dataset4
icl.df4 <- data.frame(matrix(NA, ncol=6, nrow=1))
colnames(icl.df4) <- apply(comb_nomes4_2a2, 2, function(x) paste(x[1], x[2], sep=".vs.") )
icl.df4[1,] <- trunc(infl_out2a2_dt4)

# NOTAR QUE AS COMBINAÇÕES COM A INCLUSÃO DA MASSA GARANTEM OS MAIORES icl'S nos datasets 1 e 2 !!

# juantando os 3 a 3:
#sapply(comb_nomes1_2a2, function(x) Reduce(paste, x))

## AINDA NÃO TRANSPOR! SÓ DEPOIS DA INCLUSÃO DA 4A4

help(data.frame)
#dataset1
#t(icl.df1)
icl.df1 <- as.data.frame(c(icl.df1, trunc(infl_out3a3_dt1)))
colnames(icl.df1)[7:10] <- apply(comb_nomes1_3a3, 2, function(x) paste(x[1], x[2], x[3], sep=".vs.") )
# icl.df1 <- t(icl.df1)
# colnames(icl.df1) <- "ICL"
#dataset2
icl.df2 <- as.data.frame(c(icl.df2, trunc(infl_out3a3_dt2)))
colnames(icl.df2)[7:10] <- apply(comb_nomes2_3a3, 2, function(x) paste(x[1], x[2], x[3], sep=".vs.") )
# icl.df2 <- t(icl.df2)
# colnames(icl.df2) <- "ICL"
#dataset3
icl.df3 <- as.data.frame(c(icl.df3, trunc(infl_out3a3_dt3)))
colnames(icl.df3)[7:10] <- apply(comb_nomes3_3a3, 2, function(x) paste(x[1], x[2], x[3], sep=".vs.") )
# icl.df3 <- t(icl.df3)
# colnames(icl.df3) <- "ICL"
#dataset4
icl.df4 <- as.data.frame(c(icl.df4, trunc(infl_out3a3_dt4)))
colnames(icl.df4)[7:10] <- apply(comb_nomes4_3a3, 2, function(x) paste(x[1], x[2], x[3], sep=".vs.") )
# icl.df4 <- t(icl.df4)
# colnames(icl.df4) <- "ICL"



# juntando os full
#dataset1
#t(icl.df1)
icl.df1 <- as.data.frame(c(icl.df1, trunc(infl_out4a4_dt1)))
colnames(icl.df1)[11] <- "full"
icl.df1 <- t(icl.df1)
colnames(icl.df1) <- "ICL"
icl.df1 <- data.frame(icl.df1)
#dataset2
icl.df2 <- as.data.frame(c(icl.df2, trunc(infl_out4a4_dt2)))
colnames(icl.df2)[11] <- "full"
icl.df2 <- t(icl.df2)
colnames(icl.df2) <- "ICL"
icl.df2 <- data.frame(icl.df2)
#dataset3
icl.df3 <- as.data.frame(c(icl.df3, trunc(infl_out4a4_dt3)))
colnames(icl.df3)[11] <- "full"
icl.df3 <- t(icl.df3)
colnames(icl.df3) <- "ICL"
icl.df3 <- data.frame(icl.df3)
#dataset4
icl.df4 <- as.data.frame(c(icl.df4, trunc(infl_out4a4_dt4)))
colnames(icl.df4)[11] <- "full"
icl.df4 <- t(icl.df4)
colnames(icl.df4) <- "ICL"
icl.df4 <- data.frame(icl.df4)
## gráficos para evolução dos ICL's:

# p1_icl <- ggplot() + geom_line(aes(y = ICL, x = rownames(icl.df1), group = 1), size=1.5,
#                            data = icl.df1, stat="identity")
# p1_icl

# para ggplot parar de ajustar automaticamente meus fatores:
# sol em: https://groups.google.com/forum/#!topic/ggplot2/CpmvClYT3Hw
icl.df1$id.ordered <- factor(rownames(icl.df1), levels=rownames(icl.df1))
icl.df2$id.ordered <- factor(rownames(icl.df2), levels=rownames(icl.df2))
icl.df3$id.ordered <- factor(rownames(icl.df3), levels=rownames(icl.df3))
icl.df4$id.ordered <- factor(rownames(icl.df4), levels=rownames(icl.df4))

pc1_icl <- ggplot(icl.df1)+
  geom_line( aes(x=id.ordered, y=ICL, group=1), size=0.7, color='#E69F00')+
  #scale_color_manual(values=c('#E69F00', '#56B4E9'))+
  xlab("Variables Combinations")+
  ggtitle("Influence of variables on ICL - Dataset 1")+
  theme(plot.title = element_text(hjust=0.5))

# x11()
# p1_icl

pc2_icl <- ggplot(icl.df2)+
  geom_line( aes(x=id.ordered, y=ICL, group=1), size=0.7, color='#56B4E9')+
  #scale_color_manual(values=c('#E69F00', '#56B4E9'))+
  xlab("Variables Combinations")+
  ggtitle("Influence of variables on ICL - Dataset 2")+
  theme(plot.title = element_text(hjust=0.5))

pc3_icl <- ggplot(icl.df3)+
  geom_line( aes(x=id.ordered, y=ICL, group=1), size=0.7, color='#E69F00')+
  #scale_color_manual(values=c('#E69F00', '#56B4E9'))+
  xlab("Variables Combinations")+
  ggtitle("Influence of variables on ICL - Dataset 3")+
  theme(plot.title = element_text(hjust=0.5))

pc4_icl <- ggplot(icl.df4)+
  geom_line( aes(x=id.ordered, y=ICL, group=1), size=0.7, color='#56B4E9')+
  #scale_color_manual(values=c('#E69F00', '#56B4E9'))+
  xlab("Variables Combinations")+
  ggtitle("Influence of variables on ICL - Dataset 4")+
  theme(plot.title = element_text(hjust=0.5))

library(gridExtra)

x11()
grid.arrange(pc1_icl, pc2_icl, nrow=2)
savePlot("fig3a_parte2.PNG", type = "png",
         device = dev.cur())
x11()
grid.arrange(pc3_icl, pc4_icl, nrow=2)
savePlot("fig3b_parte2.PNG", type = "png",
         device = dev.cur())
## colocar os gráficos juntos com as tabelas no relatório!!


# savePlot("fig2.PNG", type = "png",
#          device = dev.cur())

### 3) R vs optico com probabilidade (z scores) pertencimento

# require(hexbin)

# dataset1
# x11()
pc1_p <- ggplot(data1_clean, aes(Core, B_Band))+
  stat_summary_hex(aes(z = outc1$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 1 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
#pc1_p
# dataset2
pc2_p <- ggplot(data2_clean, aes(Radio, B_Band))+
  stat_summary_hex(aes(z = outc2$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 2 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
#pc2_p
# datset3
pc3_p <- ggplot(data3_clean, aes(VLA_6cm, I_Band))+
  stat_summary_hex(aes(z = outc3$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 3 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
pc3_p
# datset4
pc4_p <- ggplot(data4_clean, aes(Bmag, Sint))+
  stat_summary_hex(aes(z = outc4$z[,1]))+
  scale_fill_gradientn(colours = c("yellow", "blue"))+
  ggtitle("Dataset 3 - Probability of belonging to group 1")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
pc4_p

## alterar eixos para o plot4 ? Bmag seria optico e deveria ir no eixo y?
x11()
grid.arrange(pc1_p,pc2_p,pc3_p,pc4_p, ncol=2)

savePlot("fig4_parte2.PNG", type = "png",
         device = dev.cur())


### 4) gráficos do item 1 com ellipses:

## plots 2 a 2
# dataset1
library(ggplot2)

#data1.df$cluster <- as.factor(data1.df$cluster)
pce1 <- sapply(1:length(comb), function(i){
  apply(comb_nomes1, 2, function(x){
    pc[i] <- ggplot(data1_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data1_clean[,x[1]], data1_clean[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset2
pce2 <- sapply(1:length(comb), function(i){
  apply(comb_nomes2, 2, function(x){
    pc[i] <- ggplot(data2_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data2_clean[,x[1]], data2_clean[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset3
pce3 <- sapply(1:length(comb), function(i){
  apply(comb_nomes3, 2, function(x){
    pc[i] <- ggplot(data3_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster, shape=R1), size = 1.2)+
      scale_shape_manual(values=c(1, 3))+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data3_clean[,x[1]], data3_clean[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})
# dataset4
pce4 <- sapply(1:length(comb), function(i){
  apply(comb_nomes4, 2, function(x){
    pc[i] <- ggplot(data4_clean, aes_string(x[1], x[2]))+
      geom_point(aes(colour = cluster), size = 1.2, shape=1)+
      scale_color_manual(values=c('#E69F00', '#56B4E9'))+
      stat_ellipse(aes(data4_clean[,x[1]], data4_clean[,x[2]], color=cluster), type="norm" )+
      #ggtitle("Comparação R>1 e R<1")+
      theme(plot.title = element_text(hjust=0.5))
  })  
})

library(gridExtra)

x11()
grid.arrange(pce1[[1]],pce1[[2]],pce1[[3]],pce1[[4]],pce1[[5]],pce1[[6]],
             pce2[[1]],pce2[[2]],pce2[[3]],pce2[[4]],pce2[[5]],pce2[[6]],
             pce3[[1]],pce3[[2]],pce3[[3]],pce3[[4]],pce3[[5]],pce3[[6]],
             pce4[[1]],pce4[[2]],pce4[[3]],pce4[[4]],pce4[[5]],pce4[[6]], ncol=6,
             top = "Cluster G=2 vs R>1/R<1")

savePlot("fig2_parte2.PNG", type = "png",
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


############## usando DBSCAN:

library(fpc)

x <- as.matrix(sikora.df[,c(3,1)])

# teste 1:

# dataset1:
set.seed(123)
set.seed(1984)
# para epsilon fazer alguma padronização -- utilizando a escala
# ou range
# para MinPts, usar 10% do numero total de pontos?
out1.db <- fpc::dbscan(data1_clean[,1:4], eps = 1, MinPts = 5)
library("factoextra")
fviz_cluster(out1.db, data = data1_clean[,1:2], stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

#dataset2:
set.seed(1984)
out2.db <- fpc::dbscan(data2_clean[,1:4], eps = 1, MinPts = 5)

#dataset3:
set.seed(1984)
out3.db <- fpc::dbscan(data3_clean[,1:4], eps = 1, MinPts = 5)

#dataset4:
set.seed(1984)
out4.db <- fpc::dbscan(data4_clean[,1:4], eps = 1, MinPts = 5)


## plots 2 a 2 (SEM ELIPSES)
library(factoextra)
p = list()
# dataset1
pc1 <- sapply(1:length(comb), function(i){
  apply(comb_nomes1, 2, function(x){
    p[i] <- fviz_cluster(out1.db, data = data1_clean, choose.vars = c(x[1], x[2]),  stand = FALSE,
                         ellipse = TRUE, show.clust.cent = TRUE,
                         geom = "point",palette = "jco", ggtheme = theme_classic())
  })  
})

#help("fviz_cluster")
#p1[[3]]
# dataset2
pc2 <- sapply(1:length(comb), function(i){
  apply(comb_nomes2, 2, function(x){
    p[i] <- fviz_cluster(out2.db, data = data2_clean, choose.vars = c(x[1], x[2]),  stand = FALSE,
                         ellipse = TRUE, show.clust.cent = TRUE,
                         geom = "point",palette = "jco", ggtheme = theme_classic())
  })  
})
# dataset3
pc3 <- sapply(1:length(comb), function(i){
  apply(comb_nomes3, 2, function(x){
    p[i] <- fviz_cluster(out3.db, data = data3_clean, choose.vars = c(x[1], x[2]),  stand = FALSE,
                         ellipse = TRUE, show.clust.cent = TRUE,
                         geom = "point",palette = "jco", ggtheme = theme_classic())
  })  
})
# dataset4
pc4 <- sapply(1:length(comb), function(i){
  apply(comb_nomes4, 2, function(x){
    p[i] <- fviz_cluster(out4.db, data = data4_clean, choose.vars = c(x[1], x[2]),  stand = FALSE,
                         ellipse = TRUE, show.clust.cent = TRUE,
                         geom = "point",palette = "jco", ggtheme = theme_classic())
  })  
})
# obs: o warning diz respeito a como estamos preenchendo a lista que foi criada em branco

library(gridExtra)

#help(grid.arrange)

x11()
grid.arrange(pc1[[1]],pc1[[2]],pc1[[3]],pc1[[4]],pc1[[5]],pc1[[6]],
             pc2[[1]],pc2[[2]],pc2[[3]],pc2[[4]],pc2[[5]],pc2[[6]],
             pc3[[1]],pc3[[2]],pc3[[3]],pc3[[4]],pc3[[5]],pc3[[6]],
             pc4[[1]],pc4[[2]],pc4[[3]],pc4[[4]],pc4[[5]],pc4[[6]], ncol=6,
             top = "Cluster G=2 vs R>1/R<1")

savePlot("fig-dbscan_sem_outliers_nos_dados.PNG", type = "png",
         device = dev.cur())

