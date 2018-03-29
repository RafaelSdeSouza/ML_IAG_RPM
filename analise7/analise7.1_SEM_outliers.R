################ ANALISE 7 (26-MAR-2018)
#rm(list=ls())

# setwd('/home/allan/Documents/astro/MLG_IAG_USP/analise7')

# -------------------------- SEM ELIMINAR OUTLIERS ---------------------

#------------- GMM

## lendo os data sets
data1.loc<-file.choose()
data2.loc<-file.choose()
data3.loc<-file.choose()
data4.loc<-file.choose()

data1.df <- read.table(data1.loc, header=TRUE)
data2.df <- read.table(data2.loc, header=TRUE)
data3.df <- read.table(data3.loc, header=TRUE)
data4.df <- read.table(data4.loc, header=TRUE)

## aplicando log para as variaveis do dataset 3:
data3.df <- as.data.frame(sapply(data3.df, log))

## eliminando os zeros e-Inf do dataset3:
row_sub <- apply(data3.df, 1, function(row) all( row != 0 && row != -Inf))
data3.df <- data3.df[row_sub, ]

## eliminando Speak do dataset4
library(magrittr)
data4.df <- data4.df %>% .[,-4]
#data4.df <- data4.df[,-4]

## transformando Sint na escala log
data4.df[,4] <- log(data4.df[,4])


## eliminando os outliers
# vamos eliminar todos os pontos em todas as colunas que ultrapassem o percentil 95% e fique aquem do peercentil 5%
# dataset1
data1.df <- data1.df[,1:4]
for(j in 1:ncol(data1.df)){
  quant<-quantile(data1.df[,j],c(0.05,0.95))
  data1.df <- data1.df[data1.df[,j] >= quant[1] & data1.df[,j] <= quant[2],]
}

# dataset2
data2.df <- data2.df[,1:4]
for(j in 1:ncol(data2.df)){
  quant<-quantile(data2.df[,j],c(0.05,0.95))
  data2.df <- data2.df[data2.df[,j] >= quant[1] & data2.df[,j] <= quant[2],]
}

# dataset3
data3.df <- data3.df[,1:4]
for(j in 1:ncol(data3.df)){
  quant<-quantile(data3.df[,j],c(0.05,0.95))
  data3.df <- data3.df[data3.df[,j] >= quant[1] & data3.df[,j] <= quant[2],]
}

# dataset4
#j=2
data4.df <- data4.df[,1:4]
for(j in 1:ncol(data4.df)){
  quant<-quantile(data4.df[,j],c(0.05,0.95))
  data4.df <- data4.df[data4.df[,j] >= quant[1] & data4.df[,j] <= quant[2],]
}


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


## calculo do nro de observacoes em cada grupo:

tab_cluster_dt1 <- data.frame(table(data1.df$cluster))
colnames(tab_cluster_dt1)[1] <- "Cluster"

tab_cluster_dt2 <- as.data.frame(table(data2.df$cluster))
colnames(tab_cluster_dt2)[1] <- "Cluster"

tab_cluster_dt3 <- as.data.frame(table(data3.df$cluster))
colnames(tab_cluster_dt3)[1] <- "Cluster"

tab_cluster_dt4 <- as.data.frame(table(data4.df$cluster))
colnames(tab_cluster_dt4)[1] <- "Cluster"


## icl de cada resultado com todas as variaveis:

icl_df <- data.frame(ICL = c(icl(out1), icl(out2), icl(out3), icl(out4)))
rownames(icl_df) <- paste0("Dataset", 1:4)


# ------------- preparacao PLOTS 2 a 2 (SEM ELIPSES)
## estrutura de combinacoes para os plots 
comb = combn(ncol(data1.df[,1:4]),2) # apenas a estrutura

# para pegar e passar os nomes das variaveis corretamente para o ggplot
nomes1 <- colnames(data1.df[,1:4])
nomes2 <- colnames(data2.df[,1:4])
nomes3 <- colnames(data3.df[,1:4])
nomes4 <- colnames(data4.df[,1:4])

# estrutura
comb_nomes <- matrix(NA, nrow=2, ncol=ncol(comb))
# p/ dataset1
comb_nomes1 <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes1[comb[i,j]]
  })
})
# p/ dataset2
comb_nomes2 <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes2[comb[i,j]]
  })
})
# p/ dataset3
comb_nomes3 <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes3[comb[i,j]]
  })
})
# p/ dataset4
comb_nomes4 <- sapply(1:ncol(comb),function(j){
  sapply(1:2,function(i){
    comb_nomes[i,j]<-nomes4[comb[i,j]]
  })
})

## plots 2 a 2 (SEM ELIPSES)
library(ggplot2)
# criando a estutura de lista para receber todos os plots
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
# obs: o warning diz respeito a como estamos preenchendo a lista que foi criada em branco

library(gridExtra)
#x11()
# grid.arrange(p1[[1]],p1[[2]],p1[[3]],p1[[4]],p1[[5]],p1[[6]],
#              p2[[1]],p2[[2]],p2[[3]],p2[[4]],p2[[5]],p2[[6]],
#              p3[[1]],p3[[2]],p3[[3]],p3[[4]],p3[[5]],p3[[6]],
#              p4[[1]],p4[[2]],p4[[3]],p4[[4]],p4[[5]],p4[[6]], ncol=6,
#              top = "Cluster G=2 vs R>1/R<1")

# savePlot("fig1_GMM_2a2_SEM_outliers.PNG", type = "png",
#          device = dev.cur())

### 2) gráficos do item 1 com ellipses:

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

#x11()
# grid.arrange(pe1[[1]],pe1[[2]],pe1[[3]],pe1[[4]],pe1[[5]],pe1[[6]],
#              pe2[[1]],pe2[[2]],pe2[[3]],pe2[[4]],pe2[[5]],pe2[[6]],
#              pe3[[1]],pe3[[2]],pe3[[3]],pe3[[4]],pe3[[5]],pe3[[6]],
#              pe4[[1]],pe4[[2]],pe4[[3]],pe4[[4]],pe4[[5]],pe4[[6]], ncol=6,
#              top = "Cluster G=2 vs R>1/R<1")

# savePlot("fig2_GMM_2a2_elipses_SEM_outliers.PNG", type = "png",
#          device = dev.cur())


#------------- Comparações ICL

## todas as combinacoes 3 a 3:
# estrutura de combinacoes para as colunas
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
  #set.seed(1984)
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

## todos os 4 parametros:
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


## criando os data.frames a partir das combinacoes 3a3 e modelos completos:
#dataset1
icl.df1 <- as.data.frame(c(signif(infl_out4a4_dt1, 5), signif(infl_out3a3_dt1, 5)))
rownames(icl.df1)[2:5] <- apply(comb_nomes1_3a3, 2, function(x) paste(x[1], x[2], x[3], sep=".vs.") )
rownames(icl.df1)[1] <- "Full"
colnames(icl.df1) <- "ICL"

#dataset2
icl.df2 <- as.data.frame(c(signif(infl_out4a4_dt2, 5), signif(infl_out3a3_dt2, 5)))
rownames(icl.df2)[2:5] <- apply(comb_nomes2_3a3, 2, function(x) paste(x[1], x[2], x[3], sep=".vs.") )
rownames(icl.df2)[1] <- "Full"
colnames(icl.df2) <- "ICL"

#dataset3
icl.df3 <- as.data.frame(c(signif(infl_out4a4_dt3, 5), signif(infl_out3a3_dt3, 5)))
rownames(icl.df3)[2:5] <- apply(comb_nomes3_3a3, 2, function(x) paste(x[1], x[2], x[3], sep=".vs.") )
rownames(icl.df3)[1] <- "Full"
colnames(icl.df3) <- "ICL"

#dataset4
icl.df4 <- as.data.frame(c(signif(infl_out4a4_dt4, 5), signif(infl_out3a3_dt4, 5)))
rownames(icl.df4)[2:5] <- apply(comb_nomes4_3a3, 2, function(x) paste(x[1], x[2], x[3], sep=".vs.") )
rownames(icl.df4)[1] <- "Full"
colnames(icl.df4) <- "ICL"


## gráficos para evolução dos ICL's:

# para ggplot parar de ajustar automaticamente meus fatores:
# sol em: https://groups.google.com/forum/#!topic/ggplot2/CpmvClYT3Hw
icl.df1$id.ordered <- factor(rownames(icl.df1), levels=rownames(icl.df1))
icl.df2$id.ordered <- factor(rownames(icl.df2), levels=rownames(icl.df2))
icl.df3$id.ordered <- factor(rownames(icl.df3), levels=rownames(icl.df3))
icl.df4$id.ordered <- factor(rownames(icl.df4), levels=rownames(icl.df4))


# dataset1

# calculos da variacao %:
var_icl_df1 <- trunc( c( -(icl.df1[2,"ICL"] - icl.df1[1,"ICL"])/icl.df1[1,"ICL"],
                         -(icl.df1[3,"ICL"] - icl.df1[1,"ICL"])/icl.df1[1,"ICL"],
                         -(icl.df1[4,"ICL"] - icl.df1[1,"ICL"])/icl.df1[1,"ICL"],
                         -(icl.df1[5,"ICL"] - icl.df1[1,"ICL"])/icl.df1[1,"ICL"])*10^3)/10^3
var_icl_df1 <- paste0(var_icl_df1*100,'%')
# sol para truncar corretamente em: https://stackoverflow.com/questions/23158165/truncate-decimal-to-specified-places

library(ggplot2)

p1_icl_new <- 
  icl.df1 %>%
  ggplot(aes(x = id.ordered, y = ICL)) +
  geom_point(size = 2) +
  geom_hline(aes(yintercept = ICL[1]), color = '#56B4E9', size = 2, alpha=0.5) +
  geom_segment(aes(xend = id.ordered, yend = ICL[1]),
               size = 1, color = '#E69F00', linetype = "dashed", alpha=0.5) +
  scale_x_discrete(name = "Variables in the GMM Model") +
  scale_y_continuous(name = "ICL")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  ggtitle("Dataset 1")+
  theme(plot.title = element_text(hjust=0.5))+
  annotate("text", x=c(1.8, 2.8, 3.8, 4.8), y=c(-800, -400, rep(-800,2)),
           label=c(var_icl_df1))

# dataset2

# calculos da variacao %:
var_icl_df2 <- trunc( c( -(icl.df2[2,"ICL"] - icl.df2[1,"ICL"])/icl.df2[1,"ICL"],
                         -(icl.df2[3,"ICL"] - icl.df2[1,"ICL"])/icl.df2[1,"ICL"],
                         -(icl.df2[4,"ICL"] - icl.df2[1,"ICL"])/icl.df2[1,"ICL"],
                         -(icl.df2[5,"ICL"] - icl.df2[1,"ICL"])/icl.df2[1,"ICL"]) *10^3)/10^3
var_icl_df2 <- paste0(var_icl_df2*100,'%')

p2_icl_new <-
  icl.df2 %>%
  ggplot(aes(x = id.ordered, y = ICL)) +
  geom_point(size = 2) +
  geom_hline(aes(yintercept = ICL[1]), color = '#56B4E9', size = 2, alpha=0.5) +
  geom_segment(aes(xend = id.ordered, yend = ICL[1]),
               size = 1, color = '#E69F00', linetype = "dashed", alpha=0.5) +
  scale_x_discrete(name = "Variables in the GMM Model") +
  scale_y_continuous(name = "ICL")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  ggtitle("Dataset 2")+
  theme(plot.title = element_text(hjust=0.5))+
  annotate("text", x=c(1.8, 2.8, 3.8, 4.8), y=c(-800, -400, rep(-800,2)),
           label=var_icl_df2)
# https://stackoverflow.com/questions/35618260/removing-legend-ggplot-2-2


# dataset3

# calculos da variacao %:
# aqui o ICL Full eh positivo, então não precisa do menos
var_icl_df3 <- trunc( c( (icl.df3[2,"ICL"] - icl.df3[1,"ICL"])/icl.df3[1,"ICL"],
                         (icl.df3[3,"ICL"] - icl.df3[1,"ICL"])/icl.df3[1,"ICL"],
                         (icl.df3[4,"ICL"] - icl.df3[1,"ICL"])/icl.df3[1,"ICL"],
                         (icl.df3[5,"ICL"] - icl.df3[1,"ICL"])/icl.df3[1,"ICL"]) *10^3)/10^3
var_icl_df3 <- paste0(var_icl_df3*100,'%')

p3_icl_new <-
  icl.df3 %>%
  ggplot(aes(x = id.ordered, y = ICL)) +
  geom_point(size = 2) +
  geom_hline(aes(yintercept = ICL[1]), color = '#56B4E9', size = 2, alpha=0.5) +
  geom_segment(aes(xend = id.ordered, yend = ICL[1]),
               size = 1, color = '#E69F00', linetype = "dashed", alpha=0.5) +
  scale_x_discrete(name = "Variables in the GMM Model") +
  scale_y_continuous(name = "ICL")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  ggtitle("Dataset 3")+
  theme(plot.title = element_text(hjust=0.5))+
  annotate("text", x=c(1.7, 2.7, 3.7, 4.7), y=c(200, -400, 200, 600),
           label=var_icl_df3)


# dataset4

# calculos da variacao %:
# aqui o ICL Full eh positivo, então não precisa do menos
var_icl_df4 <- trunc( c( (icl.df4[2,"ICL"] - icl.df4[1,"ICL"])/icl.df4[1,"ICL"],
                         (icl.df4[3,"ICL"] - icl.df4[1,"ICL"])/icl.df4[1,"ICL"],
                         (icl.df4[4,"ICL"] - icl.df4[1,"ICL"])/icl.df4[1,"ICL"],
                         (icl.df4[5,"ICL"] - icl.df4[1,"ICL"])/icl.df4[1,"ICL"]) *10^3)/10^3
var_icl_df4 <- paste0(var_icl_df4*100,'%')

p4_icl_new <-
  icl.df4 %>%
  ggplot(aes(x = id.ordered, y = ICL)) +
  geom_point(size = 2) +
  geom_hline(aes(yintercept = ICL[1]), color = '#56B4E9', size = 2, alpha=0.5) +
  geom_segment(aes(xend = id.ordered, yend = ICL[1]),
               size = 1, color = '#E69F00', linetype = "dashed", alpha=0.5) +
  scale_x_discrete(name = "Variables in the GMM Model") +
  scale_y_continuous(name = "ICL")+
  theme_bw() +
  theme(panel.grid = element_blank())+
  ggtitle("Dataset 4")+
  theme(plot.title = element_text(hjust=0.5))+
  annotate("text", x=c(1.7, 2.7, 3.7, 4.7), y=c(8000, 5000, 3000, 3000),
           label=var_icl_df4)


# x11()
# grid.arrange(p1_icl_new, p2_icl_new, p3_icl_new, p4_icl_new, ncol=2,
#              top = "Increase/Decrease in ICL when removing/leaving Variables in the GMM Model")

# savePlot("fig3_icl_SEM_outliers.PNG", type = "png",
#          device = dev.cur())


## GMM com G=1 para todos
library(mclust)
# dataset1
set.seed(1984)
out1_1g <- Mclust(data1.df[,1:4], G=1)
icl1_1g <- icl(out1_1g)

# dataset2
set.seed(1984)
out2_1g <- Mclust(data2.df[,1:4], G=1)
icl2_1g <- icl(out2_1g)

# dataset3
set.seed(1984)
out3_1g <- Mclust(data3.df[,1:4], G=1)
icl3_1g <- icl(out3_1g)

# dataset4
set.seed(1984)
out4_1g <- Mclust(data4.df[,1:4], G=1)
icl4_1g <- icl(out4_1g)

icl_1g.df <- data.frame(rbind(icl1_1g, icl2_1g, icl3_1g, icl4_1g))
colnames(icl_1g.df) <- "ICL_G=1"
rownames(icl_1g.df) <- paste0("Dataset", 1:4)



#------------- Probabilidade (z scores) pertencimento

# dataset1
p1_p <- ggplot(data1.df, aes(Core, B_Band))+
  stat_summary_hex(aes(z = out1$z[,1]))+
  scale_fill_gradientn (colors = c('#E69F00', '#56B4E9', "#4F687F"),
                        labels=c('','group 2',0.5,'group 1', ''))+
  ggtitle("Dataset 1 - Probability of belonging to groups 1 and 2")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda
#ideia de label em: https://stackoverflow.com/questions/7323191/how-do-i-manually-change-the-key-labels-in-a-legend-in-ggplot2

# dataset2
p2_p <- ggplot(data2.df, aes(Radio, B_Band))+
  stat_summary_hex(aes(z = out2$z[,1]))+
  scale_fill_gradientn (colors = c('#E69F00', '#56B4E9', "#4F687F"),
                        labels=c('','group 2',0.5,'group 1', ''))+
  ggtitle("Dataset 2 - Probability of belonging to group 1 and 2")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda

# datset3
p3_p <- ggplot(data3.df, aes(VLA_6cm, I_Band))+
  stat_summary_hex(aes(z = out3$z[,1]))+
  scale_fill_gradientn (colors = c('#E69F00', '#56B4E9', "#4F687F"),
                        labels=c('','group 2',0.5,'group 1', ''))+
  ggtitle("Dataset 3 - Probability of belonging to group 1 and 2")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda

# datset4
p4_p <- ggplot(data4.df, aes(Bmag, Sint))+
  stat_summary_hex(aes(z = out4$z[,1]))+
  scale_fill_gradientn (colors = c('#E69F00', '#56B4E9', "#4F687F"),
                        labels=c('','group 2',0.5,'group 1', ''))+
  ggtitle("Dataset 3 - Probability of belonging to group 1 and 2")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(fill='probability') # para alterar título da legenda


## alterar eixos para o plot4 ? Bmag seria optico e deveria ir no eixo y?
library(gridExtra)
# x11()
# grid.arrange(p1_p,p2_p,p3_p,p4_p, ncol=2)

# savePlot("fig4_prob_clusters_SEM_outliers.PNG", type = "png",
#          device = dev.cur())


#------------- Figura com divisão dos tipos (somente datasets 1 e 2) e R>1, R<1

data1.df_type <- read.table(data1.loc, header=TRUE)
data2.df_type <- read.table(data2.loc, header=TRUE)

data1.df_type <- merge(data1.df, data1.df_type)
data2.df_type <- merge(data2.df, data2.df_type)

p1_t <- ggplot(data1.df_type, aes(Core, B_Band))+
  geom_point(aes(colour = Type, shape=cluster), size = 1.8)+
  scale_shape_manual(values=c(3, 16))+
  ggtitle("Dataset 1 - Type vs Cluster")+
  theme(plot.title = element_text(hjust=0.5))

p2_t <- ggplot(data2.df_type, aes(Radio, B_Band))+
  geom_point(aes(colour = Type, shape=cluster), size = 1.8)+
  scale_shape_manual(values=c(3, 16))+
  ggtitle("Dataset 2 - Type vs Cluster")+
  theme(plot.title = element_text(hjust=0.5))

# grid.arrange(p1_t,p2_t,ncol=2)


# savePlot("fig5_gruposAGNS_SEM_outliers.PNG", type = "png",
#          device = dev.cur())

###### fim da analise SEM outliers

