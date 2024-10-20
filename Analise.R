#Análise de Clustes - TCC LETÍCIA

# ------------------------- 1) Preparacao de Dados -----------------------------
#1.1 Importar bibliotecas

pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel", 
             "knitr", 
             "kableExtra",
             "reshape2",
             "ggplot2",
             "cluster", 
             "factoextra",
             "dplyr",
             "tidyr",
             "qcc",
             "patchwork",
             "plotly",
             "zoo",
             "glue",  
             "NbClust",
             "fpc",
             "ds")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# 1.2) Importar o dataset
set.seed(1)
caminho <- "~/Library/Mobile Documents/com~apple~CloudDocs/USP/TCC/banco de dados/"
carregar_dados = function(a) {
  doc <- paste (a,".csv", sep="")
  x <- read.csv(paste(caminho,doc, sep=""), sep = ";", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
  renomear <- c("seq", "nr_nota", "item_cod", "vl_unit", "qt_fat", "vl_total", "dt_emissao", "natureza_op",
                "cliente_cod")
  names(x)[1:9] <- renomear
  x <- slice(x, -1) 
  return (x)
}

ano3 = "ano3"
ano3 <- carregar_dados(ano3)

ano21 = "ano21"
ano21 <- carregar_dados(ano21)

ano22 = "ano22"
ano22 <- carregar_dados(ano22)

ano12 = "ano12"
ano12 <- carregar_dados(ano12)

ano11 ="ano11"
ano11 <- carregar_dados(ano11)


#1.4) Concatenar arquivos:
df = rbind(ano3, ano22, ano21, ano11, ano12)

#1.5) Criacar coluna mes e ano para fazer a analise mensalmente.
df <- separate(df, col = "dt_emissao", into = c("dia", "mes", "ano"), sep = '/', remove = FALSE)

#1.6) Filtrar as naturezas que indicam vendas normais.
nat = c("540100", "610100", "610101", "510108", "640100", "510115", "610700", "610902")
df <- filter(df, natureza_op %in% nat)

#1.8) Base de dados final
df_final <- df
df_final$vl_total <- gsub(",",".",df_final$vl_total)
df_final <- transform(df_final, mes = as.numeric(mes), 
                      ano = as.numeric(ano),
                      vl_total = as.numeric(vl_total),
                      dt_emissao = as.Date(dt_emissao,"%d/%m/%Y" ))
str(df_final)

#1.9) Vamos separar os ultimos 8 meses em data frames para que analise seja feita de forma evolutiva
# Maior data 31/08/2023, entáo vamos separar em 8 data frames.
# Formando várias bases de dados

separar = function(x, dt) {
  mes <- filter (x, dt_emissao<=dt)
  df_mes <- select(mes, cliente_cod, vl_total, dt_emissao, nr_nota)
  return (df_mes)
}
df_jan <- separar(df_final, "2023-01-31")
df_fev <- separar(df_final, "2023-02-28")
df_marc <- separar(df_final, "2023-03-31")
df_abril <- separar(df_final, "2023-04-30")
df_maio <- separar(df_final, "2023-05-31")
df_junho <- separar(df_final, "2023-06-30")
df_julho <- separar(df_final, "2023-07-31")
df_ago <- separar(df_final, "2023-08-31")


# 2.2) Criando variaveis do RFM:

var_RFM = function(data_frame) {
  
  rfm_nota_mes <- data_frame %>% group_by(cliente_cod, nr_nota) %>% summarise(dt_emissao = max(dt_emissao), monetario = sum(vl_total)) #agrupamento por nota nos mes
  rfm_mes <- rfm_nota_mes %>% group_by(cliente_cod) %>%
    summarise(ultima_compra = max(dt_emissao),primeira_compra = min (dt_emissao),frequencia = n(),
              monetario = sum(monetario))%>% ungroup() #agrupamento a nivel de cliente, contando quantas notas tiveram no referente mes
   maior_data<-rfm_mes$ultima_compra[which.max(rfm_mes$ultima_compra)] #maior data de faturamento do mes
   rfm_mes["recencia"] <- as.numeric(date(maior_data) - rfm_mes$ultima_compra) #criando a coluna recencia
   rfm_mes <- rfm_mes %>% select(cliente_cod,frequencia, monetario, recencia) #selecionando as colunas uteis para analise
   rfm_mes_pad <- as.data.frame(scale(rfm_mes[,2:4])) #padronizacao dos dados
  return (list(rfm_mes, rfm_mes_pad)) #retorna dois dataframes
}


rfm_jan <- var_RFM(df_jan)[[1]];rfm_jan_pad <- var_RFM(df_jan)[[2]]
rfm_fev <- var_RFM(df_fev)[[1]];rfm_fev_pad <- var_RFM(df_fev)[[2]]
rfm_marc <- var_RFM(df_marc)[[1]];rfm_marc_pad <- var_RFM(df_marc)[[2]]
rfm_abril <- var_RFM(df_abril)[[1]];rfm_abril_pad <- var_RFM(df_abril)[[2]]
rfm_maio <- var_RFM(df_maio)[[1]];rfm_maio_pad <- var_RFM(df_maio)[[2]]
rfm_junho <- var_RFM(df_junho)[[1]];rfm_junho_pad <- var_RFM(df_junho)[[2]]
rfm_julho <- var_RFM(df_julho)[[1]];rfm_julho_pad <- var_RFM(df_julho)[[2]]
rfm_ago <- var_RFM(df_ago)[[1]];rfm_ago_pad <- var_RFM(df_ago)[[2]]


# 2.3) Grafico das vendas acumuladas

graf_evolu <- data.frame(mes = c("janeiro", "fevereiro","marco","abril", 
                            "maio", "junho", "julho", "agosto"),
                    monetario = c((sum(rfm_jan$monetario)),
                                  (sum(rfm_fev$monetario)),
                                  (sum(rfm_marc$monetario)),
                                  (sum(rfm_abril$monetario)),
                                  (sum(rfm_maio$monetario)),
                                  (sum(rfm_junho$monetario)),
                                  (sum(rfm_julho$monetario)),
                                  (sum(rfm_ago$monetario))),
                    
                    qnt_vendas = c((sum(rfm_jan$frequencia)),
                                   (sum(rfm_fev$frequencia)),
                                   (sum(rfm_marc$frequencia)),
                                   (sum(rfm_abril$frequencia)),
                                   (sum(rfm_maio$frequencia)),
                                   (sum(rfm_junho$frequencia)),
                                   (sum(rfm_julho$frequencia)),
                                   (sum(rfm_ago$frequencia))))

ggplot(data=graf_evolu, aes(x=reorder(mes, qnt_vendas), weights=qnt_vendas, fill=fix)) +
  geom_bar(aes(fill=mes),color="Black") +
  xlab("Meses de vendas/2023") + 
  ylab("Frequência de vendas")

#Estatistica descritivas de janeiro e agosto

gds(rfm_julho$monetario)

# BOX PLOT mes de janeiro
hist(rfm_jan$recencia, main = "Recência (R)", ylab = "Recência de compras Jan/2023", 
     xlab = "Recência", col = "blue", breaks = 50, xlim=c(0,800), ylim=c(0,600))

hist(rfm_jan$frequencia, main = "Frequência (F)", ylab = "Acumulado de frequências Jan/2023", 
     xlab = "Frequência", col = "gray", breaks = 50, xlim=c(0,100), ylim=c(0,2500))

hist(rfm_jan$monetario, main = "Monetário (M)", ylab = "Acumulado de monetário Jan/2023", 
     xlab = "Monetário", col = "red", breaks = 130, xlim=c(0,1000000), ylim=c(0,3000))

# 2.rfm_maio# 2.4) Encontrando o melhor numero de clusters para aplicacao da clusterizacao

#https://medium.com/@ozturkfemre/unsupervised-learning-determination-of-cluster-number-be8842cdb11
#https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/#google_vignette

# 1) Método de Elbow - Metodo de teste direto
fviz_nbclust(rfm_jan_pad, kmeans, method = "wss", k.max = 10) + labs(subtitle = "Método do cotovelo - Janeiro/2023") + 
  xlab("Número de Clusters k") + ylab("WCSS")
fviz_nbclust(rfm_ago_pad, kmeans, method = "wss", k.max = 10) + labs(subtitle = "Método do cotovelo - Agosto/2023") + 
  xlab("Número de Clusters k") + ylab("WCSS")



# 2) Método da Silhueta - Método de teste direto
fviz_nbclust(rfm_jan_pad, kmeans, method = "silhouette") + labs(subtitle = "Método da Silhueta - Janeiro/2023") + 
  xlab("Número de Clusters k") + ylab("Silhueta Média (SI)")

fviz_nbclust(rfm_ago_pad, kmeans, method = "silhouette") + labs(subtitle = "Método da Silhueta - Agosto/2023") + 
  xlab("Número de Clusters k") + ylab("Silhueta Média (SI)")

#3) Indice de Calinski-Harabasz

calinski <- function(df) {
  ch <- c()
  for (i in 2:10) {
    km <- kmeans(df, i) # executar a clusterização
    ch[i] <- calinhara(df, # dados
                       km$cluster, # atribuições dos clusters
                       cn = max(km$cluster) # número total de clusters
    )
  }
  ch <- ch[2:10]
  k <- 2:10
  plot(k, ch, xlab = "Número de Clusters k",
       ylab = "Índice HC",
       main = "Índice Caliński-Harabasz - Agosto/2023", cex.main = 1,
       col = "dodgerblue1", cex = 1.0,
       lty = 1, type = "o", lwd = 1, pch = 20,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl = -0.5)
  abline(v = which(ch == max(ch)) + 1, lwd = 1, col = "blue", lty = "dashed")
}

calinski(rfm_ago_pad)




# 4) Método 30 possibilidades
#https://cran.r-project.org/web/packages/NbClust/NbClust.pdf
res<-NbClust(rfm_abril_pad, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "ch")
res$All.index
res$Best.nc
res$All.CriticalValues
res$Best.partition


# 2.5) Aplicando a clusterizacao

cluster_kmeans_jan <- kmeans(rfm_jan_pad, centers = 4)
cluster_kmeans_fev <- kmeans(rfm_fev_pad, centers = 4)
cluster_kmeans_marc <- kmeans(rfm_marc_pad, centers = 4)
cluster_kmeans_abril <- kmeans(rfm_abril_pad, centers = 4)
cluster_kmeans_maio <- kmeans(rfm_maio_pad, centers = 4)
cluster_kmeans_junho <- kmeans(rfm_junho_pad, centers = 4)
cluster_kmeans_julho <- kmeans(rfm_julho_pad, centers = 4)
cluster_kmeans_ago <- kmeans(rfm_ago_pad, centers = 4)



#2.6) Concatenando o cluster:


rfm_jan_pad$cluster_K <- factor(cluster_kmeans_jan$cluster)
rfm_jan$cluster_K <- factor(cluster_kmeans_jan$cluster)

rfm_fev_pad$cluster_K <- factor(cluster_kmeans_fev$cluster)
rfm_fev$cluster_K <- factor(cluster_kmeans_fev$cluster)

rfm_marc_pad$cluster_K <- factor(cluster_kmeans_marc$cluster)
rfm_marc$cluster_K <- factor(cluster_kmeans_marc$cluster)

rfm_abril_pad$cluster_K <- factor(cluster_kmeans_abril$cluster)
rfm_abril$cluster_K <- factor(cluster_kmeans_abril$cluster)

rfm_maio_pad$cluster_K <- factor(cluster_kmeans_maio$cluster)
rfm_maio$cluster_K <- factor(cluster_kmeans_maio$cluster)

rfm_junho_pad$cluster_K <- factor(cluster_kmeans_junho$cluster)
rfm_junho$cluster_K <- factor(cluster_kmeans_junho$cluster)

rfm_julho_pad$cluster_K <- factor(cluster_kmeans_julho$cluster)
rfm_julho$cluster_K <- factor(cluster_kmeans_julho$cluster)

rfm_ago_pad$cluster_K <- factor(cluster_kmeans_ago$cluster)
rfm_ago$cluster_K <- factor(cluster_kmeans_ago$cluster)


# Criando Scors para os clusters a fim de padronizar a numeracao do cluster:


#grafico 3D
grafico_3d = function(rfm_dado, mes_plot) {
  fig <- plot_ly(rfm_dado, x = ~recencia, y = ~frequencia, z = ~monetario, text = ~cluster_K, color = ~cluster_K)
  fig <- fig %>% add_markers()
  fig <- fig %>% layout(title = glue('Análise 3D Clusterizacao {mes_plot}/2023'), scene = list(xaxis = list(title = 'Recencia'),
                                                                                               yaxis = list(title = 'Frequencia'),
                                                                                               zaxis = list(title = 'Monetario')))
return(fig)}

#Analise final analista JANEIRO
fig1 <- grafico_3d(rfm_jan, mes_plot = ('JANEIRO'))
fig1
x<- c('Clientes OURO', 
      'Clientes PRATA', 
      'Clientes iniciais',
      'Clientes Perdidos')
y <- c(2,4,3,1)
rfm_jan$resultado_jan <- plyr::mapvalues(rfm_jan$cluster_K, y, x)

#Analise final analista FEVEREIRO
fig2 <- grafico_3d(rfm_fev, mes_plot = ('FEVEREIRO'))
fig2
y <- c(3,1,4,2)
rfm_fev$resultado_fev <- plyr::mapvalues(rfm_fev$cluster_K, y, x)


#Analise final analista MARÇO
fig3 <- grafico_3d(rfm_marc, mes_plot = ('MARÇO'))
fig3

y <- c(2,4,1,3)
rfm_marc$resultado_marc <- plyr::mapvalues(rfm_marc$cluster_K, y, x)

#Analise final analista ABRIL
fig4 <- grafico_3d(rfm_abril, mes_plot = ('ABRIL'))
fig4

y <- c(4,1,3,2)
rfm_abril$resultado_abril <- plyr::mapvalues(rfm_abril$cluster_K, y, x)

#Analise final analista MAIO
fig5 <- grafico_3d(rfm_maio, mes_plot = ('MAIO'))
fig5
y <- c(4,3,1,2)
rfm_maio$resultado_maio <- plyr::mapvalues(rfm_maio$cluster_K, y, x)

#Analise final analista JUNHO
fig6 <- grafico_3d(rfm_junho, mes_plot = ('JUNHO'))
fig6

y <- c(1,2,3,4)
rfm_junho$resultado_junho <- plyr::mapvalues(rfm_junho$cluster_K, y, x)

#Analise final analista JULHO
fig7 <- grafico_3d(rfm_julho, mes_plot = ('JULHO'))
fig7
y <- c(4,1,3,2)
rfm_julho$resultado_julho <- plyr::mapvalues(rfm_julho$cluster_K, y, x)

#Analise final analista AGOSTO
fig8 <- grafico_3d(rfm_ago, mes_plot = ('AGOSTO'))
fig8
y <- c(3,2,4,1)
rfm_ago$resultado_ago <- plyr::mapvalues(rfm_ago$cluster_K, y, x)

#COMPARAÇÃO MES A MES


rfm_jan_comp <- rfm_jan %>% select(cliente_cod,resultado_jan)
rfm_fev_comp <- rfm_fev %>% select(cliente_cod,resultado_fev)
rfm_mar_comp <- rfm_marc %>% select(cliente_cod,resultado_marc)
rfm_abril_comp <- rfm_abril %>% select(cliente_cod,resultado_abril)
rfm_maio_comp <- rfm_maio %>% select(cliente_cod,resultado_maio)
rfm_junho_comp <- rfm_junho %>% select(cliente_cod,resultado_junho)
rfm_julho_comp <- rfm_julho %>% select(cliente_cod,resultado_julho)
rfm_ago_comp <- rfm_ago %>% select(cliente_cod,resultado_ago)


juncao_dados <- rfm_jan_comp %>% right_join(rfm_fev_comp, by = c('cliente_cod' = 'cliente_cod'))
juncao_dados <- juncao_dados %>% right_join(rfm_mar_comp, by = c('cliente_cod' = 'cliente_cod'))
juncao_dados <- juncao_dados %>% right_join(rfm_abril_comp, by = c('cliente_cod' = 'cliente_cod'))
juncao_dados <- juncao_dados %>% right_join(rfm_maio_comp, by = c('cliente_cod' = 'cliente_cod'))
juncao_dados <- juncao_dados %>% right_join(rfm_junho_comp, by = c('cliente_cod' = 'cliente_cod'))
juncao_dados <- juncao_dados %>% right_join(rfm_julho_comp, by = c('cliente_cod' = 'cliente_cod'))
juncao_dados <- juncao_dados %>% right_join(rfm_ago_comp, by = c('cliente_cod' = 'cliente_cod'))


#Análise Scores

juncao_dados_score<- juncao_dados 
juncao_dados_score[] <- lapply(juncao_dados_score, as.character)
juncao_dados_score[juncao_dados_score=='Clientes OURO']<-'1'
juncao_dados_score[juncao_dados_score=='Clientes PRATA']<-'2'
juncao_dados_score[juncao_dados_score=='Clientes iniciais']<-'3'
juncao_dados_score[juncao_dados_score=='Clientes Perdidos']<-'4'


renomear <- c("01/2023", "02/2023", "03/2023", "04/2023", "05/2023", "06/2023", "07/2023", "08/2023")

names(juncao_dados_score)[2:9] <- renomear
juncao_dados_score[2:9] <- lapply(juncao_dados_score[2:9], as.factor)

write.table(juncao_dados_score, file=
              "~/Library/Mobile Documents/com~apple~CloudDocs/USP/TCC/banco de dados/resultado_para_analise4.csv", sep=",", row.names = FALSE)

write.table(rfm_fev, file=
              "~/Library/Mobile Documents/com~apple~CloudDocs/USP/TCC/banco de dados/rfmfev.csv", sep=",", row.names = FALSE)
write.table(rfm_jan, file=
              "~/Library/Mobile Documents/com~apple~CloudDocs/USP/TCC/banco de dados/rfmjan.csv", sep=",", row.names = FALSE)
