library(rvest)
library(dplyr)
library(stringr)

setwd('~/Documents/R/frentes-parlamentares/')
principal <- "http://www.camara.gov.br/internet/deputado/frentes.asp"

x <- html_table(html_nodes(html(principal), "table"), fill=TRUE)

x <- x[[1]]
x <- x[-5]

colnames(x) <- c("nome", "data_pub", "coord_presi", "telefone")

v <- html_attr(html_nodes(html(principal), "table a"), "href")
z <- grep("integra", v)
x$sites <- v[c(-1, -z)]

x$sites <- sapply(x$sites, function(x) { return(gsub(
  "/internet/", "http://www.camara.gov.br/internet/", x)) })


# Não existe a Frente 9 no site - "Frente Parlamentar em Apoio à Duplicação da BR 251, no Trecho entre Montes Claros a Salinas - MG."
# Vamos exclui-la
x <- x[-9,]

planilha <- NULL



for (i in 1:length(x$sites)) {
  print(i)
  pagina <- html_table(html_nodes(html(x$sites[i]), "table")[[1]], fill=TRUE)
  pagina$frente <- x$nome[i]
  pagina$data <- x$data[i]
  pagina$coord_presi <- x$coord_presi[i]
  planilha <- rbind(planilha, pagina)
}

colnames(planilha) <- c("dep", "partido", "uf", "frente", "data", "coord_presi")

planilha <- planilha[-which(is.na(planilha$partido)),]
write.csv(planilha, "planilha.csv", row.names=FALSE)



nomes_coluna <- x$nome
nomes_linha <- read.csv('dep.csv', header=FALSE, stringsAsFactors=FALSE)[[1]]

planilha_geral <- matrix(nrow=length(nomes_linha), ncol=length(nomes_coluna))
colnames(planilha_geral) <- nomes_coluna
rownames(planilha_geral) <- nomes_linha



for ( i in 1:length(nomes_coluna)) {
  print(i)
  df_temp <- filter(planilha, frente==nomes_coluna[i])
  for ( j in 1:length(nomes_linha)) {
    if ( length(grep(nomes_linha[j], df_temp$dep)) >= 1 )
      planilha_geral[j,i] <- 1
    else
      planilha_geral[j,i] <- 0
  }
}

write.csv(planilha_geral, "planilha_geral.csv")
  

library(MASS)
d <- dist(t(planilha_geral)) # euclidean distances between the rows

write.csv(d, "dist.csv")

fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results


plot((1:46), fit$eig, type="l")

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(planilha_geral), cex=.7)



########### Por Partidos




nomes_coluna <- x$nome
nomes_linha <- names(table(read.csv('dep.csv', header=FALSE, stringsAsFactors=FALSE)[[2]]))


planilha_geral <- matrix(0, nrow=length(nomes_linha), ncol=length(nomes_coluna))
colnames(planilha_geral) <- nomes_coluna
rownames(planilha_geral) <- nomes_linha


i <- 1
j <- 1

for ( i in 1:length(nomes_coluna)) {
  print(i)
  df_temp <- filter(planilha, frente==nomes_coluna[i])
  tabela <-  table(df_temp$partido)
  tabela <- round(prop.table(tabela), digits=3)
  for (j in 1:length(nomes_linha)) {
    for (k in 1:length(tabela)) {
      if (names(tabela[k])==nomes_linha[j] ) {
        planilha_geral[j,i] <- tabela[k]        
      }
    }
  }
}




tabela_partidos <- as.data.frame(planilha_geral)
write.csv(tabela_partidos, "tabela_partidos.csv")

i <- 1

for (i in 1:length(planilha_geral[,1])) {
  print(rownames(planilha_geral)[i])
  print(names(sort(planilha_geral[i,], decreasing=TRUE)) [1:3])
}