library(rvest)
library(dplyr)
library(stringr)

setwd('~/Documentos/Codes/R/frentes-parlamentares/')
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


# Não existe a Frente 8 no site - "Frente Parlamentar em Apoio à Duplicação da BR 251, no Trecho entre Montes Claros a Salinas - MG."
# Vamos exclui-la
x <- x[-8,]

planilha <- NULL



for (i in 1:length(x$sites)) {
  print(i)
  pagina <- html_table(html_nodes(html(x$sites[i]), "table")[[1]], fill=TRUE)
  pagina$frente <- x$nome[i]
  pagina$data <- x$data[i]
  pagina$coord_presi <- x$coord_presi[i]
  planilha <- rbind(planilha, pagina)
}

planilha <- planilha[-which(is.na(planilha$partido)),]
colnames(planilha) <- c("dep", "partido", "uf", "frente", "data", "coord_presi")
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
