##########################################################
# os dados de longitude e latitude foram tirados do ibge #
##########################################################

# Ler a base de dados com as cidades atendidas e suas coordenadas:

# install.packages("readxlsb")
library(readxlsb)
# Manipulando os dados da forma que vou precisar:

BDRegioes <- read_xlsb(file.choose(),sheet = "BD_Regiões")
BDRegioes <- BDRegioes[,1:7]
BD <- BDRegioes[,c("Cidade","UF","Região","Latitude","Longitude")]
BD <- BD[BD$Latitude!= "NÃO POSSUI",]
BD$Latitude <- as.numeric(BD$Latitude)
BD$Longitude <- as.numeric(BD$Longitude)
# se plotarmos as logitudes e latitudes teremos algo semelhante ao mapa do Brasil
MapaBrasil <- cbind(BD$Longitude,BD$Latitude)
plot(MapaBrasil)
# A ideia é criar uma matriz de distancias entre todas as cidades listadas.
# Calculando as distancias com a formula de Harvesine

# install.packages("geosphere")
library(geosphere)
qtd_linhas <- nrow(BD)
dist_matrix <- matrix(0, nrow = qtd_linhas, ncol = qtd_linhas)
# o loop pode levar alguns instantes
for (i in 1:qtd_linhas) {
  for (j in 1:qtd_linhas) {
    dist_matrix[i, j] <- distHaversine(
      c(BD[i, "Longitude"], BD[i, "Latitude"]),
      c(BD[j, "Longitude"], BD[j, "Latitude"])
    )
  }
}

colnames(dist_matrix) <- BD$Região
rownames(dist_matrix) <- BD$Região
# vou salvar essa matriz para não precisar ficar rodando.
save(dist_matrix, file = "MatrizDistancias.RData")

#ideias
# criar um cluster de distâncias entre cidades para agrupamento das cargas
# criar um otimizador de rotas (baseado nas coordenadas) para criar uma "ótima" rota para ditar as cargas (PO)
# agrupar as notas fiscais que vão para mesmas cidades e cidades próximas (tendo que precisa-se seguir a regra de veículo)
# agrupar por regiões do Brasil (Ultima opção)
