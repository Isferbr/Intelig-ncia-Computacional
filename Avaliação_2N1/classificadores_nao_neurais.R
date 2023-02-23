# ---------------------------------------------------------------------#
# --------------- Classificador Vizinho mais Próximo ------------------#
# ---------------------------------------------------------------------#
# @autor Ismael Fernandes
# @data 15/05/2022
# @disciplina Inteligência Artifical - ADS IFCE Canindé

## ------------------------------------------------------------------ #
##               Etapa 1 - Carregando o dataset
## ------------------------------------------------------------------ #
#dados <- read.csv('C:/Users/Alunos/Documents/IA/parkinsons.csv', header = TRUE, sep = ',')
dados <- read.csv('parkinsons.csv', header = TRUE, sep = ',')
#View(dados)

## ------------------------------------------------------------------ #
##  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
## ------------------------------------------------------------------ #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ndados <- nrow(dados) # numero de dados
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino 

# Normalizando os dados
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
dados.norm <- as.data.frame( lapply(dados[,2:23], nor) )

taxa_acertos <- c()
for (i in 1:10) {
# Definindo dados para treinamento e teste
indices_treino <- sample(ndados, ntreino, replace= FALSE)
dados.treino <- dados.norm[indices_treino,]
#View(dados.treino)
dados.teste <- dados.norm[-indices_treino,]
#View(dados.teste)

# Definindo as classes para treinamento e teste
dados.treino.classes <- dados[indices_treino,24]
dados.teste.classes <- dados[-indices_treino,24]

## -------------------------------------------------- #
##            Etapa 3 - Classificação
## -------------------------------------------------- #
library(class)

# Função para medir a acurácia
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

## ---------------------------------------------------------------------------#
##          Classificador K-vizinhos mais próximos (KNN)
## 
## Função: knn1(train, test, cl)
##
## Argumentos da função knn()
## - train: matriz ou data frame de casos do conjunto de treinamento
## - test: matriz ou data frame de casos do conjunto de teste
## - cl: classes (factor) para classificações corretas do conjunto de treinamento
##----------------------------------------------------------------------------#
resultado_nn <- knn1(dados.treino,dados.teste,cl=dados.treino.classes)

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado_nn, dados.teste.classes)
names(class.comparison) <- c("Respostas Previstas", "Respostas Observadas")
#View(class.comparison)

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao_nn <- table(resultado_nn,dados.teste.classes)
taxa_acertos[i] <- accuracy(matriz_confusao_nn)

}
# Plotando os dados de treino
plot(dados.treino$MDVP.Fo.Hz., dados.treino$MDVP.Shimmer.dB.,pch = 21,
     bg = c('blue','green'))

# #################################################################### #
##                              RESULTADOS                            ##
# #################################################################### #
media <- mean(taxa_acertos)
mediana <- median(taxa_acertos)
minimo <- min(taxa_acertos)
maximo <- max(taxa_acertos)
desvio_padrao <- sd(taxa_acertos)

# Criando uma tabela com as medidas descritivas
tabela <- rbind(media,mediana,minimo,maximo,desvio_padrao); tabela

# Salvando uma tabela em um arquivo .txt
write.csv(tabela, 'Resultados_KNN1_Parkinson.txt',row.names = TRUE)

# ---------------------------------------------------------------------#
# --------------- Classificador Vizinho mais Próximo ------------------#
# ---------------------------------------------------------------------#
# @autor Ismael Fernandes
# @data 15/05/2022
# @disciplina Inteligência Artifical - ADS IFCE Canindé

## ------------------------------------------------------------------ #
##               Etapa 1 - Carregando o dataset
## ------------------------------------------------------------------ #
#dados <- read.csv('C:/Users/Alunos/Documents/IA/dermatology.csv', header = TRUE, sep = ',')
dados <- read.csv('dermatology.csv', header = TRUE, sep = ',')
#View(dados)

## ------------------------------------------------------------------ #
##  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
## ------------------------------------------------------------------ #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ndados <- nrow(dados) # numero de dados
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino 

# Normalizando os dados
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
dados.norm <- as.data.frame( lapply(dados[,1:34], nor) )

taxa_acertos <- c()
for (i in 1:10) {
# Definindo dados para treinamento e teste
indices_treino <- sample(ndados, ntreino, replace= FALSE)
dados.treino <- dados.norm[indices_treino,]
#View(dados.treino)
dados.teste <- dados.norm[-indices_treino,]
#View(dados.teste)

# Definindo as classes para treinamento e teste
dados.treino.classes <- dados[indices_treino,35]
dados.teste.classes <- dados[-indices_treino,35]

## -------------------------------------------------- #
##            Etapa 3 - Classificação
## -------------------------------------------------- #
library(class)

# Função para medir a acurácia
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

## ---------------------------------------------------------------------------#
##          Classificador K-vizinhos mais próximos (KNN)
## 
## Função: knn1(train, test, cl)
##
## Argumentos da função knn()
## - train: matriz ou data frame de casos do conjunto de treinamento
## - test: matriz ou data frame de casos do conjunto de teste
## - cl: classes (factor) para classificações corretas do conjunto de treinamento
##----------------------------------------------------------------------------#
resultado_nn <- knn1(dados.treino,dados.teste,cl=dados.treino.classes)

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado_nn, dados.teste.classes)
names(class.comparison) <- c("Resultados Previstos", "Resultados Observados")
#View(class.comparison)

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao_nn <- table(resultado_nn,dados.teste.classes)
taxa_acertos <- accuracy(matriz_confusao_nn)

}
# Plotando os dados de treino
plot(dados.treino$X3.1, dados.treino$X0.3,pch = 21,
     bg = c('blue','green','red','black','gray','yellow'))

# #################################################################### #
##                              RESULTADOS                            ##
# #################################################################### #
media <- mean(taxa_acertos)
mediana <- median(taxa_acertos)
minimo <- min(taxa_acertos)
maximo <- max(taxa_acertos)
desvio_padrao <- sd(taxa_acertos)

# Criando uma tabela com as medidas descritivas
tabela <- rbind(media,mediana,minimo,maximo,desvio_padrao); tabela

# Salvando uma tabela em um arquivo .txt
write.csv(tabela, 'Resultados_KNN1_Dermatology.txt',row.names = TRUE)

# ---------------------------------------------------------------------#
# -------------- Classificador K-vizinhos mais próximos ---------------#
# ---------------------------------------------------------------------#
# @autor Ismael Fernandes
# @data 17/05/2022
# @disciplina Inteligência Artifical - ADS IFCE Canindé

## ------------------------------------------------------------------ #
##               Etapa 1 - Carregando o dataset
## ------------------------------------------------------------------ #
#dados <- read.csv('C:/Users/Alunos/Documents/IA/parkinsons.csv', header = TRUE, sep = ',')
dados <- read.csv('parkinsons.csv', header = TRUE, sep = ',')
#View(dados)

## ------------------------------------------------------------------ #
##  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
## ------------------------------------------------------------------ #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ndados <- nrow(dados) # numero de dados
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino 

# Normalizando os dados
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
dados.norm <- as.data.frame( lapply(dados[,2:23], nor) )

taxa_acertos <- c()
for (i in 1:10) {
# Definindo dados para treinamento e teste
indices_treino <- sample(ndados, ntreino, replace= FALSE)
dados.treino <- dados.norm[indices_treino,]
#View(dados.treino)
dados.teste <- dados.norm[-indices_treino,]
#View(dados.teste)

# Definindo as classes para treinamento e teste
dados.treino.classes <- dados[indices_treino,24]
dados.teste.classes <- dados[-indices_treino,24]

## ------------------------------------------------------------------------- #
##                     Etapa 3 - Classificação
## ------------------------------------------------------------------------- #
library(class)

# Função para medir a acurácia
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

## ---------------------------------------------------------------------------#
##          Classificador K-vizinhos mais próximos (KNN)
## 
## Função: knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
##
## Argumentos da função knn()
## - train: matriz ou data frame de casos do conjunto de treinamento
## - test: matriz ou data frame de casos do conjunto de teste
## - cl: classes (factor) para classificações corretas do conjunto de treinamento
## - k: número de vizinhos
## - l: voto mínimo para definir a decisão
## - prob: Se TRUE, a proporção de votos para a classe vencedora será retornado como um atributo provável
## - use.all: SE TRUE, todas as distâncias iguais ao do K-ésimo vizinho são incluídas.
##            SE FALSE, uma seleção aleatória das distâncias igual ao do k-ésimo vizinho é escolhida
##----------------------------------------------------------------------------#

resultado_knn <- knn(dados.treino, dados.teste, cl = dados.treino.classes,
                     k=3, l=0, prob = FALSE, use.all = TRUE)

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado_knn, dados.teste.classes)
names(class.comparison) <- c("Respostas Previstas", "Respostas Observadas")
#View(class.comparison)

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao_knn <- table(resultado_knn,dados.teste.classes)
taxa_acertos <- accuracy(matriz_confusao_knn)

}
# Plotando os dados de treino
plot(dados.treino$MDVP.Fo.Hz., dados.treino$MDVP.Shimmer.dB.,pch = 21,
     bg = c('blue','green'))

# #################################################################### #
##                              RESULTADOS                            ##
# #################################################################### #
media <- mean(taxa_acertos)
mediana <- median(taxa_acertos)
minimo <- min(taxa_acertos)
maximo <- max(taxa_acertos)
desvio_padrao <- sd(taxa_acertos)

# Criando uma tabela com as medidas descritivas
tabela <- rbind(media,mediana,minimo,maximo,desvio_padrao); tabela

# Salvando uma tabela em um arquivo .txt
write.csv(tabela, 'Resultados_KNN_Parkinson.txt',row.names = TRUE)

# ---------------------------------------------------------------------#
# -------------- Classificador K-vizinhos mais próximos ---------------#
# ---------------------------------------------------------------------#
# @autor Ismael Fernandes
# @data 17/05/2022
# @disciplina Inteligência Artifical - ADS IFCE Canindé

## ------------------------------------------------------------------ #
##               Etapa 1 - Carregando o dataset
## ------------------------------------------------------------------ #
#dados <- read.csv('C:/Users/Alunos/Documents/IA/dermatology.csv', header = TRUE, sep = ',')
dados <- read.csv('dermatology.csv', header = TRUE, sep = ',')
#View(dados)

## ------------------------------------------------------------------ #
##  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
## ------------------------------------------------------------------ #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ndados <- nrow(dados) # numero de dados
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino 

# Normalizando os dados
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
dados.norm <- as.data.frame( lapply(dados[,1:34], nor) )

# Definindo dados para treinamento e teste
indices_treino <- sample(ndados, ntreino, replace= FALSE)
dados.treino <- dados.norm[indices_treino,]
#View(dados.treino)
dados.teste <- dados.norm[-indices_treino,]
#View(dados.teste)

taxa_acertos <- c()
for (i in 1:10) {
# Definindo as classes para treinamento e teste
dados.treino.classes <- dados[indices_treino,35]
dados.teste.classes <- dados[-indices_treino,35]

## ------------------------------------------------------------------------- #
##                     Etapa 3 - Classificação
## ------------------------------------------------------------------------- #
library(class)

# Função para medir a acurácia
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

## ---------------------------------------------------------------------------#
##          Classificador K-vizinhos mais próximos (KNN)
## 
## Função: knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
##
## Argumentos da função knn()
## - train: matriz ou data frame de casos do conjunto de treinamento
## - test: matriz ou data frame de casos do conjunto de teste
## - cl: classes (factor) para classificações corretas do conjunto de treinamento
## - k: número de vizinhos
## - l: voto mínimo para definir a decisão
## - prob: Se TRUE, a proporção de votos para a classe vencedora será retornado como um atributo provável
## - use.all: SE TRUE, todas as distâncias iguais ao do K-ésimo vizinho são incluídas.
##            SE FALSE, uma seleção aleatória das distâncias igual ao do k-ésimo vizinho é escolhida
##----------------------------------------------------------------------------#

resultado_knn <- knn(dados.treino, dados.teste, cl = dados.treino.classes,
                     k=3, l=0, prob = FALSE, use.all = TRUE)

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado_knn, dados.teste.classes)
names(class.comparison) <- c("Respostas Previstas", "Respostas Observadas")
#View(class.comparison)

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao_knn <- table(resultado_knn,dados.teste.classes)
taxa_acertos <- accuracy(matriz_confusao_knn)

}
# Plotando os dados de treino
plot(dados.treino$X3.1, dados.treino$X0.3,pch = 21,
     bg = c('blue','green','red','black','gray','yellow'))

# #################################################################### #
##                              RESULTADOS                            ##
# #################################################################### #
media <- mean(taxa_acertos)
mediana <- median(taxa_acertos)
minimo <- min(taxa_acertos)
maximo <- max(taxa_acertos)
desvio_padrao <- sd(taxa_acertos)

# Criando uma tabela com as medidas descritivas
tabela <- rbind(media,mediana,minimo,maximo,desvio_padrao); tabela

# Salvando uma tabela em um arquivo .txt
write.csv(tabela, 'Resultados_KNN_Dermatology.txt',row.names = TRUE)

# ---------------------------------------------------------------------#
# --------------- Classificador Vizinho mais Próximo ------------------#
# ---------------------------------------------------------------------#
# @autor Ismael Fernandes
# @data 17/05/2022
# @disciplina Inteligência Artifical - ADS IFCE Canindé

## ------------------------------------------------------------------ #
##               Etapa 1 - Carregando o dataset
## ------------------------------------------------------------------ #
#dados <- read.csv('C:/Users/Alunos/Documents/IA/parkinsons.csv', header = TRUE, sep = ',')
dados <- read.csv('parkinsons.csv', header = TRUE, sep = ',')
#View(dados)

## ------------------------------------------------------------------ #
##  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
## ------------------------------------------------------------------ #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ndados <- nrow(dados) # numero de dados
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino 

taxa_acertos <- c()
for (i in 1:10) {
# Definindo dados para treinamento e teste
indices_treino <- sample(ndados, ntreino, replace= FALSE)
dados.treino <- dados[indices_treino,]
#View(dados.treino)
dados.teste <- dados[-indices_treino,]
#View(dados.teste)

# Definindo as classes para teste
dados.teste.classes <- dados.teste[,24]

# Calculando os centroides das classes
dados.treino.saudavel <- dados.treino[dados.treino$status == "0",2:23]
dados.treino.doenca_parkinson <- dados.treino[dados.treino$status == "1",2:23]

centroide.saudavel <- apply(dados.treino.saudavel,2,mean)
centroide.doenca_parkinson <- apply(dados.treino.doenca_parkinson,2,mean)
dados.centroides <- list(centroide.saudavel,centroide.doenca_parkinson)

## ------------------------------------------------------------------------- #
##                     Etapa 3 - Classificação
## ------------------------------------------------------------------------- #
library(class)
library(pracma) 

# Função para medir a acurácia
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

## ---------------------------------------------------------------------------#
##          Classificador Distância Mínima ao Centróide (DMC)
##----------------------------------------------------------------------------#

# Inicializando as variaveis distancia e classe
d <- c()
resultado <- c()

# Calculando as distancias de cada linha do teste para todas as linhas do treino
for(i in 1:nteste){
  for(j in 1:2){
    d[j] <- Norm(as.matrix(dados.teste[i,2:23] - dados.centroides[[j]]))
  }
  if (which.min(d) == 1) {
    resultado[i] <- "0"
  }
  if (which.min(d) == 2){
    resultado[i] <- "1"
  }
}

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado, dados.teste.classes)
names(class.comparison) <- c("Respostas Previstas", "Respostas Observadas")
#View(class.comparison)

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao_dmc <- table(resultado,dados.teste.classes)
taxa_acertos <- accuracy(matriz_confusao_dmc)

}
# Plotando os dados de treino
plot(dados.treino$MDVP.Fo.Hz., dados.treino$MDVP.Shimmer.dB.,pch = 21,
     bg = c('blue','green'))

# #################################################################### #
##                              RESULTADOS                            ##
# #################################################################### #
media <- mean(taxa_acertos)
mediana <- median(taxa_acertos)
minimo <- min(taxa_acertos)
maximo <- max(taxa_acertos)
desvio_padrao <- sd(taxa_acertos)

# Criando uma tabela com as medidas descritivas
tabela <- rbind(media,mediana,minimo,maximo,desvio_padrao); tabela

# Salvando uma tabela em um arquivo .txt
write.csv(tabela, 'Resultados_DMC_Parkinson.txt',row.names = TRUE)

# ---------------------------------------------------------------------#
# --------------- Classificador Vizinho mais Próximo ------------------#
# ---------------------------------------------------------------------#
# @autor Ismael Fernandes
# @data 17/05/2022
# @disciplina Inteligência Artifical - ADS IFCE Canindé

## ------------------------------------------------------------------ #
##               Etapa 1 - Carregando o dataset
## ------------------------------------------------------------------ #
#dados <- read.csv('C:/Users/Alunos/Documents/IA/dermatology.csv', header = TRUE, sep = ',')
dados <- read.csv('dermatology.csv', header = TRUE, sep = ',')
#View(dados)

## ------------------------------------------------------------------ #
##  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
## ------------------------------------------------------------------ #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ndados <- nrow(dados) # numero de dados
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino 

taxa_acertos <- c()
for (i in 1:10) {
# Definindo dados para treinamento e teste
indices_treino <- sample(ndados, ntreino, replace= FALSE)
dados.treino <- dados[indices_treino,]
#View(dados.treino)
dados.teste <- dados[-indices_treino,]
#View(dados.teste)

# Definindo as classes para teste
dados.teste.classes <- dados.teste[,35]

# Calculando os centroides das classes
dados.treino.psoriase <- dados.treino[dados.treino$X2.3 == "1",1:34]
dados.treino.dermatite_seborreica <- dados.treino[dados.treino$X2.3 == "2",1:34]
dados.treino.liquen_plano <- dados.treino[dados.treino$X2.3 == "3",1:34]
dados.treino.pitiriase_rosea <- dados.treino[dados.treino$X2.3 == "4",1:34]
dados.treino.dermatite_cronica <- dados.treino[dados.treino$X2.3 == "5",1:34]
dados.treino.pitiriase_rubra_pilar <- dados.treino[dados.treino$X2.3 == "6",1:34]

centroide.psoriase <- apply(dados.treino.psoriase,2,mean)
centroide.dermatite_seborreica <- apply(dados.treino.dermatite_seborreica,2,mean)
centroide.liquen_plano <- apply(dados.treino.liquen_plano,2,mean)
centroide.pitiriase_rosea <- apply(dados.treino.pitiriase_rosea,2,mean)
centroide.dermatite_cronica <- apply(dados.treino.dermatite_cronica,2,mean)
centroide.pitiriase_rubra_pilar <- apply(dados.treino.pitiriase_rubra_pilar,2,mean)
dados.centroides <- list(centroide.psoriase,centroide.dermatite_seborreica,centroide.liquen_plano,centroide.pitiriase_rosea,centroide.dermatite_cronica,centroide.pitiriase_rubra_pilar)

## ------------------------------------------------------------------------- #
##                     Etapa 3 - Classificação
## ------------------------------------------------------------------------- #
library(class)
library(pracma) 

# Função para medir a acurácia
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

## ---------------------------------------------------------------------------#
##          Classificador Distância Mínima ao Centróide (DMC)
##----------------------------------------------------------------------------#

# Inicializando as variaveis distancia e classe
d <- c()
resultado <- c()

# Calculando as distancias de cada linha do teste para todas as linhas do treino
for(i in 1:nteste){
  for(j in 1:6){
    d[j] <- Norm(as.matrix(dados.teste[i,1:34] - dados.centroides[[j]]))
  }
  if (which.min(d) == 1) {
    resultado[i] <- "1"
  }
  if (which.min(d) == 2){
    resultado[i] <- "2"
  }
  if (which.min(d) == 3) {
    resultado[i] <- "3"
  }
  if (which.min(d) == 4) {
    resultado[i] <- "4"
  }
  if (which.min(d) == 5) {
    resultado[i] <- "5"
  }
  if (which.min(d) == 6) {
    resultado[i] <- "6"
  }
}

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado, dados.teste.classes)
names(class.comparison) <- c("Respostas Previstas", "Respostas Observadas")
#View(class.comparison)

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao_dmc <- table(resultado,dados.teste.classes)
taxa_acertos <- accuracy(matriz_confusao_dmc)

}
# Plotando os dados de treino
plot(dados.treino$X3.1,dados.treino$X0.10,pch = 21,
     bg = c('blue','green','red','black','gray','yellow'))

# #################################################################### #
##                              RESULTADOS                            ##
# #################################################################### #
media <- mean(taxa_acertos)
mediana <- median(taxa_acertos)
minimo <- min(taxa_acertos)
maximo <- max(taxa_acertos)
desvio_padrao <- sd(taxa_acertos)

# Criando uma tabela com as medidas descritivas
tabela <- rbind(media,mediana,minimo,maximo,desvio_padrao); tabela

# Salvando uma tabela em um arquivo .txt
write.csv(tabela, 'Resultados_DMC_Dermatology.txt',row.names = TRUE)
