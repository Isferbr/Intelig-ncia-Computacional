# ---------------------------------------------------------------------#
# --------------- Classificador Vizinho mais Próximo ------------------#
# ---------------------------------------------------------------------#
# @autor Raphael Carvalho
# @data 02/05/2022
# @disciplina Inteligência Artifical - ADS IFCE Canindé

## ------------------------------------------------------------------ #
##               Etapa 1 - Carregando o dataset "iris
## ------------------------------------------------------------------ #
data("iris")
View(iris)

# Plotar os dados das classes (setosa, virgínica e versicolor)
plot(iris$Petal.Length, iris$Petal.Width, pch=21,
     bg=c("red","green","blue")[as.numeric(iris$Species)])
plot(iris[,1:4], pch=21, 
     bg=c("red","green","blue")[as.numeric(iris$Species)])

## ------------------------------------------------------------------ #
##  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
## ------------------------------------------------------------------ #

# Dividir o conjunto em treinamento e teste
ndados <- nrow(iris) # numero de dados
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino 

# Definindo dados para treinamento e teste
indices_treino <- sample(ndados, ntreino, replace=F)
iris.treino <- iris[indices_treino,]
iris.teste <- iris[-indices_treino,]

# Definindo as classes para teste
iris.teste.classes <- iris.teste[,5]

# Calculando os centroides das classes
iris.treino.setosa <- iris.treino[iris.treino$Species == "setosa",1:4]
iris.treino.versicolor <- iris.treino[iris.treino$Species == "versicolor",1:4]
iris.treino.virginica <- iris.treino[iris.treino$Species == "virginica",1:4]

centroide.setosa <- apply(iris.treino.setosa,2,mean)
centroide.versicolor <- apply(iris.treino.versicolor,2,mean)
centroide.virginica <- apply(iris.treino.virginica,2,mean)
iris.centroides <- list(centroide.setosa,centroide.versicolor,centroide.virginica)

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
  for(j in 1:3){
      d[j] <- Norm(as.matrix(iris.teste[i,1:4] - iris.centroides[[j]]))
  }
  if (which.min(d) == 1) {
    resultado[i] <- "setosa"
  }
  if (which.min(d) == 2){
    resultado[i] <- "versicolor"
  }
  if (which.min(d) == 3) {
    resultado[i] <- "virginica"
  }
}

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao_dmc <- table(resultado,iris.teste.classes)
accuracy(matriz_confusao_dmc)

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado, iris.teste.classes)
names(class.comparison) <- c("Espécies Previstas", "Espécies Observadas")
View(class.comparison)

# Plotando os dados de teste 
plot(iris.teste$Petal.Length, iris.teste$Petal.Width, pch=21,
     bg=c("red","green","blue")[as.numeric(iris.teste$Species)])
points(centroide.setosa[3],centroide.setosa[4], pch = 8, col="red")
points(centroide.versicolor[3],centroide.versicolor[4], pch = 8, col="green")
points(centroide.virginica[3],centroide.virginica[4], pch = 8, col="blue")
