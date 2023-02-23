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
set.seed(1234)
ndados <- nrow(iris) # numero de dados
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino 

# Normalizando os dados
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
iris.norm <- as.data.frame( lapply(iris[,1:4], nor) )

# Definindo dados para treinamento e teste
indices_treino <- sample(ndados, ntreino, replace=F)
iris.treino <- iris.norm[indices_treino,]
View(iris.treino)
iris.teste <- iris.norm[-indices_treino,]
View(iris.teste)

# Definindo as classes para treinamento e teste
iris.treino.classes <- iris[indices_treino,5]
iris.teste.classes <- iris[-indices_treino,5]

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
resultado_nn <- knn1(iris.treino,iris.teste,cl=iris.treino.classes)

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado_nn, iris.teste.classes)
names(class.comparison) <- c("Espécies Previstas", "Espécies Observadas")
View(class.comparison)

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao_nn <- table(resultado_nn,iris.teste.classes)
accuracy(matriz_confusao_nn)
