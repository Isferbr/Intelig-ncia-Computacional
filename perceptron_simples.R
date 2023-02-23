# ---------------------------------------------------------------------#
# -------------- Classificador Perceptron Simples ---------------------#
# ---------------------------------------------------------------------#
# @autor Raphael Carvalho
# @data 01/06/2022
# @disciplina Inteligência Artifical - ADS IFCE Canindé

# Limpando Console e Enviroment
cat("\014") # Equivalente Ctrl+L
rm(list=ls()) 

## ------------------------------------------------------------------ #
##               Etapa 1 - Carregando o dataset "iris
## ------------------------------------------------------------------ #
data("iris")
View(iris)

# Plotar os dados das classes (setosa, virgínica e versicolor)
plot(iris$Petal.Length, iris$Petal.Width, pch=21,
     bg=c("red","green","blue")[as.numeric(iris$Species)],
     xlab = 'Comprimento da Pétala', ylab = 'Largura da Pétala')
title('Conjunto de Dados Iris')
plot(iris[,1:4], pch=21, 
     bg=c("red","green","blue")[as.numeric(iris$Species)])

## ------------------------------------------------------------------ #
##  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
## ------------------------------------------------------------------ #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ndados <- nrow(iris) # numero de dados
ncaracteristicas <- ncol(iris) - 1 # numero de características
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

## ------------------------------------------------------------------------- #
##                     Etapa 3 - Treinamento
## ------------------------------------------------------------------------- #
library(class)

# Passo de aprendizagem
passo <- 0.05
# Número de épocas
nepocas = 2000;
# Número de classes
nclasses = 3
# Vetor com erro quadrático em cada época de treinamento
eqm <- numeric(nepocas)

# Matrix de pesos com valores aleatórios
W <- matrix( runif((ncaracteristicas+1)*nclasses), ncaracteristicas+1, nclasses)

# Definindo as saídas desejadas
d <- matrix(-1, nclasses, ntreino)
d[1,iris.treino.classes == 'setosa'] <- 1
d[2,iris.treino.classes == 'versicolor'] <- 1
d[3,iris.treino.classes == 'virginica'] <- 1
View(d)

for (k in 1:nepocas){
  # Variavel para soma dos erros
  soma <- 0 

  for(t in 1:ntreino){
    X <- c(-1, as.numeric(iris.treino[t,]))
    
    # Calculando a saída do neurônio
    u <- X %*% W
    # Calculando o erro e o erro quadrático
    erro <- d[,t] - u
    # Atualizando os pesos W
    W <- W + passo * t(t(erro) %*% X)
  }
  #eqm[k] <- soma/ntreino
}

## ------------------------------------------------------------------------- #
##                     Etapa 3 - Teste
## ------------------------------------------------------------------------- #

# Definindo as saídas desejadas de teste
dteste <- matrix(-1, nclasses, nteste)
dteste[1,iris.teste.classes == 'setosa'] <- 1
dteste[2,iris.teste.classes == 'versicolor'] <- 1
dteste[3,iris.teste.classes == 'virginica'] <- 1
View(dteste)

# Resultado do Teste
resultado <- c()

for(t in 1:nteste){
  X <- c(-1, as.numeric(iris.teste[t,]))

  # Calculando a saída do neurônio
  u <- X %*% W
  
  if (which.max(u) == 1) {
    resultado[t] <- "setosa"
  }
  if (which.max(u) == 2){
    resultado[t] <- "versicolor"
  }
  if (which.max(u) == 3) {
    resultado[t] <- "virginica"
  }
}
  
# Função para medir a acurácia
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado, iris.teste.classes)
names(class.comparison) <- c("Espécies Previstas", "Espécies Observadas")
View(class.comparison)

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao <- table(resultado,iris.teste.classes)
matriz_confusao
accuracy(matriz_confusao)
