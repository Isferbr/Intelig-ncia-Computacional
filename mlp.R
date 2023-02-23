# ---------------------------------------------------------------------#
# ------------ Classificador Perceptron Multicamadas (MLP) ------------#
# ---------------------------------------------------------------------#
# @autor Raphael Carvalho
# @data 20/06/2022
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
nor <- function(x) { (x -min(x))/(max(x)-min(x)) }
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
passo <- 0.01
# Número de épocas
nepocas = 500;
# Número de classes
nclasses = 3
# Vetor com erro quadrático em cada época de treinamento
eqm <- numeric(nepocas)
# Neurônio da camada oculta
q <- 2*ncaracteristicas + 1 # Regra de Kolmogorov
#q <- round((ncaracteristicas + nclasses)/2) # Regra da média para 

# Matrix de pesos com valores aleatórios
# Camada Oculta
W <- matrix( runif((ncaracteristicas+1)*q), ncaracteristicas+1, q)
#W2 <- matrix( runif((q+1)*q2), (q+1) , q2) #2a camada oculta

# Camada de Saída
M <- matrix( runif((q+1)*nclasses), q+1, nclasses)

# Definindo as saídas desejadas
d <- matrix(-1, nclasses, ntreino)
d[1,iris.treino.classes == 'setosa'] <- 1
d[2,iris.treino.classes == 'versicolor'] <- 1
d[3,iris.treino.classes == 'virginica'] <- 1
View(d)

# Função sigmoide
sigmoide <- function(x){
  y <- (1/(1 + exp(-x)))
}

# Função para calcular o perceptron usando a função sigmoide
# Type 1: sigmoide
# Type 2: tangente hiperbólica
perceptron <- function(x, w, type){
  if(type==1){
    z <- sigmoide(x %*% w)
  }
  if(type==2){
    z <- tanh(x %*% w)
  }
  return(z)
}

# Função para calcular o perceptron usando a função sigmoide
perceptronSig <- function(x, w){
  z <- sigmoide(x %*% w)
  return(z)
}

# Função para calcular o perceptron usando a função tangente hiperbólica
perceptronTanh <- function(x, w){
  z <- tanh(x %*% w)
  return(z)
}

# Função para calcular delta
# Type 1: sigmoide
# Type 2: tangente hiperbólica
delta <- function(x, type){
  if(type==1){
    z <- x*(1-x)
  }
  if(type==2){
    z <- 0.5*(1-x^2)
  }
  return(z)
}

# Etapa de Treinamento
for (k in 1:nepocas){
  # Variavel para soma dos erros
  eq <- 0 
  
  for(t in 1:ntreino){
    # Entrada da camada oculta
    X <- c(-1, as.numeric(iris.treino[t,]))
    
    # Sentido Direto
    # -------------------------------------------
    # Camada Oculta - calculando a saída
    z <- perceptron(X,W,2)
    
    # Entrada da camada saída
    Z <- c(-1, as.numeric(z))
    
    # Camada Saída - calculando saída 
    o <- perceptron(Z,M,2)
    
    # Sentido Inverso - Backpropagation
    #-------------------------------------------
    # Calculando o erro da saída
    erroSaida <- d[,t] - o
    deltaSaida <- delta(o,2)
    saida <- erroSaida*deltaSaida
    
    # Calculado o erro da camada oculta
    erroOculta <- saida%*%t(M[2:(q+1),]) 
    #erroOculta <- t(M[2:(q+1),]%*%t(saida)) 
    deltaOculta <- delta(z,2)
    oculta <- erroOculta*deltaOculta
    
    # Atualizando os pesos M da camada de saída
    M <- M + passo * t(t(saida) %*% Z )
    # Atualizando os pesos W da camada de oculta
    W <- W + passo * t(t(oculta) %*% X )
    
    # Acumula o Erro Quadrático Médio
    eq = eq + sum(0.5*erroSaida^2);
  }
  eqm[k] <- eq/ntreino
}

plot(eqm,type='l', col='blue', xlab = 'Época de Treinamento', 
     ylab = 'EQM')
title('Erro Quadrático Médio')

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
  # Entrada da camada oculta
  X <- c(-1, as.numeric(iris.teste[t,]))
  
  # Camada Oculta - calculando a saída
  z <- perceptron(X,W,2)
  
  # Entrada da camada saída
  Z <- c(-1, as.numeric(z))
  
  # Camada Saída - calculando saída 
  o <- perceptron(Z,M,2)
  
  if (which.max(o) == 1) {
    resultado[t] <- "setosa"
  }
  if (which.max(o) == 2){
    resultado[t] <- "versicolor"
  }
  if (which.max(o) == 3) {
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
