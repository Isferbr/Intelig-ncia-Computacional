# Perceptron Multicamadas - Portas XOR/XNOR
# Inteligencia Aplicada - Redes Neurais
# Autor: Ismael Fernandes Brito
# Data: 18/06/2022

# Limpando Console e Enviroment
cat("\014") # Equivalente Ctrl+L
rm(list=ls()) 

## ------------------------------------------------------------------ #
##               Etapa 1 - Carregando o dataset "XOR" ou XNOR
## ------------------------------------------------------------------ #
# Caso queira testar a outra porta lógica é só comentar a atual e descomentar a outra e rodar novamente
# dados da porta lógica XOR
dados <- read.csv('xor.csv', header= TRUE, sep = ",")
# dados da porta lógica XNOR
#dados <- read.csv('xnor.csv', header= TRUE, sep = ",")
View(dados)

# Vetor das respostas desejadas da Porta Lógica
d <- dados[,3]

# Gráfico da porta lógica XOR
# Plotar os dados das classes (x1, x2)
plot(dados$x1, dados$x2, pch=21,
     bg=c("red")[as.numeric(dados$XOR)],
     xlab = 'X1', ylab = 'X2')
title('Porta Lógica XOR')

# Gráfico da porta lógica XNOR
# Plotar os dados das classes (x1, x2)
#plot(dados$x1, dados$x2, pch=21,
#     bg=c("red")[as.numeric(dados$XNOR)],
#     xlab = 'X1', ylab = 'X2')
#title('Porta Lógica XNOR')

## ------------------------------------------------------------------ #
##  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
## ------------------------------------------------------------------ #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ncaracteristicas <- ncol(dados) - 1 # numero de características
# Número de classes
nclasses = 2
# Neurônio da camada oculta
q <- 2*ncaracteristicas + 1 # Regra de Kolmogorov

# Matrix de pesos com valores aleatórios
# Camada Oculta
W <- matrix(runif((ncaracteristicas+1)*q), ncaracteristicas+1, q)

# Camada de Saída
M <- matrix(runif((q+1)*nclasses), q+1, nclasses)

# ------------------------------------------------------------------------- #
#                          FASE DE APRENDIZAGEM
# ------------------------------------------------------------------------- #
library(class)

# Passo de aprendizagem
passo <- 0.01
# Número de épocasa
nEpocas = 1000;
# Vetor com erro quadrático em cada época de treinamento
eqm <- numeric(nEpocas)

# Função sigmoide
sigmoide <- function(x){
  y <- (1/(1 + exp(-x)))
}

# Função Tangente Hiperbolica
tanh <- function(x){
  z <- 0.5*(1-x^2)
}

# Função para calcular delta
delta <- function(x){
  z <- x*(1-x)
}

# Etapa de Treinamento
for(i in 1:nEpocas){
  # Variavel para soma dos erros
  eq <- 0
  # Resultado do Teste
  resultado <- c()
  for(t in 1:4){
    # Entrada da camada oculta
    X <- c(-1, as.numeric(dados[t,1:2]))
    
    # Sentido Direto
    # ----------------------------------------------------------------------
    # Camada Oculta - calculando a saída
    z <- sigmoide(X %*% W)
  
    # Entrada da camada saída
    Z <- c(-1, as.numeric(z))

    # Camada Saída - calculando saída
    o <- sigmoide(Z %*% M)
    
    # Sentido Inverso - Backpropagation
    #-----------------------------------------------------------------------
    # Calculando o erro da saída
    erroSaida <- d[t] - o
    deltaSaida <- delta(o)
    saida <- erroSaida*deltaSaida
    
    # Calculado o erro da camada oculta
    erroOculta <- saida %*% t(M[2:(q+1),])
    deltaOculta <- delta(z)
    oculta <- erroOculta*deltaOculta
    
    # Atualizando os pesos M da camada de saída
    M <- M + passo * t(t(saida) %*% Z )
    # Atualizando os pesos W da camada de oculta
    W <- W + passo * t(t(oculta) %*% X)
    
    # Acumula o Erro Quadrático Médio
    eq = eq + sum(0.5*erroSaida^2);
    
    # Rotula a saida do neuronio da camada de saida
    if (which.max(o) == 1) {
      resultado[t] <- "0"
    }
    if (which.max(o) == 2){
      resultado[t] <- "1"
    }
  }
  eqm[i] <- eq/4
}

# Plotando o gráfico
plot(1:nEpocas, eqm, xlab = 'Época de Aprendizagem', ylab = 'Erro', type = 'l', col = 'blue')
title('Erro Quadrático Médio')

# ------------------------------------------------------------------------- #
#                                RESULTADOS                                 # 
# ------------------------------------------------------------------------- # 

# Função para medir a acurácia
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado, dados[,3])
names(class.comparison) <- c("Resultado Previsto", "Resultado Observado")
View(class.comparison)

# Criando a matriz de confusão e calculando a taxa de acerto
matriz_confusao <- table(resultado, dados[,3])
matriz_confusao
accuracy(matriz_confusao)

# Plotando a reta de separaçao
vx1 <- seq(-1.5,1.5,0.1)
vx2 <- -1*(W[,2]/W[,3])*vx1 + W[,1]
plot(vx1, vx2, type = 'l', col = 'red')

# Plotando os pontos
x1 <- dados$x1
x2 <- dados$x2
points(x1, x2, col = 'blue')
title('Pontos e Reta')
