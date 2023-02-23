# Perceptron Multicamadas - Porta XOR
# Inteligencia Aplicada - Redes Neurais
# Autor: Ismael Fernandes Brito
# Data: 18/06/2022

# Limpando Console e Enviroment
cat("\014") # Equivalente Ctrl+L
rm(list=ls()) 

# Entrada bias do neuronio - x0
x0 <- -1

# Vetores com os exemplos (0,0), (0,1), (1,0) e (1,1), usando -1 ao invés de 0
ex1 <- c(x0,-1,-1)
ex2 <- c(x0,-1,1)
ex3 <- c(x0,1,-1)
ex4 <- c(x0,1,1)
Xe <- cbind(ex1,ex2,ex3,ex4) # cada coluna é um exemplo x

# Vetor das respostas desejadas
d <- c(-1,1,1,-1)

# Vetor de Pesos - w0, w1 e w2
W <- runif(3)

# Vetor de Pesos - m0, m1 e m2
M <- runif(3)

# -----------------------------------------------# 
#             FASE DE APRENDIZAGEM               # 
# -----------------------------------------------# 

# Taxa de aprendizagem ou Passo de aprendizagem (eta)
passo <- 0.01
# Quantidade Maxima de epocas para aprendizagem
nEpocas <- 200
# Vetor com erro quadrático em cada época de treinamento
eqm <- numeric(nEpocas)

# Calculando as distancias de cada linha do teste para todas as linhas do treino
for(i in 1:nEpocas){
  soma <- 0
  for(t in 1:4){
    # Calculando a saída do neurônio
    u <- W %*% Xe[,t]
    # Calculando o erro e o erro quadrático
    erro <- d[t] - u
    soma <- soma + 0.5*erro^2
    # Atualizando os pesos W
    W <- W + passo * erro %*% Xe[,t]
  }
  eqm[i] <- soma/4
}

# Plotando o gráfico
plot(1:nEpocas, eqm, xlab = 'Época de Treinamento', ylab = 'Erro', type = 'l', col = 'blue')
title('Erro Quadrático Médio')

# ------------# 
#  RESULTADOS # 
# ------------# 

# Plotando a reta de separaçao
vx1 <- seq(-1.5,1.5,0.1)
vx2 <- -1*(W[,2]/W[,3])*vx1 + W[,1]
plot(vx1, vx2, type = 'l', col = 'red')

# Plotando os pontos
x1 <- Xe[2,]
x2 <- Xe[3,]
points(x1, x2, col = 'blue')
title('Pontos e Reta')
# ---------------------------------------------------------------------------- #
# Perceptron Simples - Porta XOR
# Inteligencia Aplicada - Redes Neurais
# Autor: Ismael Fernandes Brito
# Data: 18/06/2022

# Limpando Console e Enviroment
cat("\014") # Equivalente Ctrl+L
rm(list=ls()) 

# Entrada bias do neuronio - x0
x0 <- -1

# Vetores com os exemplos (0,0), (0,1), (1,0) e (1,1), usando -1 ao invés de 0
ex1 <- c(x0,-1,-1)
ex2 <- c(x0,-1,1)
ex3 <- c(x0,1,-1)
ex4 <- c(x0,1,1)
Xe <- cbind(ex1,ex2,ex3,ex4) #cada coluna é um exemplo x

# Vetor das respostas desejadas
d <- c(-1,1,1,-1)

# Vetor de Pesos - w0, w1 e w2
W <- runif(3)

# -----------------------------------------------# 
#             FASE DE APRENDIZAGEM               # 
# -----------------------------------------------# 

# Taxa de aprendizagem ou Passo de aprendizagem (eta)
passo <- 0.01
# Quantidade Maxima de epocas para aprendizagem
nEpocas <- 200
# Vetor com erro quadrático em cada época de treinamento
eqm <- numeric(nEpocas)

# Calculando as distancias de cada linha do teste para todas as linhas do treino
for(i in 1:nEpocas){
  soma <- 0
  for(t in 1:4){
    # Calculando a saída do neurônio
    u <- W %*% Xe[,t]
    # Calculando o erro e o erro quadrático
    erro <- d[t] - u
    soma <- soma + 0.5*erro^2
    # Atualizando os pesos W
    W <- W + passo * erro %*% Xe[,t]
  }
  eqm[i] <- soma/4
}

# Plotando o gráfico
plot(1:nEpocas, eqm, xlab = 'Época de Treinamento', ylab = 'Erro', type = 'l', col = 'blue')
title('Erro Quadrático Médio')

# ------------# 
#  RESULTADOS # 
# ------------# 

# Plotando a reta de separaçao
vx1 <- seq(-1.5,1.5,0.1)
vx2 <- -1*(W[,2]/W[,3])*vx1 + W[,1]
plot(vx1, vx2, type = 'l', col = 'red')

# Plotando os pontos
x1 <- Xe[2,]
x2 <- Xe[3,]
points(x1, x2, col = 'blue')
title('Pontos e Reta')
# ---------------------------------------------------------------------------- #
# Perceptron Simples - Porta XNOR
# Inteligencia Aplicada - Redes Neurais
# Autor: Ismael Fernandes Brito
# Data: 18/06/2022

# Limpando Console e Enviroment
cat("\014") # Equivalente Ctrl+L
rm(list=ls()) 

# Entrada bias do neuronio - x0
x0 <- -1

# Vetores com os exemplos (0,0), (0,1), (1,0) e (1,1), usando -1 ao invés de 0
ex1 <- c(x0,-1,-1)
ex2 <- c(x0,-1,1)
ex3 <- c(x0,1,-1)
ex4 <- c(x0,1,1)
Xe <- cbind(ex1,ex2,ex3,ex4) #cada coluna é um exemplo x

# Vetor das respostas desejadas
d <- c(1,-1,-1,1)

# Vetor de Pesos - w0, w1 e w2
W <- runif(3)

# -----------------------------------------------# 
#             FASE DE APRENDIZAGEM               # 
# -----------------------------------------------# 

# Taxa de aprendizagem ou Passo de aprendizagem (eta)
passo <- 0.01
# Quantidade Maxima de epocas para aprendizagem
nEpocas <- 200
# Vetor com erro quadrático em cada época de treinamento
eqm <- numeric(nEpocas)

# Calculando as distancias de cada linha do teste para todas as linhas do treino
for(i in 1:nEpocas){
  soma <- 0
  for(t in 1:4){
    # Calculando a saída do neurônio
    u <- W %*% Xe[,t]
    # Calculando o erro e o erro quadrático
    erro <- d[t] - u
    soma <- soma + 0.5*erro^2
    # Atualizando os pesos W
    W <- W + passo * erro %*% Xe[,t]
  }
  eqm[i] <- soma/4
}

# Plotando o gráfico
plot(1:nEpocas, eqm, xlab = 'Época de Treinamento', ylab = 'Erro', type = 'l', col = 'blue')
title('Erro Quadrático Médio')

# ------------# 
#  RESULTADOS # 
# ------------# 

# Plotando a reta de separaçao
vx1 <- seq(-1.5,1.5,0.1)
vx2 <- -1*(W[,2]/W[,3])*vx1 + W[,1]
plot(vx1, vx2, type = 'l', col = 'red')

# Plotando os pontos
x1 <- Xe[2,]
x2 <- Xe[3,]
points(x1, x2, col = 'blue')
title('Pontos e Reta')