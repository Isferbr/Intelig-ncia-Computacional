# -------------------------- Questão 02 -------------------------------#
# ---------------------------------------------------------------------#
# ------------- Classificador Perceptron Multicamandas-----------------#
# ---------------------------------------------------------------------#
# Autor: Ismael Fernandes Brito
# Data: 27/06/2022
# Disciplina Inteligência Artificial - ADS IFCE Canindé

# Limpando Console e Enviroment
cat("\014") # Equivalente Ctrl+L
rm(list=ls())

# ------------------------------------------------------------------- #
#              Etapa 1 - Carregando o dataset "parkinson's"
# ------------------------------------------------------------------- #
dados <- read.csv('parkinsons.csv', header = TRUE, sep = ",")
#View(dados)  

# Plotar os dados das classes (0 - Saudável e 1 - DP)
#plot(dados$Jitter.DDP, dados$RPDE, pch=21,
#bg=c("green","blue")[as.numeric(dados$status)],
#xlab = 'Jitter.DDP', ylab = 'RPDE')
#title('Conjunto de Dados Parkinson')

# ------------------------------------------------------------------- #
#  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
# ------------------------------------------------------------------- #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ndados <- nrow(dados) # numero de dados
ncaracteristicas <- ncol(dados) - 2 # numero de caracteristicas
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino

# Normalizando os dados
nor <-function(x){(x-min(x))/(max(x)-min(x))}
dados.norm <- as.data.frame(lapply(dados[,2:23], nor))

taxa_acertos <- c()
Nr <- 10 # numero de rodadas de treinamento/teste
for (r in 1:Nr){
  
  # Definindo dados para treinamento e teste
  indices_treino <- sample(ndados, ntreino, replace=FALSE)
  dados.treino <- dados.norm[indices_treino,]
  #View(dados.treino)
  dados.teste <- dados.norm[-indices_treino,]
  #View(dados.teste)
  
  # Definindo as classes para treinamento e teste
  dados.treino.classes <- dados[indices_treino,24]
  dados.teste.classes <- dados[-indices_treino,24]
  
  # -------------------------------------------------------------------------- #
  #                      Etapa 3 - Treinamento
  # -------------------------------------------------------------------------- #
  library(class)
  
  # Passo de aprendizagem
  passo <- 0.05
  # Número de épocas
  nepocas = 1000;
  # Número de classes
  nclasses = 2
  # Vetor com erro quadrático em cada época de treinamento
  eqm <- numeric(nepocas)
  # Neurônio da camada oculta
  #q <- 2*ncaracteristicas + 1 # Regra de Kolmogorov
  q <- round((ncaracteristicas + nclasses)/2) # Regra da média para 
  
  # Matrix de pesos com valores aleatórios
  # Camada Oculta
  W <- matrix( runif((ncaracteristicas+1)*q), ncaracteristicas+1, q)
  #W2 <- matrix( runif((q+1)*q2), (q+1) , q2) #2a camada oculta
  
  # Camada de Saída
  M <- matrix( runif((q+1)*nclasses), q+1, nclasses)
  
  # Definindo as saídas desejadas de treino
  d <- matrix(-1, nclasses, ntreino)
  d[1,dados.treino.classes == '0'] <- 1
  d[2,dados.treino.classes == '1'] <- 1
  #View(d)
  
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
      X <- c(-1, as.numeric(dados.treino[t,]))
      
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
  
  # -------------------------------------------------------------------------- #
  #                         Etapa 3 - Teste
  # -------------------------------------------------------------------------- #
  
  # Definindo as saídas desejadas de teste
  dteste <- matrix(-1, nclasses, nteste)
  dteste[1,dados.teste.classes == '0'] <- 1
  dteste[2,dados.teste.classes == '1'] <- 1
  #View(dteste)
  
  # Resultado do Teste
  resultado <- c()
  
  for(t in 1:nteste){
    X <- c(-1, as.numeric(dados.teste[t,]))
    
    # Camada Oculta - calculando a saída
    z <- perceptron(X,W,2)
    
    # Entrada da camada saída
    Z <- c(-1, as.numeric(z))
    
    # Camada Saída - calculando saída 
    o <- perceptron(Z,M,2)
    
    if (which.max(o) == 1) {
      resultado[t] <- '0'
    }
    if (which.max(o) == 2){
      resultado[t] <- '1'
    }
    
  }
  
  # Função para medir a acurácia
  accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
  
  # Criando uma tabela comparando o resultado previsto com o observado (teste)
  class.comparison <- data.frame(resultado, dados.teste.classes)
  names(class.comparison) <- c("Resultado Previsto", "Resultado Observado")
  #View(class.comparison)
  
  # Criando a matriz de confusão e calculando a taxa de acerto
  matriz_confusao <- table(resultado,dados.teste.classes)
  matriz_confusao
  taxa_acertos[r] <- accuracy(matriz_confusao)
  
}

# -------------------------------------------------------------------- #
#                               RESULTADOS                             #
# -------------------------------------------------------------------- #
media <- mean(taxa_acertos)
mediana <- median(taxa_acertos)
minimo <- min(taxa_acertos)
maximo <- max(taxa_acertos)
desvio_padrao <- sd(taxa_acertos)

# Criando uma tabela com as medidas descritivas
tabela <- rbind(media,mediana,minimo,maximo,desvio_padrao); tabela

# Salvando uma tabela em um arquivo .txt
write.csv(tabela, 'Resultados_MLP_Parkinson.txt',row.names = TRUE)

# Salvando a matriz de confusao em um arquivo.txt
write.csv(matriz_confusao, 'Matriz_Confusao_MLP_Parkinson.txt',row.names = TRUE)
