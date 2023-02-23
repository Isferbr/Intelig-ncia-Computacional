# -------------------------- Questão 03 -------------------------------#
# ---------------------------------------------------------------------#
# -------------- Classificador Perceptron Simples ---------------------#
# ---------------------------------------------------------------------#
# Autor: Ismael Fernandes Brito
# Data: 18/06/2022
# Disciplina Inteligência Artificial - ADS IFCE Canindé

# Limpando Console e Enviroment
cat("\014") # Equivalente Ctrl+L
rm(list=ls())

# ------------------------------------------------------------------- #
#               Etapa 1 - Carregando o dataset "dermatology"
# ------------------------------------------------------------------- #
dados <- read.csv('dermatology.csv', header=T, sep = ",")
View(dados)  

# ------------------------------------------------------------------- #
#  Etapa 2 - Normalizando os dados e dividindo em treinamento e teste
# ------------------------------------------------------------------- #

# Dividir o conjunto em treinamento e teste
set.seed(1234)
ndados <- nrow(dados) # numero de dados
ncaracteristicas <- ncol(dados) - 1 # numero de caracteristicas
ptreino <- 0.8 # 80% para treinamento
ntreino <- round(ndados*ptreino)
nteste <- ndados - ntreino

# Normalizando os dados
nor <-function(x){(x-min(x))/(max(x)-min(x))}
dados.norm <- as.data.frame(lapply(dados[,1:34], nor))

taxa_acertos <- c()
Nr <- 10 # numero de rodadas de treinamento/teste
for (r in 1:Nr){

# Definindo dados para treinamento e teste
indices_treino <- sample(ndados, ntreino, replace=FALSE)
dados.treino <- dados.norm[indices_treino,]
View(dados.treino)
dados.teste <- dados.norm[-indices_treino,]
View(dados.teste)

# Definindo as classes para treinamento e teste
dados.treino.classes <- dados[indices_treino,35]
dados.teste.classes <- dados[-indices_treino,35]

# -------------------------------------------------------------------------- #
#                      Etapa 3 - Treinamento
# -------------------------------------------------------------------------- #
library(class)

# Passo de aprendizagem
passo <- 0.01
# Número de épocas
nepocas = 100;
# Número de classes
nclasses = 6
# Vetor com erro quadrático em cada época de treinamento
eqm <- numeric(nepocas)

# Matrix de pesos com valores aleatórios
W <- matrix(runif((ncaracteristicas+1)*nclasses), ncaracteristicas+1, nclasses)

# Definindo as saídas desejadas de treino
d <- matrix(-1, nclasses, ntreino)
d[1,dados.treino.classes == '1'] <- 1
d[2,dados.treino.classes == '2'] <- 1
d[3,dados.treino.classes == '3'] <- 1
d[4,dados.treino.classes == '4'] <- 1
d[5,dados.treino.classes == '5'] <- 1
d[6,dados.treino.classes == '6'] <- 1
View(d)

for (k in 1:nepocas){
  # Variavel para soma dos erros
  soma <- 0 
  
  for(t in 1:ntreino){
    X <- c(-1, as.numeric(dados.treino[t,]))
    
    # Calculando a saída do neurônio
    u <- X %*% W
    # Calculando o erro e o erro quadrático
    erro <- d[,t] - u
    # Atualizando os pesos W
    W <- W + passo * t(t(erro) %*% X)
  }
  #eqm[k] <- soma/ntreino
}

# -------------------------------------------------------------------------- #
#                        Etapa 3 - Teste
# -------------------------------------------------------------------------- #

# Definindo as saídas desejadas de teste
dteste <- matrix(-1, nclasses, nteste)
dteste[1,dados.teste.classes == '1'] <- 1
dteste[2,dados.teste.classes == '2'] <- 1
dteste[3,dados.teste.classes == '3'] <- 1
dteste[4,dados.teste.classes == '4'] <- 1
dteste[5,dados.teste.classes == '5'] <- 1
dteste[6,dados.teste.classes == '6'] <- 1
View(dteste)

# Resultado do Teste
resultado <- c()

for(t in 1:nteste){
  X <- c(-1, as.numeric(dados.teste[t,]))
  
  # Calculando a saída do neurônio
  u <- X %*% W
  
  if (which.max(u) == 1) {
    resultado[t] <- '1'
  }
  if (which.max(u) == 2){
    resultado[t] <- '2'
  }
  if (which.max(u) == 3) {
    resultado[t] <- '3'
  }
  if (which.max(u) == 4){
    resultado[t] <- '4'
  }
  if (which.max(u) == 5) {
    resultado[t] <- '5'
  }
  if (which.max(u) == 6){
    resultado[t] <- '6'
  }
}

# Função para medir a acurácia
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

# Criando uma tabela comparando o resultado previsto com o observado (teste)
class.comparison <- data.frame(resultado, dados.teste.classes)
names(class.comparison) <- c("Resultado Previsto", "Resultado Observado")
View(class.comparison)

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
#tabela <- rbind(media,mediana,minimo,maximo,desvio_padrao); tabela
