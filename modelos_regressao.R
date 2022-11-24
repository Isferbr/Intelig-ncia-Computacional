##
## Avaliação N1
##
## Autor: Ismael Fernandes
##
## Disciplina: Inteligência Artificial
## Curso: Análise e Desenvolvimento de Sistemas
## IFCE - Campus Canindé

# Dados do Aerogerador
dados <- read.csv('aerogerador.csv', header = TRUE, sep = ";")
plot(dados$velocidade, dados$energia, xlab = 'Velocidade', ylab = 'Energia', col = 'blue')

# Separando os dados em treinamento e teste
# Treinamento = 80% das amostras
set.seed(9)
tr <- round(0.8*nrow(dados))
# Seleciona aleatoriamente os dados para treinamento
treino <- sample(nrow(dados), tr, replace = FALSE)
dados.treino <- dados[treino,]
View(dados.treino)

# Seleciona aleatoriamente os dados para teste
dados.teste <- dados[-treino,]
View(dados.teste)

## Aplicando o Modelo  de Regressão

# ----------------------------------
#     Modelo de Regressão Linear
# ----------------------------------
lr <- lm(energia ~ velocidade, dados.treino)
summary(lr) # resumo dos dados do modelo de regressão linear

# Gráfico para Reta Linear
# Gráfico 1
par(mfrow = c(1,1))
plot(energia ~ velocidade, dados.treino, col = 'navy', pch = 20, cex = 1.5,
     xlab = 'Velocidade', ylab = 'Energia', main = 'Energia X Velocidade (Treinamento)')
abline(lr, col = 'red', lwd = 2)
grid(lwd = 2)

um
# Gráfico 2
library(ggpubr)
library(ggplot2)
ggplot(dados.treino, aes(x = velocidade, y = energia)) +
  geom_point(color = 'orange', lwd = 2) +
  geom_smooth(method = 'lm', formula = y ~ x, col = 'seagreen') +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label.., 
                                          sep = "*plain(\",\")~~")), 
                        label.x = 0, label.y = 420) + 
  ggtitle('Energia X Velocidade (Treinamento)') +
  xlab('Velocidade') +
  ylab('Energia') + 
  theme(plot.title = element_text(hjust = 0.5))

# Validando o modelo de Regressão Linear

# Calculando o valor previsto pelo modelo
res.test <- predict(lr, newdata = data.frame(velocidade = dados.teste[,1]))
# imprimindo o valor previsto com valor real
dados.teste[,2]
res.test

# Plotando os dados de teste e o resultado de teste
# Gráfico 1
plot(energia ~ velocidade, dados.teste, col = 'red', pch = 20, cex = 1.5)
abline(lr, col = 'gold3', lwd = 2)
grid(lwd = 2)

# Gráfico 2
ggplot() + 
  geom_point(aes(x = dados.teste$velocidade, y = dados.teste$energia),
             color = 'red', lwd = 2) + 
  geom_smooth(method = 'lm', formula = y ~ x, col = 'gold3',
              aes(x = dados.teste$velocidade, y = dados.teste$energia)) + 
  ggtitle('Energia X Velocidade (Teste)') +
  xlab('Velocidade') +
  ylab('Energia') + 
  theme(plot.title = element_text(hjust = 0.5))

# Comparando a performance do modelo de Regressão Linear
teste1 <- data.frame(obs = dados.teste[,2], pred = res.test)
View(teste1)
library(caret)
defaultSummary(teste1)

# ---------------------------------------------------------------
#           Modelo de Regressão Não Linear (Polinomial)
# ---------------------------------------------------------------
nlr <- lm(energia ~ poly(velocidade, 2, raw = TRUE), dados.treino)
summary(nlr) # resumo do modelo polinomial

# Gráficos para Reta Linear
# Gráfico 1
par(mfrow = c(1,1))
plot(energia ~ velocidade,  dados.treino, col = 'deepskyblue3', pch = 20, cex = 1.5)
x1 <- seq(0,250, by = 0.1)
lines(x1, predict(nlr, newdata = data.frame(velocidade = x1), col = 'mediumvioletred', lwd = 2))
grid(lwd = 2)

# Gráfico 2
library(ggpmisc)
ggplot(dados.treino, aes(x = velocidade, y = energia)) + 
  geom_point(color = 'deepskyblue3', lwd = 2) + 
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2, raw = TRUE), col = 'mediumvioletred') + 
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), 
                                 sep = "*plain(\",\")~~")), 
               formula = y ~ poly(x, 2, raw = TRUE),
               label.x = 0, label.y = 300) + 
  ggtitle('Energia X Velocidade (Treinamento)') +
  xlab('Velocidade') +
  ylab('Energia') + 
  theme(plot.title = element_text(hjust = 0.5))

# Validando o Modelo Polinomial
res.test2 <- predict(nlr, newdata = data.frame(velocidade = dados.teste[,1]))
dados.teste[,2]
res.test2

# Desempenho do Modelo de Segunda Ordem
teste2 <- data.frame(obs = dados.teste[,2], pred = res.test2)
View(teste2)
defaultSummary(teste2)

# Plotando os dados de teste e o resultado de teste
# Gráfico 1
plot(energia ~ velocidade, dados.teste, col = 'red', pch = 20, cex = 1.5,
     main = 'Energia X Velocidade (Teste)')
x1 <- seq(0,250, by = 0.1)
lines(x1, predict(nlr, newdata = data.frame(velocidade = x1), col = 'sandybrown', lwd = 2))
grid(lwd = 2)

# Gráfico 2
ggplot() + 
  geom_point(aes(x = dados.teste$velocidade, y = dados.teste$energia),
             color = 'red', lwd = 2) + 
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2, raw = TRUE), col = 'sandybrown',
              aes(x = dados.teste$velocidade, y = dados.teste$energia)) + 
  ggtitle('Energia X Velocidade (Teste)') + 
  xlab('Velocidade') + 
  ylab('Energia') + 
  theme(plot.title = element_text(hjust = 0.5))

# -----------------------------------------------------------------------------------------
nlr2 <- lm(energia ~ poly(velocidade, 3, raw = TRUE), dados.treino)
summary(nlr2) # resumo do modelo polinomial

# Gráficos para Reta Linear
# Gráfico 1
par(mfrow = c(1,1))
plot(energia ~ velocidade,  dados.treino, col = 'deepskyblue3', pch = 20, cex = 1.5)
x1 <- seq(0,250, by = 0.1)
lines(x1, predict(nlr2, newdata = data.frame(velocidade = x1), col = 'mediumvioletred', lwd = 2))
grid(lwd = 2)

# Gráfico 2
ggplot(dados.treino, aes(x = velocidade, y = energia)) + 
  geom_point(color = 'deepskyblue3', lwd = 2) + 
  geom_smooth(method = 'lm', formula = y ~ poly(x, 3, raw = TRUE), col = 'mediumvioletred') + 
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(adj.rr.label), 
                                 sep = "*plain(\",\")~~")), 
               formula = y ~ poly(x, 3, raw = TRUE),
               label.x = 0, label.y = 300) +  
  ggtitle('Energia X Velocidade (Treinamento)') +
  xlab('Velocidade') +
  ylab('Energia') + 
  theme(plot.title = element_text(hjust = 0.5))

# Validando o Modelo Polinomial
res.test3 <- predict(nlr2, newdata = data.frame(velocidade = dados.teste[,1]))
dados.teste[,2]
res.test3

# Desempenho do Modelo de Terceira Ordem
teste3 <- data.frame(obs = dados.teste[,2], pred = res.test3)
View(teste3)
defaultSummary(teste3)

# Plotando os dados de teste e o resultado de teste
# Gráfico 1
plot(energia ~ velocidade, dados.teste, col = 'red', pch = 20, cex = 1.5,
     main = 'Energia X Velocidade (Teste)')
x1 <- seq(0,250, by = 0.1)
lines(x1, predict(nlr2, newdata = data.frame(velocidade = x1), col = 'sandybrown', lwd = 2))
grid(lwd = 2)

# Gráfico 2
ggplot() + 
  geom_point(aes(x = dados.teste$velocidade, y = dados.teste$energia),
             color = 'red', lwd = 2) + 
  geom_smooth(method = 'lm', formula = y ~ poly(x, 3, raw = TRUE), col = 'sandybrown',
              aes(x = dados.teste$velocidade, y = dados.teste$energia)) + 
  ggtitle('Energia X Velocidade (Teste)') + 
  xlab('Velocidade') + 
  ylab('Energia') + 
  theme(plot.title = element_text(hjust = 0.5))









