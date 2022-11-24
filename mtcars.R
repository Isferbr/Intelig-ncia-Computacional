##
## Avaliação N1 - Questão 04
##
## Autor: Ismael Fernandes
##
## Disciplina: Inteligência Artificial
## Curso: Análise e Desenvolvimento de Sistemas
## IFCE - Campus Canindé

# Passo 1: Carregar os pacotes que serão utilizados
if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, scatterplot3d)

library(scatterplot3d)

# Passo 2: Carregar o banco de dados
data(mtcars)           # Carregamento dos dados
View(mtcars)           # Visualização dos dados em janela separada
glimpse(mtcars)        # Visualização de um resumo dos dados

# Modelo de Regressão Linear Múltipla
# Estimação do modelo de regressão linear múltipla
mlr <- lm(mpg ~ hp + wt, mtcars)
coef <- coefficients(mlr)
summary(mlr)

## Outliers nos resíduos
summary(rstandard(mlr))

## Ausência de Multicolinearidade
pairs.panels(mtcars)

## Obtenção do IC 95% para os coeficientes
confint(mlr)

## Passo 5: Gráfico de Dispersão
graph <- scatterplot3d(mtcars$mpg ~ mtcars$hp + mtcars$wt, 
                       main = 'Gráfico de Dispersão',
                       pch = 16, angle = 30, color = 'steelblue', box = FALSE,
                       xlab = 'Potência do motor (hp)', ylab = 'Peso do veículo (wt)', zlab = 'Consumo de combustível (mpg)')
graph$plane3d(mlr, col = 'black', draw_polygon = TRUE)


##################### Métodos de seleção de modelos ###########################

pacman::p_load(MASS)


mod.inicial <- lm(mpg ~ hp + wt, data = mtcars)
mod.simples <- lm(mpg ~ 1, data = mtcars)

stepAIC(mod.inicial, scope = list(upper = mod.inicial,
                                  lower = mod.simples), direction = "backward")


# Distribuição dos resíduos
plot(mlr, which = 1)

# Normalidade dos resíduos
plot(mlr, which = 2)




