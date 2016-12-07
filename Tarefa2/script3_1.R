
# script 3_1 tarefa 2

setwd('~/workspaces/analytics2/Tarefa2')

# -------------------------------------------------------------
#  1. Baixe os dados de treino e teste.

graduados.treino <- read.csv('graduados_treino.csv')
graduados.teste <- read.csv('graduados_teste.csv')

# -------------------------------------------------------------
# 2. Usando todas as variáveis disponíveis (disciplinas do primeiro e segundo período), 
#     use validação cruzada (nos dados de treino) para tunar um modelo de regressão Ridge.

set.seed(825) # for reproducing these results
ridge <- train(cra ~., data = graduados.treino,
               method='ridge',
               lambda = 4,
               preProcess=c('scale', 'center'))

ridge

ridge.pred <- predict(ridge, graduados.teste)

sqrt(mean(ridge.pred - graduados.teste$cra)^2)


