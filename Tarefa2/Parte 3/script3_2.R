
# script 3_1 tarefa 2

setwd('~/Desktop/Link to AD2/analytics2/Tarefa2')

# -------------------------------------------------------------
#  1. Baixe os dados de treino e teste.

graduados.treino <- read.csv('Parte 3/graduados_treino_model2.csv', sep=' ')

# -------------------------------------------------------------
# 3. Usando todas as variáveis disponíveis (disciplinas do primeiro e segundo período), 
#     use validação cruzada (nos dados de treino) para tunar um modelo de regressão
#     Ridge.

library(ISLR)
library(caret)
library(dplyr)

adapted.graduados.treino <- graduados.treino %>%
  select(Cálculo.Diferencial.e.Integral.I,
         Álgebra.Vetorial.e.Geometria.Analítica,
         Programação.I,
         Introdução.à.Computação,
         Leitura.e.Produção.de.Textos,
         Laboratório.de.Programação.I,
         Programação.II, 
         Cálculo.Diferencial.e.Integral.II,
         Matemática.Discreta,
         Laboratório.de.Programação.II,
         Teoria.dos.Grafos,
         Fundamentos.de.Física.Clássica,
         cra,
         ALU_NOVAMATRICULA)
rownames(adapted.graduados.treino) <- adapted.graduados.treino$ALU_NOVAMATRICULA
adapted.graduados.treino$ALU_NOVAMATRICULA <- NULL


set.seed(825)
fitControl <- trainControl(method = "cv",
                           number = 10)
# Set seq of lambda to test
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))


ridge <- train(cra~., data = adapted.graduados.treino,
               method='ridge',
               trControl = fitControl,
               tuneGrid = lambdaGrid,
               preProcess=c('center', 'scale')
)
ridge
#lambda encontrado: lambda = 0.2154435

graduados.teste <- read.csv('Parte 3/graduados_teste_model2.csv', sep=' ')

adapted.graduados.teste <- graduados.teste %>%
  select(Cálculo.Diferencial.e.Integral.I,
         Álgebra.Vetorial.e.Geometria.Analítica,
         Programação.I,
         Introdução.à.Computação,
         Leitura.e.Produção.de.Textos,
         Laboratório.de.Programação.I,
         Programação.II, 
         Cálculo.Diferencial.e.Integral.II,
         Matemática.Discreta,
         Laboratório.de.Programação.II,
         Teoria.dos.Grafos,
         Fundamentos.de.Física.Clássica,
         cra,
         ALU_NOVAMATRICULA)
rownames(adapted.graduados.teste) <- adapted.graduados.teste$ALU_NOVAMATRICULA
adapted.graduados.teste$ALU_NOVAMATRICULA <- NULL

ridge.pred <- predict(ridge, adapted.graduados.teste)

sqrt(mean(ridge.pred - adapted.graduados.teste$cra)^2)
# RSME = 0.8991327


# -----------------------------------------------------------------------
# 4. Mesmo que o item acima mas usando um modelo de regressão Lasso.

lasso <- train(cra ~., adapted.graduados.treino,
               method='lasso',
               trControl = fitControl,
               tuneGrid = lambdaGrid,
               preProc=c('scale','center'))
lasso
# lambda encontrado: 0.9

predict.enet(lasso$finalModel, type='coefficients', s=lasso$bestTune$fraction, 
             mode='fraction')
# eliminou Calculo I e Programação I
# Lab Programacao I => efeito negativo

lasso.pred <- predict(lasso, adapted.graduados.teste)
sqrt(mean(lasso.pred - adapted.graduados.teste$cra)^2)
# RSME = 0.7726666

plot(varImp(lasso))
plot(varImp(ridge))

# -----------------------------------------------------------------------
# 5. Compare os dois modelos nos dados de teste em termos de RMSE.
# 
#  RMSE (ridge) = 0.8991327
#  RMSE (lasso) = 0.7726666
# 
#  Observa-se que a regressão utilizando o método de Ridge têm um erro maior que a
#  regressão que utiliza o método de Lasso.


# -------------------------------------------------------------------------------
# 6. Quais as variáveis mais importantes segundo o modelo de regressão Lasso? 
#    Alguma variável foi descartada? Quais?
#
#    Pelo plot acima, verifica-se que as variáveis mais importantes para o modelo
#    são Teoria dos Grafos, Programação II e Laboratório de Programação II.
#  
#    As seguintes variáveis foram descartadas:
#     . Cálculo.Diferencial.e.Integral.I
#     . Programação.I
#
#    OBS.: verificou-se que a disciplina Lab de Programação I tem um efeito negativo
#    no cálculo do cra
#

# -------------------------------------------------------------------------------
# 7. Re-treine o melhor modelo (dessa vez nos dados de treino sem validação cruzada) 
#    e reporte o RMSE no teste.

set.seed(825) # for reproducing these results
lasso2 <- train(cra ~., adapted.graduados.treino,
                method='lasso')
lasso2
# lambda encontrado: 0.5

predict.enet(lasso2$finalModel, type='coefficients', s=lasso2$bestTune$fraction, 
             mode='fraction')
# eliminou:
# Calculo I
# Programação I
# Lab Programacao I
# Fund. de Fisica Classica
# Calculo II

lasso2.pred <- predict(lasso2, adapted.graduados.teste)
sqrt(mean(lasso2.pred - adapted.graduados.teste$cra)^2)
# RSME = 0.2988228

#   RMSE(lasso2) sem validação cruzada: 0.2988228


# -------------------------------------------------------------------------
# 8. Use o modelo treinado em 6 e aplique nos dados de teste que vamos disponibilizar.





# -------------------------------------------------------------------------
# 9. Crie novos atributos a partir dos existentes para tentar melhorar o seu modelo.





