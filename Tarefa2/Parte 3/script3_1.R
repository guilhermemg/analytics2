
# script 3_1 tarefa 2

#setwd('~/Desktop/Link to AD2/analytics2/Tarefa2')
setwd('~/workspaces/analytics2/Tarefa2')

# --------------------------------------------------------------------------------------------------------------------
#  1. Baixe os dados de treino e teste.

graduados.treino <- read.csv('Parte 3/graduados_treino_model2.csv', sep=' ')

# --------------------------------------------------------------------------------------------------------------------
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

# lambda = 0.2154435


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

# RSME = 0.8564798

# ----------------------------------------------------------------------------------------------------------------------
# 4. Mesmo que o item acima mas usando um modelo de regressão Lasso.


lasso <- train(cra ~., adapted.graduados.treino,
               method='lasso',
               preProc=c('scale','center'),
               trControl=fitControl)
lasso

# lambda = 0.9

predict.enet(lasso$finalModel, type='coefficients', s=lasso$bestTune$fraction, 
             mode='fraction')

lasso.pred <- predict(lasso, adapted.graduados.teste)
sqrt(mean(lasso.pred - adapted.graduados.teste$cra)^2)

# RSME = 0.7695863

varImp(lasso)
varImp(ridge)

plot(varImp(lasso, scale = FALSE))
plot(varImp(ridge, scale = FALSE))

# ---------------------------------------------------------------------------------------------------------------------
# 5. Compare os três modelos nos dados de teste em termos de RMSE.
# 
#  RMSE (ridge) = 0.8564798
#  RMSE (lasso) = 0.7695863
# 
#  Observa-se que a regressão utilizando o método de Ridge têm um erro maior que a
#  regressão que utiliza o método de Lasso.


# --------------------------------------------------------------------------------------------------------------------
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


# ---------------------------------------------------------------------------------------------------------------------
# 6.1 Treino com regressão linear com as variáveis mais importantes dos dados de treino

var.mais.importantes.treino <- graduados.treino %>%
  select(Programação.II, 
         Laboratório.de.Programação.II,
         Teoria.dos.Grafos,
         cra)
#rownames(var.mais.importantes) <- var.mais.importantes$ALU_NOVAMATRICULA

lmfit <- train(cra ~., data = var.mais.importantes.treino,
               method='lm',
               trControl = fitControl,
               preProc=c('scale', 'center'))
lmfit

# RMSE (linear)(treino) = 0.5214437
# Rsquared (linear)(treino) = 0.7406672

coef(lmfit$finalModel)

var.mais.importantes.teste <- graduados.teste %>%
  select(Programação.II, 
         Laboratório.de.Programação.II,
         Teoria.dos.Grafos,
         cra)

lmfit.pred <- predict(lmfit, var.mais.importantes.teste)
sqrt(mean(lmfit.pred - var.mais.importantes.teste$cra)^2)

# RMSE (linear)(pred - teste) = 0.6198759

plot(varImp(lmfit, scale = FALSE))

# --------------------------------------------------------------------------------------------------------------
# 7. Re-treine o melhor modelo (dessa vez nos dados de treino sem validação cruzada) 
#    e reporte o RMSE no teste.

set.seed(825) # for reproducing these results
ridge2 <- train(cra ~., data = adapted.graduados.treino,
               method='ridge',
               lambda = 4,
               preProcess=c('scale', 'center'))

ridge2

ridge2.pred <- predict(ridge2, adapted.graduados.teste)

sqrt(mean(ridge2.pred - adapted.graduados.teste$cra)^2)

#   RMSE(ridge) sem validação cruzada: 0.8202559
#


# -----------------------------------------------------------------------------------------------------------
# 8. Use o modelo treinado em 6 e aplique nos dados de teste que vamos disponibilizar.

treino.mais.teste <- rbind(adapted.graduados.teste, adapted.graduados.treino)

# Melhor modelo encontrado foi o linear: lmfit

lmfit.pred <- predict(lmfit, treino.mais.teste)

sqrt(mean(lmfit.pred - treino.mais.teste$cra)^2)

# RMSE = 0.3333295


# ------------------------------------------------------------------------------------------------------------
# 9. Crie novos atributos a partir dos existentes para tentar melhorar o seu modelo.

novo.treino <- adapted.graduados.treino %>% 
  dplyr::select(-Fundamentos.de.Física.Clássica, -Leitura.e.Produção.de.Textos, -Cálculo.Diferencial.e.Integral.I) %>%
  dplyr::mutate(media_labs = (Laboratório.de.Programação.I + Laboratório.de.Programação.II)/2) %>%
  dplyr::mutate(media_calc = (Cálculo.Diferencial.e.Integral.II + Álgebra.Vetorial.e.Geometria.Analítica)/2) %>%
  dplyr::mutate(media_prog = (Programação.I + Programação.II)/2)


novo.teste <- adapted.graduados.teste %>% 
  dplyr::select(-Fundamentos.de.Física.Clássica, -Leitura.e.Produção.de.Textos, -Cálculo.Diferencial.e.Integral.I) %>%
  dplyr::mutate(media_labs = (Laboratório.de.Programação.I + Laboratório.de.Programação.II)/2) %>%
  dplyr::mutate(media_calc = (Cálculo.Diferencial.e.Integral.II + Álgebra.Vetorial.e.Geometria.Analítica)/2) %>%
  dplyr::mutate(media_prog = (Programação.I + Programação.II)/2)


novo.lasso <- train(cra ~., novo.treino,
               method='lasso',
               preProc=c('scale','center'),
               trControl=fitControl)
novo.lasso

# lambda = 0.9

predict.enet(novo.lasso$finalModel, type='coefficients', s=novo.lasso$bestTune$fraction, 
             mode='fraction')

novo.lasso.pred <- predict(novo.lasso, novo.teste)
sqrt(mean(novo.lasso.pred - novo.teste$cra)^2)

# RSME = 0.4819051

varImp(novo.lasso)
