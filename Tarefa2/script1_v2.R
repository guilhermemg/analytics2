# Tarefa 2

# O desempenho dos alunos nos dois primeiros períodos consegue explicar, em algum grau, seus desempenhos no curso como um todo?

# Vamos tentar responder essa pergunta usando regressão linear. Vamos fazer isso quebrando a pergunta anterior nas seguintes perguntas:

# Um modelo de regressão múltipla com todas as variáveis é plausível para explicar a variação em y? Em que grau?
# Todas as variáveis são úteis para o modelo de regressão?
# Se a resposta para a pergunta anterior foi não, construa um novo modelo sem essas variáveis e o compare ao modelo com todas as variáveis (e.g. em termos de R2 e RSE).
# Analise os plots de resíduos de cada variável e veja se algum (um ou mais) deles indica não aleatoriedade dos erros.
# Que período consegue explicar melhor o desempenho final (primeiro ou segundo)?
# Use o modelo para predizer o seu próprio desempenho e compare a predição com o seu CRA atual. Comente o resultado.

library(dplyr)
library(tidyr)
library(plyr)

setwd('~/workspaces/analytics2/')

df <- read.csv('Tarefa2/graduados_model.csv', sep=' ')  

head(df)

# create column media_per1

final_df <- df %>%
    mutate(media_p1 = (Cálculo.Diferencial.e.Integral.I + 
             Álgebra.Vetorial.e.Geometria.Analítica + 
             Programação.I +
             Introdução.à.Computação +
             Leitura.e.Produção.de.Textos + 
             Laboratório.de.Programação.I) / 6)


head(final_df)
