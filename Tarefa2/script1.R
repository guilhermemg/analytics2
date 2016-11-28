
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
  
df <- read.csv('Tarefa2/graduados.csv')  

head(df)

# periodo 1
disciplinas_per1 <- c(
  'Cálculo Diferencial e Integral I',
  'Laboratório de Programação I',
  'Álgebra Vetorial e Geometria Analítica',
  'Introdução à Computação',
  'Programação I',
  'Leitura e Produção de Textos'
)

per1 <- df %>%
  filter(disciplina %in% disciplinas_per1 ) %>%
  drop_na() %>%
  group_by(disciplina) %>%
  group_by(matricula) %>%
  summarise(media = mean(media), p=1)

per1
count(per1)


# periodo 2

disciplinas_per2 <- c(
  'Laboratório de Programação II',
  'Cálculo Diferencial e Integral II',
  'Matemática Discreta',
  'Teoria dos Grafos',
  'Fundamentos de Física Clássica',
  'Programação II'
)

per2 <- df %>%
    filter(disciplina %in% disciplinas_per2) %>%
    drop_na() %>%
    group_by(disciplina) %>%
    group_by(matricula) %>%
    summarise(media = mean(media), p=2)


head(per2)
count(per2)

# demais periodos
demais_per <- df %>%
  filter(! disciplina %in% disciplinas_per1 & ! disciplina %in% disciplinas_per2) %>%
  drop_na() %>%
  group_by(disciplina) %>%
  group_by(matricula) %>%
  summarise(media = mean(media), p=10)

head(demais_per)            
count(demais_per)


# join dataframes
new_df <- rbind.fill(per1, per2, demais_per)
head(new_df)


# ----------------------------------------------------------


# df <- read.csv('Tarefa2/graduados.csv')  
# 
# head(df)
# 
# per1_v2 <- df %>%
#   filter(disciplina %in% disciplinas_per1) %>%
#   drop_na() %>%
#   group_by(matricula) %>%
#   summarise(media = mean(media))
# 
# per1_v2



          