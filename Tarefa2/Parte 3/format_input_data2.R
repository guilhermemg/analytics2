
# ------------------------------------------------------------------------
# 2. Calcule o CRA dos alunos com base 
#    no script de preprocessamento (Links para um site externo) do lab anterior. 

library(readr)
library(dplyr)
library(plyr)

#setwd('~/Desktop/Link to AD2/analytics2')
setwd('~/workspaces/analytics2')

X <- read_csv("Tarefa2/Parte 3/graduados_treino.csv", col_types = cols(ALU_NOVAMATRICULA = col_character()))
X

graduados <- X  %>%
  mutate(ALU_NOVAMATRICULA = as.factor(ALU_NOVAMATRICULA))
head(graduados)

str(graduados)
summary(graduados)
View(graduados)
graduados <- graduados %>%
  arrange(ALU_NOVAMATRICULA)

graduados.clean <- graduados %>%
  filter(!is.na(MAT_MEDIA_FINAL))

summary(graduados.clean)
View(graduados.clean)

library(reshape2)

graduados.cra <- graduados.clean %>%
  dplyr::mutate(cra.contrib = MAT_MEDIA_FINAL*CREDITOS) %>%
  dplyr::group_by(ALU_NOVAMATRICULA) %>%
  dplyr::summarise(cra = sum(cra.contrib)/sum(CREDITOS))
head(graduados.cra)

library(tidyr)

graduados.model.input2 <- graduados.clean %>%
  dplyr::group_by(ALU_NOVAMATRICULA, DISCIPLINA)  %>%
  dplyr::filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  dplyr::ungroup() %>%
  dplyr::select(ALU_NOVAMATRICULA, DISCIPLINA, MAT_MEDIA_FINAL) %>% 
  dplyr::mutate(DISCIPLINA = as.factor(gsub(" ",".", DISCIPLINA))) %>%
  dcast(ALU_NOVAMATRICULA ~ DISCIPLINA, mean) %>%
  merge(graduados.cra) %>%
  #filter(!is.na(graduados.cra))
  tidyr::drop_na()

head(graduados.model.input2)
View(graduados.model.input2)

write.table(graduados.model.input2, 'Tarefa2/Parte 3/graduados_treino_model2.csv')
