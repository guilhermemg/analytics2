
library(dplyr)
library(plyr)

setwd('~/workspaces/analytics2')

X <- read_csv("Tarefa2/graduados_treino.csv", col_types = cols(ALU_NOVAMATRICULA = col_character()))
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
  ungroup() %>%
  mutate(cra.contrib = MAT_MEDIA_FINAL*CREDITOS) %>%
  group_by(ALU_NOVAMATRICULA) %>%
  summarise(numDisciplinas = nrow(.))
  summarise(cra = sum(cra.contrib)/sum(CREDITOS))
head(graduados.cra)

#library(tidyr)

graduados.model.input2 <- graduados.clean %>%
  group_by(ALU_NOVAMATRICULA, DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(MAT_MEDIA_FINAL, DISCIPLINA, MAT_MEDIA_FINAL) %>% 
  mutate(DISCIPLINA = as.factor(gsub(" ",".", DISCIPLINA))) %>%
  dcast(MAT_MEDIA_FINAL ~ DISCIPLINA, mean) %>%
  merge(graduados.cra) %>%
  #filter(!is.na(graduados.cra))
  drop_na()

head(graduados.model.input2)
View(graduados.model.input2)

write.table(graduados.model.input2, 'Tarefa2/graduados_model2.csv')
