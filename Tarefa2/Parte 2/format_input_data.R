
setwd('~/Desktop/Link to AD2/analytics2')

library(readr)
library(dplyr)
graduados <- read_csv("Tarefa2/Parte 2/graduados.csv", col_types = cols(matricula = col_character())) %>%
  mutate(matricula = as.factor(matricula))
head(graduados)

str(graduados)
summary(graduados)
View(graduados)
graduados <- graduados %>%
  arrange(matricula)


graduados.clean <- graduados %>%
  filter(!is.na(media))

summary(graduados.clean)
View(graduados.clean)


graduados.cra <- graduados.clean %>%
  group_by(matricula) %>%
  mutate(cra.contrib = media*creditos) %>%
  summarise(cra = sum(cra.contrib)/sum(creditos))

head(graduados.cra)

library(reshape2)
library(tidyr)

graduados.model.input <- graduados.clean %>%
  group_by(matricula,disciplina)  %>%
  filter(media == max(media)) %>%
  ungroup() %>%
  select(matricula,disciplina,media) %>% 
  mutate(disciplina = as.factor(gsub(" ",".",disciplina))) %>%
  dcast(matricula ~ disciplina, mean) %>%
  merge(graduados.cra) %>%
  #filter(!is.na(graduados.cra))
  drop_na()

head(graduados.model.input)
View(graduados.model.input)

write.table(graduados.model.input, 'Tarefa2/graduados_model.csv')
