# Parte 3: 
# Escolha 2 das 3 perguntas que você listou e construa o relatório
# final deste problema respondendo-as com estatística e visualizações.
#
# Pergunta 1:
# Quais os cursos que possuem as maiores taxas de evasão? 
#   Resposta esperada: Provavelmente cursos de exatas possuem as maiores taxas
# de evasão, dentre eles certamente estarão os cursos de CC e EEletrica, devido
# a reconhecida demanda do curso.
#
# Pergunta 2:
# Existe alguma relação evidente entre média do curso e a taxa de evasão que ele
# apresenta ?
#   Resposta esperada: É provável que não exista relação evidente ou visível, uma vez
# que os dados de evasão são relativos ao primeiro período, e as esperadas notas baixas
# desses que evadem são eventualmente diluídas pelas notas daqueles que continuam o 
# curso, sejam essas notas (de quem continua) baixas ou altas, elas pesarão mais
# na média histórica do curso, porque é de se esperar também que a maioria não desista
# do curso.
#
# Pergunta 3:
# Como é a distribuição da média do curso de computação ao longo dos perídos?
# E dos curso de engenharia elétrica e medicina? Existe uma regressão linear que
# represente bem essa distribuição (p-valor < .05)?
#   Resposta Esperada: espera-se que a nota em eng. elétrica e computação caiam com
# o passar dos anos, devido ao aumento da exigência dos cursos. Não deve existir uma
# relação linear entre essas duas colunas ( Periodo e Media do Curso).
#

library(dplyr)
library(tidyr)

setwd("~/Desktop/Link to AD2/analytics2/")

df4 <- read.csv("tarefa2/alunosUFCGAnon.csv")

df_test4 <- df4[sample(1:nrow(df4), 10000), ]

# ----------------------------------------------------------------------
# Perguntas 1 e 2

by_curso <- df4 %>%
  group_by(Nome_Curso) %>%
  drop_na(Media_Disciplina) %>%
  summarise(mediaCurso = mean(Media_Disciplina),
            taxaEvasao = sum(Cod_Evasao))

by_curso_desc_taxa_evasao <-  arrange(by_curso, desc(taxaEvasao))
print(by_curso_desc_taxa_evasao)
plot(by_curso_desc_taxa_evasao$mediaCurso, by_curso_desc_taxa_evasao$taxaEvasao,
     xlab = 'Media Curso', ylab = 'Taxa de Evasão', main = 'Média Histórica vs Taxa de Evasão')

by_curso_desc_media <- arrange(by_curso, desc(mediaCurso))
print(by_curso_desc_media)

#
# Resposta - Pergunta 1:
# O histograma mostra os 5 cursos com maiores taxas de evasão
#
cinco_maiores_evasao <- by_curso_desc_taxa_evasao[1:5,]
barplot(cinco_maiores_evasao$taxaEvasao, main="Cinco Maiores Taxas de Evasão", xlab="Cursos", 
        ylab="Taxa de Evasão", names.arg=c("EEletrica","CComputacao","Enferm.","EAlim","ECivil"))


# Resposta - Pergunta 2:
# O plot deixa claro que não existe relação evidente entre essas duas variáveis
#
plot(by_curso$mediaCurso, by_curso$taxaEvasao)


# ---------------------------------------------------------------------------
# Resposta Pergunta 3 - a)

media_comp_by_periodo <- df4 %>%
  group_by(Nome_Curso) %>%
  filter(Nome_Curso == 'CIÊNCIA DA COMPUTAÇÃO - D') %>%
  group_by(Periodo) %>%
  drop_na(Media_Disciplina) %>%
  summarise(mediaCurso = mean(Media_Disciplina))

print(media_comp_by_periodo)

plot(media_comp_by_periodo, xlab = 'Periodo', ylab='Media', main='Media de CC vs Periodo')

modelCC <- lm(media_comp_by_periodo$Periodo ~ media_comp_by_periodo$mediaCurso)
summary(modelCC)
plot(modelCC)

# como os plots demonstram (qq-plot, plot de residuos e plot do modelo linear)
# existe uma relação linear alta entre periodo e média do curso naquele perído
# em ciência da computação. (p-valor = 7.07e-07)

# ---------------------------------------------------------------------------
# Resposta Pergunta 3 - b)

media_ee_by_periodo <- df4 %>%
  group_by(Nome_Curso) %>%
  filter(Nome_Curso == 'ENGENHARIA ELÉTRICA') %>%
  group_by(Periodo) %>%
  drop_na(Media_Disciplina) %>%
  summarise(mediaCurso = mean(Media_Disciplina))

print(media_ee_by_periodo)

plot(media_ee_by_periodo, xlab = 'Periodo', ylab='Media', main='Media de EE vs Periodo')

modelEE <- lm(media_ee_by_periodo$Periodo ~ media_ee_by_periodo$mediaCurso)
summary(modelEE)
plot(modelEE)

# como os plots demonstram (qq-plot, plot de residuos e plot do modelo linear)
# existe uma relação linear alta entre periodo e média do curso naquele perído
# em eng. elétrica. (p-valor = 0.005145)

# ---------------------------------------------------------------------------
# Resposta Pergunta 3 - c)

media_med_by_periodo <- df4 %>%
  group_by(Nome_Curso) %>%
  filter(Nome_Curso == 'MEDICINA - D') %>%
  group_by(Periodo) %>%
  drop_na(Media_Disciplina) %>%
  summarise(mediaCurso = mean(Media_Disciplina))

print(media_med_by_periodo)

plot(media_med_by_periodo, xlab = 'Periodo', ylab='Media', main='Media de Medicina vs Periodo')

modelMED <- lm(media_med_by_periodo$Periodo ~ media_med_by_periodo$mediaCurso)
summary(modelMED)
plot(modelMED)

# como os plots demonstram (qq-plot, plot de residuos e plot do modelo linear)
# existe uma relação linear alta entre periodo e média do curso naquele perído
# em medicina. (p-valor = 2.272e-05)

