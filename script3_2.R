# Questao 3
# Existe correlação entre as notas de Matemática Discreta e Cáclulo I 
# para o curso de Ciência da Computação? 
#
# a) Se sim, a correlação é positiva ou negativa? 
#     Sim, existe uma correlação positiva.
#
#
# b) Qual a força dessa correlação? (Dica: cada par ordenada de notas (x,y) 
# onde x é a nota de Cálculo I e y de Matemática Discreta corresponde a um 
# aluno que cursou as duas disciplinas).
#
#     

setwd("~/Desktop/Link to AD2/analytics2/")

library(dplyr)
library(tidyr)

df = read.csv("tarefa2/alunosUFCGAnon.csv")

df3 <- subset(df, select = c(Nome_Disciplina, Media_Disciplina, Matricula))

# take sample
df_test3 <- df3[sample(1:nrow(df3), 10000), ]

# ====================================================================

# 1. Extrai vetor com notas de calculo 1 e estudantes
# 2. Extrai vetor com notas de matematica discreta e estudantes
# 3. Faz join em dataframe com group_by estudante

notas_calculo <- df_test3[ which(df_test3$Nome_Disciplina=='CALCULO DIFERENCIAL E INTEGRAL I'),]
count(notas_calculo)

notas_discreta <- df_test3[which(df_test3$Nome_Disciplina=='MATEMÁTICA DISCRETA'), ]
count(notas_discreta)

typeof(notas_calculo)
typeof(notas_discreta)

# ====================================================================

notas_calc <- df_test3 %>%
            filter(Nome_Disciplina == 'CALCULO DIFERENCIAL E INTEGRAL I') %>%
            group_by(Matricula) %>%
            summarise(media = mean(Media_Disciplina)) %>%
            drop_na(media)
print(notas_calc)
#print(notas_calc[which(notas_calc$Matricula=='B10018'), ])

notas_md <- df_test3 %>%
              filter(Nome_Disciplina == 'MATEMÁTICA DISCRETA') %>%
              group_by(Matricula) %>%
              summarise(media = mean(Media_Disciplina)) %>%
              drop_na(media)
#print(notas_md[which(notas_md$Matricula=='B10018'), ])
print(notas_md)

final_df <- notas_md %>% full_join(notas_calc, by='Matricula')
#%>%
#              filter(!is.null(media.y) & !is.null(media.x)) %>%
#              drop_na()
print(final_df)

notas_mod1 = lm(final_df$media.x ~ final_df$media.y, data = final_df)

# -------------------------------------------------------------
# testa se existe estudantes com matricula nas duas disciplinas
by_student <- group_by(df_test3, Matricula)
by_student_nn <- summarise(by_student,
                           count=n(),
                           nota=mean(Media_Disciplina, na.rm=TRUE))
print(by_student_nn)

stds_2_disciplines <- filter(by_student_nn, count > 1)
print(stds_2_disciplines)
