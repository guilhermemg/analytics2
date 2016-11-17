# Questao 3 -------------------------------------------------------------------------

library(Hmisc)

df = read.csv("~/Desktop/Graduacao/Periodos/Periodo_11/AD2/analytics2/tarefa2/alunosUFCGAnon.csv")

df3 <- subset(df, select = c(Nome_Disciplina, Media_Disciplina, Matricula))

# take sample
df_test3 <- df3[sample(1:nrow(df3), 1000), ]

df.md.av <- df_test3[ which(df_test3$Nome_Disciplina=='MATEMÁTICA DISCRETA' | 
                                df_test3$Nome_Disciplina=='CALCULO DIFERENCIAL E INTEGRAL I'), ]

head(df.md.av)

df.md.av.2 <- aggregate(df.md.av,
                        by=list(df.md.av$Matricula),
                        FUN=mean,
                        na.rm=TRUE)
print(df.md.av.2)


attach(df_test3)
aggdata_test3 <- aggregate(df_test3, 
                           by=list(df_test3$Matricula,
                                   #df_test2$Media_Disciplina,
                                   df_test3$Nome_Disciplina),
                           FUN=mean, 
                           na.rm=TRUE)
print(aggdata_test3)

plot(aggdata_test3$Media_Disciplina, aggdata_test3$Periodo_Relativo,
     xlab='Media Disciplina', ylab='Periodo Relativo')



