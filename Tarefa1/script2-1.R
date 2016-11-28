# Questao 2 -------------------------------------------------------------------------
# Em qual período estão as melhores e piores notas do curso de Ciência da Computação?
#  
# Resposta:
#   Piores Notas: Periodo 28 (Caso excepcional)
#   Melhores Notas: Periodo 8

# Codigo usado:

df = read.csv("~/Desktop/Graduacao/Periodos/Periodo_11/AD2/analytics2/tarefa2/alunosUFCGAnon.csv")

df_test <- subset(df, select = c(Nome_Disciplina, Media_Disciplina, Periodo_Relativo))

# take sample
#df_test2 <- df_test[sample(1:nrow(df_test), 1000), ]

head(df_test)

#boxplot(df_test2$Media_Disciplina, na.rm=TRUE)
#hist(df_test2$Nome_Disciplina)

attach(df_test)
aggdata_test <- aggregate(df_test, 
                          by=list(df_test$Periodo_Relativo),
                          #df_test2$Media_Disciplina,
                          #df_test2$Nome_Disciplina),
                          FUN=mean, 
                          na.rm=TRUE)
print(aggdata_test)
plot(aggdata_test$Media_Disciplina, aggdata_test$Periodo_Relativo,
     xlab='Media Disciplina', ylab='Periodo Relativo')
boxplot(aggdata_test$Periodo_Relativo)

detach(df_test)
