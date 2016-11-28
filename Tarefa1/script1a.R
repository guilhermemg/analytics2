

df1a = read.csv("~/Desktop/Graduacao/Periodos/Periodo_11/AD2/analytics2/tarefa2/alunosUFCGAnon.csv")

#attach(df2)
#aggdata2 <- aggregate(df2, by=list(df$Periodo, df$Media_Disciplina),
#                     FUN=mean, na.rm=TRUE)
#print(aggdata2)
#detach(df2)

#df_test = df2[sample(1:nrow(df2), 1000),]
df_test <- subset(df1a, select = c(Nome_Disciplina, Media_Disciplina))
#df_test_1a <- df_test[sample(1:nrow(df_test), 1000), ]
#head(df_test_1a)

#boxplot(df_test2$Media_Disciplina, na.rm=TRUE)
#hist(df_test2$Nome_Disciplina)

attach(df_test)
aggdata_test <- aggregate(df_test, 
                          by=list(df_test$Nome_Disciplina,
                                  df_test$Media_Disciplina),
                          FUN=mean, 
                          na.rm=TRUE)
print(aggdata_test)
detach(df_test)

