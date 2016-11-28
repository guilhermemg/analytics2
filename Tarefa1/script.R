
df = read.csv("~/Desktop/Graduacao/Periodos/Periodo_11/AD2/analytics2/tarefa2/alunosUFCGAnon.csv")

# Questao 1 --------------------------------------------------------------------------
# a) Em quais cursos estão as melhores e piores notas? 
#
#
#
# b) Por que você acha que isso acontece em cada caso?
#
# Disciplinas de comeco de curso geralmente possuem medias mais baixas, devido
# principalmente à inexperência dos estudantes com o ritmo de estudos na universidade.
# Já no que tange a disciplinas que não pertecem ao DSC pode se tratar de casos de 
# desistência de curso e/ou de incompatibilidade com o assunto da disciplina.
#
# Disciplinas com maiores médias acontecem no final do curso, uma vez que o aluno
# deve se encontrar motivado devido a proximidade com o final do curso.

# Codigo usado

# aggregate by course and take mean
attach(df)
aggdata <- aggregate(df, by=list(df$Nome_Disciplina, df$Media_Disciplina),
                     FUN=mean, na.rm=TRUE)
print(aggdata)
detach(df)
