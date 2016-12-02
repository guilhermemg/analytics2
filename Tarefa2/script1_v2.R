# Tarefa 2

# O desempenho dos alunos nos dois primeiros períodos consegue explicar, em algum 
# grau, seus desempenhos no curso como um todo?

# Vamos tentar responder essa pergunta usando regressão linear. Vamos fazer isso 
# quebrando a pergunta anterior nas seguintes perguntas:

# a) Um modelo de regressão múltipla com todas as variáveis é plausível para explicar
#       a variação em y? Em que grau?
# b) Todas as variáveis são úteis para o modelo de regressão?
# c) Se a resposta para a pergunta anterior foi não, construa um novo modelo sem 
#       essas variáveis e o compare ao modelo com todas as variáveis (e.g. em termos de R2 e RSE).
# d) Analise os plots de resíduos de cada variável e veja se algum (um ou mais) 
#       deles indica não aleatoriedade dos erros.
# e) Que período consegue explicar melhor o desempenho final (primeiro ou segundo)?
# f) Use o modelo para predizer o seu próprio desempenho e compare a predição 
#       com o seu CRA atual. Comente o resultado.

setwd('~/Desktop/Link to AD2/analytics2/')

df <- read.csv('Tarefa2/graduados_model.csv', sep=' ')  

head(df)
View(df)

# library(outliers)
# 
# outlier(df[2:ncol(df)])
# rm.outlier(df[2:ncol(df)])

# -----------------------------------------------------------------------------------

model.p1.p2 <- lm(cra ~ Cálculo.Diferencial.e.Integral.I +
                Álgebra.Vetorial.e.Geometria.Analítica +
                Programação.I +
                Introdução.à.Computação +
                Leitura.e.Produção.de.Textos + 
                Laboratório.de.Programação.I +
                Programação.II + 
                Cálculo.Diferencial.e.Integral.II +
                Matemática.Discreta +
                Laboratório.de.Programação.II +
                Teoria.dos.Grafos +
                Fundamentos.de.Física.Clássica, data=df)
summary(model.p1.p2)

# a) Um modelo de regressão múltipla com todas as variáveis é plausível para explicar
#       a variação em y? Em que grau?

#         Segundo se observa no summary do modelo acima, o p-valor é bastante baixo,
#        menor que 0.05 (p-valor = 3.537e-14), e portanto, o modelo é aceitável.
#         Apesar disso, vemos também que o Adjusted R-squared é baixo, o que significa
#        que devemos buscar eliminar variáveis que devem estar atrapalhando a 
#        predição.
#         Pelo Adjusted R-squared, observa-se que pouco mais da metade da variação é explicada pelo
#        modelo. Adjusted R-squared = 0.6393

plot(model.p1.p2)

#coefficients(model.p1.p2)
#confint(model.p1.p2, level=.95)
#fitted(model.p1.p2)
#residuals(model.p1.p2)
#anova(model.p1.p2)
#vcov(model.p1.p2)
#influence(model.p1.p2)


# -----------------------------------------------------------------------------------

# Variable selection
library(MASS)

model.opt <- lm(cra ~ 
                    Cálculo.Diferencial.e.Integral.I +
                    Álgebra.Vetorial.e.Geometria.Analítica +
                    Programação.I +
                    Introdução.à.Computação +
                    Leitura.e.Produção.de.Textos + 
                    Laboratório.de.Programação.I +
                    Programação.II + 
                    Cálculo.Diferencial.e.Integral.II +
                    Matemática.Discreta +
                    Laboratório.de.Programação.II +
                    Teoria.dos.Grafos +
                    Fundamentos.de.Física.Clássica, 
                data=df)
final_model <- stepAIC(model.opt, direction="both")
final_model$anova

summary(model.opt)
summary.aov(model.opt)

summary(final_model)
plot(final_model)

# b) Todas as variáveis são úteis para o modelo de regressão?
#         Não, após aplicar o método de seleção de modelos baseado no AIC (Akaike 
#       information criterion), chegou-se ao seguinte modelo:
#             cra = 0.09881 * Álgebra.Vetorial.e.Geometria.Analítica + 
#                   0.10241 * Leitura.e.Produção.de.Textos + 
#                   0.30538 * Programação.II +
#                   0.21886 * Matemática.Discreta +
#                   0.09098 * Teoria.dos.Grafos
#                   1.15308
#
#         Como se observa, apenas cinco disciplinas são realmente significativas
#       para previsão do cra, considerando apenas conjunto das disciplinas do 1o e do 2o
#       periodos.
#         O modelo tem as seguintes medidas:
#           p-value: < 2.2e-16
#           F-statistic: 33.66
#           Adjusted R-squared:  0.6577
#
#         Nota-se, portanto, uma melhora no nível de explicação da variabilidade que pode ser
#       explicada pelo modelo proposto.

# ------------------------------------------------------------------------------------------
summary(model.p1.p2)
summary(final_model)

# c) Se a resposta para a pergunta anterior foi não, construa um novo modelo sem 
#       essas variáveis e o compare ao modelo com todas as variáveis 
#       (e.g. em termos de R2 e RSE).
#     Modelo Final encontrado:
#       cra = 1.067e-15 * Programação.II + -1.094e-15 * Matemática.Discreta + 7.24
#     Comparação com modelo com todas as variáveis (model.p1.p2):
#       model           p-valor       R2          R2-Adj        RSE
#       model.p1.p2     3.537e-14     0.6902      0.6393        0.52
#       final_model     < 2.2e-16     0.6778      0.6577        0.5065                      
#
#     Como se pode observar,o modelo final é superior ao modelo com todas as variáveis
#     em todas as medidas exploradas.


# ---------------------------------------------------------------------------------------------

plot(model.p1.p2)
plot(final_model)

# d) Analise os plots de resíduos de cada variável e veja se algum (um ou mais) 
#       deles indica não aleatoriedade dos erros.

# ---------------------------------------------------------------------------------------------

model.p1 <- lm(cra ~ Cálculo.Diferencial.e.Integral.I +
                    Álgebra.Vetorial.e.Geometria.Analítica +
                    Programação.I +
                    Introdução.à.Computação +
                    Leitura.e.Produção.de.Textos + 
                    Laboratório.de.Programação.I,
                    data=df)
summary(model.p1)

model.p2 <- lm(cra ~ Programação.II + 
                 Cálculo.Diferencial.e.Integral.II +
                 Matemática.Discreta +
                 Laboratório.de.Programação.II +
                 Teoria.dos.Grafos +
                 Fundamentos.de.Física.Clássica,
               data=df)
summary(model.p2)


# e) Que período consegue explicar melhor o desempenho final (primeiro ou segundo)?
#    Como se observar nos resultados dos comandos acima, para o modelo 1,
#     que considera apenas as disciplinas do 1 periodo, nós temos a seguinte
#     configuração:
#        p-valor = 1.954e-10
#        F-statistic = 13.43
#        Adjusted R-squared = 0.4673
#     Já para o modelo 2, o qual considera apenas as disciplinas do 2o periodo, nós
#     temos a seguinte configuração:
#        p-valor = 3.458e-10
#        F-statistic = 13.03
#        Adjusted R-squared = 0.4592
#     Portanto, o modelo 1 descreve/prever melhor o cra do aluno para o curso como
#     um todo.

# ---------------------------------------------------------------------------------------------

# f) Use o modelo para predizer o seu próprio desempenho e compare a predição 
#       com o seu CRA atual. Comente o resultado.
#     Fórmula do modelo: 
#       cra = 1.067e-15 * Programação.II + -1.094e-15 * Matemática.Discreta + 7.24
#
#       Programacao.II = 8.2
#       Matematica.Discreta = 7.9
#       cra estimado = 7,24
#       cra correto = 7,80
