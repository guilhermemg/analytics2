
# remove outlier
# remove_outliers <- function(x, na.rm = TRUE, ...) {
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#   H <- 1.5 * IQR(x, na.rm = na.rm)
#   y <- x
#   y[x < (qnt[1] - H)] <- NA
#   y[x > (qnt[2] + H)] <- NA
#   y
# }

# final_df3 <- final_df2 %>%
#                 filter(matricula != 1268289547 &
#                          matricula != 1269495444 &
#                          matricula != 1269495448)

# library(outliers)
# outlier(final_df2)

View(final_df3)

model1 <- lm(cra ~ media_p1 + media_p2, data=final_df2)
plot(model1)

# model1 <- lm(cra ~ Cálculo.Diferencial.e.Integral.I +
#                Álgebra.Vetorial.e.Geometria.Analítica +
#                Programação.I +
#                Introdução.à.Computação +
#                Leitura.e.Produção.de.Textos + 
#                Laboratório.de.Programação.I +
#                Programação.II + 
#                Cálculo.Diferencial.e.Integral.II +
#                Matemática.Discreta +
#                Laboratório.de.Programação.II +
#                Teoria.dos.Grafos +
#                Fundamentos.de.Física.Clássica,
#              data=final_df3)