

data(iris)
pairs(iris[,1:4])

pairs(iris[,1:4], col=iris$Species)

library(GGally)
ggpairs(iris[,1:4], colour="gray20")

# ------------------------------------------------------------------------------

require(datasets)
data("swiss")
require(GGally)
require(ggplot2)

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=loess, fill="red", color="red", ...) +
    geom_smooth(method=lm, fill="blue", color="blue", ...)
  p
}

g = ggpairs(swiss,columns = 1:4, lower = list(continuous = my_fn))
g


# ---------------------------------------------------------------------------------

setwd('~/Desktop/Link to AD2/analytics2/')

df <- read.csv('Tarefa2/graduados_model.csv', sep=' ')  

sub.df <- subset(df, select = c(Matemática.Discreta, 
                                Programação.II,
                                Teoria.dos.Grafos,
                                Leitura.e.Produção.de.Textos,
                                Álgebra.Vetorial.e.Geometria.Analítica,
                                cra))
head(sub.df)
View(sub.df)

g = ggpairs(sub.df, columns = 1:6, lower = list(continuous = my_fn))
g