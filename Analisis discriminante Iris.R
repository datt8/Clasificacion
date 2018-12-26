
# Análisis gráfico

library(ggplot2)
library(ggpubr)
graf.sepal.len <- ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_density(aes(colour = Species))

graf.sepal.wid <- ggplot(data = iris, aes(x = Sepal.Width)) +
  geom_density(aes(color = Species))

graf.petal.len <- ggplot(data = iris, aes(x = Petal.Length)) +
  geom_density(aes(colour = Species))

graf.petal.wid <- ggplot(data = iris, aes(x = Petal.Width)) +
  geom_density(aes(colour = Species))

ggarrange(graf.petal.len, graf.petal.wid, graf.sepal.len, graf.sepal.wid,
          common.legend = TRUE, legend = "top")

scatterplotMatrix(iris[,-5])

summary(iris)

# Análisis de la normalidad

library(reshape2)
library(knitr)
library(dplyr)
datos.orden <- melt(iris, value.name = "valor")
kable(datos.orden %>% 
        group_by(Species, variable) %>% 
        summarise(pvalor = round(shapiro.test(valor)$p.value,2)))

# Heterocedasticidad

library(biotools)
boxM(iris[,1:4], iris[,5])


# Preparación del modelo
setosa <- filter(iris, Species == "setosa")
versicolor <- filter(iris, Species == "versicolor")
virginica <- filter(iris, Species == "virginica")

set.seed(888)
muestra <- sample(50)
muestra <- muestra[1:35]

virginica.train <- virginica[muestra,]
virginica.test <- virginica[-muestra,]
versicolor.train <- versicolor[muestra,]
versicolor.test <- versicolor[-muestra,]
setosa.train <- setosa[muestra,]
setosa.test <- setosa[-muestra,]

iris.train <- rbind(setosa.train, virginica.train, versicolor.train)
iris.test <- rbind(setosa.test, virginica.test, versicolor.test)

#Modelo lineal

library(MASS)
modelo.lineal <- lda(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, 
                     data = iris.train)
prediccion.lineal <- predict(object = modelo.lineal, newdata = iris.test[,-5])

table(iris.test$Species, prediccion.lineal$class, dnn = c("Real", "Predicho"))

plot(modelo.lineal, dimen = 1, type = "b")

#Modelo cuadratico

modelo.cuad <- qda(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, 
                   data = iris.train)
modelo.cuad
prediccion.cuad <- predict(object = modelo.cuad, newdata = iris.test[,-5])
table(iris.test$Species, prediccion.cuad$class, dnn = c("Real", "Predicho"))