
library(readxl) # Se carga la librería para leer la fuente de los datos
library(dplyr) # Esta librería permite limpiar el dataset
data <- read_excel("bd_ecv2016_HOGARESMONOPARENTALES.xlsx", 
                   sheet = "Hoja1") # Se lee la base de datos
data <- data.frame(data[,-1], row.names = data$NHOGAR) # Se coloca esa variable como nombre de la fila (identificativo del registro)
names(data) <- c("Ayuda por familia en año pasado", "Renta percibida por menores de 16", 
                 "Vacaciones fuera (> 1semana)", "Gastos imprevistos", "Televisión en color", 
                 "Ordenador", "Capacidad para llegar a fin de mes", "Propiedad de la casa", 
                 "Nº miembros de hogar", "Renta disponible año anterior", "Riesgo de pobreza", 
                 "Región", "Edad de la persona más mayor del hogar", "Horas de trabajo semanal", 
                 "Nº miembros mayores de 16", "Sexo del mayor", "Situación laboral") # Se ponen los nombres verdaderos del dataset

# Limpieza de los datos. Se pasa a factor y se renombran las categorías.

data$`Vacaciones fuera (> 1semana)` <- factor(data$`Vacaciones fuera (> 1semana)`, 
                                              levels = c(1,2), labels = c("Sí", "No"))
data$`Gastos imprevistos` <- factor(data$`Gastos imprevistos`, levels = c(1,2), 
                                    labels = c("Sí", "No"))
data$Ordenador <- if_else(data$Ordenador != 1, 2, 1)
data$Ordenador <- factor(data$Ordenador, levels = c(1,2), labels = c("Sí", "No"))
data$`Capacidad para llegar a fin de mes` <- factor(data$`Capacidad para llegar a fin de mes`,
                                                    levels = c(1, 2, 3, 4, 5, 6), 
                                                    labels = c("Muy díficil", "Díficil", "Poco díficil", "Poco fácil", "Fácil", "Muy fácil"))
data$`Propiedad de la casa` <- factor(data$`Propiedad de la casa`, levels = c(1, 2, 3, 4, 5), 
                                      labels = c("Propia", "Propia con hipoteca", "Alquiler a precio mercado", "Alquiler por debajo de mercado", "Cesión gratuita"))
data$`Riesgo de pobreza` <- factor(data$`Riesgo de pobreza`, levels = c(0,1), 
                                   labels = c("Normal", "En riesgo de pobreza"))
data$`Sexo del mayor` <- factor(data$`Sexo del mayor`, levels = c(0, 1), 
                                labels = c("Mujer", "Hombre"))
data$`Situación laboral` <- factor(data$`Situación laboral`, levels = seq(1, 11, 1), 
                                   labels = c("Asalariado TC", "Asalariado TP", "Cuenta propia TC", 
                                              "Cuenta propia TP", "Parado", "Estudiante", "Jubilado", 
                                              "Incapacitado", "Militar", "Hogar", "Otros"))

data.def <- data[,-c(5, 10)] # Se elimina la televisión en color y la renta del año anterior (muy relacionada)

str(data.def) # Todas las variables numéricas lo son
summary(data.def$`Riesgo de pobreza`) # La muestra está balanceada (60-40 a favor de los que están fuera del riesgo)

# Selección de variables

library(MASS)
modelo.entero <- glm(formula = `Riesgo de pobreza` ~ . , data = data.def, family = "binomial") # Modelo con todas las variables
modelo.nulo <- glm(formula = `Riesgo de pobreza` ~ 1, data = data.def, family = "binomial") # Modelo con sólo constante

mod1 <- stepAIC(modelo.nulo, scope = list(lower = modelo.nulo, upper = modelo.entero), direction = "forward") # Se seleccionan añadiendo variables
mod2 <- stepAIC(modelo.entero, direction = "backward") # Se seleccionan quitando variables
mod3 <- stepAIC(modelo.entero, direction = "both") # Se seleccionan quitando/añadiendo variables según sea apropiado

## Todos los AIC dan lo mismo (se elige el 1)
AIC(mod1) 
AIC(mod2)
AIC(mod3)

mod1$call # Estas son las variables seleccionadas

# Creación muestra train y test. Se crean 2 para garantizar la estabilidad del modelo

## Muestra 1
set.seed(88)
muestra1 <- sample(x = nrow(data.def), size = nrow(data.def)*.7)
data.train1 <- data.def[muestra1,]
data.test1 <- data.def[-muestra1,]
summary(data.train1$`Riesgo de pobreza`) # Se comprueba el balanceo (se cumple)
summary(data.test1$`Riesgo de pobreza`) # Se comprueba el balanceo (se cumple)

## Muestra 2
set.seed(66)
muestra2 <- sample(x = nrow(data.def), size = nrow(data.def)*.7)
data.train2 <- data.def[muestra2,]
data.test2 <- data.def[-muestra2,]
summary(data.train2$`Riesgo de pobreza`) # Se comprueba el balanceo (se cumple)
summary(data.test2$`Riesgo de pobreza`) # Se comprueba el balanceo (se cumple)

# Árbol decision

# Se crean ambos árboles, cada uno con su muestra. El método es class, ya que la dependiente es un factor
library(rpart)
arbol1 <- rpart(`Riesgo de pobreza` ~ `Situación laboral` + `Capacidad para llegar a fin de mes` 
                + `Ayuda por familia en año pasado` + `Nº miembros de hogar` + `Gastos imprevistos` 
                + `Propiedad de la casa`, method = "class", data = data.train1)

arbol2 <- rpart(`Riesgo de pobreza` ~ `Situación laboral` + `Capacidad para llegar a fin de mes` 
                + `Ayuda por familia en año pasado` + `Nº miembros de hogar` + `Gastos imprevistos` 
                + `Propiedad de la casa`, method = "class", data = data.train2)

## Árbol 1

library(rpart.plot)
printcp(arbol1) # Se observan las variables elegidas. El objetivo es minimizar el error (dado por xerror) de esta forma se queda con 2.
plotcp(arbol1) # Otra afirmación más de que el parámetro elegido debe ser el 2 (corte con abscisas)
summary(arbol1) # Resumen completo del árbol e importancia de las variables.

arbol1.podado <- prune(arbol1, cp = arbol1$cptable[2]) # Se poda el arbol con el parametro de complejidad elegido
rpart.plot(arbol1.podado, type = 4, extra = 108, fallen.leaves = TRUE, main = "Árbol de Decisión 1", box.palette = "BuOr") # Se grafica el árbol
# La interpretación es la que sigue: existe un 74 % de posibilidades de que siendo parado, incapacitado o asalariado a tiempo parcial
# la familia se encuentre en riesgo de pobreza. Si no pertenece el mayor del hogar a eso, la probabilidad baja al 19%
# Se consigue un 78 % de acierto en entrenamiento (nada malo si se compara con el logístico)

### Predicción

arbol1.pred <- predict(arbol1.podado, data.test1, type = "class") # Se crea el objeto predicho
arbol1.matriz.conf <- table(data.test1$`Riesgo de pobreza`, arbol1.pred, dnn = c("Real", "Predicho"))
arbol1.matriz.conf # La matriz de confusión resultante es muy desfavorable.

# El nivel de acierto se reduce al 71%, pero los errores por el lado de los considerados como pobres de multiplican (aumentan un 25% respecto al logístico)

### Predicción con más complejidad 
arbol1.podado.complex <- prune(arbol1, cp = arbol1$cptable[5])
prp(arbol1.podado.complex, type = 4, extra = 108, fallen.leaves = TRUE, main = "Árbol de Decisión 1")

arbol1.pred.complex <- predict(arbol1.podado.complex, data.test1, type = "class")
arbol1.matriz.conf.complex <- table(data.test1$`Riesgo de pobreza`, arbol1.pred.complex, dnn = c("Real", "Predicho"))
arbol1.matriz.conf.complex

# A pesar del aumento a máximos de la complejidad el modelo sólo mejora un 2,78% y aunque la clasificación de las familias pobres mejora (los errores aumentan un 12%)
# No se cree que merezca la pena ese aumento de complejidad.

## Árbol 2

printcp(arbol2) # Se observan las variables elegidas. El objetivo es minimizar el error (dado por xerror) de esta forma se queda con 2.
plotcp(arbol2) # Otra afirmación más de que el parámetro elegido debe ser el 2 (corte con abscisas)
summary(arbol2) # Resumen completo del árbol e importancia de las variables.

arbol2.podado <- prune(arbol2, cp = arbol2$cptable[2]) # Se poda con el parámetro elegido
rpart.plot(arbol2.podado, type = 4, extra = 108, fallen.leaves = TRUE, main = "Árbol de Decisión 2", box.palette = "BuOr") # Se grafica
# Existe un 66% de que siendo parada, empleado en hogar, incapacitado o asalariado a tiempo parcial de estar en riesgo de pobreza. Si está en otra categoría, 17%.
# El nivel de acierto en entrenamiento es similar al logístico y al anterior (76%)

### Predicción

arbol2.pred <- predict(arbol2.podado, data.test2, type = "class")
arbol2.matriz.conf <- table(data.test2$`Riesgo de pobreza`, arbol2.pred, dnn = c("Real", "Predicho"))
arbol2.matriz.conf # Bastante similar al entrenamiento (acierto 75%), pero los mismos problemas que antes

### Predicción con más complejidad
arbol2.podado.complex <- prune(arbol2, cp = arbol1$cptable[3])
prp(arbol2.podado.complex, type = 4, extra = 108, fallen.leaves = TRUE, main = "Árbol de Decisión 2")

arbol2.pred.complex <- predict(arbol2.podado.complex, data.test2, type = "class")
arbol2.matriz.conf.complex <- table(data.test2$`Riesgo de pobreza`, arbol2.pred.complex, dnn = c("Real", "Predicho"))
arbol2.matriz.conf.complex

# Al contrario que en el anterior, el problema se arregla por el otro lado (los no pobres), por lo tanto no merece la pena el aumento

# Existe inestabilidad en los árboles debido a la diferencia de tratamiento de los trabajadores en hogar.

## Arboles condicionados

### Árbol 1

library(party)
arbol1.cond <- ctree(`Riesgo de pobreza` ~ `Situación laboral` + `Capacidad para llegar a fin de mes` + 
                       `Ayuda por familia en año pasado` + `Nº miembros de hogar` + `Gastos imprevistos` + 
                       `Propiedad de la casa`, data = data.train1)

plot(arbol1.cond, main = "Árbol de inferencia condicional 1") # A pesar de la pureza de muchos nodos terminales, el acierto no es significativamente alto.

arbol1.cond.pred <- predict(arbol1.cond, data.test1, type = "response")
arbol1.cond.pred.matriz.conf <- table(data.test1$`Riesgo de pobreza`, arbol1.cond.pred, dnn = c("Real", "Predicho"))
arbol1.cond.pred.matriz.conf # El resultado es muy similar al de los árboles sin condiciones

### Árbol 2

arbol2.cond <- ctree(`Riesgo de pobreza` ~ `Situación laboral` + 
                      `Capacidad para llegar a fin de mes` + `Ayuda por familia en año pasado` + 
                       `Nº miembros de hogar` + `Gastos imprevistos` + `Propiedad de la casa`, 
                     data = data.train2)

plot(arbol2.cond, main = "Árbol de inferencia condicional 2")

arbol2.cond.pred <- predict(arbol2.cond, data.test2, type = "response")
arbol2.cond.pred.matriz.conf <- table(data.test2$`Riesgo de pobreza`, arbol2.cond.pred, dnn = c("Real", "Predicho"))
arbol2.cond.pred.matriz.conf # Los fuera de pobreza los predice muy bien, pero los que están en riesgo de pobreza no (<60%)