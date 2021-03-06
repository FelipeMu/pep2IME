
# IMPORTANTE: Para que los resultados mostrados coincidan con las respuestas usar la opci�n "Run" para hacer correr el Script
# en lugar de "Source"



# Un estudio recolecta medidas anat�micas de 247 hombres y 260 mujeres (Heinz et al., 2003). Estas
# mediciones están disponibles en el archivo Body-EP12.csv que acompaña a este enunciado. El estudio
# incluye nueve mediciones del esqueleto (ocho diámetros y una profundidad de hueso a hueso) y doce
# mediciones de grosor (circunferencias) que incluyen el tejido.


library(boot)
library(ggpubr)
library(ez)
library(tidyverse)
library(dplyr)
library(car)

# Lectura de datos:

datos <- read.csv(file.choose())
# Se pide construir un modelo de regresión lineal m�ltiple para predecir la variable Peso, de acuerdo con las
# siguientes instrucciones:

# 1. Definir la semilla a utilizar, que corresponde a los �ltimos cuatro d�gitos del RUN (sin considerar el
# d�gito verificador) del integrante de menor edad del equipo

# La semilla ser�a la siguiente:
set.seed(6177)

# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un n�mero par) o 50 hombres (si la semilla es impar)

# El n�mero es impar, por lo que la muestra consiste en un grupo de hombres (Gender = 1)
datos[["Gender"]] <- factor(datos[["Gender"]]) 
variables <- datos %>% filter(Gender == "1")
variables <- variables[sample(nrow(variables), 50),]

# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras

# Si a cada columna se le asigna un n�mero del 1 al 23 (sin contar ni el peso ni el g�nero, y asignando 23 a la Altura
# Height) se encuentra aleatoriamente un vector con los posibles valores.
vectorAleatorio <- sample(1:23, 8)

# Luego, las variables ser�an:

# Hip.Girth = Grosor a la altura de las caderas
# Waist.Girth = Grosor a la altura de la cintura
# Height = Altura
# Ankles.diameter = Suma de los di�metros de los tobillos
# Elbows.diameter = Suma de los di�metros de los codos
# Wrists.diameter = Suma de los di�metros de las mu�ecas
# Calf.Maximum.Girth = Grosor promedio de la parte más ancha de las pantorrillas
# Biacromial.diameter = Di�metro biacromial (a la altura de los hombros)

# 4. Seleccionar, de las otras variables, una que el equipo considere que podr�a ser �til para predecir la
# variable Peso, justificando bien esta selecci�n

# La otra variable seleccionada ser� Navel.Girth(Grosor a la altura del ombligo), debido a que el aumento de peso
# provoca la acumulaci�n de grasa en zonas abdominales, lo cual puede cambiar el grosor de esta zona de forma 
# directa

# 5. Usando el entorno R, construir un modelo de regresi�n lineal simple con el predictor seleccionado
# en el paso anterior


modeloSimple <- lm(Weight ~ Navel.Girth, data = variables)
print(summary(modeloSimple))

# Se grafica Navel,Girth vs Weight
p <- ggscatter(variables, x= "Navel.Girth", y = "Weight", color = "blue", fill = "blue",xlab = "Grosor ombligo", ylab = "Peso")

p <- p + geom_smooth(method = lm, se = FALSE, colour = "red")

# Se grafica el modelo
plot(modeloSimple)

# Del modelo se puede apreciar que la distribuci�n de los residuos pareciese ser normal y no muy variable. A�n as� se
# destaca la presencia de valores at�picos que, por evitar la imprudencia, por el momento estos no ser�n eliminados.

# Se grafican los residuos

b0 <- modeloSimple$coefficients[1]
b1 <- modeloSimple$coefficients[2]

residuos <- variables[["Weight"]] - ( b1 * variables[["Navel.Girth"]] + b0)
variables2 <- data.frame(variables, residuos)

r <- ggscatter(variables2, x= "Navel.Girth", y = "residuos", color = "blue", fill = "blue", xlab = "Gr osor ombligo", ylab = "Residuos")

r <- r + geom_hline(yintercept = 0, colour = "red")

pr <- ggarrange(p,r,ncol = 2, nrow = 1)


# Se muestran gr�ficos p y r
print(pr)


# Ahora se comprueban las condiciones para usar el modelo RLS

# 1) Tendencia lineal

print(cor(variables$Navel.Girth, variables$Weight))

# Luego, debido a la fuerte correlaci�n (0.8407), se puede asumir que existe una tendencia lineal entre las variables.

# 2) Normalidad de los residuos

# Para comprobar la normalidad de los residuos se utiliza la prueba de Shapiro-Wilk

print(shapiro.test(variables2$residuos))

# Con un valor p = 0.3429 se concluye que los residuos siguen una distribuci�n normal

# 3) Variabilidad de los residuos
# Del gr�fico de pr se puede apreciar que la variabilidad de los residuos pareciese ser constante

# 4) Observaciones independientes
# Al ser parte de un estudio y representar menos del 10% de la poblaci�n, puede concluir que los
# datos son independientes

# Luego, se han verificado todas las condiciones para usar el modelo RLS



# 6. Usando herramientas para la exploraci�n de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de
# regresi�n lineal simple obtenido en el paso 5

# Importar la librer�a "leaps"
# La librer�a leaps contiene la funci�n regsubsets(), la cual determina
# la regres�n de los mejores subconjuntos (subsets)
library(leaps)

# Mejor subconjunto se encuentra entregando un vector con los �ndices de las variables.
# En este caso el valor 23 equivale al peso (Weight) y el valor 24 equivale a la altura (Height)
mejorSubconjunto <- regsubsets(Weight ~ .,
                               data = variables[, c(23, 14, 12, 24, 9, 6, 7, 19, 1)],
                               nbest = 1, # Determinar un modelo
                               nvmax = 5) # m�ximo de cinco variables predictoras
resumen <- summary(mejorSubconjunto)
print(resumen)

# Obtener el n�mero de predictores recomendado
which.max(resumen$adjr2)
# El n�mero de predictores recomendado fue 5.
# Obtener los cinco predictores recomendados:
print(resumen$which[5,])

# Luego, los cinco predictores a seleccionar son
# Hip.Girth
# Waist.Girth
# Ankles.diameter
# Elbows.diameter
# Height

# Agregar estos al modelo simple, que ya contaba con el ancho del ombligo.
modeloMultiple <- lm(Weight ~ Navel.Girth + Hip.Girth + Waist.Girth + Ankles.diameter + Elbows.diameter + Height, data = variables)
summary(modeloMultiple)

# 7. Evaluar los modelos y "arreglarlos" en caso de que tengan alg�n problema con las condiciones que
# deben cumplir

# El modelo m�ltiple cumple con las condiciones de que las variables predictoras que lo componen son num�ricas o dicot�micas
# y ninguna de ellas corresponde a una constante. Adem�s las observaciones son independientes al tratarse de diferentes
# sujetos de prueba. La variables dependiente (peso) es num�rcia a nivel de intervalo sin restricciones.

# Para comprobar la no existencia de multicolinealidad se revisa la tabla de correlaciones.

revisarCor <- variables %>% select(Weight, Hip.Girth, Navel.Girth, Waist.Girth, Ankles.diameter, Elbows.diameter, Height)

cor(revisarCor)

# Se pueden ver correlaciones fuertes entre predictores, por lo que hay que modificar el modelo. Pero primero, se revisar�n el 
# resto de las condiciones.


# Funci�n que permite comprobar condiciones de un modelo (independencia, normalidad y homosteacidad de residuos, y multicolinealidad)
# Argumentos:
# modelo: Modelo m�ltiple a analizar
# Valor:
# No hay retorno
evaluarCondiciones <- function(modelo){

  
  # Se comprueban las condiciones para el modelo m�ltiple
  # Independencia de los residuos
  print(durbinWatsonTest(modelo))
  
  # Se verifica normalidad de los residuos
  print(shapiro.test(modelo$residuals))
  
  #Se verifica homosteacidad de los residuos
  print(ncvTest(modelo))
  
  # Se comprueba multicolinealidad
  vifs <- vif(modelo)
  
  print(vifs)
  
  # Tolerancia
  print(1/vifs)
  
  # Vifs medio
  print(mean(vifs))
}

# Se plantea un valor de alfa
a <- 0.01

# Se comprueban las condiciones para el modelo multiple
evaluarCondiciones(modeloMultiple)

# Por el momento no se est� cumpliendo con que el VIF promedio no es mayor a 1 (4.024)

# Se elimina el predictor Navel.Girth, pues tiene el mayor valor p en el modelo original (0.9474)
modeloMultiple2 <- update(modeloMultiple, . ~ . - Navel.Girth)
summary(modeloMultiple2)
evaluarCondiciones(modeloMultiple2)

# Sigue sin cumplirse condici�n de que el VIF promedio sea menor a 1 (2.595)

# Se elimina el predictor Elbows.diameter, por ser aquel que actualmente tiene el valor p m�s alto (0,35806)
modeloMultiple3 <- update(modeloMultiple2, . ~ . - Elbows.diameter)
summary(modeloMultiple3)
evaluarCondiciones(modeloMultiple3)

# El VIF promedio sgue siendo mayor a 1 (2.392111)

# Se deber�a elimina el predictor Ankles.diameter, por su valor p = 0.0175
modeloMultiple4 <- update(modeloMultiple3, . ~ . - Ankles.diameter)
summary(modeloMultiple4)

evaluarCondiciones(modeloMultiple4)

# Nuevamente el valor VIF promedio es 3.111, por lo tanto se realiza una �ltima eliminaci�n de predictor (pues si
# se eliminan 2 m�s el modelo pasar� a ser simple)

# Se elimina el predictor Waist.Girth, por su valor p m�s elevado que el resto (0.000246)
modeloMultiple5 <- update(modeloMultiple4, . ~ . - Waist.Girth)
summary(modeloMultiple5)

evaluarCondiciones(modeloMultiple5)

# Finalmente se cumple la condici�n del VIF promedio (1.1356, el cual, si bien no es menor a 1, es un valor bastante
# cercano, que en este punto se considera aceptable). Finalmente se debe comprobar la condici�n de la no existencia de multicolinealidad

revisarCor <- variables %>% select(Weight, Hip.Girth, Height)
cor(revisarCor)

# En donde los predictores restantes (Hip.Girth y Height) tienen una correlaci�n que no es fuerte (0.3455). Por lo tanto
# se cumplen todas las condiciones del modelo.
##########################

# 8. Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo (o utilizando
# validaci�n cruzada)

library(caret)

# Se evalua el poder predictivo en base a validaci�n cruzada

#MODELO SIMPLE
n <- nrow(variables)

n_entrenamiento <- floor(0.7 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- variables[muestra, ]
prueba <- variables[-muestra, ]

# Ajuste del modelo simple con 5 pliegues

modeloS2 <- train(Weight ~ Navel.Girth, data = entrenamiento, 
                  method = "lm", trControl = trainControl(method = "cv", number = 5))

# Error cuadr�tico medio para datos de entrenamiento
mse_entrenamiento1 <- modeloS2$results$RMSE
print(mse_entrenamiento1)

# Predicciones para el conjunto de prueba
predicciones <- predict(modeloS2, prueba)

# Error cuadr�tico medio para datos de prueba
error <- prueba[["Weight"]] - predicciones
mse_prueba1 <- sqrt(mean(error ** 2))

print(mse_prueba1)

# Con la semilla del origen 6177 se puede ver que el error cuadr�tico medio de los datos de prueba (6.215518)
# no var�a significativamente respecto al de entrenamiento (6.0830), lo que lleva a pensar que lo m�s probable
# es que el modelo no est� sobreajustado, pudi�ndose adaptar a los datos no utilizados para construirlo, lo que podr�a
# implicar que el modelo presenta un poder predictivo alto.


#MODELO M�LTIPLE FINAL


# Ajuste del modelo m�ltiple con 5 pliegues

modeloM2 <- train(Weight ~  Hip.Girth + Height, data = entrenamiento, 
                  method = "lm", trControl = trainControl(method = "cv", number = 5))

# Error cuadr�tico medio para datos de entrenamiento
mse_entrenamiento2 <- modeloM2$results$RMSE
print(mse_entrenamiento2)

# Predicciones para el conjunto de prueba
predicciones2 <- predict(modeloM2, prueba)

# Error cuadr�tico medio para datos de prueba
error2 <- prueba[["Weight"]] - predicciones2
mse_prueba2 <- sqrt(mean(error2 ** 2))

print(mse_prueba2)

# Con la semilla 6177 nuevamente se observa que el valor del error cuadr�tico medio de los datos de prueba (4.8307)
# no var�a significativamente en comparaci�n al del entrenamiento (4.9202), inclusive disminuye, lo cual nuevamente 
# podr�a implicar que el modelo no est� sobreajustado, aumentando as� el poder predictivo para datos no utilizados 
# para la construcci�n del modelo.




