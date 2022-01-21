library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)
library(ez)

library(dplyr)
library(leaps)
library(car)
library(lmtest)
library(caret)
library(pROC)

##################################
########### PREGUNTA 1 ########### 
##################################
# Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales evaluadores
# (instructor, capitán, comandante y general) califican a los spacetroopers son similares,
# por lo que le ha solicitado estudiar si existen diferencias significativas en el promedio
# de la evaluación realizada por cada uno de los oficiales.
# El Lord Sith ha sido muy claro al solicitar un reporte de aquellos oficiales
# cuyas evaluaciones presenten diferencias.

# Desarrollo:

#Se especifica y almacena la ruta del directorio de la base de datos.
dir <- "~/../Desktop"
base <- "Datos PEP 2.csv"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se especifica el formato de codificación UTF-8
datos <- read.csv2(arch, fileEncoding = "UTF-8")

# Dado el enunciado, es posible observar que se está pidiendo edentificar diferencias
# entre grupos que evalúan a spacetroopers, es por ello que sería adecuado llevar a cabo
# una Prueba Anova de una via para muestras corelacionadas.

# Para ello, se deben verificar una serie de condiciones:

#1. La escala con que se mide la variable dependiente tiene las propiedades de una escala 
# de intervalos iguales.

#2. Las mediciones son independientes al interior de cada grupo.

#3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una 
# distribución normal.

#4. La matriz de varianzas-covarianzas es esférica. Como explica Horn (2008, p. 1), 
# esta condición establece que las varianzas entre los diferentes niveles de las
# medidas repetidas deben ser iguales.

#VERIFICACIONES:

#verificación 1:
# Como se puede observar la variable dependiente es numérica y se encuentra en la
# misma medida para que evaluador.

#verificación 2:
# Es razonable pensar que cada evaluador estudio a un spacetrooper de acuerdo
# a su propio criterio, y es por ello que es lógico pensar que sus mediciones
# fueran tomadas de forma independiente.

#verificación 3: 
# Para esta condición sería necesario hacer un estudio en base a los gráficos
# QQ:


# se obtienen las columnas necesarias para el estudio:
eval_instructor <- datos["eval_instructor"]
eval_capitan <- datos["eval_capitan"]
eval_comandante <- datos["eval_comandante"]
eval_general <- datos["eval_general"]
# instacias: cada spacetrooper evaluado
spacetrooper <- factor(1:nrow(datos))
# se crea el dataframe apropiado
datosE <- data.frame(spacetrooper, eval_instructor, eval_capitan, eval_comandante, eval_general)

# primero el dataframe se lleva a formato largo:
#Se pivotean las variables
datosE <- datosE %>% pivot_longer(c("eval_instructor", "eval_capitan", "eval_comandante", "eval_general"),
                                names_to = "Evaluadores",  
                                values_to = "puntaje Evaluacion"
) 


# se procede a realizar los gráficos QQ para los distintos evaluador:
#   Comprobción   de   normalidad .
g <-   ggqqplot(datosE, x = "puntaje Evaluacion", y = "Evaluadores", color = "Evaluadores") 
g <-   g + facet_wrap (~  Evaluadores)
g <-   g + rremove("x.ticks") + rremove("x.text") 
g <-   g + rremove("y.ticks") + rremove("y.text") 
g <-   g + rremove("axis.title")
print(g)

# Se puede observar a través de las gráficas que las observaciones realizadas por
# cada evaluador efectivamente siguen una distribución normal y existen una cantidad
# muy mínima de valores atípicos. Por lo que se avanza con cautela y se define un 
# alpha=0.01
shapiro.test(datos[["eval_instructor"]])
shapiro.test(datos[["eval_instructor"]])
shapiro.test(datos[["eval_instructor"]])
shapiro.test(datos[["eval_instructor"]])





##################################
########### PREGUNTA 2 ########### 
##################################
# A fin de determinar si es necesario establecer programas de entrenamiento diferenciados para clones y reclutas,
# Lord Vader quiere saber si es posible distinguir entre ambas clases de soldados con los datos actuales.
# Para ello, ha solicitado evaluar un modelo clasificador que contemple entre 2 y 5 variables predictoras. Considere que, para ser aceptable, el modelo:
# • Debe lograr una exactitud (accuracy) de al menos 0,8 en datos de prueba
# • No puede considerar casos con demasiada influencia (considerando la distancia de Cook)
# • No debe presentar autocorrelación (usando la prueba de Durbin-Watson para un retardo y un nivel de significación α = .01)
# • No debe presentar multicolinealidad severa (considerando el factor de inflación de la varianza, con un VIF promedio inferior a 1,03).
# Considere la semilla 4666 para obtener una muestra de 400 datos, 80% de los cuales serán empleados para ajustar el modelo y el 20% restante, para evaluarlo.

# Desarrollo:

# Establecer la semilla y el alfa
set.seed(4666)
alpha <- 0.01

# Se separan los conjuntos de entrenamiento y prueba
n <- 400 # 400 datos
n_entrenamiento <- floor(0.8 * n)

datosMuestra <- sample_n(datos,
                         size = n,
                         replace = FALSE) # Obtener una muestra de 400 datos.

# Convertir los datos string a factores
datosMuestra <- datosMuestra %>% mutate_if(is.character,as.factor)

# Obtener la muestra del tamaño pedido (400 datos)
muestra <- sample.int(n = n,
                      size = n_entrenamiento,
                      replace = FALSE)
entrenamiento <- datosMuestra[muestra, ] # 80% para ajustar el modelo
prueba <- datosMuestra[-muestra, ] # 20% restante para evaluarlo


# Obtener las mejores variables predictoras (de 2 a 5)
mejorSubconjunto <- regsubsets(es_clon ~ ., # Buscamos predecir si es clon o recluta
                               data = datosMuestra,
                               nbest = 1, # determinar 1 modelo
                               nvmax = 5, # máximo de cinco variables predictoras
                               force.in = NULL,
                               force.out = NULL,
                               method = "exhaustive")

resumen <- summary(mejorSubconjunto)
print(resumen)

# Obtener el número de predictores recomendado
which.max(resumen$adjr2)
# El número de predictores recomendado fue 5.
# Obtener los cinco predictores recomendados:
print(resumen$which[5,])

# Luego, los cinco mejores predictores son:
# estatura
# peso
# imc
# velocidad
# agilidad

# Crear un modelo ajustado de RLM basado en estos predictores, utilizando los datos de entrenamiento
rlm <- glm(as.numeric(es_clon) ~ estatura + peso + imc + velocidad + agilidad,
           family = binomial(link = "logit"),
           data = entrenamiento)
summary(rlm)

# Ahora que tenemos el modelo inicial, falta ajustarlo.
# Comprobar si no hay multicolinealidad.