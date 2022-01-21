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


#Lord Vader desea saber si los niveles de exigencia con que los distintos oficiales 
#evaluadores (instructor, capitán, comandante y general) califican a los spacetroopers 
#son similares, por lo que le ha solicitado estudiar si existen diferencias significativas 
#en el promedio de la evaluación realizada por cada uno de los oficiales. El Lord Sith ha 
#sido muy claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones presenten
#diferencias.

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

