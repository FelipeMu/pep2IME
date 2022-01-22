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

##########################
# NOTA.: Al momento de ejectuar el script esperar aproximadamente 15 [seg ]para ver los
#        resultados
##########################


#=======================
#===== PREGUNTA 1 ======
#=======================

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


#largos: se observan los largos para decidir el valor de type en la función ezanova
neval_instructor <- nrow(eval_instructor)
neval_capitan <- nrow(eval_capitan)
neval_comandante <- nrow(eval_comandante)
neval_general <- nrow(eval_general)

# instacias: cada spacetrooper evaluado
spacetrooper <- factor(1:nrow(datos))
# se crea el dataframe apropiado
datosE <- data.frame(spacetrooper, eval_instructor, eval_capitan, eval_comandante, eval_general)

# primero el dataframe se lleva a formato largo:
#Se pivotean las variables
datosE <- datosE %>% pivot_longer(c("eval_instructor", "eval_capitan", "eval_comandante", "eval_general"),
                                names_to = "Evaluadores",  
                                values_to = "puntaje_evaluacion"
) 


# se procede a realizar los gráficos QQ para los distintos evaluador:
#   Comprobción   de   normalidad .
g <-   ggqqplot(datosE, x = "puntaje_evaluacion", y = "Evaluadores", color = "Evaluadores") 
g <-   g + facet_wrap (~  Evaluadores)
g <-   g + rremove("x.ticks") + rremove("x.text") 
g <-   g + rremove("y.ticks") + rremove("y.text") 
g <-   g + rremove("axis.title")
print(g)


# Se puede observar a través de las gráficas de las observaciones realizadas por
# cada evaluador efectivamente siguen una distribución normal y existen una cantidad
# muy mínima de valores atípicos, por lo que se decide avanzar con cautela
# y definir un nivel de significancia de alpha = 0.01. 
alfa <- 0.01


#verificación 4:
# Para la comprobación de esta condición, la función ezAnova() entrega el resultado de la 
# Prueba de esfericidad de Mauchly.

# Antes de seguir avanzando se establecen se establecen las condiciones a constrastar para
# la prueba de esfericidad de Mauchly.
#H0: las varianzas-covarianzas de las k muestras son iguales para los grupos.
#HA: al menos una de las muestras tiene varianza-covarianza diferente a alguna de los demás grupos. 
datosE$Evaluadores <- factor(datosE$Evaluadores )
cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
pruebaEzAnova <- ezANOVA(data = datosE,
                         dv = puntaje_evaluacion,
                         within = Evaluadores,
                         wid = spacetrooper,
                         type = 2,
                         return_aov = TRUE)
print(pruebaEzAnova)

# Se puede observar que el p-value retornado por la prueba de esfericidad de Mauchly
# es 0.905, el cual supera en gran medida al nivel de significación establecido, es por
# ello que se falla en rechazar la hipótesis nula en favor de la alternativa y por lo
# tanto se puede concluir con 99% de confianza que las varianzas entre los grupos
# son aproximadamente iguales.

# Ya con las condiciones ya verificadas, se procede establecer las hipótesis a contrastar
# con respecto a la prueba ezAnova:
#H0: el puntaje promedio para los spacetroopers es igual por cada evaluador.
#HA: el puntaje promedio para los spacetroopers es diferente en al menos un evaluador.

#CONLCUSIÓN
# Observando los resultados retornados por ezAnova en las líneas anteriores, se puede
# observar que el p-value=0.09 aproximadamente, un valor por encima del nivel de
# signifcancia establecido, lo cual indica que no se rechaza la hipótesis nula y por
# lo tanto se peude concluir con 99% de confianza que el promedio de lospuntajes 
# establecidos por cada evaluador son similares.

#Gráfico del tamaño del efecto.
g3 <- ezPlot(data = datosE , 
             dv = puntaje_evaluacion,
             wid = spacetrooper, 
             between = Evaluadores,
             y_lab = "puntaje", 
             x = Evaluadores)
print(g3)


# Adicionalmente, se puede realizar el procedimiento post-hoc con el único
# fin de apoyar a la conclusión descrita anteriormente:

# Se puede observar que los p-values para las combinaciones de todos los
# evaluadores es mayor al nivel de significancia, lo que confirma que
# no hay diferencias significativas en los puntajes promedios de cada
# evaluador.

#Procedimiento ANOVA con aov ().
cat(" Procedimiento    ANOVA    usando    aov \ n\ n") 
pruebaAnova <- aov(puntaje_evaluacion ~ Evaluadores, data = datosE) 
print(summary(pruebaAnova))

#Prueba HSD de Tukey .
post_hoc <- TukeyHSD(pruebaAnova,
                     "Evaluadores",
                     ordered = TRUE,
                     conf.level = 1 - alfa)
print(post_hoc)




#=======================
#===== PREGUNTA 3 ======
#=======================

# (9 puntos) Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca 
# en las lecturas dadas) en donde un estudio o experimento, relacionado con el 
# sentir de los santiaguinos ante el aumento de la violencia de la delincuencia
# necesite utilizar una prueba de suma de rangos de Wilcoxon (también llamada prueba
# de Mann–Whitney–Wilcoxon o prueba U de Mann-Whitney debido a problemas con la 
# escala de la variable dependiente en estudio. Indiqué cuáles serían las 
# variables involucradas en su ejemplo (con sus respectivos niveles) y 
# las hipótesis nula y alternativa a contrastar.

#ejemplo:

# La central de Policías De Investigaciones (PDI) desea conocer el miedo que sienten
# las personas al salir por la noche (21:00 hacia adelante) ante el aumento de la 
# violencia de la delicuencia. Para ello, han creado dos aplicaciones de celular
# AppService1 y AppServce2 que tienen como objetivo pedir pedir presionando solo
# un boton en caso de estar en una situación peligrosa. Es por esto que ha reunido
# a 35 personas, las cuales son asignadas a dos grupos de forma azarosa. 
# Cada uno de estos grupos deberá evaluar una aplicación (n_AppService1=18 y 
# nAppService2=17). Cada participante debe evaluar 6 aspectos del rendimiento de la 
# aplicación, cada uno de los cuales se mide con una escala Likert de 7 puntos, 
# donde 1 significa “muy malo” y 7, “muy bueno”. El nivel de eficacia y eficiencia
# que cada individuo da a la aplicación corresponde al promedio simple de las
# puntuaciones de los 6 aspectos evaluados.




