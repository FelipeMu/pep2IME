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
# NOTA.: Al momento de ejectuar el script para la pregunta 1 esperar aproximadamente 15 [seg] 
#        para ver los resultados.
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
# misma medida para cada evaluador.

#verificación 2:
# Es razonable pensar que cada evaluador evalúo a un spacetrooper de acuerdo
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


# largos: se observan los largos para decidir el valor de type en la función ezanova
# (se ocupa type=2: mismos largos todos los grupos)

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
#  Comprobación de normalidad .
g <-   ggqqplot(datosE, x = "puntaje_evaluacion", y = "Evaluadores", color = "Evaluadores") 
g <-   g + facet_wrap (~  Evaluadores)
g <-   g + rremove("x.ticks") + rremove("x.text") 
g <-   g + rremove("y.ticks") + rremove("y.text") 
g <-   g + rremove("axis.title")
print(g)


# Se puede observar a través de las gráficas de las observaciones realizadas por
# cada evaluador efectivamente siguen una distribución normal y existen una cantidad
# muy mínima de valores atípicos, por lo que se decide avanzar con cautela
# y definir un nivel de significancia de alfa = 0.01. 
alfa <- 0.01


#verificación 4:
# Para la comprobación de esta condición, la función ezAnova() entrega el resultado de la 
# Prueba de esfericidad de Mauchly.

# Antes de seguir avanzando se establecen las condiciones a constrastar para
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
# lo tanto se puede concluir con 99% de confianza que el promedio de los puntajes 
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


# Comprobar si no hay multicolinealidad entre los predictores.
corCheck <- entrenamiento  %>% select(estatura, peso, imc, velocidad, agilidad)
cor(corCheck)
# Existe muy fuerte multicolinealidad en algunos predictores, lo cual debe ser
# arreglado.

# Crear un modelo ajustado de regresión lineal múltiple basado en estos predictores,
# utilizando los datos de entrenamiento
rlm <- lm(as.numeric(es_clon) ~ estatura + peso + imc + velocidad + agilidad,
          data = entrenamiento)
summary(rlm)

# Función para evaluar condiciones de un modelo
# Argumentos:
# modelo: Modelo múltiple a analizar
# Valor:
# No hay retorno
evaluarCondiciones <- function(rlm)
{
  # Independencia de los residuos
  print("Prueba de Durbin-Watson")
  print(durbinWatsonTest(rlm))
  
  # Se verifica normalidad de los residuos
  print("Prueba de normalidad de Shapiro:")
  print(shapiro.test(rlm$residuals))
  
  # Distancia de Cook
  print("Distancia de Cook mayor a 1")
  print(which(cooks.distance(rlm) > 1))
  
  # Se verifica homosteacidad de los residuos
  print("Homosteacidad de los residuos:")
  print(ncvTest(rlm))
  
  # Se comprueba multicolinealidad
  vifs <- vif(rlm)
  print("VIFs")
  print(vifs)
  
  # Tolerancia
  cat("\nTolerancia:", (1/vifs))
  
  # VIF medio
  cat("\nVIF medio:", mean(vifs))
}


### PRUEBA 1
evaluarCondiciones(rlm)

# La prueba de Durbin-Watson se cumple con p=0.684
# La prueba de Shapiro se cumple con p=0.2737
# No hay residuos con distancia de Cook mayor a 1
# La prueba de homosteacidad se cumple con p=0.25497
# El VIF medio es 87945.01, lo cual es excesivo.

# Para reducir el VIF medio se elimina un predictores con valor de VIF
# excesivo.
# Este predictor es: imc

rlm_sin_imc <- update(rlm, . ~ . - imc)

### PRUEBA 2
evaluarCondiciones(rlm_sin_imc)

# La prueba de Durbin-Watson se cumple con p=0.748
# La prueba de Shapiro se cumple con p=0.1682
# No hay residuos con distancia de Cook mayor a 1
# La prueba de homosteacidad se cumple con p=0.6585
# El VIF medio es 1.07665, lo cual es ligeramente más alto que lo esperado (1.03)

# En ambos se cumplen todas las pruebas, el VIF medio es 1.07
# El valor de VIF más alto ahora es el de estatura, con VIF=1.1304
# Eliminar el predictor de estatura

rlm_sin_imc_ni_estatura <- update(rlm_sin_imc, . ~ . - estatura)

### PRUEBA 3
evaluarCondiciones(rlm_sin_imc_ni_estatura)

# La prueba de Durbin Watson se cumple con p=0.772
# La prueba de Shapiro se cumple con p=0.1495
# No hay residuos con distancia de Cook mayor a 1
# La prueba de homostacidad se cumple con p=0.59
# El VIF medio es 1.01, lo cual es un nivel aceptado.

# Como se cumplen todas las condiciones, es momento de hacer predicciones.

### Modelo final
# Ajustar modelo usando validación cruzada de cinco pliegues
rlm_final <- train(as.numeric(es_clon) ~ peso + velocidad + agilidad,
                   data = entrenamiento,
                   method = "lm",
                   trControl = trainControl(method = "cv", number = 5))

# Resumen del modelo
print(summary(rlm_final))

# Error cuadrático medio para datos de entrenamiento
mse_entrenamiento <- rlm_final$results$RMSE
print(mse_entrenamiento)

# Predicciones para el conjunto de prueba
predicciones <- predict(rlm_final, prueba)

# Error cuadrático medio para datos de prueba
error <- as.numeric(prueba[["es_clon"]]) - predicciones
mse_prueba <- sqrt(mean(error ** 2))

print(mse_prueba)

# Evaluar exactitud
umbral <- 1.5
predicciones2 <- sapply(predicciones, function(p) ifelse(p>=umbral, 2, 1))
predicciones2 <- factor(predicciones2)
confusionMatrix(predicciones2, factor(as.numeric(prueba[["es_clon"]])))
# La exactitud es de 0.925

# En resumen, el valor del error cuadrático medio de los datos de la prueba
# no varía significativamente con los del entrenamiento (0.2607 y 0.2767 respectivamente),
# por lo que el modelo no está sobreajustado.

# No hay residuos con Distancia de Cook superior a 1
# La exactitud es de 0.925, lo cual es superior al 0.8.
# No existe autocorrelación con un nivel de significación alfa=0.01
# El VIF medio es 1.01
# El modelo cumple todas las condiciones pedidas.



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

# La central de Policías De Investigaciones (PDI) desea disminuir el miedo que sienten
# las personas al salir por la noche (21:00 hacia adelante) ante el aumento de la 
# violencia de la delicuencia. Para ello, han creado dos aplicaciones de celular
# AppService1 y AppServce2 que tienen como objetivo pedir ayuda presionando solo
# un boton en caso de estar en una situación peligrosa. Es por esto que ha reunido
# a 35 personas, las cuales son asignadas a dos grupos de forma azarosa. 
# Cada uno de estos grupos deberá evaluar una aplicación (n_AppService1=18 y 
# nAppService2=17). Cada participante debe evaluar 6 aspectos del rendimiento de la 
# aplicación, cada uno de los cuales se mide con una escala Likert de 7 puntos, 
# donde 1 significa “muy malo” y 7, “muy bueno”. El nivel de eficacia y eficiencia
# que cada individuo da a la aplicación corresponde al promedio simple de las
# puntuaciones de los 6 aspectos evaluados.

# ejemplo datos:

#                          AppService1              AppService2
#                             4.5                       6.7
#                             4.1                       3.2
#                             5.0                       2.8
#                              .                         .  
#                              .                         .
#                              .                         .
#                   Media      n                         m

# Las variables involucradas en este ejemplo serían el tipo de aplicación
# que tendría dos niveles, una para cada aplicación (AppService1 y AppService2) y
# por otro lado una variable puntaje otorgada por cada persona al momento de evaluar
# el rendimiento de la aplicación.

# Las hipótesis a contrastar serían las siguietnes:

#H0: no hay diferencia en el rendimiento de ambas aplicaciones (eficacia y eficiencia muy bastante similar).
#HA: sí hay diferencia en el rendimiento de ambas aplicaciones.


