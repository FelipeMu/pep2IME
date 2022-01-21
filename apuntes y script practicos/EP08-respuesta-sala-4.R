library(MASS)
library(tidyverse)
library(ggpubr)
library(dplyr)
library(ez)

# Instalar paquete MASS:
# install.packages("MASS")

##############################
#####     Pregunta 1     #####
##############################

# Los desórdenes alimenticios son un problema de salud mental bastante frecuente que afectan, solo en
# Estados Unidos, a millones de niños y adolescentes. El paquete MASS de R incluye el conjunto de datos
# anorexia que reporta el peso inicial, el peso final y el tratamiento recibido por 72 mujeres con diagnóstico
# de anorexia. Determine si existen diferencias significativas en el peso final de las mujeres para cada
# tratamiento.

##############################
#####     Desarrollo     #####
##############################

#Cargar conjuntos de datos
datos <- anorexia

#Renombrar las columnas 
datos <- datos %>% rename(Tratamiento = Treat, Peso_previo = Prewt, Peso_posterior = Postwt)


# Renombrar los niveles de Tratamiento
datos[["Tratamiento"]] <- factor(datos[["Tratamiento"]], levels = c("Cont", "CBT", "FT"),
                           labels = c("Control","Cognitivo conductual", "Family"))

# Quitar la columna peso_previo
datos <- dplyr::select(datos, Tratamiento, Peso_posterior)


datos[["instancia"]] <- factor(1:nrow(datos))


#Hipotesis:
# H0: el peso promedio final de las mujeres es igual para los 3 tratamientos.
# Ha: el peso promedio final de las mujeres es distinto para al menos uno de los tratamientos.

# Como se necesita comparar simultaneamente 3 medias muestrales (pesos finales por tratamiento), 
# se propone utilizar la prueba ANOVA para muestras independientes. 

# Condiciones:
# 1) La escala con la que se mide la variable dependiente (peso) tiene las propiedades de una escala de intervalos iguales (libras)
# 2) El paquete MASS nos garantiza que las muestras fueron obtenidas de manera aleatoria e independiente desde la poblacion de origen.
# 3) Como se observa en la prueba de normalidad realizada posteriormente, es posible suponer un distribuci�n normal,
#    pero existen ciertos datos atipicos, por lo cual se decide tomar un valor alpha mas bajo 0.01

# Comprobacion de normalidad
g <- ggqqplot(datos,
              x = "Peso_posterior",
              y = "Tratamiento",
              color = "Tratamiento")

g <- g + facet_wrap(~ Tratamiento)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)



#cat("Procedimiento  ANOVA  usando  aov\n\n")
#prueba  <- aov(Peso_posterior ~ Tratamiento , data = datos)
#print(summary(prueba))



#Procedimiento  ANOVA  con  ezANOVA ().
cat("\n\nProcedimiento  ANOVA  usando  ezANOVA\n\n")
prueba2  <- ezANOVA(
  data = datos ,
  dv = Peso_posterior ,
  between = Tratamiento ,
  wid = instancia ,
  return_aov = TRUE)
print(prueba2)

#Con este valor de p = 0.00044 < alfa (0.01), se puede asegurar con un 95% de confianza que existe evidencia suficiente 
# para rechazar la hipotesis nula (H0) en favor de la hipotesis alterntiva. Por lo tanto, segun los datos,
# existe una diferencia significativa entre los pesos finales de las mujeres segun el tratamiento.