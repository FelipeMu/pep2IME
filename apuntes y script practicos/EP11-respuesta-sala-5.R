

###ENUNCIADO 1###

# Se plantea semilla
set.seed(100)

# Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos
# grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación
# Monte Carlo.

# Pregunta: Un equipo de investigación de la USACH desea estudiar si existe diferencia en el ingreso per cápita
# entre las viviendas de personas casadas y solteras en todo Chile. Los estudiantes se dieron cuenta que los datos
# no siguen una distribución normal, por lo que se solicita responder la pregunta en base a una simulación Monte
# Carlo

# Se importan librerías:

library("readxl")
library("dplyr")

#Se escriben funciones necesarias para realizar el método


# Función que calcula la diferencia entre la evaluación de dos grupos de observaciones
# Argumentos:
# a, b: vectores numéricos
# F: Función con la cual se evalúa cada grupo de observaciones
# Valor:
# Diferencia F(a)-F(b)
calcular_diferencia <- function(a,b,F){
  # Diferencia de las evaluaciones
  dif <- F(a)-F(b)
  # Retorno
  return(dif)
}

# Función que permite calcular el p-valor en una simulación Monte Carlo cuando la hipótesis alternativa es
# considerada "two-sided"
# Argumentos:
# dist: distribución del estadístico luego de las permutaciones
# obs: estadístico de interés original
# repeticón: cantidad de permutaciones
# Valor:
# p-valor calculado
valor_p <- function(dist, obs, repeticion){
  # Se realizan cálculos necesarios para encontrar el p-valor
  dividendo <- sum(abs(dist)> abs(obs)) + 1
  divisor <- repeticion + 1
  # Se calcula el p-valor
  p <- dividendo/divisor
  # Retorno
  return(p)
}

# Función para hacer una permutación y calcular el estadístico
# de interés.
# Argumentos:
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar.
# - FUN: función del estadístico E para el que se calcula la diferencia.
# Valor:
# -diferencia E_1 -E_2
permutar <- function(muestra_1, muestra_2, FUN){
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  # Hacer la permutación
  permutacion <- sample(c(muestra_1, muestra_2), size = n_1 + n_2, 
                        replace = FALSE)
  
  # Asignar elementos a los dos grupos.
  permutacion_1 <- permutacion[1 :n_1]
  permutacion_2 <- permutacion[n_1 + 1 : n_2]
  # Calcular y devolver la diferencia de medias.
  return(calcular_diferencia(permutacion_1, permutacion_2, FUN))
}




# Lectura de datos
my_data <- read_excel(file.choose())

# Se filtra las observaciones de personas solteras, ignorando las respuestas nulas con respecto a la cantidad de 
# personas en el hogar
solt2 <- my_data %>% filter(ecivil == "Soltero(a)", numper != "NA")

# Se eligen 300 muestras
solt <- solt2[sample(nrow(solt2), 300),]


# Se filtra las observaciones de personas casadas, ignorando las respuestas nulas con respecto a la cantidad de 
# personas en el hogar
casd2 <- my_data %>% filter(ecivil == "Casado(a)", numper != "NA")

# Se eligen 300 muestras
casd <- casd2[sample(nrow(casd2), 300),]

# Se encuentra el ingreso per capita por cada tipo de hogar
solt <- solt %>% mutate(ingresoS = ytotcorh / as.numeric(numper)) %>% select(ingresoS)
casd <- casd %>% mutate(ingresoC = ytotcorh/ as.numeric(numper)) %>% select(ingresoC)

# Hipótesis:

# H0: No hay diferencia per capita entre solteros y casados (µs = µc, en donde µs corresponde a la media de las personas
#     solteras y µc corresponde a la media de las personas casadas) 

# Ha: Hay diferencia per capita entre solteros y casados (µs != µc)

# Se define un valor de alfa

alfa <- 0.05


# Para desarrollar el ejercicio por medio del metodo Monte Carlo se realizaran 259 permutaciones
per <- 759

# Se calcula el valor observado con los datos originales
obs <- calcular_diferencia(solt$ingresoS, casd$ingresoC, mean)

# Vector en donde se guardarán los valores de los estadísticos
dist <- rep(NA, per)

# Cálculo de las permutaciones y los estadísticos correspondientes.
for(i in 1: per){
  dist[i]<- permutar(solt$ingresoS, casd$ingresoC, mean)
}

# Cálculo del p-valor
p <- valor_p(dist, obs, per)

print(p)

# Por ende, debido a que el p-valor es mayor al valor de alfa (0.4289 > 0.05), con 95% de confianza no hay evidencia
# suficiente como para rechazar la hipótesis nula en favor de la alternativa, es decir no existe diferencia en los 
# ingresos per capita entre las viviendas de personas casadas y solteras.



###ENUNCIADO 2###

# Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de
# dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
# utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping
# aunque este no sea necesario.


# Pregunta: Una controvertida tienda que vende zapatillas únicamente a mujeres según la cantidad de hijos vivos que hayan
# tenido desea saber si varía el promedio de hijos por estado civil de las mujeres("Casado(a)", "Soltero(a)", "Viudo(a)")
# con el fin de cambiar sus estrategias de venta y de marketing, enfocándose en personas pertenecientes a un único tipo de
# estado civil. Se solicita responder la pregunta utilizando bootstrapping. Se busca seleccionar un grupo de 500 observaciones
# con 359 remuestreos.

# Se establece un seed
set.seed(159)

library(boot)
library(ggpubr)
library(ez)
library(tidyverse)
library(readxl)
library(dplyr)


# Función que remuestrea una muestra en base a columnas cas, sol y viu
# Argumentos:
# i: Instancia de remuestreo
# Valor:
# matriz con valores remuestreados
boot_manual <- function(i){
  cas_copia <- sample(1:nrow(cas), replace = TRUE) 
  sol_copia <- sample(1:nrow(sol), replace = TRUE)
  viu_copia <- sample(1:nrow(viu), replace = TRUE)
  rbind(cas[cas_copia,], sol[sol_copia,], viu[viu_copia,])
}

# Función que calcula el estadístico F
# Argumentos:
# f: dataframe
# Valor:
# estadístico F
encontrar_F<- function(f){
  # Encontrar estadístico F
  anova <- ezANOVA(f, dv = s4, between = ecivil, 
                   wid = Instancia, return_aov = FALSE)
  anova[["ANOVA"]][["F"]]
}


# Se establece un alfa
alfa <- 0.05

# Se plantean hipótesis

# H0: No hay diferencia entre las medias de hijos según estado civil (µs = µc = µv, en donde µs corresponde a la
# media de las mujeres solteras, µc corresponde a la media de las mujeres casadas y µv corresponde a la media de
# las mujeres viudas) 

# Ha: Hay diferencia en almenos una de las media de hijos de acuerdo al estado civil de las mujeres (µs != µc != µv)

# Lectura de datos
my_data <- read_excel(file.choose())

# Se filtran las observaciones referentes a Mujeres que se encuentren en alguno de los estados civiles 
# definidos. Luego se selecciona las columnas de estado civil (ecivil) y su número de hijos vivos (s4)
variables <- my_data %>% filter(sexo=="Mujer" & (ecivil == "Casado(a)" | ecivil == "Soltero(a)" | 
                                                  ecivil == "Viudo(a)")) %>% select(ecivil, s4)

# Se seleccionan 450 observaciones
variables <- variables[sample(nrow(variables), 500),]

# Se establece vector Instancias
Instancia <- (1:500)

# Se construye el dataframe con los datos en investigación
datosFinales <- data.frame(Instancia, variables)

# Se seleccionan instancias de datos de acuerdo al estado civil
cas <- datosFinales %>% filter(ecivil == "Casado(a)")
sol <- datosFinales %>% filter(ecivil == "Soltero(a)")
viu <- datosFinales %>% filter(ecivil == "Viudo(a)")




# Se calcula el valor observado original aplicando ezANOVA
observado <- ezANOVA(datosFinales, dv = s4, between = ecivil, 
                    wid = Instancia, return_aov = FALSE)

# Se establece el número de remuestreos
R <- 359

# Se realizan los remuestreos
distribuciones  <- lapply(1:R, boot_manual)

# Se calculan los valores F suprimiendo los mensajes de Warning
suppressMessages(suppressWarnings(valores_F <- sapply(distribuciones, encontrar_F)))

# Se calcula el valor P
numerador <- sum (abs(valores_F) > abs(observado[["ANOVA"]][["F"]])) + 1
denominador <- R + 1
p <- numerador / denominador 
print(p)


# Luego, con el valor p obtenido al ser mayor al valor de alfa (0.064 > 0.05), con 95% de confianza se puede afirmar que 
# no hay evidencia suficiente como para rechazar la hipótesis nula en favor de la alternativa, por lo tanto no existe diferencia
# entre las medias de hijos de los estados civiles de las mujeres. Ahora se procede con el procedimiento post-hoc.


# Análisis post -hoc.
# Función para calcular la diferencia de medias de la cantidad de hijos para 2 estados civiles
# Argumentos:
# datos: en formato largo
# ecivil1: Estado civil 1
# ecivil2: Estado civil 2
# Valor:
# Diferencia de medias

media_diferencias <- function(datos , ecivil1, ecivil2) {
  i1 <- datos[["ecivil"]] == ecivil1
  i2 <- datos[["ecivil"]] == ecivil2
   media <- mean(datos[["s4"]][i1]) - mean(datos[["s4"]][i2])
   return(media)
   }

 # Funci ón para generar la diferencia de medias para las distribuciones encontradas
# Argumentos:
# dist: distribuciones encontradas
# ecivil1: Estado civil 1
# ecivil2: Estado civil 2
# Valor:
# Diferencia de medias
 distribucion_diferencias <- function(dist, columna_1, columna_2){
 B <- length(dist)
 distribucion <- c()
 
 for (i in 1:B) {
  datos <- as.data.frame(dist[i])
 
  diferencia <- media_diferencias (datos , columna_1, columna_2)
  distribucion <- c( distribucion , diferencia )
 }
  
 return(distribucion)
 }
 
# Se guardan estados civiles en variables 
c <- "Casado(a)"
s <- "Soltero(a)"
v <- "Viudo(a)"

 
 # Calcular diferencias observadas en la muestra original.
 dif1 <- media_diferencias(datosFinales, c , s)
 dif2 <- media_diferencias(datosFinales, c , v)
 dif3 <- media_diferencias(datosFinales, s , v)


 # Generar diferencias por cada una de las distribuciones encontradas
 dif_dist_1 <- distribucion_diferencias(distribuciones , c , s )
 dif_dist_2 <- distribucion_diferencias(distribuciones , c , v )
 dif_dist_3 <- distribucion_diferencias(distribuciones , s , v )
 
 # Obtener valores p para cada una de las diferencias (En el numerador el valor + 1 se deja fuera de la suma
 # ya que según la página 7 del capítulo 12 de los apuntes así debiese obtenerse el valor p, a diferencia a como
 # se menciona que se debe de hacer en un procedimiento post-hoc explicado en el script de la página 22 de los
 # apuntes).
 num1 <- sum(abs(dif_dist_1) > abs(dif1)) + 1
 den1 <- R + 1
 p_1 <- num1 / den1
 
 num2 <- sum(abs(dif_dist_2) > abs(dif2)) + 1  
 den2 <- R + 1
 p_2 <- num2 / den2
 
 num3 <- sum(abs(dif_dist_3) > abs(dif3)) + 1 
 den3 <- R + 1
 p_3 <- num3 / den3

# Luego, de acuerdo al análisis Post-Hoc no existen diferencias cosiderables entre las diferencias de medias 
# de los grupos (como había sido previsto al calcular el valor p) debido a que el valor de p de la diferencia
# entre mujeres casadas y solteras es de 0.467 > 0.05, la diferencia entre mujeres casadas y viudas es de
# 0.506 > 0.05, y la  diferencia entre mujeres solteras 
# y viudas es de 0.456 > 0.05.
