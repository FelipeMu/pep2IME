library(ggpubr)
library(dplyr)
library(tidyverse)
library(ez)

##############################
#####     Pregunta 1     #####
##############################

# Pregunta 1
# El siguiente código R carga los datos que aparecen en una tabla que compara las mejores soluciones
# encontradas por cuatro algoritmos para instancias del problema del vendedor viajero con solución optima
# conocida, tomados desde una memoria de título del DIINF. Con estos datos responda la pregunta de
# investigación: ¿Hay algoritmos mejores que otros

##############################
#####     Desarrollo     #####
##############################

texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 26 15.6 16.3 17.2 19
'brock400_4' 30 13.8 16.3 17.4 19
'C2000.9' 77 54.2 56.6 59.4 63
'c-fat500-10' 123 122 122 122 123
'hamming10-2' 509 340.2 416.1 419.4 509
'johnson32-2-4' 13 13 13 13 13
'keller6' 56 31.5 40 42.5 45.2
'MANN_a81' 1097 1079.2 1079.2 1079.2 1092.9
'p-hat1500-1' 9 3.9 5.1 5.9 7
'p-hat1500-3' 91 72.8 74.7 81.6 83
'san1000' 12 4.6 4.6 4.7 7
'san400_0.7_1' 37 16.6 17.5 17.5 18
'san400_0.9_1' 97 41.1 51.4 53.4 89
'frb100-40' 97 63.4 73.7 77.5 79
'frb59-26-1' 56 36.2 42.9 45.3 45
'1et.2048' 313 229.4 265.4 277.9 289.4
'1zc.4096' 376 270.8 290.2 304.4 325.5
'2dc.2048' 21 12.6 15.7 16.9 18
")
datos <- read.table(textConnection(texto), header = TRUE)

# Llevar todos los datos a una misma escala calculando el error
datos$R <- -1*(datos$R - datos$Optimo) / datos$Optimo
datos$R2 <- -1*(datos$R2 - datos$Optimo) / datos$Optimo
datos$R3 <- -1*(datos$R3 - datos$Optimo) / datos$Optimo
datos$G <- -1*(datos$G - datos$Optimo) / datos$Optimo

# Llevar los datos a formato largo:
datos2 <- datos %>% pivot_longer(c("R", "R2", "R3", "G"),
                                 names_to = "algoritmo", values_to = "soluciones")

datos2[["algoritmo"]] <- factor(datos2[["algoritmo"]])
datos2[["Instancia"]] <- factor(datos2[["Instancia"]])

#Hipotesis:
# H0 = Las soluciones de los algoritmos no varían
# HA = Las soluciones de los algoritmos varían

# Como se necesita comparar simultáneamente 4 medias muestrales (soluciones algoritmos) para muestras pareadas, 
# se propone utilizar la prueba ANOVA para muestras correlacionadas.


# Condiciones:
# 1) La escala con la que se mide la variable dependiente, es la misma, ya que se calculó el error de cada algoritmo
#    respecto al optimo, usando la formula (Vreal-Vaprox)/Vreal
# 2) El problema nos garantiza que las mediciones son independientes al interior de cada grupo
# 3) Se estudia la normalidad:

# Comprobación de normalidad:
g <- ggqqplot(datos2,
              x = "soluciones",
              y = "algoritmo",
              color = "algoritmo")

g <- g + facet_wrap(~ algoritmo)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Las muestras se comportan de manera normal, pero para las columnas
# G y R se pueden apreciar puntos que salen de la norma. por lo que
# se procede a usar un α = 0.01

# 4) Se estudia si la matriz de varianzas-covarianzas es esférica:

# Procedimiento ANOVA con ezANOVA()
cat("\n\nProcedimiento ANOVA con ezANOVA\n\n")
prueba <- ezANOVA(data = datos2, dv = soluciones, within = algoritmo,
                   wid = Instancia, return_aov = TRUE)
print(prueba)

#dado que el test de esfericidad mauchly's test realizado por el procedimiento ezAnova, arroja un valor de 
# p = 7.35e-8 < 0,01, es decir menor que el nivel de significancia no cumple con la esfericidad, por lo cual se debe 
# realizar una corrección con un e < 0,75, se elige la corrección de Greenhouse-Geisse, la cual arroja un valor
# p = 4.05e-06 < 0,01. Dado que el valor de p es menor al nivel de significancia se rechaza la hipótesis nula
# en favor de la hipótesis alternativa.

# Además, como p < 0,01, existe evidencia más que suficiente para rechazar la hipótesis nula.
# Por lo tanto, si hay algoritmos mejores que otros.

#Grafico del tamaño del efecto.
g2 <- ezPlot(data = datos2, 
             dv = soluciones,
             wid = Instancia, 
             within = algoritmo,
             x_lab = "algoritmo",
             y_lab = "Diferencia promedio de tiempo de ejecuci?n respecto al ?ptimo", 
             x = algoritmo)
print(g2)


# Se realiza procedimiento post Hoc
# En este caso se realiza el test de Bonferroni.
# Procedimiento post-hoc de Bonferroni.
bonferroni <- pairwise.t.test(datos2[["soluciones"]],
                               datos2[["algoritmo"]],
                               p.adj = "bonferroni", 
                               paired = TRUE)

cat("Correcci?n de Bonferroni\n") 
print(bonferroni)

# Si se observa la matriz de Bonferroni, la mayor diferencia de tiempo en lograr las soluciones corresponde
# a los algoritmos R y R3, y entre R y G. Además, si se observa el grafico de tamaño de efecto, se puede asegurar
# con un 99% de confianza que el algoritmo R es más rápido que el G.

##############################
#####     Pregunta 2     #####
##############################

# Pregunta 2
# El siguiente c?digo R carga los datos que se obtuvieron en este estudio. Con estos datos, responda la
#siguiente pregunta de investigaci?n: ?hay diferencias en los tiempos entre tareas?

##############################
#####     Desarrollo     #####
##############################

texto <- ("
words colors interfer
18 19 44
19 21 34
18 19 31
21 19 44
13 24 29
9 14 42
16 15 32
19 15 31
15 26 42
23 17 37
26 24 33
21 19 32
21 20 38
12 5 44
16 16 36
13 22 47
")
datos3 <- read.table(textConnection(texto), header = TRUE)

instancia <- factor(1:nrow(datos3))
datos3 <- datos3 %>% add_column(instancia, .before = "words")
# Llevar los datos a formato largo
datos4 <- datos3 %>% pivot_longer(c("words", "colors", "interfer"),
                                 names_to = "tareas", values_to = "tiempo")
datos4[["tareas"]] <- factor(datos4[["tareas"]])

#Hipotesis:
# H0 = Los tiempo en leer el estimulo no varian
# HA = Los tiempo en leer el estimulo varian

# Como se necesita comparar simultáneamente 4 medias muestrales (soluciones algoritmos) para muestras pareadas, 
# se propone utilizar la prueba ANOVA para muestras correlacionadas.

# Condiciones:
# 1) La escala con la que se mide la variable dependiente (tiempo) tiene las propiedades de una escala de intervalos iguales (segundos)
# 2) El problema nos garantiza que las mediciones son independientes al interior de cada grupo
# 3) Se estudia la normalidad:

# Comprobacion de normalidad
g3 <- ggqqplot(datos4,
              x = "tiempo",
              y = "tareas",
              color = "tareas")

g <- g + facet_wrap(~ algoritmo)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g3)
# Las muestras se comportan de manera normal, por lo que se usa un alpha de 0.05


# 4) Se estudia si la matriz de varianzas-covarianzas es esférica:

# Procedimiento ANOVA con ezANOVA()
# Procedimiento ANOVA con ezANOVA()
cat("\n\nProcedimiento ANOVA con ezANOVA\n\n")
prueba2 <- ezANOVA(data = datos4, dv = tiempo, within = tareas,
                  wid = instancia, return_aov = TRUE)
print(prueba2)

#dado que la prueba de  mauchly arroja un valor p = 0.18 > 0.05 alpha, no es necesario realizar una corrección

#Grafico del tamaño del efecto.
g2 <- ezPlot(data = datos4, 
             dv = tiempo,
             wid = instancia, 
             within = tareas,
             x_lab = "tareas",
             y_lab = "tiempo en interpretar el estimulo", 
             x = tareas)
print(g2)

# Se realiza procedimiento post Hoc:
# En este caso se realiza la prueba de Bonferroni.
# Procedimiento post-hoc de Bonferroni.

bonferroni2 <- pairwise.t.test(datos4[["tiempo"]],
                               datos4[["tareas"]],
                               p.adj = "bonferroni", 
                               paired = TRUE)

cat("Corrección de Bonferroni\n") 
print(bonferroni2)

#La prueba de ANOVA arroja un valor p de 4.978967e-12 < 0.05, se rechaza la hipótesis nula en favor de la hipótesis
# alternativa, lo que quiere decir que hay diferencias en los tiempos que tardan los estudiantes en interpretar
# el estímulo. 
# Con los datos obtenidos de la prueba de Bonferroni, se puede concluir que la mayor diferencia se encuentra
# entre las lecturas de los estímulos de colors e interfer y entre words e interfer. Además analizando
# el grafico de tamaño de efecto, el estímulo donde los estudiantes tardan más en responder es interfer.
