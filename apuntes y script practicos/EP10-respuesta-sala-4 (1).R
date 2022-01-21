library(ggpubr)
library(tidyverse)
library(ez)

##############################
#####     Pregunta 1     #####
##############################

# Dos artículos reportan el porcentaje de acierto alcanzado por dos algoritmos de clasificación,
# específicamente el Bayes ingenuo (C4) y el Bayes ingenuo oculto (C2), en diferentes conjuntos de prueba
# disponibles en el UCI Machine Learning Repository. ¿Es uno de los algoritmo mejor que el otro?

##############################
#####     Desarrollo     #####
##############################

texto <-("
Dataset C2 Dataset C4
'tae' 43.82 'credit' 85.67
'anneal' 97.44 'monks' 61.68
'pasture-production' 85.27 'soybean' 91.52
'contact-lenses' 67.77 'segment' 90.74
'primary-tumor' 47.52 'squash-unstored' 61.11
'kr-s-kp' 91.90 'mushroom' 95.27
'solar-flare-C' 87.68 'page-blocks' 92.95
'monks1' 99.44 'grub-damage' 47.23
'white-clover' 78.73 'cmc' 50.49
'ecoli' 79.48 'waveform' 79.30
'nursery' 93.72 'postoperatie' 66.11
'squash-stored' 57.44 -- --
")
datos <- read.table(textConnection(texto), header = TRUE, na.strings = "--")

# Se obtienen las columnas de C2 y C4
c2 <- datos$C2
c4 <- datos$C4

# Hipótesis:
# H0 = No hay diferencia en el porcentaje de aciertos por ambos algoritmos (se distribuyen de igual forma)
# HA = Si hay diferencia en el porcentaje de aciertos por ambos algoritmos (distribuciones distintas)

# Se estudia la normalidad de los algoritmos:
g_c2 <- gghistogram(data = datos,
                    x = "C2",
                    bins = 10,
                    xlab = "Algoritmo C2",
                    ylab = "Frecuencia",
)

g_c4 <- gghistogram(data = datos,
                    x = "C4",
                    bins = 10,
                    xlab = "C1",
                    ylab = "Algoritmo C4",
)
print(g_c2)
print(g_c4)

# Como las distribuciones no se asemejan a una normal se utilizara  
# la prueba de Mann-Whitney.

# Condiciones:
# 1) Las observaciones de ambas muestras son independientes
# 2) La escala de medicion empleada debe ser a lo menos ordinal.

# Las muestras son independientes al tratarse de dos algoritmos distintos.
# La escala de medición es ordinal, ya que se trata de los porcentajes de aciertos alcanzados por los algoritmos,
# por lo que se puede hablar de una relación de orden tipo "mayor a", "menor que", "mayor o igual que"

# Como se cumplen las dos condiciones podemos proceder con la prueba.

# Se establece un nivel de significación
alfa = 0.05
# Hacer la prueba de Mann-Whitney
prueba <- wilcox.test(c2, c4, alternative = "two.sided", conf.level = 1-alfa)
print(prueba)
# p-value = 0.6947

# Conclusión:
# Como p > alfa, no existe evidencia para rechazar la hipótesis nula a favor de la 
# hipótesis alternativa.
# Se concluye con un 95% de confianza que no hay diferencia en el porcentaje de aciertos por ambos algoritmos.


##############################
#####     Pregunta 2     #####
##############################
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de interés de
# los créditos en Chile, necesite utilizar una prueba de los rangos con signo de Wilcoxon, debido a
# problemas con la escala de la variable dependiente en estudio. Indiqué cuáles serían las variables/niveles
# involucrados en su ejemplo y las hipótesis nula y alternativa a contrastar.

##############################
#####     Desarrollo     #####
##############################

# Desde que en el  mes de julio de 2021 el Banco Central comenzó con las alzas de tasas post pandemia, las tasas de créditos tanto de consumo como comerciales e hipotecarios,
# también ha aumentado. Esto significa que es más caro para las personas y empresas acceder a ellos para poder invertir, lo que frena el ritmo en el que crece el consumo.
# Fuente: https://www.biobiochile.cl/noticias/economia/actualidad-economica/2021/10/15/lo-bueno-y-lo-malo-del-alza-de-la-tasa-de-interes-para-el-bolsillo-de-los-chilenos.shtml

# Se realiza un estudio donde se elige aleatoriamente a 10 participantes que han utilizado tarjetas de crédito de casa comerciales a lo largo de tres años.
# Se les consulta cuánto han percibido la variación de interés por medio de una escala Likert de 7 puntos,
# en donde 1 representa que los intereses casi o prácticamente no subieron,
# y 7 representa que han percibido un aumento considerable.

# Las variables involucradas son
# La variable dependiente: La percepción de las personas respecto a la subida de las tasas de interés de créditos
# La variable independiente: Las tasas de interés de los créditos en sí
# Y las hipótesis son:
# Hipótesis nula H0= Las tasas de interés de los créditos comerciales de las personas no ha tenido diferencias en su percepción a lo largo de tres años.
# Hipótesis alternativa HA= Las tasas de interés de los créditos comerciales ha sido mayor a lo largo de tres años.



##############################
#####     Pregunta 3     #####
##############################

#El siguiente texto muestra porcentaje de acierto alcanzado por tres algoritmos de clasificación en
#diferentes conjuntos de prueba disponibles en el UCI Machine Learning Repository. Los algoritmos
#corresponden a C3: averaged one-dependence estimator (AODE), C6: locally weighted naive-Bayes y C7:
#  random forest. ¿Existe un algoritmo mejor o peor que los otros?

##############################
#####     Desarrollo     #####
##############################

texto2 <- ("
Dataset C3 C6 C7
'eucalyptus' 58.15 58.96 58.84
'pendigits' 97.26 94.25 95.11
'primary-tumor' 46.93 48.99 37.75
'iris' 92.11 91.44 92.77
'optdigits' 96.34 93.64 91.24
'waveform' 84.36 83.06 79.12
'yeast' 57.18 56.92 55.70
'glass' 73.27 75.13 72.77
'solar-flare-X' 97.28 93.85 95.43
'sonar' 80.70 80.23 77.80
'hepatitis' 83.23 81.94 80.69
'page-blocks' 96.39 93.59 96.41
'solar-flare-C' 87.98 87.36 85.49
'pima-diabetes' 74.45 74.19 72.11
'credit' 84.51 84.66 82.77
'solar-flare-m' 87.36 86.43 84.90
")
datos2 <- read.table(textConnection(texto2), header = TRUE)

#se decide usar prueba de friedman, ya que la escala en la que se mide es ordinal, por lo cual no se cumple
#una de las condiciones de anova.

#se deben comprobar las condiciones de esta prueba:
# 1) La variable independiente debe ser categórica y tener a lo menos tres niveles:
#     la variable independiente en este caso es algoritmo, efectivamente es categorica, y cuenta con tres niveles
#     #C3", "C6" y "C7", los cuales son el algoritmo empleado.
# 2) La escala de la variable dependiente debe ser, a lo menos, ordinal: 
#     la varible porcentaje de acierto es efectivamente a los menos ordinal, ya que se puede ordenar de mayor a menor
# 3) Se puede asumir, que la muestra es indepiendente de la poblacion si se confia en la base de datos.


# HipÃ³tesis:
# H0 = No hay diferencia en el porcentaje de aciertos de los algoritmos
# HA = Si hay diferencia en el porcentaje de aciertos de los algoritmos


# Llevamos los datos a formato largo
datos2 <- datos2 %>% pivot_longer(c("C3", "C6", "C7"),
                                  names_to = "algoritmo", values_to = "aciertos")
datos2[["algoritmo"]] <- factor(datos2[["algoritmo"]])
datos2[["Dataset"]] <- factor(datos2[["Dataset"]])

g2 <- ezPlot(data =datos2, 
             dv = aciertos, 
             wid = Dataset, 
             between = algoritmo, 
             y_lab = "% Aciertos",
             x = algoritmo)
print(g2)


prueba2 <- friedman.test(aciertos ~ algoritmo | Dataset, data = datos2)
print(prueba2)


post_hoc <- pairwise.wilcox.test(datos2$aciertos,
                                 datos2$algoritmo,
                                 p.adjust.method = "holm",
                                 paired = TRUE)
print(post_hoc)

#Conclusion
#   la realización de la prueba friedman arrojo un valor p de 0.00633 < 0.05 (alfa), por lo cual se rechaza la 
# hipotesis nula en favor de la hipotesis alternativa, es decir, hay una diferencia en los porcentajes de aciertos
# de los algoritmos.
#   Mediante un analisis post hoc, se obtiene un valor p de 0.0023 entre los algoritmos C3 y C7, es decir, estos dos
# algoritmos son los que tienen una mayor diferencia en su desempeño (porcentaje de acierto)
# Ademas si se observa el grafico del tamaño de efecto se concluye que el mejor algoritmo es el C3, 
# seguido por C6 y C7.



##############################
#####     Pregunta 4     #####
##############################

# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en
# donde un estudio o experimento, relacionado con el alza que han experimentado las tasas de interés de
# los créditos en Chile, necesite utilizar una prueba de Kruskal-Wallis, debido a problemas con la normalidad
# de los datos. Indiqué cuáles serían las variables/niveles involucrados en su ejemplo y las hipótesis nula y
# alternativa a contrastar.

##############################
#####     Desarrollo     #####
##############################

# Desde que en el  mes de julio de 2021 el Banco Central comenzó con las alzas de tasas post pandemia, las tasas de créditos tanto de consumo como comerciales e hipotecarios,
# también ha aumentado. Esto significa que es más caro para las personas y empresas acceder a ellos para poder invertir, lo que frena el ritmo en el que crece el consumo.
# Fuente: https://www.biobiochile.cl/noticias/economia/actualidad-economica/2021/10/15/lo-bueno-y-lo-malo-del-alza-de-la-tasa-de-interes-para-el-bolsillo-de-los-chilenos.shtml

# Se realiza un estudio donde se elige aleatoriamente a 30 personas pertenecientes a tres bancos: Banco de Chilezuela, Banco Tandanser y Banco Nordicbankm
# para evaluar el consumo mensual de los clientes a lo largo de los años 2020 y 2021.
# Se quiere conocer si ha existido una variación en el consumo debido al aumento de las tasas de interés de los créditos.

# Las variables involucradas son
# Variable dependiente: El consumo mensual de los clientes de los bancos 
# Variable independiente: El mes y año del consumo.
# Y las hipótesis son:
# Hipótesis nula H0= El consumo crediticio de los clientes no ha cambiado entre enero del 2020 y diciembre del 2021.
# Hipótesis alternativa HA= El consumo crediticio de los clientes ha tenido diferencias entre enero del 2020 y diciembre del 2021.

