library(ggpubr)
library(knitr)
library(tidyr)
library(car)
library(ez)
library(pwr)
library(ggplot2)
library(tidyverse)


#La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
#los pollitos es beneficioso, tanto para las avícolas como para los consumidores no veganos. En el paquete
#datasets de R (importado nativamente) está el conjunto de datos chickwts con los resultados de un
#experimento hecho (supuestamente en 1948) para medir la efectividad de varios suplementos alimenticios
#en la tasa de crecimiento de las aves, en donde pollitos recién nacidos se separaron aleatoriamente en
#seis grupos, y a cada grupo se le dio un suplemento distinto por seis semanas. Se reportan los pesos, en
#gramos, alcanzados por los pollitos. Para productores de la 7º región, es especialmente importante saber
#si deberían usar suplementos basados en linaza (linseed), soya (soybean), habas (horsebean) o carne
#(meatmeal)

datos <- chickwts


# Se necesita saber si existe alguna efectividad entre aplicar distintos
#suplementos, entre ellos:
# linaza (linseed)
# soya (soybeaN)
# habas (horsebeans)
# carne (meetmeal)

feedprev <- datos$feed
feed <- factor(feedprev)
weight <- datos$weight
instancia <- 1:nrow(datos)


Datos <- data.frame(instancia, feed, weight)
feedN <- Datos$feed


#Se obtienen los datos necesarios para el problema.
Datos <- Datos %>% filter(feedN == "linseed" | feedN == "meatmeal" | feedN == "soybean" | feedN == "horsebean")

# Dado que se desea contemplar si existe algun suplemento mejor que otr,
# se procede a establecer que la mejor prueba para este contexto es la 
# Prueba Anova de una vía para muestras independiente. El hecho de que
# dividireron los pollitos en distintos grupos y a cada grupo se le
# alimento con un suplemento específico, es que es razobable usar dicho Test.

# Sin embargo, para utilizar este test, es necesario verificar algunas condiciones:


# 1. La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
# iguales.
# 2. Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
# 3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
# 4. Las k muestras tienen varianzas aproximadamente iguales.

#verificando 1.
# Para este caso la variable dependiente es el peso, el cual se puede observar que se mide en "gramos"
# para cualquier grupo. Luego, la variable se mide en escalas de intervalos iguales.

#verificando 2:
# Dado que se habla de un experimento, es razonable pensar que las muestras obtenidas (cada grupo de pollitos)
# es independiente de las otras.

#verificando 3.
#Para verificar esta condicion, se procede a realizar el grafico QQ para cada grupo:

g <- ggqqplot(Datos,
              x = "weight", #varaible dependiente
              y = "feed",   # variable independiente
              color = "feed")

g <- g + facet_wrap(~ feed)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)
# Se puede observar a traves de los graficos que efectivamente los grupos
# se acercan a una distribución normal, salvo por uno pocos datos (valores atípicos)
# sin embargo, se procede a continuar con cautela y se toma un alfa=0.01
alpha<- 0.01

#verificando4.
#Para esta condicion se hace uso de la preuba de levene:
hom <- leveneTest(y = Datos$weight, group = Datos$feed, center = "median")
cat("\n")
cat("-------------------------------------------------------------------------\n")
cat("                          Prueba de Levene                               \n")
cat("-------------------------------------------------------------------------\n")
cat("\n")
print(hom)
# Las hipotesis a constrastar son:
#H0: Las variazan son aproximadamentes iguales
#HA: Las muestrs presentan varianzas distintas
# Para este caso la prueba Levene arroja un p-value=0.5633, lo que permite
# explicar que se falla en rechazar la hipotesis nula y por lo tanto se puede
# concluir con 99% de confianza que las varianzas de los grupos son similares.

# Ya con las 4 condiciones verificadas, se porocede a realizar la prueba ezAnova:

#estableciendo los hipotesis a contrastar:
#H0: el peso promedio para los los grupos es igual para todos los tipos de alimento.
#HA: el peso promedio para los grupos es diferente para al menos un tipo de alimento.
pruebaEzAnova <- ezANOVA(data = Datos,
                         dv = weight,
                         between = feed,
                         wid = instancia,
                         type = 3,
                         return_aov = TRUE)
print(pruebaEzAnova)


#Gráfico del tamaño del efecto.
g2 <- ezPlot(data = Datos , 
             dv = weight,
             wid = instancia, 
             between = feed,
             y_lab = "Peso promedio de pollitos [gramos]", 
             x = feed)
print(g2)


# Luego de efectuar la preuba de ezAnova se puede ver que el valor de p-value
# es igual a 9.23x10^-5, un valor muy por debajo del nivel de significancia 
# establecido, esto provoca que se rechaze la hipotesis nula en favor de la
# alternativa, por ende se puede concluir con 99% que que existe al menos
# un grupo que tiene un peso significativo en comparación a los grupos restantes.


#Luego, para identificar que grupos(s) son los que presentan una diferencia 
# significativa, se procede a realizar una pba Post-Hoc. Para ello, se utiliza
# el Test Tukey:


# 6.- Análsis POST-HOC
post_hoc <- TukeyHSD(pruebaEzAnova[["aov"]])
print(post_hoc)

# Haciendo uso del grafico del tamaño de efecto y los datos que retorna la 
# funcion TukeyHSD(), se puede discernir que dado que los "p adj" entre
# los suplementos soybean, linseed y meatmeal es mayor a alpha, contribuye
#  a que no hay una diferencia significativa en el peso promedio de los pollitos.
# Paralelamente, se puede notar que para las comparaciones del suplemnento
# horsebean con meatmeal y soybean existe una diferencia significativa.
# En conclusión , los productos deberían basarse esencialmente en suplementos
# con el meatmeal, soybean y linseed.