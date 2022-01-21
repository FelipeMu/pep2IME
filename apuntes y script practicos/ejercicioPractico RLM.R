library(ggpubr)
library(ggplot2)
library(dplyr)
library(leaps)
library(car)
library(lmtest)
library(caret)
#Se especifica y almacena la ruta del directorio de la base de datos.
dir <- "~/../Desktop"
base <- "Body-EP12.csv"
arch <- file.path(dir, base)

# Se realiza la lectura de los datos, se escpecifica el formato de codificación UTF-8
datos <- read.csv(arch, fileEncoding = "UTF-8")
datos

#definiendo la semilla a utilizar:
semilla <- 2166
set.seed(semilla)

# Como la semilla es un numero par, se decide tomar una muestra de 50 mujeres:
tamano_muestra <- 50
# Primero se debe filtrar los datos:
mujeres <- filter(datos, Gender==0)
# Posteriormente, se procede a crear la muestra:
#replace : F // para no volver a ingresar la observacion ingresada a la muestra nuevamwente al conjunto "mujeres"
datos_filtrados <- mujeres[sample(nrow(mujeres), 50, replace = F), ]

# Ahora se debe selecionar 8 variables predictoreas de forma aleatoria:
# Se debe tener en cuenta que se quiere predecir el peso de las mujeres, 
# por lo tanto, las variables "weight" y "Gender" no deberian ser parte
# de las variables a buscar:

#Se guarda la columna peso para utilizarlo mas adelante.
peso <- datos_filtrados["Weight"]


# de colocan como nulas:
datos_filtrados["Gender"] <- NULL
datos_filtrados["Weight"] <- NULL

#datos filtrados para incorporar el peso mas adelante
datos_filtrados_conpeso <- datos_filtrados

# Se selecionan de forma aleatoria 8 predictores
#obtenemos los nombres de las columnas:
nombreCol <- colnames(datos_filtrados)
#seleccionamos los 8 predictores de forma random a traves de sample()
predictores <- sample(nombreCol, 8, replace=F)
predictores

# 4to PASO:

#Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la
#variable Peso, justificando bien esta selección
VarRestantes <- setdiff(nombreCol, predictores)
VarRestantes

# para decicidr cual variable podria ser util para predecir el peso, se hace uso
# de la funcion step, con el objetivo de visualizar cual de todas es la que presenta
# menor AIC:

# Para ello, se acota el data.frame con las variables contenidas en "VarRestantes"
newdata <- datos_filtrados[VarRestantes]

# Para usar la funcion step() se debe definir el modelo nula y completo;
# para el caso del completo se debe incorporar nuevamente la variable peso:


nulo <- lm(Weight ~ 1, data = datos_filtrados)

datos_filtrados_conpeso <- cbind(peso, newdata)
completo <- lm(Weight ~ ., data = datos_filtrados_conpeso)

# posterior a esto se hace un ajuste escalonada:
adelante <- step(nulo, scope = list(upper = completo), direction = "both",
                 trace = 1, steps = 5)



