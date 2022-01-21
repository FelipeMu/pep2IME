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
waist <- datos_filtrados["Waist.Girth"]

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
datos_filtrados_conpeso <- cbind(peso, datos_filtrados_conpeso)

nulo <- lm(Weight ~ 1, data = datos_filtrados_conpeso)

newdata <- cbind(peso, newdata)
completo <- lm(Weight ~ ., data = newdata)

# posterior a esto se hace un ajuste escalonada:
seleccion_escalonada <- step(nulo, scope = list(upper = completo), direction = "both",
                 trace = 1, steps = 6)
# como se puede observar a travpes de una seleccion esclonada de predictores, se llego a la
# conclusion que aquella variable predictora que mejora el modelo en mayor medida (y por lo
# tanto lo penaliza de forma mínima) es "Waist.Girth" (Grosor a la altura de la cintura), 
# esta contribuye a bajar el AIC  de 237.42 a 161.04. Es por ello  que se decide tomar este 
# predictor como una variable util para predecir el peso  de las mujeres.


# Adicionalmente, con la función cor se obtiene el nivel de correlación que existe
# entre las variables del data.frame, así, se ve que Waist.Girth presenta la
# correlación mas fuerte con Weight, esto motiva aún mas a seleccionar dicha variable
# como predictor del modelo.
correlaciones0 <- round(cor(x = newdata, method = "pearson"), 5)
print(correlaciones0)

# Ya con la variable selecionada se procede a relaizar la preuba de Regresion Lineal simple:

RLS<- lm(Weight ~ Waist.Girth, data = newdata)
print(summary(RLS))
# Se puede observar que el coeficiente de determinacion equivale a 0.7915,
# lo que hace notar que dicho predictor lograr explicar la variabilidad de la
# respuesta en un ~ 80%, dejando solo un 20% de la variabilidad de la respuesta
# sin explicar.

# Se puede observar tambien de los datos entregados por la funcion lm que la 
# pendiente entre la variable de respuesta y la predictora es 1.14137, lo cual
# indica que si se aumenta el grosor a la altura de la cintura es una unidad (1 cm)
# entonces el peso aumenta aproximademanre en 1.14 [kg]. Tambien se puede observar 
# que la intercepción es -19.24538, que indica la respuesta que se obtendría en caso que 
# la variable predictora(Waist.Girth) tuviera valor 0. Esto suponiendo que el modelo
# fuese valido para Waist.Girth=0, que no siempre ocurre. De hecho para este caso 
# es imposible que una persona tenga peso negativo.

#Se grafica el modelo.
g <- ggscatter(newdata, x = "Waist.Girth", y = "Weight", color = "blue",
               xlab = "Grosor a la altura de la cintura", ylab = "Peso")
print(g)



# SEXTO PASO
# Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de
# regresión líneal simple obtenido en el paso 5

# se procede ajustar los modelos 

#Se realiza el ajuste del modelo nulo y completo (será utilizado para el cuarto paso)
#Para ello, se agrega nuevamente la columna peso
predictores_p6 <- datos_filtrados[predictores]
predictores_p6 <- cbind(peso, predictores_p6)
predictores_p6 <- cbind(waist, predictores_p6)

rls_update <- lm(Weight ~ Waist.Girth, data = predictores_p6)
print(summary(rls_update))

#Se grafica el modelo.
g_2 <- ggscatter(predictores_p6, x = "Waist.Girth", y = "Weight", color = "blue",
                 xlab = "Grosor a la altura de la cintura", ylab = "Peso")
print(g_2)

#modelo completo
completo_2 <- lm(Weight ~ ., data = predictores_p6)

#Para ello se realiza un ajuste con selección escalonanda
adelante_2 <- step(rls_update, scope = list(upper = completo_2), direction = "both",
                   trace = 1)

#Con esto, se agregan cuatro predictores al modelo: Thigh.Girth (Grosor promedio de ambos 
# muslos bajo el pliegue del gluteo), Wrists.diameter (Suma de los diámetros de las muñecas),
# Calf.Maximum.Girth (Grosor promedio de la parte más ancha de ambas pantorrillas) y 
# Shoulder.Girth (Grosor de los hombros sobre los músculos deltoides), esta cantidad
# queda dentro del rango solicitado de agregar 2-5 predictores. Estos predictores se
# agregan dado que minimizan el valor de AIC (es decir, mejoran el modelo).

# Posterior a esto, se actulaiza el modelo rls

modelo_final <- update(rls_update, . ~ . + Thigh.Girth + Wrists.diameter + 
                         Calf.Maximum.Girth + Shoulder.Girth)


# SEPTIMO PASO
# Evaluar los modelos y "arreglarlos" en caso de que tengan algún problema con las condiciones que
# deben cumplir.
#Esto en torno a un nivel de significancia de
alfa <- 0.05


#Comprobación de que los datos presentan una relación lineal.
#Este se puede verificar con:
correlaciones <- round(cor(x = newdata, method = "pearson"), 3)
print(correlaciones)

#Donde el R obtenido para el par Weight ~ Waist.Girth corresponde a 0.890, lo que indica
# que existe una relación relativamente fuerte, así se puede comprobar que los datos siguen
# una tendencia lineal.

#Comprobación de la distribución de los residuos (aproxima a la normal)
cat("Prueba de normalidad para los residuos\n")
print(shapiro.test(rls_update$residuals))

#Se cumple con la normalidad de residuos, puesto que el p-value obtenido está
# por sobre el nivel de significancia, por lo que se puede concluir que los resi
# -duos tienen un comportamiento aproximado a normal.

#Comprobación de la variabilidad de los puntos entorno a la línea de mínimos cuadrados
# debe ser aproximadamente constante.
b_1 <- rls_update$coefficients[2]
b_0 <- rls_update$coefficients[1]
residuos <- newdata[["Weight"]] - (b_1 * newdata[["Waist.Girth"]] + b_0)
muestra_mujeres <- data.frame(newdata, residuos)
g_var <- ggscatter(muestra_mujeres, x = "Waist.Girth", y = "residuos", color = "blue", fill = "blue",
                   xlab = "Grosor a la altura de la cintura", ylab = "Residuos")
g_var <- g_var + geom_hline(yintercept = 0, colour = "red")
print(g_var)


#Ante esto, al observar la gráfica se puede apreciar que la variabilidad de de los
# residuos es relativamente constante.

#Comprobación de que las observaciones deben ser independientes entre si.
#En este caso, las observaciones son independientes entre sí, pues han sido
# seleccionadas de manera aleatoria y corresponden a menos del 10% de la población.

#Se realiza la comprobación de condiciones para el RLM.
#Las variables predictoras deben ser cuantitativas o dicotómicas.
#   Si, son cuantitativas, ya que cada uno de los predictores son 'medidas' numéricas.
#
#La variable de respuesta debe ser cuantitativa y continua, sin restricciones para su variabilidad.
#   Si, la variable respuesta(peso) es cuantitativa y además continua.
#
#Los predictores deben tener algún grado de variabilidad (no pueden ser constantes).
#   Si, los predictores poseen variabilidad; no son constantes (son medidas que varían).
#
#Cada predictor se relaciona linealmente con la variable de respuesta.
#   Si, puesto que al analizar las correlaciones con la variable de respuesta (peso)
#   se puede ver que presentan valores muy cercanos a 1 (fuertemente relacionados).
correlaciones_2 <- round(cor(x = predictores_p6, method = "pearson"), 3)

#Comprobación de independencia de los residuos.
cat("\nPrueba de Durbin-Watson para autocorrelaciones.")
cat("entre errores:\n")
print(durbinWatsonTest(modelo_final))

#Comprobación de normalidad de los residuos.
cat("\nPrueba de normalidad para los residuos:\n")
print(shapiro.test(modelo_final$residuals))

#Comprobación de homocedasticidad de los residuos.
cat("\nPrueba de homocedasticidad para los residuos:\n")
print(ncvTest(modelo_final))

#Comprobación de multicolinealidad
vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")


#Ante esto se puede ver que:
# i)Se cumple con la independencia de los residuos, puesto que el p-value obtenido
#   está muy por sobre el nivel de significancia, por lo que se puede concluir que 
#   en efecto los residuos son independientes.
#
# ii)Se cumple con la normalidad de residuos, puesto que el p-value obtenido está
#   por sobre el nivel de significancia, por lo que se puede concluir que los resi
#   -duos tienen un comportamiento aproximado a normal.
#
# iii)Se cumple con la homocedasticidad de los residuos, puesto que el p-value obtenido
#   está muy por sobre el nivel de significancia, por lo que se puede concluir que
#   los residuos tienen varianzas similares para cada nivel de los predictores.
#
# iv)En el caso de la multicolinealidad, los datos recabados sugieren que el modelo
#   podría estar sesgado, puesto que las tolerancias no superan (en la mayoría de 
#   los casos) el valor 0.4, donde además el VIF promedio supera el valor 2.5, lo 
#   que aumenta la preocupación en este aspecto.
modelo_final_corregido <- update(modelo_final, . ~ .  -Shoulder.Girth - Calf.Maximum.Girth)

#Comprobación de multicolinealidad
vifs_corregido <- vif(modelo_final_corregido)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs_corregido)
cat("- Tolerancias:\n")
print(1 / vifs_corregido)
cat("- VIF medio:", mean(vifs_corregido), "\n")

#Con lo anterior, eliminando dos predictores se pudo corregir el modelo, obteniendo
# valores que permiten verificar la multicolinealidad.

# OCTAVO PASO
#Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo 
# (o utilizando validación cruzada)
 # Para evaluar el poder predictivo se hace uso del conjunto entrenamiento y prueba:
###########RLS#################
nRLS <- nrow(newdata)
n_entrenamientoRLS <- floor(0.8 * nRLS) # 80# conjunto entrenamiento
muestraRLS <- sample.int(n=nRLS, size=n_entrenamientoRLS, replace = F)
entrenamientoRLS <- muestra_mujeres[muestraRLS, ]
pruebaRLS <- muestra_mujeres[-muestraRLS, ]
#aplicando el modelo
modelo_cruzadaRLS <- train(Weight ~ Waist.Girth, data = entrenamientoRLS, method = "lm",
                           trControl = trainControl(method = "cv", number = 5))
print(summary(modelo_cruzadaRLS))

#datos importantes

#Se obtiene el MSE (error cuadrático medio) para el conjunto entrenamiento.
mse_entrenamientoRLS <- modelo_cruzadaRLS$results$RMSE

#Se realizan las predicciones
prediccionesRLS <- predict(modelo_cruzadaRLS, pruebaRLS)

#Se calcula el error cuadrático medio para el conjunto de prueba.
errorRLS <- pruebaRLS[["Weight"]] - prediccionesRLS
mse_pruebaRLS <- mean(errorRLS ** 2)
rmse_pruebaRLS <- sqrt(mse_pruebaRLS)


#######RLM################
#PARA RLM
#Se crea el conjunto de entrenamiento para la validación.
nRLM <- nrow(predictores_p6)
n_entrenamientoRLM <- floor(0.8 * nRLM)
muestraRLM <- sample.int(n=nRLM, size=n_entrenamientoRLM, replace = F)
entrenamientoRLM <- predictores_p6[muestraRLM, ]
pruebaRLM <- predictores_p6[-muestraRLM, ]

modelo_cruzadaRLM <- train(Weight ~ Waist.Girth + Thigh.Girth + Wrists.diameter, data = entrenamientoRLM, method = "lm",
                           trControl = trainControl(method = "cv", number = 5))
print(summary(modelo_cruzadaRLM))


#Se obtiene el MSE (error cuadrático medio) para el conjunto entrenamiento.
mse_entrenamientoRLM <- modelo_cruzadaRLM$results$RMSE

#Se realizan las predicciones
prediccionesRLM <- predict(modelo_cruzadaRLM, pruebaRLM)

#Se calcula el error cuadrático medio para el conjunto de prueba.
errorRLM <- pruebaRLM[["Weight"]] - prediccionesRLM
mse_pruebaRLM <- mean(errorRLM ** 2)
rmse_pruebaRLM <- sqrt(mse_pruebaRLM)

#Errores
cat("\nError cuadrático medio RLS para entrenamiento: ", mse_entrenamientoRLS, "\n")
cat("\nError cuadrático medio RLS para prueba: ", rmse_pruebaRLS, "\n")
cat("\nError cuadrático medio RLM para entrenamiento: ", mse_entrenamientoRLM, "\n")
cat("\nError cuadrático medio RLM para prueba: ", rmse_pruebaRLM, "\n")


