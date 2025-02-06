# Dataset HeartRate
data<-read.csv(file.choose(), sep=',', header=TRUE)

# Instalar dependencias
# Este comando solo necesitas ejecutarlo una vez
# install.packages(c("tidyverse", "lubridate", "forecast", "tseries", "zoo"))

# Cargamos las librerias
library(zoo)
library(tidyverse)  # Manejo de datos
library(lubridate)  # Manejo de fechas
library(forecast)   # Modelado de series temporales
library(tseries)    # Pruebas estadísticas

# Definimos la frecuencia: 24 h * 60 min / 10 min
frequency(serie_temp_interpolada_10min) <- 144 

# Alternativa con ts
serie_temp_interpolada_10min <- ts(serie_temp_interpolada_10min)


# Descomposición STL (Seasonal and Trend decomposition using Loess)
descomposicion <- stl(serie_temp_interpolada_10min, s.window = "periodic")

acf(serie_temp_interpolada_10min)

# Serie diferenciada
serie_diferenciada<-diff(serie_temp_interpolada_10min)
plot(serie_diferenciada, type = "l", main = "Serie Diferenciada (Primera Diferencia)")
adf.test(serie_diferenciada)

# Ahora que hemos hecho la diferencia, el resultado es mucho mejor
acf(serie_diferenciada)
# Resultado:
# data:  serie_diferenciada
# Dickey-Fuller = -16.598, Lag order = 12, p-value = 0.01
# alternative hypothesis: stationary

# Ajustar un modelo ARIMA automáticamente
# Importante: El modelo arima se tiene que hacer con la serie original,
# así te coje las diferencias que hayas hecho
modelo <- auto.arima(serie_temp_interpolada_10min)
summary(modelo)

# Número de datos a visualizar antes de la predicción
ultimos_n <- 100

# Obtener los últimos 100 valores de la serie diferenciada
datos_recientes <- tail(serie_temp_interpolada_10min, ultimos_n)

# Realizar la predicción
horizonte_pred <- 5  # Número de pasos a predecir
predicciones <- forecast(modelo, h = horizonte_pred)

# Crear un gráfico con los últimos datos y predicciones
plot(datos_recientes, 
     type = "l", 
     col = "blue", 
     lwd = 2, 
     xlim = c(length(serie_temp_interpolada_10min) - 100, 
              length(serie_temp_interpolada_10min) + horizonte_pred),
     ylim = c(min(serie_temp_interpolada_10min) - 5,
              max(serie_temp_interpolada_10min) + 5),
     main = "Últimos 100 datos y Predicciones", 
     ylab = "Frecuencia cardiaca", 
     xlab = "Tiempo")

# Agregar las predicciones al gráfico
lines((length(serie_temp_interpolada_10min) + 1) : (length(serie_temp_interpolada_10min) + horizonte_pred), predicciones$mean, col = "red", lwd = 2)

# Agregar bandas de confianza
lines((length(serie_temp_interpolada_10min) + 1) : (length(serie_temp_interpolada_10min) + horizonte_pred), predicciones$lower[,2], col = "gray", lty = 2)
lines((length(serie_temp_interpolada_10min) + 1) : (length(serie_temp_interpolada_10min) + horizonte_pred), predicciones$upper[,2], col = "gray", lty = 2)

# legend("topright", legend = c("Datos recientes", "Predicción", "Intervalo de confianza"), col = c("blue", "red", "gray"), lty = c(1,1,2), lwd = 2)
