# Instalar dependencias
# Este comando solo necesitas ejecutarlo una vez
# install.packages(c("tidyverse", "lubridate", "forecast", "tseries", "zoo"))

# Cargamos las librerias
library(zoo)
library(tidyverse)  # Manejo de datos
library(lubridate)  # Manejo de fechas
library(forecast)   # Modelado de series temporales
library(tseries)    # Pruebas estadísticas

data<-read.csv(file.choose(),sep=',',header=TRUE)

# Preparamos el dataset asegurandonos que tenga el heart rate y fecha y hora
# Además, hacemos una converión a date type
dataset<-data.frame(Heart_Rate=data$heart_rate, Date=as.POSIXct(data$create_time, format = "%m/%d/%Y, %I:%M:%S %p"))

# Reducimos el dataset a una sola semana ya que de otro modo tarda demasiado
dataset[1,] # Primera fecha: 2021-06-14 15:30:59
limDate<-as.POSIXct("6/28/2021, 3:31:00 PM", format = "%m/%d/%Y, %I:%M:%S %p")
filas<-which(dataset$Date<limDate)

dataset_week<-dataset[filas,]

#Hacemos limpieza de datos duplicados (misma fecha y hora)
duplicados<-duplicated(dataset_week$Date)
duplicados<-which(duplicados==TRUE)
datos_sin_duplicados <- dataset_week[-duplicados, ]
# Si duplicados está vacío, puede que datos_sin_duplicados haya salido vacío tmb
#datos_sin_duplicados<-dataset_week

# Creamos el objeto de serie temporal
serie_temp_zoo <- zoo(datos_sin_duplicados$Heart_Rate, order.by = datos_sin_duplicados$Date)

# Podemos mostrarlo en un gráfico
plot(serie_temp_zoo, main = "Temperatura Corporal a lo largo del tiempo")

# Creamos intervalos de tiempo regulares para poder normalizar el dataset
intervalos <- seq(from = min(datos_sin_duplicados$Date), to = max(datos_sin_duplicados$Date), by = "10 min")

# Finalmente normalizamos el dataset
serie_temp_interpolada_5min <- zoo(na.approx(serie_temp_zoo, xout = intervalos), order.by = intervalos)

# Definimos la frecuencia: 24 h * 60 min / 10 min
frequency(serie_temp_interpolada_5min) <- 144 

# Alternativa con ts
serie_temp_interpolada_5min <- ts(serie_temp_interpolada_5min)


# Descomposición STL (Seasonal and Trend decomposition using Loess)
descomposicion <- stl(serie_temp_interpolada_5min, s.window = "periodic")

acf(serie_temp_interpolada_5min)

# Serie diferenciada
serie_diferenciada<-diff(serie_temp_interpolada_5min)
plot(serie_diferenciada, type = "l", main = "Serie Diferenciada (Primera Diferencia)")
adf.test(serie_diferenciada)

# Ahora que hemos hecho la diferencia, el resultado es mucho mejor
acf(serie_diferenciada)
# Resultado:
# data:  serie_diferenciada
# Dickey-Fuller = -16.598, Lag order = 12, p-value = 0.01
# alternative hypothesis: stationary

# Ajustar un modelo ARIMA automáticamente
modelo <- auto.arima(serie_diferenciada)
summary(modelo)


# Número de datos a visualizar antes de la predicción
ultimos_n <- 100

# Obtener los últimos 100 valores de la serie diferenciada
datos_recientes <- tail(serie_diferenciada, ultimos_n)

# Realizar la predicción
horizonte_pred <- 5  # Número de pasos a predecir
predicciones <- forecast(modelo, h = horizonte_pred)

# Crear un gráfico con los últimos datos y predicciones
plot(datos_recientes, 
     type = "l", 
     col = "blue", 
     lwd = 2, 
     xlim = c(length(serie_diferenciada)-100, 
              length(serie_diferenciada)+horizonte_pred),
     ylim = c(min(serie_diferenciada)-5,
              max(serie_diferenciada)+5),
     main = "Últimos 100 datos y Predicciones", 
     ylab = "Valor", 
     xlab = "Tiempo")

# Agregar las predicciones al gráfico
lines((length(serie_diferenciada) + 1):(length(serie_diferenciada) + horizonte_pred), predicciones$mean, col = "red", lwd = 2)

# Agregar bandas de confianza
lines((length(serie_diferenciada) + 1):(length(serie_diferenciada) + horizonte_pred), predicciones$lower[,2], col = "gray", lty = 2)
lines((length(serie_diferenciada) + 1):(length(serie_diferenciada) + horizonte_pred), predicciones$upper[,2], col = "gray", lty = 2)

# legend("topright", legend = c("Datos recientes", "Predicción", "Intervalo de confianza"), col = c("blue", "red", "gray"), lty = c(1,1,2), lwd = 2)
