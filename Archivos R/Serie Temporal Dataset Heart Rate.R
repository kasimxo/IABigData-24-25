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
limDate<-as.POSIXct("6/21/2021, 3:31:00 PM", format = "%m/%d/%Y, %I:%M:%S %p")
filas<-which(dataset$Date<limDate)

dataset_week<-dataset[filas,]

#Hacemos limpieza de datos duplicados (misma fecha y hora)
duplicados<-duplicated(dataset_week$Date)
duplicados<-which(duplicados==TRUE)
datos_sin_duplicados <- dataset_week[-duplicados, ]
# Si duplicados está vacío, puede que datos_sin_duplicados haya salido vacío tmb
datos_sin_duplicados<-dataset_week

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

# Descomposición STL (Seasonal and Trend decomposition using Loess)
descomposicion <- stl(serie_temp_interpolada_5min, s.window = "periodic")






# Creamos un modelo ARIMA
modelo_arima <- auto.arima(serie_temp_interpolada_5min)


# Realizar predicciones
prediccion <- forecast(modelo_arima, h = 10)  # Predecir los siguientes 10 valores

# Graficar la predicción
plot(prediccion, main = "Predicción ARIMA de Temperatura a 5 minutos")
