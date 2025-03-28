# El objetivo de este archivo es el siguiente:
# El dataset de HeartRate no tiene los datos estructurados, es decir:
# - Contiene duplicados
# - No tiene intervalos de tiempo regulares entre los datos

# Por tanto, se va a limpiar el archivo eliminando los duplicados,
# haciendo las conversiones de tipo de dato pertinentes y
# haciendo interpolación de datos para crear intervalos regulares

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

dataset_week<-dataset

#Hacemos limpieza de datos duplicados (misma fecha y hora)
duplicados<-duplicated(dataset_week$Date)
duplicados<-which(duplicados==TRUE)
datos_sin_duplicados <- dataset_week[-duplicados, ]
# Si duplicados está vacío, puede que datos_sin_duplicados haya salido vacío tmb
#datos_sin_duplicados<-dataset_week

# Creamos el objeto de serie temporal
serie_temp_zoo <- zoo(datos_sin_duplicados$Heart_Rate, order.by = datos_sin_duplicados$Date)

# Podemos mostrarlo en un gráfico
plot(serie_temp_zoo, main = "Frecuencia cardiaca a lo largo del tiempo")

# Creamos intervalos de tiempo regulares para poder normalizar el dataset
intervalos <- seq(from = min(datos_sin_duplicados$Date), to = max(datos_sin_duplicados$Date), by = "10 min")

# Finalmente normalizamos el dataset
serie_temp_interpolada_10min <- zoo(na.approx(serie_temp_zoo, xout = intervalos), order.by = intervalos)

# Aquí ya podemos exportar el archivo
write.csv(serie_temp_interpolada_10min, file.choose())
