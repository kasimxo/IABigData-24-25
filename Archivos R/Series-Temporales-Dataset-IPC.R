# Archivo para trabajar con el Dataset IPC
data<-read.csv(file.choose(),sep=';',header=TRUE)

# Instalar dependencias
# Este comando solo necesitas ejecutarlo una vez
# install.packages(c("tidyverse", "lubridate", "forecast", "tseries", "zoo"))

# Cargamos las librerias
library(tidyverse)  # Manejo de datos
library(lubridate)  # Manejo de fechas
library(forecast)   # Modelado de series temporales
library(tseries)    # Pruebas estadísticas

# Las únicas columnas que nos importan son las dos últimas
x<-c(11,12)
data<-data[,x]

splitDate<-strsplit(data$PERIODO, split="M")

year<-c()
month<-c()

i<-1
while(i<=length(splitDate)){
  year<-append(year, splitDate[[i]][[1]])
  month<-append(month,splitDate[[i]][[2]])
  i<-i+1
}

value<-as.numeric(gsub(",",".",data$VALOR))

table<-data.frame(Year=year, Month=month, Value=value)
table # Examinamos y vemos que el primer valor es Septiembre de 2008

invYear<-c()
invMonth<-c()
invValue<-c()
i<-dim(table)[[1]]
while(i>=1){
  invYear<-append(invYear,table$Year[[i]])
  invMonth<-append(invMonth,table$Month[[i]])
  invValue<-append(invValue,table$Value[[i]])
  i<-i-1
}

# Tenemos que invertir la tabla para que después la ts salga bien
invTable<-data.frame(Year=invYear,Month=invMonth,Value=invValue)
invTable

tdata<-ts(invTable$Value, start=c(2008,9), frequency = 12)
tdata

plot(tdata)

acf(tdata)

# Serie diferenciada
serie_diferenciada<-diff(tdata)
plot(serie_diferenciada, type = "l", main = "Serie Diferenciada (Primera Diferencia)")
adf.test(serie_diferenciada)

# Ahora que hemos hecho la diferencia, el resultado es mucho mejor
acf(serie_diferenciada)

acf(tdata)
modelo <- auto.arima(tdata)
summary(modelo)

# Número de datos a visualizar antes de la predicción
ultimos_n <- 100

# Obtener los últimos 100 valores de la serie diferenciada
datos_recientes <- tail(tdata, ultimos_n)

# Realizar la predicción
horizonte_pred <- 5  # Número de pasos a predecir
predicciones <- forecast(modelo, h = horizonte_pred)
predicciones

plot(predicciones)