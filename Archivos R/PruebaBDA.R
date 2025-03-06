# Archivo para el examen de prueba de BDA:
# Pregunta 1: Considera el dataset del IBEX 35 durante los últimos 20 años. 
# Intenta estimar un modelo ARIMA con la herramienta R razonando de tu elección. 
# Efectúa un pronóstico puntual y de intervalo al 95% a  4 semanas vista. 
# ¿Qué se observa conforme nos alejamos en el tiempo predictivo?

# Cargamos las librerias
library(tidyverse)  # Manejo de datos
library(lubridate)  # Manejo de fechas
library(forecast)   # Modelado de series temporales
library(tseries)    # Pruebas estadísticas

# Cargamos el dataset correspondiente
data<-read.csv(file.choose(), header=TRUE, sep=',')

colClean<-gsub(pattern = '\\.',replacement = '',data$Apertura)
colClean<-gsub(pattern = ',',replacement = '\\.',colClean)

col<-as.numeric(colClean)

data$Apertura<-col

# Ahora ya tenemos los datos de apertura como número
plot(data$Apertura)

# Examinamos las fechas
data$Fecha
# La primera fecha es: 08-01-2006

tdata<-ts(data$Apertura, start=c(2006,1), frequency = 52)

plot(tdata)

acf(tdata)
adf.test(tdata)
# Por el momento el p-value no está en el rango establecido para aceptar
# la serie como estacionaria: current p-value = 0.01

# Serie diferenciada
serie_diferenciada<-diff(tdata)
plot(serie_diferenciada, type = "l", main = "Serie Diferenciada (Primera Diferencia)")
adf.test(serie_diferenciada)
# Tras realizar una diferenciación vemos que el p-value ya está dentro del 
# rango establecido para aceptar la serie como estacionaria: p-value = 0.01

# Como hemos estabilizados los datos tras una diferenciación: d = 1

acf(serie_diferenciada)
# Ahora podemos observar que q = 1 ya que en este primer rezago
# se evidencia una correlación significativa no decreciente o convergente a cero.

pacf(serie_diferenciada)
# Aquí la p sería 1 ?

modelo2<-arima(tdata, order = c(1,1,1))
plot(modelo2)
summary(modelo2)

residuos<-residuals(modelo2)
plot(residuos)
checkresiduals(residuos)
Box.test(residuos,type = "Ljung-Box") 
# Aquí para que de bien tiene que ser menor a 0.05

# Podemos comprobar nuestros valores obtenidos con auto arima:
modelo <- auto.arima(tdata)
summary(modelo)
# ARIMA(1,1,0)
# ARIMA es (p,d,q)

modelo2<-arima(tdata, order = c(1,1,0))
plot(modelo2)
summary(modelo2)

residuos<-residuals(modelo2)
plot(residuos)
checkresiduals(residuos)
Box.test(residuos,type = "Ljung-Box") 
# P-value alto (p-value > 0.05): No hay evidencia suficiente para rechazar 
# la hipótesis nula. En este caso, los residuos no presentan autocorrelación 
# significativa, lo que sugiere que el modelo es adecuado y que no quedan 
# patrones de autocorrelación no modelados.
tail(tdata, 1)[[1]]
predicciones <- forecast(modelo, h = 4, level = c(95))
par(mfrow = c(1, 2))
plot(predicciones, ylim = c(0,16000))
plot(predicciones, xlim = c(  2024, 2026.5), ylim=c(0,16000))



# Pregunta 2: Reconstrucción archivo Diabetes432025

# Cargamos el dataset correspondiente
data<-read.csv(file.choose(), header=TRUE, sep=',')
head(data)

# Primero consideramos las dimensiones del dataset para saber 
# cuantos datos podemos modificar
dim(data)
# Tiene 81 filas, por lo que idealmente reconstruiremos 4 filas como mucho

#Estudio de nulos en la fila edad
desconocidosEdad<-which(is.na(data$Edad)==TRUE)
length(desconocidosEdad) # 2
Edadconocidos<-data$Edad[-desconocidosEdad]
ks.test(Edadconocidos,"pnorm",mean=mean(Edadconocidos),sd=sd(Edadconocidos))
# Como el p-value es mayor al umbral establecido, 
# aceptamos la distribución como normal
muestra<-rnorm(length(desconocidosEdad),mean(Edadconocidos),sd(Edadconocidos))
data$Edad[desconocidosEdad]<-muestra # Ya lo hemos reconstruido

#Estudio de nulos en la fila parto
desconocidosParto<-which(is.na(data$Parto)==TRUE)
length(desconocidosParto)# 2
Partoconocidos<-data$Parto[-desconocidosParto]
# Vamos a analizar las proporciones de los partos para sacar una u otra categoría
# Esto se hace en las columnas entre 0 y 1
frecuenciasParto <- table(data$Parto, useNA = "no") # puedes hacerlo así
frecuenciasParto<-table(Partoconocidos) # o así
propociones<-prop.table(frecuenciasParto)
# Sustituir los NA en 'columna_binaria' según la proporción de 0s y 1s
data$Parto[desconocidosParto]<-sample(c(0, 1), 
                                length(desconocidosParto), 
                                replace = TRUE,
                                prob = propociones)

#Estudio de nulos en la fila prn
desconocidosPRN<-which(is.na(data$PRN)==TRUE)
length(desconocidosPRN) # 2
PRNconocidos<-data$PRN[-desconocidosPRN]
ks.test(PRNconocidos,"pnorm",mean=mean(PRNconocidos),sd=sd(PRNconocidos))
# Como el p-value es mayor al umbral establecido, 
# aceptamos la distribución como normal
muestra<-rnorm(length(desconocidosPRN),mean(PRNconocidos),sd(PRNconocidos))
data$PRN[desconocidosPRN]<-muestra # Ya lo hemos reconstruido

#Estudio de nulos en la fila sexo
desconocidosSEXO<-which(is.na(data$Sexo)==TRUE)
length(desconocidosSEXO) # 0

#Estudio de nulos en la fila cordón
desconocidosCORDON<-which(is.na(data$Cordon)==TRUE)
length(desconocidosCORDON) # 0

#Estudio de nulos en la fila aceite
desconocidosACEITE<-which(is.na(data$Aceite)==TRUE)
length(desconocidosACEITE) # 0

#Estudio de nulos en la fila perímetro
desconocidosPERIMETRO<-which(is.na(data$Perimetro)==TRUE)
length(desconocidosPERIMETRO) # 0

#Estudio de nulos en la fila talla
desconocidosTALLA<-which(is.na(data$Talla)==TRUE)
length(desconocidosTALLA) # 1
TALLAconocidos<-data$PRN[-desconocidosTALLA]
ks.test(TALLAconocidos,"pnorm",mean=mean(TALLAconocidos),sd=sd(TALLAconocidos))
# Como el p-value es mayor al umbral establecido, 
# aceptamos la distribución como normal
muestra<-rnorm(length(desconocidosTALLA),mean(TALLAconocidos),sd(TALLAconocidos))
data$PRN[desconocidosTALLA]<-muestra # Ya lo hemos reconstruido



# Pregunta 3: Test de Chi-square para Titaniccualtitativo independencia el Sexo  
# y supervivencia

# Cargamos el dataset correspondiente
data<-read.csv(file.choose(), header=TRUE, sep=',')
head(data)

MapSexoSupervivencia<-function (sexo, sobrevive){
  x<-c()
  i<-1
  while(i<=dim(data)[[1]]){
    if(data$Survived[[i]]==sobrevive &&
       data$Sex[[i]]==sexo){
      x<-append(x, 1)
    }
    i<-i+1
  }
  return(x)
}

MujerS<-MapSexoSupervivencia('female','Si')
MujerN<-MapSexoSupervivencia('female','No')
HombreS<-MapSexoSupervivencia('male','Si')
HombreN<-MapSexoSupervivencia('male','No')

# Funcion reduce
ReduceSexoSupervivencia<-function(x){
  y<-sum(x)
  return (y)
}

CountMujerS<-ReduceSexoSupervivencia(MujerS)
CountMujerN<-ReduceSexoSupervivencia(MujerN)
CountHombreS<-ReduceSexoSupervivencia(HombreS)
CountHombreN<-ReduceSexoSupervivencia(HombreN)

frameSexoSupervivencia<-data.frame(Si=c(CountMujerS,CountHombreS),
                                   No=c(CountMujerN,CountHombreN),
                                   row.names = c("Mujer","Hombre"))

# Mostramos el resultado:
frameSexoSupervivencia

# CHI CUADRADO
# Determina si dos variables categóricas están relacionadas 
# (es decir, si son dependientes) o si son independientes.
chisq.test(frameSexoSupervivencia)
# Si el p_value está por debajo del umbral quiere decir que ESTÁN relacionadas

# Resulta mucho más útil ver los datos en "relativo", es decir,
# teniendo en cuenta el total
total<-sum(frameSexoSupervivencia)

SupervivenciaRelativo<-data.frame(Si=c(CountMujerS/(CountMujerS + CountMujerN),
                                       CountHombreS/(CountHombreS + CountHombreN)),
                                  No=c(CountMujerN/(CountMujerS + CountMujerN),
                                       CountHombreN/(CountHombreS + CountHombreN)),
                                  row.names = c("Mujer","Hombre"))

SupervivenciaRelativo



# Pregunta 5: Gráfico de las palabras más frecuentes en los corpus crude y acq

library("tm")

# Selecciona la carpeta crude
folderCrude<-choose.dir(caption = "Selecciona la carpeta crude")
# Selecciona la carpeta acq
folderAcq<-choose.dir(caption = "Selecciona la carpeta acq")

# Crear el corpus utilizando los archivos combinados
corpus<-Corpus(DirSource(c(folderCrude, folderAcq)), 
               readerControl = list(reader = readPlain))

corpus

corpus<-tm_map(corpus, tolower)
corpus<-tm_map(corpus, stemDocument)
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, removePunctuation)
tdm<-TermDocumentMatrix(corpus)
inspect(tdm)

# Para obtener los términos que aparezcan más de x veces
findFreqTerms(tdm, 20)

tf<-rowSums(as.matrix(tdm))
tf<-sort(tf, decreasing = TRUE)
length(tf)
top10<-tf[1:10]
top10
barplot(top10, horiz = TRUE)
