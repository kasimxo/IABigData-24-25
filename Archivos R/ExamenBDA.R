# Examen BDA 18/03/2025

# Pregunta 1.
# Considera el dataset de Ibex35  durante los últimos 20 años 
# a intervalos de meses. 
# Intenta estimar un posible modelo ARIMA con la herramienta R 
# dando razones de tu elección. 
# Posteriormente efectúa un pronóstico puntual y de intervalo
# al 95% a  4 meses vista. 
# ¿Qué se observa conforme nos alejamos en el tiempo predictivo? 
# Compruebe todos los resultados comentados. (R )

# Cargamos las librerias
library(tidyverse)  # Manejo de datos
library(lubridate)  # Manejo de fechas
library(forecast)   # Modelado de series temporales
library(tseries)    # Pruebas estadísticas

# Cargamos el dataset correspondiente
data<-read.csv(file.choose(), header=TRUE, sep=',')

# Quitamos los separadores de miles
colClean<-gsub(pattern = '\\.',replacement = '',data$Apertura)
# Cambiamos comas por puntos para poder hacer la conversión a número
colClean<-gsub(pattern = ',',replacement = '\\.',colClean)

col<-as.numeric(colClean)

data$Apertura<-col

# Ahora ya tenemos los datos de apertura como número
plot(data$Apertura)

# Examinamos las fechas
data$Fecha
# La primera fecha es: 08-01-2006

# Ahora creamos la serie temporal, estableciendo la frecuencia en semanas 
# ya que el data set contiene datos semanales
tdata<-ts(data$Apertura, start=c(2006,1), frequency = 52)

# Visualizamos el gráfico de la serie temporal
plot(tdata)

# Analizamos si la serie es estacionaria o no
acf(tdata)
adf.test(tdata)
# Por el momento el p-value no está en el rango establecido para aceptar
# la serie como estacionaria: current p-value = 0.02963 (umbral 0.05)

# Vamos a hacer la primera diferenciación

# Serie diferenciada
serie_diferenciada<-diff(tdata)

# Visualizamos la serie diferenciada
plot(serie_diferenciada, type = "l", main = "Serie Diferenciada (Primera Diferencia)")

# Volvemos a hacer el test adf para verificar si es estacionaria
adf.test(serie_diferenciada)
# Tras realizar una diferenciación vemos que el p-value ya está dentro del 
# rango establecido para aceptar la serie como estacionaria: p-value < 0.01

# Como hemos estabilizados los datos tras una diferenciación: d = 1

acf(serie_diferenciada)
# Ahora podemos observar que q = 0 o 1 ya que el primer rezago significativo 
# (se ignora el primero de todos) está en el límite aceptable
# Además, como vemos que decae abruptamente, podemos pensar que la serie
# sigue un modelo MA(q)

pacf(serie_diferenciada)
# Con el test Función de Autocorrelación Parcial podemos sacar la p para el 
# modelo arima.
# Aquí la p sería 1 observando cómo el primer regazo supera el intervalo de
# confianza, aunque también se podría interpretar como 0, ya que no lo supera
# por demasiado.

# Con este análisis, podemos probar un modelo ARIMA d=1, q=0, p=1

# El modelo arima lo efectuamos sobre la serie no diferenciada
modelo2<-arima(tdata, order = c(1,0,1))
plot(modelo2)
summary(modelo2)
# Examinando los coeficientes, podemos encontrar la fórmula de la serie
# ar1      ma1  intercept
# 0.9895  -0.0725  9906.6859
# Yt = 9906.69 + ( 0.9895 × v ) + ( −0.0725 × e)
# siendo v el último valor de la serie y e el error.

# Ahora hacemos un estudio del modelo
residuos<-residuals(modelo2)
plot(residuos)

# Comprobamos los residuos
checkresiduals(residuos)
Box.test(residuos,type = "Ljung-Box") 
# P-value alto (p-value > 0.05): No hay evidencia suficiente para rechazar 
# la hipótesis nula. En este caso, los residuos no presentan autocorrelación 
# significativa, lo que sugiere que el modelo es adecuado y que no quedan 
# patrones de autocorrelación no modelados.
# En este caso p-value 0.8849

# Podemos comprobar nuestros valores obtenidos con auto arima:
modelo <- auto.arima(tdata)
summary(modelo)
# En este caso nos sugiere un modelo ARIMA 1, 1, 0

# Construimos un nuevo modelo con estos valores
modelo2<-arima(tdata, order = c(1,1,0))
plot(modelo2)
summary(modelo2)
# Coefficients:
# ar1
# -0.0781

residuos<-residuals(modelo2)
plot(residuos)
checkresiduals(residuos)
Box.test(residuos,type = "Ljung-Box") 
# En este caso el p-value es de 0.9865, superior al modelo anterior

# Ahora vamos a hacer la predicción de los próximos 4 valores
tail(tdata, 1)[[1]]
predicciones <- forecast(modelo, h = 4, level = c(95))
par(mfrow = c(1, 2))
# Mostramos dos gráficos, uno de la serie completa y otro el detalle del
# últimos años y las predicciones, para poder apreciar el intervalo de confianza
plot(predicciones, ylim = c(0,16000))
plot(predicciones, xlim = c(  2025.4, 2026.4), ylim=c(0,16000))
# Al avanzar en el tiempo, la confianza se pierde, por lo que el intervalo 
# de confianza cada vez es mayor




# Pregunta 2. 
# Test de Chicuadrado para Titaniccualtitativo independencia entre  
# supervivencia y sexo


# Cargamos el dataset correspondiente
data<-read.csv(file.choose(), header=TRUE, sep=',')
head(data)

# Para hacer el test de Chicuadrado vamos a emplear un map reduce
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

# Primero el Map
MujerS<-MapSexoSupervivencia('female','Si')
MujerN<-MapSexoSupervivencia('female','No')
HombreS<-MapSexoSupervivencia('male','Si')
HombreN<-MapSexoSupervivencia('male','No')

# Funcion reduce
ReduceSexoSupervivencia<-function(x){
  y<-sum(x)
  return (y)
}

# Y después el reduce
CountMujerS<-ReduceSexoSupervivencia(MujerS)
CountMujerN<-ReduceSexoSupervivencia(MujerN)
CountHombreS<-ReduceSexoSupervivencia(HombreS)
CountHombreN<-ReduceSexoSupervivencia(HombreN)

# Ahora podemos construir la tabla
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
# Como en este caso el p-value < 2.2e-16 quiere decir que están relacionados

# Resulta mucho más útil ver los datos en "relativo", es decir,
# teniendo en cuenta el total
total<-sum(frameSexoSupervivencia)

SupervivenciaRelativo<-data.frame(Si=c(CountMujerS/(total),
                                       CountHombreS/(total)),
                                  No=c(CountMujerN/(total),
                                       CountHombreN/(total)),
                                  row.names = c("Mujer","Hombre"))

SupervivenciaRelativo
# Con esto podemos ver que sobre el total de pasajeros:
# 49.69% de mujeres sobrevivieron
# 8.64% de hombres sobrevivieron
# 2.77% de mujeres no sobrevivieron
# 38.88% de hombres no sobrevivieron






# Ejercicio 4. 
# Considerar la colección de archivos acq y crude. 
# Aplicar técnicas de minería de texto de R  para:
# Obtener la lista de las 20 palabras más frecuentes. 
# Utilice el gráfico más conveniente para apreciar visualmente los resultados
# Aplique algún algoritmo de asociación 

# Cargamos la librería correspondiente
library("tm")

# Selecciona la carpeta crude
folderCrude<-choose.dir(caption = "Selecciona la carpeta crude")
# Selecciona la carpeta acq
folderAcq<-choose.dir(caption = "Selecciona la carpeta acq")

# Crear el corpus utilizando los archivos combinados
corpus<-Corpus(DirSource(c(folderCrude, folderAcq)), 
               readerControl = list(reader = readPlain))

# Mostramos el Corpus, verificando que tenemos 70 archivos
corpus

# Tratamos el corpus para eliminar mayúsculas, números, etc.
corpus<-tm_map(corpus, removeWords, stopwords("english"))
corpus<-tm_map(corpus, tolower)
corpus<-tm_map(corpus, stemDocument)
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, removePunctuation)


tdm<-TermDocumentMatrix(corpus)
inspect(tdm)
tdm # Vemos que tiene 1901 términos

# Para encontrar las palabras más frecuentes, sumamos por filas
tf<-rowSums(as.matrix(tdm))
# Ordenamos de mayor a menor
tf<-sort(tf, decreasing = TRUE)
# Seleccionamos las primeras 20
top20<-tf[1:20]
top20
# Para mostrarlas, resulta mucho más cómodo optar por un gráfico horizontal
barplot(top20, horiz = TRUE)

# Asociación de palabras
shared<-findAssocs(tdm, "said", 0.6)
# Marcamos una confianza de 0.6 para poder ver resultados
# ya que con 0.95 no hay ninguno
shared

# Asociación de archivos
# Transponemos con el objeto de tener por cada fila un documento
dtm<-t(tdm)

# Lo convertimos en matriz
m<-as.matrix(dtm)

# Función para calcular la distancia euclídea
norm_eucl<-function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)

# Aplicación de la función
m_norm<-norm_eucl(m)

# Calculamos la matriz de distancias entre los documentos
distm<-dist(m_norm)

# Clustering jerárquico a partir de la matriz de distancias
fit<-hclust(distm, method="ward.D")

# Mostramos el gráfico resultante
plot(fit)

# Ejercicio 5.
# Map Reduce TitanicCuantitativo (R)

# Cargamos el dataset correspondiente
data<-read.csv(file.choose(), header=TRUE, sep=',')
head(data)

# Edad máxima de las mujeres que  sobreviven
# Definimos la función Map
# Esta función recibe las columnas de datos y devuelve un vector 
# con 1 y 0 dependiendo de si es una mujer que sobrevive o no
MapSexoSupervivencia<-function (sexo, sobrevive){
  x<-c()
  i<-1
  while(i<=dim(data)[[1]]){
    if(data$Survived[[i]]==sobrevive &&
       data$Sex[[i]]==sexo){
      x<-append(x, 1)
    } else {
      x<-append(x, 0)
    }
    i<-i+1
  }
  return(x)
}

# Definimos la función reduce
# Esta función recibe una columa de datos y devuelve su valor máximo
ReduceSexoSupervivencia<-function(x){
  y<-max(x)
  return (y)
}

# Primero el Map
MujerS<-MapSexoSupervivencia('female','Si')
MujerS

# Y después el reduce
MaxMujerS<-ReduceSexoSupervivencia(data$Edad[which(MujerS==1)])
MaxMujerS # La edad máxima de una mujer que sobrevive es 63

# Precio medio de los hombres que no  sobreviven

# Reutilizamos la función Map anterior para identificar a los hombres que no
# sobreviven
HombreN<-MapSexoSupervivencia('male','No')
HombreN

# Ahora definimos una función Reduce que nos devolverá la media
ReduceAverage<-function(x){
  y<-mean(x)
  return (y)
}

MediaBilleteHombreNoSobrevive<-ReduceAverage(data$Precio[which(HombreN == 1)])
MediaBilleteHombreNoSobrevive # 108.9339
