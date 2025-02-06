# Series temporales sobre el dataset de TEF
data<-read.csv(file.choose(), sep=';', header=TRUE)
head(data)

# Nos quedamos con los ultimos
data<-data[,3]

# Invertimos los datos
x<-seq(length(data),1,-1)
data<-data[x]
data
# Cargamos las librerias pertinentes
# Si no las tienes instaladas:
# install.packages(c("tseries","forecast"))
library(forecast)
library(tseries)

# Damos a los datos estructura de serie temporal
tdata<-ts(data, start = c(2000,2), frequency=12)

plot(tdata)

# Con la propia representacion vemos que no es estacionaria

# Probamos con logaritmos
logdata<-log(tdata)

plot(logdata)

# Hacemos el adf test
adf.test(logdata, alternative="stationary")
# Dickey-Fuller = -2.0953, Lag order = 6, p-value = 0.5359
# Dado que p-value>0.05 LA SERIE NO ES ESTACIONARIA

# Diferenciamos la serie con objeto de convertirla en estacionaria
d1data<-diff(tdata)

plot(d1data)

# Aplicamos el test a la serie diferenciada
adf.test(d1data, alternative="stationary")
# Dickey-Fuller = -6.6728, Lag order = 6, p-value = 0.01
# Dado que p-value<0.05 LA SERIE ES ESTACIONARIA

# Por tanto: d=1

# Buscamos los coeficientes acf de la serie original y de la diferenciada

par(mfrow=c(2,2))

acf(tdata)
acf(d1data)
pacf(d1data)

# A partir de examinar el gráfico, especulamos con un ARIMA(2,1,1)
modelo<-arima(data,order=c(2,1,1))
plot(modelo)
summary(modelo)
modelo
# Coeficientes: -0.0556  -0.0702  -0.4397

# Diagnostico del modelo
tsdiag(modelo)

residuos<-residuals(modelo)
plot(residuos)

# Test Ljung-box para la normalidad de los residuos
Box.test(residuos,type = "Ljung-Box")
# X-squared = 7.0156e-06, df = 1, p-value = 0.9979
# Como es mayor que 0.05 acepto h0
# Los residuos se comportan normalmente

modelo

# Yt=media-0.0556(Yt-1)-0.0702(Yt-2)-0.4397(€t-1)+ut

pronostico<-forecast::forecast(modelo, h=2)

pronostico

plot(pronostico)


