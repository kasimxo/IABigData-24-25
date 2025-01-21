# Mi primer map Reduce
# Se trabaja sobre el dataset del titanic-ing
# Calcular el precio medio del billete según la supervivencia

# Importación de datos
data<-read.csv(file.choose(),header=TRUE,sep=",")

# Definición de la rutina map, aquí lo está agrupando por supervivencia
Map<-function(C){
  x<-c()
  i<-1
  while(i<=dim(data)[[1]]){
    if(data$Survived[[i]]==C){
      x<-append(x, data$Fare[[i]])
    }
    i<-i+1
  }
  return(x)
}

PrecioN<-Map(0)
PrecioS<-Map(1)
PrecioN
PrecioS


# Definición de la rutina reduce
Reduce<-function(x) {
  y<-mean(x)
}

# Creación data frame
PmedioS<-Reduce(PrecioS)
PmedioN<-Reduce(PrecioN)

framePreciosPorPrecio<-data.frame(PrecioS=PmedioS,PrecioN=PmediosN)
framePreciosPorPrecio

# Mi segundo MapReduce
# Objetivo:
# Obtener un vector con valores 1 y clave el sexo y la supervivencia:
# Map<(Sexo, Supervivencia), Int> -> (Mujer, Si) 1
# Rutina reduce
# Contabilizar la frecuencia de cada salida de la rutina map por clave
# Configurar un data frame 2x2 donde indeique la frecuencia relativa 
# o probabilidad por supervivencia y sexo

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
head(data)
MujerS<-MapSexoSupervivencia('female',1)
MujerN<-MapSexoSupervivencia('female',0)
HombreS<-MapSexoSupervivencia('male',1)
HombreN<-MapSexoSupervivencia('male',0)

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
frameSexoSupervivencia
