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
      x<-append(x,data$Fare[[i]])
    }
    i<-i+1
  }
  return(x)
}

PreciosN<-Map(0)
PreciosN

# Definición de la rutina reduce
Reduce<-function(C) {
  
}

