# Script para transformar el dataSet Dic_Skype en algo usable para weka
data<-read.csv(file.choose(), sep=';', header=TRUE, fileEncoding = 'Latin1')

# Limpiamos los destinos para quedarnos con el país y eliminar espacios
destinos<-sub("-.*", "", data$Destino)
destinos<-gsub(" ", "", destinos)
data$Destino<-destinos

# Limpiamos la duración
duracion<-as.numeric(unlist(strsplit(data$Duración, ":"))) * c(3600, 60, 1)
duracion<-c()
i<-1
while(i<=dim(data)[[1]]){
  duracion<-append(duracion, sum(as.numeric(unlist(strsplit(data$Duración[[i]], ":"))) * c(3600, 60, 1)))
  i<-i+1
}
data$Duración<-duracion

# Clasificamos las fechas en dos grupos: mañana o tarde
fecha<-c()
i<-1
while(i<=dim(data)[[1]]){
  if(as.numeric(unlist(strsplit(gsub(".* ", "", data$Fecha[[i]]), ":"))[[1]])<12){
    fecha<-append(fecha,  "M"   )
  } else {
    fecha<-append(fecha,  "T"   )
  }
  i<-i+1
}
data$Fecha<-fecha

# Eliminamos la columna id
data<-data[,-1]

# Exportamos el archivo
write.csv(data, file.choose())
