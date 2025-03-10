# Script para transformar el dataSet Dic_Skype en algo usable para weka
data<-read.csv(file.choose(), sep=';', header=TRUE, fileEncoding = 'Latin1')
data$Destino
destinos<-sub("-.*", "", data$Destino)

destinos<-strtrim(destinos, side="right")

destinos<-gsub(" ", "", destinos)
destinos
data$Destino<-destinos
head(data)


duracion<-as.numeric(unlist(strsplit(data$Duración, ":"))) * c(3600, 60, 1)
duracion<-c()
i<-1
while(i<=dim(data)[[1]]){
  duracion<-append(duracion, sum(as.numeric(unlist(strsplit(data$Duración[[i]], ":"))) * c(3600, 60, 1)))
  i<-i+1
}
data$Duración<-duracion

# Mostrar el resultado en segundos
head(duracion)
head(data)

fecha<-c()
i<-1
while(i<=dim(data)[[1]]){
  if(as.numeric(unlist(strsplit(gsub(".* ", "", data$Fecha[[i]]), ":"))[[1]])==TRUE){
    fecha<-append(fecha,  "M"   )
  } else {
    fecha<-append(fecha,  "T"   )
  }
  i<-i+1
}

data$Fecha<-fecha

head(data)

data<-data[,-1]
head(data)
class(data$Grupo)
class(data$Fecha)
class(data$Destino)
class(data$Tarifa.min)
class(data$Duración)
class(data$Cantidad)

write.csv(file.choose())
