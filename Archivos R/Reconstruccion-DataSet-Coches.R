# Archivo para trabajar con el dataset coches
data<-read.csv(file.choose(), header=TRUE, sep=',')
head(data)

# Limpiamos las columnas que no nos interesan
x<-c(3,4,5,6,7,9,10)
data<-data[,x]
head(data)

# Convertimos la columna de kms a formato número 
kms<-data$kilometers_driven
kms<-gsub(',','',kms)
kms<-gsub(' KM','', kms)
kms<-as.numeric(kms)
data$kilometers_driven<-kms

# Limpiamos N/A en la columna transmión
length(which(data$transmision=="N/A"))



# Quitamos las filas que tengan N/A en la engine capacity
nas<-which(data$engine_capacity=="N/A")
data<-data[-nas,]
