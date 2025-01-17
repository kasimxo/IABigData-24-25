#Tratamiento de valores nulos o desconocidos en un archivo

data<-read.csv(file.choose(), header=TRUE, sep=',')

columnas<-c(2,3,5,6,7,8,10)
data<-data[,columnas]

dim(data)

desconocidos<-which(is.na(data$Survived)==TRUE)
length(desconocidos)
#No hay ninguno que no tenga el valor de survived

#Primero terminamos de limpiar el atributo survived
# 0 -> N    1 -> S
No<-which(data$Survived==0)
Si<-which(data$Survived==1)
data$Survived[No]<-"N"
data$Survived[Si]<-"S"

#Tratamiento de la clase
#Estudio de nulos
desconocidos<-which(is.na(data$Pclass)==TRUE)
length(desconocidos)
#No hay tampoco

#Sustituimos: 1->P, 2->S, 3->T
P<-which(data$Pclass==1)
data$Pclass[P]<-"P"
S<-which(data$Pclass==2)
data$Pclass[S]<-"S"
T<-which(data$Pclass==3)
data$Pclass[T]<-"T"

#Tratamiento de la edad
#Estudio de nulos
desconocidos<-which(is.na(data$Age)==TRUE)
length(desconocidos)

#Primero escogemos los datos conocidos
Edadconocidos<-data$Age[-desconocidos]

#Total conocidos
length(Edadconocidos)

boxplot(Edadconocidos)
summary(Edadconocidos)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.42   20.12   28.00   29.70   38.00   80.00 
#Limite superior de atipico
LS<-38+3*(28-20.12)
#Busco cuanto atípicos existen
n<-length(which(Edadconocidos>LS))
p<-n/length(Edadconocidos)
p
#Como p está por debajo del 5% podemos eliminar dichos datos
Atipicos<-which(Edadconocidos>LS)
ConocidosNoatipicos<-Edadconocidos[-Atipicos]
length(ConocidosNoatipicos)

#Buscamos una posible distribución de la edad
#Podemos probar con la normal, exponencial, la poisson
ks.test(ConocidosNoatipicos,"pnorm",mean=mean(ConocidosNoatipicos),sd=sd(ConocidosNoatipicos))
#Como el p-valor está por encima de 0.05 aceptamos destribución normal
#Extraemos muestra aleatoria de dicha normal
#Tantos como desconocidos.
muestra<-rnorm(length(desconocidos),mean(ConocidosNoatipicos),sd(ConocidosNoatipicos))
data$Age[desconocidos]<-muestra
data<-data[-Atipicos,]

length(which(is.na(data$Sex)==TRUE))
#No hay ningún na en sexo

# Aquí probamos con el dataset del lusitania
data<-read.csv(file.choose(), header = TRUE, sep=',')
names(data)
columnas<-c(5,6,8,9,15,16)
data<-data[,columnas]
dim(data)
length(which(is.na(data$Age)))
# 662 NA de 1961
desconocidos<-which(is.na(data$Age)==TRUE)
desconocidos


#Primero escogemos los datos conocidos
Edadconocidos<-data$Age[-desconocidos]

#Total conocidos
length(Edadconocidos)

boxplot(Edadconocidos)
summary(Edadconocidos)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   25.00   32.00   32.72   41.00   76.00 
#Limite superior de atipico
LS<-41+3*(32-25)
#Busco cuanto atípicos existen
n<-length(which(Edadconocidos>LS))
p<-n/length(Edadconocidos)
p

#Como p está por debajo del 5% podemos eliminar dichos datos
Atipicos<-which(Edadconocidos>LS)
ConocidosNoatipicos<-Edadconocidos[-Atipicos]
length(ConocidosNoatipicos)

#Buscamos una posible distribución de la edad
#Podemos probar con la normal, exponencial, la poisson
ks.test(ConocidosNoatipicos,"pnorm",mean=mean(ConocidosNoatipicos),sd=sd(ConocidosNoatipicos))
ks.test(ConocidosNoatipicos,"punif", min(ConocidosNoatipicos), max(ConocidosNoatipicos))
ks.test(ConocidosNoatipicos, "ppois", mean(ConocidosNoatipicos))
ks.test(ConocidosNoatipicos, "pexp", rate=1/mean(ConocidosNoatipicos))
ks.test(ConocidosNoatipicos,"dnorm",mean=mean(ConocidosNoatipicos),sd=sd(ConocidosNoatipicos))
ks.test(ConocidosNoatipicos,"rnorm",mean=mean(ConocidosNoatipicos),sd=sd(ConocidosNoatipicos))
ks.test(ConocidosNoatipicos,"dunif", min(ConocidosNoatipicos), max(ConocidosNoatipicos))
ks.test(ConocidosNoatipicos,"runif", min(ConocidosNoatipicos), max(ConocidosNoatipicos))

muestra<-rnorm(length(desconocidos),mean(ConocidosNoatipicos),sd(ConocidosNoatipicos))
data$Age[desconocidos]<-muestra
data<-data[-Atipicos,]

head(data)
length(which(is.na(data$Fate)))

length(which(is.na(data$Passenger.Crew)))
length(which(is.na(data$Adult.Minor)))
length(which(is.na(data$Citizenship)))
length(which(is.na(data$Sex)))
