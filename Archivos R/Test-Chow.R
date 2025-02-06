### --- --- Test de Chow (sctest) --- --- ###
### --- ---       Funciones       --- --- ###

data<-read.csv(file.choose(),header=TRUE,sep=',') # Diabetes
head(data)
data<-data[,-1]
head(data)

# Limpiamos los datos de la columna Talla
line<-data[,7]
line<-gsub(',','.',line)
data[,7]<-line
data[,7]<-as.numeric(data[,7])

# Test de Chow
# La idea es intentar pronostica el precio del billete en función 
# de la edad del pasajero, el número de parientesw y la clase en la que viajaba
# Una vez hecha se estudiará:
# A) si existe permanencia estructural en base al sexo
# B) si existe permanencia estructural en base a la supervivencia

# Comenzamos por estudiar una regresión lineal
modelo<-lm(data$PRN~data$P+data$T+data$Edad,data=data)
summary(modelo)

# A) 
# Construimos un nuevo data set para los datos de un sexo
# y a continuación los del otro sexo
hombres<-which(data$Sexo=='V')
mujeres<-which(data$Sexo=='M')

corte<-length(hombres)
corte

Sexo<-data[hombres,]$Sexo
Sexo<-append(Sexo,data[mujeres,]$Sexo)
Parto<-data[hombres,]$Parto
Parto<-append(Parto,data[mujeres,]$Parto)
Cordon<-data[hombres,]$Cordon
Cordon<-append(Cordon,data[mujeres,]$Cordon)
Aceite<-data[hombres,]$Aceite
Aceite<-append(Aceite,data[mujeres,]$Aceite)
Edad<-data[hombres,]$Edad
Edad<-append(Edad,data[mujeres,]$Edad)
PRN<-data[hombres,]$PRN
PRN<-append(PRN,data[mujeres,]$PRN)
P<-data[hombres,]$P
P<-append(P,data[mujeres,]$P)
T<-data[hombres,]$T
T<-append(T,data[mujeres,]$T)

Frame<-data.frame(Sexo=Sexo,Parto=Parto,Cordon=Cordon,Aceite=Aceite,Edad=Edad,PRN=PRN,P=P,T=T)
head(Frame)

#install.packages('strucchange')
library(strucchange)

sctest(Frame$PRN~Frame$P+Frame$T+Frame$Edad,data=Frame,type="Chow",point=corte)

# B)

Si<-which(data$Survived=='Si')
No<-which(data$Survived=='No')
corte<-length(Si)

FrameB<-data[order(data$Survived, decreasing = TRUE),]
FrameB
sctest(FrameB$Precio~FrameB$Clase+FrameB$Edad+FrameB$Survived+FrameB$Parientes,type="Chow",point=corte)


# Considero la muestra de precios

P<-data$Precio

# Calculamos media y dseciación típica

media<-mean(P)
desviacion<-sd(P)
media
desviacion

# Veamos si los datos de precios se distribuyen normal
ks.test(P,'pnorm',mean=media,sd=desviacion)

# Veamos si la edad se distribuye normalmente
E<-data$Edad
media<-mean(E)
des<-sd(E)

ks.test(E,'pnorm',mean=media,sd=des) # p_value < 2.2e-16 NO se distribuye normalmente

# Veamos si el precio se ajusta a una uniforme

ks.test(P,'punif',min=min(P),max=max(P))

# Veamos si la edad se ajusta a una uniforme

ks.test(E,'punif',min=min(E),max=max(E))


x<-rnorm(1000,10,2)
ks.test(x,'pnorm',mean=10,sd=2)
