data<-read.csv(file.choose(),header=TRUE,sep=',')
head(data)
data<-data[,-1]
head(data)

# Test de Chow
# La idea es intentar pronostica el precio del billete en función 
# de la edad del pasajero, el número de parientesw y la clase en la que viajaba
# Una vez hecha se estudiará:
# A) si existe permanencia estructural en base al sexo
# B) si existe permanencia estructural en base a la supervivencia

# Comenzamos por estudiar una regresión lineal
modelo<-lm(Precio~Clase+Edad+Parientes,data=data)
summary(modelo)

# A) 
# Construimos un nuevo data set para los datos de un sexo
# y a continuación los del otro sexo
hombres<-which(data$Sexo=='male')
mujeres<-which(data$Sexo=='female')

corte<-length(hombres)
corte

Survived<-data[hombres,]$Survived
Survived<-append(Survived,data[mujeres,]$Survived)


Clase<-data[hombres,]$Clase
Clase<-append(Clase,data[mujeres,]$Clase)


Edad<-data[hombres,]$Edad
Edad<-append(Edad,data[mujeres,]$Edad)

Parientes<-data[hombres,]$Parientes
Parientes<-append(Parientes,data[mujeres,]$Parientes)

Precio<-data[hombres,]$Precio
Precio<-append(Precio,data[mujeres,]$Precio)

Frame<-data.frame(Survived=Survived,Precio=Precio,Parientes=Parientes,Clase=Clase,Edad=Edad)
Frame

#install.packages('strucchange')
library(strucchange)

sctest(Frame$Precio~Frame$Clase+Frame$Edad+Frame$Survived+Frame$Parientes,type="Chow",point=corte)

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
