
g<- function (x){
  y<-x-(x^3-2)/(3*x^2)
  return (y)
}

#Primera iteración
x<-1
z<-g(x)
z

#Segunda iteración
x<-x
z<-g(x)
z

#Hacemos varias iteraciones consecutivas
x<-x
z<-g(x)
z<-g(z)
z<-g(z)
z<-g(z)
z<-g(z)
z


#Ahora lo vamos a ejecutar de forma automática
#hasta que el el "error" esté por debajo de 
#la tolerancia que hayamos marcado
#Toleracia
T<-0.00001
x<-1
z<-g(x)
while(abs(z-x)>=T){
  x<-z
  z<-g(x)
}
z
