# Interpolacion:
# Se trata de reconstruir valores que no tenemos, entre dos valores que sí que tenemos.
# Por ejemplo, el lunes tenemos 100 euros.
# El miércoles tenemos 0. 
# Interpolamos para sacar el valor del martes.
# Según cómo lo calculemos, puede ser 50, 25, 75, o cualquier otro valor.
# Es muy habitual en series temporales, ya que es muy común que no tengamos
# todos los dato, o que los datos no estén estructurados a intervalos regulares,
# por lo que se interpolan para darles estructura y consistencia.

# n = numero de datos de los que dispones
# El par(s,B) indica para s la coleccion de instantes que tienes que sera n
# B es la colección de valores Y que dispones. Tambien
# t es el instante para el cual deseas hacer la estimacion

# Tenemos x intantes (s) pero no para todos tenemos valor, 
# reconstruirlos con interpolacion.

Polbase <- function(n, t, s) {
  L <- numeric(n)
  for (i in 1:n) {
    L[i] <- 1
    for (j in 1:n) {
      if (j != i) {
        L[i] <- L[i] * (t - s[j]) / (s[i] - s[j])
      }
    }
  }
  return(L)
}

Pol <- function(B,t,s){
  p<-0
  n<-length(s)
  L<-Polbase(n,t,s)
  i<-1
  while(i<=n){
    p<-p+B[i]*L[i]
    i<-i+1
  }
  return (p)
}

# Introducimos los instantes

s<-seq(1,400,2)
n<-length(s)

# Introducimos los valores de la variable

Y<-seq(1,200,1)

s<-seq(1,10,2)
# s es la colección de instantes

Y<-seq(1,21,5)
# Y la colección de los valores de la variable Y en los instantes anteriores

t<-2
# t es el instante en el cual se desea evaluar
Pol(Y,t,s)


# Ejemplo real

Instantes <-seq(0,1,0.01)
# Valores d ela variable Y
Y<-1/(1+exp(Instantes))
# Representamos la función
plot(Instantes, Y, main="Función original")

# Instantes en los que vamos a tomar datos
# (no coinciden con los instantes que sí que tenemos datos,
# por lo que tenemos que interpolar)
ValoresIntermedios<-seq(0.005,1.005,0.01)

polinomio<-c()
i<-1
while(i<=length(ValoresIntermedios)){
  polinomio<-append(polinomio,Pol(Y,ValoresIntermedios[[i]],Instantes))
  i<-i+1
}

plot(Instantes, Y, main="Interpolación")
lines(ValoresIntermedios,polinomio,col="red")

# Ejemplo real: Dataset de Heart Rate
data<-read.csv(file.choose(), sep=',', header=TRUE)




