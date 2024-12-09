'a'
1-ppois(3,3)
'b'
dpois(0,1.5)
'c'
dpois(1,1.5)
'd'
1-ppois(1,1.5)

# CALCULO DE PROBABILIDAD:
# -----------------------------------------------
# Extracciones sin reposición

# Devuelve el cálculo del binomio a sobre b
Combinatorio<-function(a,b){
  f<-factorial(a)/(factorial(b)*factorial(a-b))
  return(f)
} 

# Funcion de probabilidad:
# Calcula la probabilidad para los valores dados
# D -> número de muestras marcadas en total
# N -> el total (habitualmente el número que buscamos)
# n -> número de muestras extraidas
# r -> número de muestras extraidas marcadas
F<-function(N,D,n,r){
  x<-(Combinatorio(D,r)*Combinatorio(N-D,n-r))/Combinatorio(N,n)
  return(x)
}


# Ejemplo, sabiendo D = 5, n = 10, r = 1, calculamos N
D<-5
n<-10
r<-1

# Empezamos a contar en n + D - r (por razonamiento lógico)
N<-n+D-r
Siguiente<-N+1
NProb<-F(N,D,n,r) # Calculamos la probabilidad en N
SiguienteProb<-F(Siguiente,D,n,r) # Calculamos la probabilidad en N+1

probabilidades<-c()

# Iteramos hasta que la probabilidad empiece a descender
while(NProb<=SiguienteProb){
  Siguiente<-Siguiente+1
  NProb<-SiguienteProb
  SiguienteProb<-F(Siguiente,D,n,r)
  probabilidades<-c(probabilidades,SiguienteProb)
}

#plot(probabilidades)
#Siguiente

# Función que calcula N (desconocido) maximizando la probabilidad
# D -> las marcadas
# n -> las extraidas
# r -> las extraidas marcadas
CalcularN<-function(D,n,r){
  N<-n+D-r
  Siguiente<-N+1
  NProb<-F(N,D,n,r) # Calculamos la probabilidad en N
  SiguienteProb<-F(Siguiente,D,n,r) # Calculamos la probabilidad en N+1
  
  # Iteramos hasta que la probabilidad empiece a descender
  while(NProb<=SiguienteProb){
    Siguiente<-Siguiente+1
    NProb<-SiguienteProb
    SiguienteProb<-F(Siguiente,D,n,r)
  }
  return(Siguiente-1) # Le restamos uno para no devolver el siguiente sino el actual
}

CalcularN(5,10,1)

# Calcula el número de defectuosas totales con los valores datos
# N -> total
# n -> extraidas
# r -> extraidas defectuosas
CalcularD<-function(N,n,r){
  D<-r # Iniciamos D en el mínimo lógico (r)
  Siguiente<-D+1
  DProb<-F(N,D,n,r)
  SiguienteProb<-F(N,Siguiente,n,r)
  while(DProb<=SiguienteProb){
    DProb<-SiguienteProb
    Siguiente<-Siguiente+1
    SiguienteProb<-F(N,Siguiente,n,r)
  }
  return(D)
}

CalcularD(30,10,1)

# Test de hipótesis para la media
# --------------------------------

# Bilateral

x<-rpois(5000,3)

# Hipotesis
# Nula        Ho:media=4
# Alternativa H1:media<>4
t.test(x, mu=3, alternative='two.sided', conf.level=0.99)
# Estimación puntual de la media: mean of x
# Intervalo de confianza al 98%
# p-valor<0.02 -> Acepto H1


CalcularN(4,3,1)