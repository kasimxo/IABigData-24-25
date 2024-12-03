# DEFINICION DE FUNCIONES:

# Función que recupera Q (q es el primer divisor distinto de q de n)
findQ<-function(num){
  q<-2
  while(num%%q!=0){
    q<-q+1
  }
  return(q)
}

# Función que recupera P (n=p*q)
findP<-function(numN,numQ){
  return(numN/numQ)
}

# Función que calcula 'd' d=e mod(Euler)
findD<-function(numE, numEuler){
  d<-1
  while((d*numE)%%numEuler!=1){
    d<-d+1
  }
  return (d)
}

# Función que calcula el número de euler a partir de p q (ambos primos)
calculateEuler<-function(numP,numQ){
  euler<-(numP-1)*(numQ-1)
  return(euler)
}

# Descifrado: m=m'
decypher<-function(numD,numM1,numN){
  m<-1
  i<-1
  while(i<=numD){
    m<-(numM1*m)%%numN
    i<-i+1
  }
  return(m)
}

cypher<-function(num.m,num.e,num.n){
  c<-num.m**num.e%%num.n
  return(c)
}

Descifrar<-function(num.e,num.n,num.m1){
  q<-findQ(num.n)
  #q # en este caso 13001
  p<-findP(num.n,q)
  #p # en este caso 14009
  Euler<-calculateEuler(p,q)
  #Euler # en este caso 182104000
  d<-findD(num.e, Euler)
  #d # en este caso 14009
  m<-decypher(d,num.m1,num.n)
  return(m)
}

# Función que calcula 'd' sabiendo e y n
# La clave pública tiene forma (n, e)
# La clave privada tiene forma (n, d)
ClavePrivada<-function(num.e,num.n){
  q<-findQ(num.n)
  #q # en este caso 13001
  p<-findP(num.n,q)
  #p # en este caso 14009
  Euler<-calculateEuler(p,q)
  #Euler # en este caso 182104000
  d<-findD(num.e, Euler)
  return (d)
}


# EJERCICIO DE EXAMEN
# Nos dará: e, n, m'

# El primer divisor que no sea 1 (de n) será q
# Con eso podremos calcular p


# Ejemplo: 
# e = 165549091
# m'=16777216
# n = 182131009
e<-13
'H'
m1<-7
n<-807791
ClavePrivada(e,n)
cifrado<-cypher(12,13,807791)
cifrado
Descifrar(e,n,cifrado)


 










