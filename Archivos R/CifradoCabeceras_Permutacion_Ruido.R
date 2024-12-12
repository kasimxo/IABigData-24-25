### --- --- DECLARACION DE FUNCIONES --- --- ###

# Función que encripta un Nombre(cabecera) en un abecedario de 26 caracteres
# Parámetros:
# table: el data set que queremos
# index: el número de la columna
# y = ecA*x+ecB
# Decuelve el nombre encriptado
EncriptarNombre<-function(table, index, ecA, ecB){

  z<-toupper(names(table)[[index]])
  # Separa en cartacteres
  x<-strsplit(z,split="")
  A<-LETTERS
  
  # Comenzamos la codificacion de un nombre
  cod<-c()
  i<-1
  while(i<=length(x[[1]])){
    # Buscamos la posicion de cada letra del nombre en el Alfabeto
    w<-which(A==x[[1]][[i]])
    # Codificamos
    s<-(ecA*w+ecB)%% length(A)
    s<-s+1
    #Añadimos el caracter codificado
    charEncriptado<-encriptarChar(s)
    cod<-append(cod,charEncriptado)
    i<-i+1
  }
  # Pegamos las letras para formar un nuevo nombre
  C<-paste(cod,collapse="")
  return (C)
}

# Función que encripta todas las columnas en un abecedario de 26 caracteres
# Parámetros:
# table: el data set que queremos
# y = ecA*x+ecB
# Decuelve la tabla con las cabeceras encriptado
EncriptarColumnas<-function(tabla, ecA, ecB) {
  cabecerasEncriptadas<-c()
  i<-1
  while(i<=dim(tabla)[[2]]) {
    nom<-EncriptarNombre(tabla, i, ecA, ecB)
    cabecerasEncriptadas<-append(cabecerasEncriptadas, nom)
    i<-i+1
  }
  cabecerasEncriptadas
  colnames(tabla)<-cabecerasEncriptadas
  return (tabla)
}

# Esta función "encripta" un caracter pero lo hace en un tryCatch para evitar
# problemas si el caracter no está incluido en nuestro alfabeto
# Ejemplo: . - @ 
# Pôr defecto, ignora estos caracteres
# Si queremos que no los ignore, puedes modificar el alfabeto que estás utilizando
encriptarChar<-function(letra){
  tryCatch(
    expr = {
      letraCifrada<-LETTERS[letra]
    }
  )
  return (letraCifrada)
}

# Función que calcula el valor inverso
# Parámetros:
# a: el valor a calcular el inverso
# longitus: el módulo, la longitud del abecedario
inverso<-function(a,longitud){
  i<-1
  while(i < longitud && (i*a) %% longitud != 1){
    i<-i+1
  }
  return(i)
}

# Función que desencripta el nombre de una columna
# Parámetros:
# nombre:
# ecA: valor de la función de encriptación
# ecB: valor de la función de encriptación
# Ejemplo función encriptación: y = ecA * x + ecB
DesencriptarNombre<-function(nombre,ecA,ecB){
  x<-strsplit(nombre,split="")
  A<-LETTERS
  cod<-c()
  i<-1
  while(i<=length(x[[1]])){
    w<-which(A==x[[1]][[i]])
    w<-w-1
    s<-(inverso(ecA,length(A))*(w-ecB))%%length(A)
    cod<-append(cod,A[[s]])
    i<-i+1
  }
  C<-paste(cod,collapse="")
  return (C)
}

# Función que permuta las columnas de una tabla
# Parámetros:
# tabla: el dataset cuyas columnas quieres permutar
# permutación: el orden de la permutación
PermutarColumnas<-function(tabla,permutacion){
  tabla1<-tabla
  k<-1
  while(k<=dim(tabla)[[2]]){
    tabla1[,k]<-tabla[,permutacion[[k]]]
    k<-k+1
  }
  tabla<-tabla1
  return (tabla)
}

DesPermutarColumnas<-function(tabla, permutacion){
  tabla1<-tabla
  k<-1
  while(k<=dim(tabla)[[2]]){
    tabla1[,permutacion[[k]]]<-tabla[,k]
    k<-k+1
  }
  tabla<-tabla1
  return (tabla)
}

TestNormalidadColumna<-function(columna){
  prueba<-ks.test(columna, "pnorm", mean=mean(columna),sd=sd(columna))
  return (prueba)
}


### --- --- EJERCICIOS --- --- ###

#Leer el archivo que queramos
Salud<-read.csv(file.choose(), header=TRUE, sep = ',')

# Quitamos algunas columnas de este archivo
x<-c(1) # Las columnas que estén en este array serán ignoradas
Salud<-Salud[,-x]

# EJERCICIO 1: Encriptar los nombres de las columnas y = 31*x+7

head(Salud)
Salud<-EncriptarColumnas(Salud,31,7)
head(Salud)

# EJERCICIO 2: Permutación al azar:

# Generación de una secuencia para la permutación
Numeros<-seq(1,dim(Salud)[[2]],1)
P<-sample(Numeros,dim(Salud)[[2]],replace=FALSE)

permutado<-PermutarColumnas(Salud,P)
head(permutado) # Mostramos las columnas permutadas

# Ejercicio 3: Ruido uniforme

# Introducir un ruido normal(10,30) en la Edad y deshacerlo
ruido1<-rnorm(dim(Salud)[[1]],10,30)
Salud[,1]<-Salud[,1]+ruido1
head(Salud)

# Deshacer ruido
Salud[,1]<-Salud[,1]-ruido1
head(Salud)

# Ejercicio 4: Test ks de normalidad para la columna edad

# Test de normalidad (test de ks) Columna Age
# Si has codificado las cabeceras, importante tenerlo en cuenta
TestNormalidadColumna(Salud$MQG)
# Si el p_value < 0.05, NO ES UNA UNIFORME

# Regresión de age a través de Billing.Amount

# Test de chow según sexo
MalesNums<-which(Salud$Gender=='Male')
corte<-length(MalesNums)
