# Mi primer map Reduce
# Se trabaja sobre el dataset del titanic-ing
# Calcular el precio medio del billete según la supervivencia

# Importación de datos
data<-read.csv(file.choose(),header=TRUE,sep=",")

# Definición de la rutina map, aquí lo está agrupando por supervivencia
Map<-function(C){
  x<-c()
  i<-1
  while(i<=dim(data)[[1]]){
    if(data$Survived[[i]]==C){
      x<-append(x, data$Fare[[i]])
    }
    i<-i+1
  }
  return(x)
}

PrecioN<-Map(0)
PrecioS<-Map(1)
PrecioN
PrecioS


# Definición de la rutina reduce
Reduce<-function(x) {
  y<-mean(x)
}

# Creación data frame
PmedioS<-Reduce(PrecioS)
PmedioN<-Reduce(PrecioN)

framePreciosPorPrecio<-data.frame(PrecioS=PmedioS,PrecioN=PmediosN)
framePreciosPorPrecio

# Mi segundo MapReduce
# Objetivo:
# Obtener un vector con valores 1 y clave el sexo y la supervivencia:
# Map<(Sexo, Supervivencia), Int> -> (Mujer, Si) 1
# Rutina reduce
# Contabilizar la frecuencia de cada salida de la rutina map por clave
# Configurar un data frame 2x2 donde indeique la frecuencia relativa 
# o probabilidad por supervivencia y sexo

MapSexoSupervivencia<-function (sexo, sobrevive){
  x<-c()
  i<-1
  while(i<=dim(data)[[1]]){
    if(data$Survived[[i]]==sobrevive &&
       data$Sex[[i]]==sexo){
      x<-append(x, 1)
    }
    i<-i+1
  }
  return(x)
}
head(data)
MujerS<-MapSexoSupervivencia('female',1)
MujerN<-MapSexoSupervivencia('female',0)
HombreS<-MapSexoSupervivencia('male',1)
HombreN<-MapSexoSupervivencia('male',0)

# Funcion reduce
ReduceSexoSupervivencia<-function(x){
  y<-sum(x)
  return (y)
}

CountMujerS<-ReduceSexoSupervivencia(MujerS)
CountMujerN<-ReduceSexoSupervivencia(MujerN)
CountHombreS<-ReduceSexoSupervivencia(HombreS)
CountHombreN<-ReduceSexoSupervivencia(HombreN)

frameSexoSupervivencia<-data.frame(Si=c(CountMujerS,CountHombreS),
                                   No=c(CountMujerN,CountHombreN),
                                   row.names = c("Mujer","Hombre"))

# Mostramos el resultado:
frameSexoSupervivencia

# CHI CUADRADO
# Determina si dos variables categóricas están relacionadas 
# (es decir, si son dependientes) o si son independientes.
chisq.test(frameSexoSupervivencia)
# Si el p_value está por debajo del umbral quiere decir que ESTÁN relacionadas

# Resulta mucho más útil ver los datos en "relativo", es decir,
# teniendo en cuenta el total
total<-sum(frameSexoSupervivencia)
frameSexo
SupervivenciaRelativo<-data.frame(Si=c(CountMujerS/total,CountHombreS/total),
                                   No=c(CountMujerN/total,CountHombreN/total),
                                   row.names = c("Mujer","Hombre"))

frameSexoSupervivenciaRelativo


# Calculo de probabilidades
# probabilidad P(Si/H)=P(Si^H)P(H)
PMujerS<-frameSexoSupervivenciaRelativo$Si[[1]]/sum(frameSexoSupervivenciaRelativo[1,])
PMujerS
PMujerN<-frameSexoSupervivenciaRelativo$No[[1]]/sum(frameSexoSupervivenciaRelativo[1,])
PMujerN
PHombreS<-frameSexoSupervivenciaRelativo$Si[[2]]/sum(frameSexoSupervivenciaRelativo[2,])
PHombreS
PHombreN<-frameSexoSupervivenciaRelativo$No[[2]]/sum(frameSexoSupervivenciaRelativo[2,])
PHombreN

# Prueba chi.cuadrado para estudiar la dependencia entre el deporte y el tabaco
# Preparamos el dataframe
depFrec<-c(7,87,12,9)
depEsp<-c(4,102,7,8)
tabla<-data.frame(Frec=depFrec,Esp=depEsp,row.names = c("M","N","O","F"))
tabla

chisq.test(tabla)
# Como el p_value es 0.3571, no podemos decir que tengan relación


# Ahora vamos a probar con el data set de titaniccualitativo
# Saber si sobrevive según la clase
data<-read.csv(file.choose(),header=TRUE,sep=',')
head(data)

MapClaseSupervivencia<-function(clase, supervivencia){
  x<-c()
  i<-1
  while(i<=dim(data)[[1]]){
    if(data$Clase[[i]]==clase&&data$Survived[[i]]==supervivencia){
      x<-append(x, 1)
    }
    i<-i+1
  }
  return(x)
}

PS<-MapClaseSupervivencia('P','Si')
PN<-MapClaseSupervivencia('P','No')
SS<-MapClaseSupervivencia('S','Si')
SN<-MapClaseSupervivencia('S','No')
TS<-MapClaseSupervivencia('T','Si')
TN<-MapClaseSupervivencia('T','No')

ReduceClaseSupervivencia<-function(x){
  y<-sum(x)
  return (y)
}

CountPS<-ReduceClaseSupervivencia(PS)
CountPN<-ReduceClaseSupervivencia(PN)
CountSS<-ReduceClaseSupervivencia(SS)
CountSN<-ReduceClaseSupervivencia(SN)
CountTS<-ReduceClaseSupervivencia(TS)
CountTN<-ReduceClaseSupervivencia(TN)

frameClaseSupervivencia<-data.frame(Si=c(CountPS,CountSS, CountTS),
                                   No=c(CountPN,CountSN,CountTN),
                                   row.names = c("Primera","Segunda","Tercera"))

# Mostramos el resultado:
frameClaseSupervivencia

# Podemos hacer el chis cuadrado
chisq.test(frameClaseSupervivencia)
# Como el p_value está por debajo del umbral, están relacionas

# Ahora lo ponemos en relativo 
totalClaseSupervivencia<-sum(frameClaseSupervivencia)
frameClaseSupervivenciaRelativo<-data.frame(Si=c(CountPS/totalClaseSupervivencia,
                                         CountSS/totalClaseSupervivencia,
                                         CountTS/totalClaseSupervivencia),
                                    No=c(CountPN/totalClaseSupervivencia,
                                         CountSN/totalClaseSupervivencia,
                                         CountTN/totalClaseSupervivencia),
                                    row.names = c("Primera","Segunda","Tercera"))

# Mostramos el resultado:
frameClaseSupervivenciaRelativo

# Calculo probabilidades
PPS<-frameClaseSupervivenciaRelativo$Si[[1]]/sum(frameClaseSupervivenciaRelativo[1,])
PPN<-frameClaseSupervivenciaRelativo$No[[1]]/sum(frameClaseSupervivenciaRelativo[1,])
PSS<-frameClaseSupervivenciaRelativo$Si[[2]]/sum(frameClaseSupervivenciaRelativo[2,])
PSN<-frameClaseSupervivenciaRelativo$No[[2]]/sum(frameClaseSupervivenciaRelativo[2,])
PTS<-frameClaseSupervivenciaRelativo$Si[[3]]/sum(frameClaseSupervivenciaRelativo[3,])
PTN<-frameClaseSupervivenciaRelativo$No[[3]]/sum(frameClaseSupervivenciaRelativo[3,])
PPS
PPN
PSS
PSN
PTS
PTN


# Ejercicio:
# Dataset: Lusitania
# 1.a) Map(Sexo, status, Survived) Retorna vector de 1 y 0
#     i)  x<-Map("Male", "Married", "Saved")
#     ii) y<-Map("Female", "Single", "Lost)
#   b) Reduce: Test de proporciones entre x e y
dataLusitania<-read.csv(file.choose(),header=TRUE,sep=',')
MapLusitania<-function(sexo, estado, sobrevive){
  x<-c()
  i<-1
  while(i<=dim(dataLusitania)[[1]]){
    if(dataLusitania$Sex[[i]]==sexo &&
       dataLusitania$Status[[i]]==estado &&
       dataLusitania$Survived[[i]]==sobrevive){
      x<-append(x, 1)
    } else {
      x<-append(x,0)
    }
    i<-i+1
  }
  return (x)
}

HombreCasadoSi<-MapLusitania("Male", "Married", "Saved")
MujerSolteraNo<-MapLusitania("Female", "Single", "Lost")

ReduceLusitania<-function(x){
  y<-sum(x)
  return (y)
}

CuentaHombreCasadoSi<-ReduceLusitania(HombreCasadoSi)
CuentaMujerSolteraNo<-ReduceLusitania(MujerSolteraNo)

dataFrameLusitania<-data.frame(Hombre=CuentaHombreCasadoSi, Mujer=CuentaMujerSolteraNo)
dataFrameLusitania

length(HombreCasadoSi)

prop.test(c(CuentaHombreCasadoSi, CuentaMujerSolteraNo),c(length(HombreCasadoSi), length(MujerSolteraNo)), 
          alternative = "two.sided",
  conf.level = 0.95)

total<- sum(CuentaHombreCasadoSi, CuentaMujerSolteraNo)
prop.test(c(CuentaHombreCasadoSi, CuentaMujerSolteraNo),c(total, total), 
          alternative = "two.sided",
          conf.level = 0.95)
