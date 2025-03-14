Sigmoide<-function(x){
  y<-1/(1+exp(-x))
  return (y)
}

Nodo<-funcion(pesos, entradas, pesoInicial){
  z<-0
  i<-1
  while(i<=length(pesos)){
    z<-z+Sigmoide(entradas[[i]])*pesos[[i]]
    i<-i+1
  }
  z<-z+pesoInicial
  return (z)
}

Duracion<-function(Nodos, pesos, pesoInicial) {
  x<-0
  i<-1
  while(i<=length(Nodos)){
    x<-x+Nodos[[i]]*pesos[[i]]
    i<-i+1
  }
  return (x)
}

Entradas<-function(G){
  x<-c()
  if(G[[1]]=="COORD"){
    x<-append(x,c(1,0,0))
  } else {
    if(G[[1]]=="UCC"){
      x<-append(x,c(0,1,0))
    } else {
      x<-append(x,c(0,0,1))
    }
  }
  
  if(G$Destino=="America"){
    x<-append(x,c(1,0,0,0,0))
  } else {
    if(G$Destino=="Europa"){
      x<-append(x,c(0,1,0,0,0))
    } else {
      if(G$Destino=="Otros"){
        x<-append(x,c(0,0,1,0,0))
      } else {
        if(G$Destino=="Africa"){
          x<-append(x,c(0,0,0,1,0))
        } else {
          x<-append(x,c(0,0,0,0,1))
        }
      }
    }
  }
  
  x<-append(x,c(G$Tarifa, G$Cantidad))
  return(x)
}

pesos1<-c(-0.118, 0.223, 0.454, 0.41, -0.575, 0.248, 0.905, 0.406, 0.795, 0.24, 0.30)
pesosinicial1<--0.5859

pesos2<-c(-0.353, 0.12, 0.74, 0.117, -0.214, 0.042, 0.906, 0.259, 0.765, 0.186, 0.223)
pesoinicial2<--0.523

pesos3<-c()
