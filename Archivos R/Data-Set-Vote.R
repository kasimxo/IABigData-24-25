# Archivo R para trabajar con el dataset Vote
data<-read.csv(file.choose(), sep=',', header = TRUE)

dim(data) # Tiene 435 filas

j<-1

fin<-dim(data)[[2]]-1

while(j<=fin){
  # Reconstrucción de datos: hay que verificar NA y ?
  print(j)
  # Extraemos los datos no válidos
  columna<-c()
  columna<-which(data[,j]=='?')
  

  # Los retiramos
  conjunto<-c()
  conjunto<-data[-columna,j]
  print(length(conjunto))
  
  # Calculamos cuantos yes y cuantos no
  
  conjuntoYes<-length(which(conjunto=='y'))/length(conjunto)
  print("conjunto")

  muestra<-runif(length(columna),0,1)
  print("muestra")
  # Reconstrucción
  Reconstruccion<-c()
  i<-1
  print("inicio loop")
  while(i<=length(columna)){
    if(muestra[[i]]<conjuntoYes){
      Reconstruccion<-append(Reconstruccion, 'y')
    } else {
      Reconstruccion<-append(Reconstruccion, 'n')
    }
    i<-i+1
  }
  print("Reconstruccion")
  print(length(Reconstruccion))
  data[columna, j]<-Reconstruccion
  print(dim(data))
  j<-j+1
}

names(data)<-c(
  "Hijos discapacitados",
  "Agua",
  "Presupuestos",
  "Medicamentos",
  "Ayuda al salvador",
  "Religion en escuela",
  "Pruebas satelite",
  "Ayuda Nicaragua",
  "Misiles",
  "Inmigracion",
  "Recorte pretroleo",
  "Educacion",
  "Derecho",
  "Crimenes",
  "Aranceles",
  "Sudafrica",
  "Clase"
)

# El archivo arreglado ya esta en Github
#write.csv(data, file.choose())
