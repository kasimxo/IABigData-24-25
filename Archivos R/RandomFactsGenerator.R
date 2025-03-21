# Archivo para trabajar con el dataset VoteFixed
data<-read.csv(file.choose(), header=TRUE, sep=',')

# Genera una selección random de 20 datos
len<-dim(data)[[1]]
n<-20
x<-sample(1:len, n, replace = FALSE)
factsRaw<-data[x,]
# (votante (id 32) (Medicamentos y) (Petroleo n) (Presupuestos ?Q))
facts<-c()
i<-1
head(data)
while(i<=n){
  row<-data[x[[i]],]
  fact<-sprintf("(pasajero (id %d) (Clase %s) (Sexo %s) (Edad %s) (Precio %s))",
               row$id, row$Clase, row$Sexo, row$Edad, row$Precio)
  facts<-append(facts, fact)
  i<-i+1
}
write.csv(facts, file.choose())


# Esta parte se podría refactorizar, extrayendo los indices del loop anterior
# y haciendo una mejor muestra por pantalla
activationIndexes<-c(670, 
                     665, 
                     353, 
                     775, 
                     297, 
                     782, 
                     819,
                     104,
                     766,
                     180,
                     459,
                     669,
                     315,
                     367,
                     57,
                     343)
results<-data[activationIndexes,]$Survived
i<-1
while(i<=length(results)){
  print(results[[i]])
  i<-i+1
}
