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
while(i<=n){
  row<-data[x[[i]],]
  fact<-sprintf("(votante (id %d) (Medicamentos %s) (Clase %s) (Misiles %s))",
               row$X, row$Medicamentos, row$Clase, row$Misiles)
  facts<-append(facts, fact)
  i<-i+1
}
write.csv(facts, file.choose())