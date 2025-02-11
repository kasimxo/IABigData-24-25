# Trabajar con el dataset HeartDiseaseCleveland
data<-read.csv(file.choose(), header=TRUE, sep=',')

# Vamos a generar un nuevo archivo para trabajar en weka
# Para ello, debemos llevar a cabo ciertas tareas antes de exportarlo

# Tareas:

# Tarea 1: Transformar la columna sexo de num a texto
# 0 -> Hombres
# 1 -> Mujeres
sexo<-rep("M", dim(data)[[1]])
indexMales<-which(data$sex==0)
sexo[indexMales]<-"V"

# Tarea 2: Transformar la columna cp a texto
cp<-as.character(data$cp)

# Tarea 3: Transformar la columna fbs a texto
fbs<-as.character(data$fbs)

# Tarea 4: Transformar la columna restecg a texto
restecg<-as.character(data$restecg)

# Tarea 5: Transformar la columna exang a texto
exang<-as.character(data$exang)

# Tarea 6: Transformar la columna ca a texto
ca<-as.character(data$ca)

# Tarea 7: Transformar la columna slope a texto
slope<-as.character(data$slope)

table<-data.frame(sexo=sexo,cp=cp,fbs=fbs,restecg=restecg,exang=exang,ca=ca,slope=slope)

# Tarea 8: T
write.csv(table, file.choose())
