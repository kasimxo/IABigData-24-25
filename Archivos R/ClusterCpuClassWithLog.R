data<-read.csv(file.choose(), header=TRUE, sep=',')
head(data)
logging<-log(data$class)
plot(logging)
min(logging) # 1.791758
max(logging) # 7.047517
7.047517-1.791758 # 5.255759
5.255759/4 # 1.31394

primer<-1.791758+1.31394
segundo<-primer+1.31394
tercero<-segundo+1.31394

separated<-cut(log(data$class), breaks = c(-Inf, primer, segundo, tercero, Inf), 
    labels = c("Bajo", "Medio", "Alto", "Muy alto"))
separated
summary(separated)
data$class<-separated
write.csv(data, file.choose())
