# Funcion que recibe la lista de probabilidades junto con la realidad
# y un corte y devuelve la tasa TPR y FRP
tabla<-function(corte,prob,realidad){
 TP<-0
 TN<-0
 FP<-0
 FN<-0
 i<-1
 while(i<=length(prob)){
    if(prob[[i]]>=corte & realidad[[i]]=="SI"){
        TP<-TP+1}
    if(prob[[i]]>=corte & realidad[[i]]=="NO"){
        FP<-FP+1}
    if(prob[[i]]<corte & realidad[[i]]=="SI"){
        FN<-FN+1}
    if(prob[[i]]<corte & realidad[[i]]=="NO"){
        TN<-TN+1}
     i<-i+1
     }
   TPR<-TP/(TP+FN)
   FPR<-FP/(FP+TN)
   x<-c(TPR,FPR)
   return(x)

   }
probabilidades<-c(0.95,0.9,0.85,0.8,0.75,0.72,0.65,0.6,0.55,0.52,0.45,0.4,0.35,0.33,0.25)
R<-rep("SI",4)
R<-append(R,"NO")
R<-append(R,rep("SI",4))
R<-append(R,rep("NO",6))
TPR<-c()
FPR<-c()
cortes<-c(0.9,0.7,0.5,0.3,0.1)
# Funcion que recibe la lista de cortes y configura en un data frame la lista de 
# TPR y FPR
i<-1
while(i<=length(cortes)){
  y<-tabla(cortes[[i]],probabilidades,R)

  TPR<-append(TPR,y[[1]])
  FPR<-append(FPR,y[[2]])
  i<-i+1}

frame<-data.frame(TPR=TPR,FPR=FPR)
frame



# Regla del trapecio
trapecio<-function(x1,x2,y1,y2){
   z<-(x2-x1)*(y1+y2)/2
   return(z)
}

# AUC

Area<-function(x,y){
   i<-1
   A<-0
   j<-1
   
  while(i<length(x)){
     j<-i+1

     A<-A+trapecio(x[[i]],x[[j]],y[[i]],y[[j]])

     i<-i+1}
   return(abs(A))
}



length(frame$FPR)
length(frame$TPR)

A<-Area(frame$FPR,frame$TPR)
A

# Representacion

plot(frame$FPR,frame$TPR,main="ROC",type='b',pch=1,cex=1,lty=1,lwd=1,col='blue',
 col.main='black',ylab="Sensibilidad",xlab="1-Especificidad",sub="AUC=0.8928",
 col.sub="green")
