#Import Danych
(e1<-read.csv("Exports.csv",sep=";"))
e2 <- e1[,-1]
rownames(e2) <- e1[,1]
eksport <- as.data.frame(sapply(e2, as.numeric))
(i1<-read.csv("Imports.csv",sep=";"))
i2 <- i1[,-1]
rownames(i2) <- i1[,1]
import <- as.data.frame(sapply(i2, as.numeric))
#Obróbka
#wspó³cznik importu do eksportu
wide<-vector()
for (n in 1:30){
(wide[n]<-(import[37,n]/eksport[56,n]))
}
eksport[56,]
import[37,]
wide
