#Import Danych
getwd() #pobieramy lokalizacje plikow

#operacja wczytywania danych eksportu
(e1<-read.csv("Exports.csv",sep=";")) #wczytujemy plik do zmiennej pomocniczej
e2 <- e1[,-1] #wczytujemy do drugiej zmiennej wszystkie rekordy z wyjatkiem pierwszej kolumny
eksport <- as.data.frame(sapply(e2, as.numeric)) #zmieniamy typ danych ze znakowego na numeryczny
(rownames(eksport) <- e1[,1]) #ustawiamy pierwsza kolumne jako nazwy wierszy
(colnames(eksport) <- c(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) #ustawiamy nazwy kolumn

#operacja wczytywania danych importu (powtarzamy powyzsze czynnosci)
(i1<-read.csv("Imports.csv",sep=";"))
i2 <- i1[,-1]
import <- as.data.frame(sapply(i2, as.numeric))
(rownames(import) <- i1[,1])
(colnames(import) <- c(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))

#Obrobka
#wspolczynnik importu do eksportu
w1<-vector() #tworzymy pusty wektor zeby zapisac w nim wartosci
for (n in 1:30){ #jest 30 lat
(w1[n]<-(import[37,n]/eksport[56,n])) #dla kazdego roku bierzemy sume import i dzielimy ja przez sume exportu, zapisujemy do wektora
}

(wide <- rbind(as.numeric(colnames(import)), w1)) #dodajemy lata do wspolczynnika dla przejzystosci
#wide #wyswietlamy wspolczynnik
#plot(wide[1,], wide[2,], main="Wspolczynnik importu do eksportu", xlab="rok", ylab="wspolczynnik", type="h", col=1, xaxt="n")
#text(wide[2,], wide[1,], labels = wide[2,], pos=1)
#lines(wide[1,], wide[2,])
#points(wide[1,], wide[2,], pch=20)
#axis(1, at = seq(1990, 2019, by = 1), las=2)

library(ggplot2)

wide2<-data.frame(wide)

import2 <- import[37,]
eksport2 <- eksport[56,]
dane<-data.frame(import2, eksport2)

iks<-data.frame(wide[1,])

ggplot(dane, aes(fill="dupa", y=seq(1990, 2019, by = 1), x=iks))+
  geom_bar(position="dodge", stat="identity")



?ggplot






barplot(w1, main="Wspolczynnik importu do eksportu", xlab="rok", ylab="wspolczynnik", ylim=c(0,1.2),cex.names=0.8, names.arg=wide[1,])

?barplot

wide

str(wide)

str(w1)

wide




#min max

#which(import == min(import[37,]))
#which(import == max(import[37,]))


