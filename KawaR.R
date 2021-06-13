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

wide<- cbind(as.vector(t(as.numeric(colnames(import)))),w1) #dodajemy lata do wspolczynnika dla przejrzystosci
var(wide[,2]) #patrzymy na odchylenie
#Wykres slopkowy z linia wyznacznika

#przygotujemy ramkê dane pod wykres
(r1 <- rep(1990:2019, times=2)) #pierwsza kolumna to ka¿da data powtorzona po sobie dwa razy
(rok <- sort(r1)) #uzyskujemy to powtarzajac zestaw wszystkich lat dwa razy i sortujac
(legenda <- rep(c("import","eksport"), times=30)) #podobnie druga kolumna to frazy import i eksport powtarzane w kolko
il1<-vector() #trzecia kolumna to wartosci importu i eksportu dla kazdego roku
for (n in 0:60){ #uzyskujemy ja za forem, ktory dla parzystych liczb wpisuje wartosc eksportu, a dla nieparzystych importu
  if(n%%2==0){
    il1[n]<- eksport[56,(n/2)]
  }
  if(n%%2==1){
    il1[n]<- import[37,(as.integer(n/2)+1)]#jako ze przechodziny po forze z 1 do 60, to indeks bedziemy musieli podzielic przez 2, jako ze musimy to zrobic takze dla liczb nieparzystych, to wymyslilem taki trik z integerem, glupie rozwiazanie, ale dziala
  }
}
(ilosc<-unlist(il1))
(w2 <- rep(w1, times=2))#czwarta kolumna to wartosc wspolczynnika obliczonego wczesniej, takze powielona 2 razy
(wspolczynnik <- sort(w2)*50000) #mnozymy razy 50000, by wspolczynnik byl wyzej na grafie i roznice z roku na rok byly bardziej widoczne

dane_graf_1 <- data.frame(rok,legenda,ilosc,wspolczynnik)#laczymy wszysto w jedna rakme

library(ggplot2) #wczytujemy pakiet ggplot2
ggplot(dane_graf_1) + 
  geom_bar(aes(fill=legenda, y=ilosc, x=rok), position="dodge", stat="identity") + #tworzymy podwujny wykres kolumnowy
  expand_limits( x = c(1990,NA), y = c(0,NA)) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  scale_y_continuous(breaks = scales::breaks_width(10000)) +
  geom_line(aes(x=rok, y=wspolczynnik)) #i linie przedstawiajaca wspolczynnik
  
#ggsave(file="wykres.eps",device="eps", path="/Users/wiktor/Desktop/KawaR", width=20, height=20, units="cm")

#min max
names(import)[which(as.vector(t(import[37,])) == min(as.vector(t(import[37,]))))] #min import
names(import)[which(as.vector(t(import[37,])) == max(as.vector(t(import[37,]))))] #max import
names(eksport)[which(as.vector(t(eksport[56,])) == min(as.vector(t(eksport[56,]))))] #min eksport
names(eksport)[which(as.vector(t(eksport[56,])) == max(as.vector(t(eksport[56,]))))] #max eksport
#lata w ktorych mamy ekstrema przydaja nam sie w dlaszej pracy

#Mapa swiata z krajami importujacymi/eksportujacymi

#install.packages("rgdal")
library(rgdal) #wczytujemy, by moc wczytac plik z mapa
kontor <- readOGR( #wczytujemy kontur mapy
  dsn= paste0(getwd(),"/TM_WORLD_BORDERS_SIMPL-0.3.shp") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
#Import

#wybieramy tylko dane o Europie i resetujemy ich indeksy
Europa <- kontor[kontor@data$REGION==150 & kontor@data$NAME!="Russia" , ]
rownames(Europa@data) <- 1:nrow(Europa@data)
#znajdujemy w danych mapy kraje dla ktorych my mamy dane o kawie
Kraje_z_importem <- Europa@data[Europa@data$NAME==gsub("   ", "", rownames(import)[2]),]
for (n in 3:37){
  Kraje_z_importem <- rbind(Kraje_z_importem,Europa@data[Europa@data$NAME==gsub("   ", "", rownames(import)[n]),])
}
#sprawdzamy jakie indeksy maja te kraje, by tylko je pomalowac
idimport<-as.numeric(rownames(Import_mapd))


library(RColorBrewer) #biblioteka do kolorow
paleta <- brewer.pal(9, "YlOrRd") #wybralismy palte idaca od zoltego do czerwonego
paleta <- colorRampPalette(paleta)(100) #robimy 100 krokow, bo zmiany sa male i przy malej ilosci krokow nie bylo by ich widac

kawa_kraje_i<-c(2:3,5:6,9:29,31,33,35) #indeksy tylko tych krajow, ktore maja zarowno dane importu i kontur (uzyskane manualnie patrzac na dane)

for (i in 1:30){ #dla kazdego roku
#towrzymy skale barw
Barwa_od_Importu <- cut(import[kawa_kraje_i,i], 100)
Kol_Import<-vector() #pusty wektor dla barw
Kol_Import[1:50] <- "#FFFFFF" #wypelniamy wszytko na bialo
Kol_Import[idimport] <- paleta[as.numeric(Barwa_od_Importu)] #po kolei do odpowiednich indeksow wpisujemy kolor zaleznie od ilosci improtu
#rysowanie z zapisywanie
png(file=paste("i_",i,"_mapa.png",sep="")) #nazwa z indeksem by nie nadpisywalo
plot(Europa, col=Kol_Import ,  bg = "#A6CAE0", xlab=colnames(import)[i]) #bardzo prosta fukcja, przyjmuje mape konturowa europy, odpowiednio sperparowane kolory i kolor t³a, by moc je odruznic, podpisane takze rokiem by bylo widac zmiane gdy dane zmieniaj sie nieznacznie
dev.off()
}

#Eksport (pomijamy komentarze, które by by³y takie same)

Kraje_z_eksportem <- kontor@data[kontor@data$NAME==gsub("   ", "", rownames(eksport)[1]),]
for (n in 2:54){
  Kraje_z_eksportem <- rbind(Kraje_z_eksportem,kontor@data[kontor@data$NAME==gsub("   ", "", rownames(eksport)[n]),])
}

ideksport<-as.numeric(rownames(Kraje_z_eksportem))+1 #dodawanie jeden, by pokolorowac odpowiednie kraje

kawa_kraje_e<-c(1,3:9,11,13:44,46:48,50:54)

for (e in 1:30){
  as.numeric(Barwa_od_Eksportu)
Barwa_od_Eksportu <- cut(eksport[kawa_kraje_e,e], 100)
Kol_Eksport<-vector()
Kol_Eksport[1:246] <- "#FFFFFF" #mamy wiecej bo jest caly swiat
Kol_Eksport[ideksport] <- paleta[as.numeric(Barwa_od_Eksportu)]
Kol_Eksport
png(file=paste("e_",e,"_mapa.png",sep=""))
plot(kontor, col=Kol_Eksport ,  bg = "#A6CAE0", xlab=colnames(eksport)[e])
dev.off()
}



