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
wide

#plot(wide[1,], wide[2,], main="Wspolczynnik importu do eksportu", xlab="rok", ylab="wspolczynnik", type="h", col=1, xaxt="n")
#text(wide[2,], wide[1,], labels = wide[2,], pos=1)
#lines(wide[1,], wide[2,])
#points(wide[1,], wide[2,], pch=20)
#axis(1, at = seq(1990, 2019, by = 1), las=2)

library(ggplot2)
(r1 <- rep(1990:2019, times=2))
(rok <- sort(r1))
(legenda <- rep(c("import","eksport"), times=30))
il1<-vector()
for (n in 0:60){
  if(n%%2==0){
    il1[n]<- eksport[56,(n/2)]
  }
  if(n%%2==1){
    il1[n]<- import[37,(as.integer(n/2)+1)]
  }
}
(ilosc<-unlist(il1))
(w2 <- rep(w1, times=2))
(wspolczynnik <- sort(w2)*50000)

dane_graf_1 <- data.frame(rok,legenda,ilosc,wspolczynnik)
dane_graf_1

ggplot(dane_graf_1) + 
  geom_bar(aes(fill=legenda, y=ilosc, x=rok), position="dodge", stat="identity") +
  expand_limits( x = c(1990,NA), y = c(0,NA)) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  scale_y_continuous(breaks = scales::breaks_width(10000)) +
  geom_line(aes(x=rok, y=wspolczynnik)) 
  

#ggsave(file="wykres.eps",device="eps", path="/Users/wiktor/Desktop/KawaR", width=20, height=20, units="cm")


#min max
names(import)[which(as.vector(t(import[37,])) == min(as.vector(t(import[37,]))))] #min import
names(import)[which(as.vector(t(import[37,])) == max(as.vector(t(import[37,]))))] #max import
names(eksport)[which(as.vector(t(eksport[56,])) == min(as.vector(t(eksport[56,]))))] #min eksport
names(eksport)[which(as.vector(t(eksport[56,])) == max(as.vector(t(eksport[56,]))))] #max eksport

max(as.vector(t(import[37,])))
max(as.vector(t(eksport[56,])))

library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"TM_WORLD_BORDERS-0.3.shp") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

library(broom)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
spdf_fortified <- tidy(my_spdf, region = "NAME")

try_require(c("gpclib", "maptools"))
unioned <- unionSpatialPolygons(cp, invert(polys))
# Plot it
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() 

library(dplyr)
library(africa@data)

# Make sure the variable you are studying is numeric
africa@data$POP2005 <- as.numeric( africa@data$POP2005 )

# Distribution of the population per country?
africa@data %>% 
  ggplot( aes(x=as.numeric(POP2005))) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white')


#Wykres kolowy eksportu
#install.packages("scales")
library(scales) #wczytuje pakiet, z ktorego funkcje pozwola mi sumowac wartosci w rzedach i kolumnach

#utworzenie ramek danych zawierajacych panstwa i sumy wyeksportowanych paczek kawy w latach 1990-2019 i przypisanie ich
#do zmiennych o nazwach kontynentow (dane byly wybierane z ramki danych o nazwie „eksport”, a panstwa odpowiednio 
#przypisane do kontynentów na ktorych sie znajduja)

(ameryka_polnocna <- rowSums(eksport[c(11,20,24,33,35,37),]))
(ameryka_poludniowa <- rowSums(eksport[c(2,3,7,14,22,39,40,49,51),]))
(ameryka_centralna <- rowSums(eksport[c(9,13,15,23,27),]))
(afryka <- rowSums(eksport[c(1,4,5,6,8,10,12,16,17,18,19,21,28,30,31,32,36,42,43,45,48,50,54,55),]))
(azja <- rowSums(eksport[c(25,26,29,34,41,44,46,47,52,53),]))
(australia_i_oceania <- rowSums(eksport[c(38),]))

#zsumowanie danych liczbowych dla kazdego kontynentu
(ameryka_polnocna <- colSums(as.data.frame(ameryka_polnocna)))
(ameryka_poludniowa <- colSums(as.data.frame(ameryka_poludniowa)))
(ameryka_centralna <- colSums(as.data.frame(ameryka_centralna)))
(afryka <- colSums(as.data.frame(afryka)))
(azja <- colSums(as.data.frame(azja)))
(australia_i_oceania <- colSums(as.data.frame(australia_i_oceania)))

#utworzenie ramki danych o nazwie „kontynenty”, z ktorej pozniej wybrane zostana osobno nazwy kolumn i dane liczbowe 
#i przypisane do zmiennych potrzebnych do utworzenia wykresu
(kontynenty <- data.frame(ameryka_polnocna,ameryka_poludniowa,ameryka_centralna,afryka,azja,australia_i_oceania))

(wartosci<-as.numeric(kontynenty))
pie_labels <- (paste0(round(100 * wartosci/sum(wartosci), 2), "%"))

#install.packages("RColorBrewer")
library(RColorBrewer) #wczytuje pakiet zawierajacy w sobie palety barw
myPalette <- brewer.pal(6, "Set2")

#tworzenie wykresu
pie(wartosci, labels = pie_labels, border="white", col=myPalette, values="%",
      main="Procentowy udział kontynentów eksportujących kawę w latach 1990-2019")
legend(1.1, 1, legend = c("Ameryka Północna", "Ameryka Południowa", "Ameryka Centralna", "Afryka", "Azja", "Australia i Oceania"), 
       fill =  myPalette, cex=0.7)


#Wykres kolowy dla importu
(europa <- rowSums(import[c(2:29, 31, 33, 35),]))
(europa <- as.data.frame(europa))

(wartosci_europa <- europa[,1])
(panstwa <- rownames(europa))

panstwa2 <- vector()
for(i in 1:31){
(panstwa2[i] <- (paste(i,panstwa[i], sep=" ", round(100 * wartosci_europa[i]/sum(wartosci_europa), 2), "%")))
}
panstwa2
wartosci_europa[14]

pie(wartosci_europa, border="white", col=rainbow(31), values="%", cex=0.8,
    main="Procentowy udział kontynentów eksportujących kawę w latach 1990-2019")
legend(1.1, 1, legend=panstwa2, fill=rainbow(31), cex=0.37)


