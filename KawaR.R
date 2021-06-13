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

#install.packages("rgdal")
library(rgdal)
my_spdf <- readOGR( 
  dsn= paste0(getwd(),"/TM_WORLD_BORDERS_SIMPL-0.3.shp") , 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

Europa <- my_spdf[my_spdf@data$REGION==150 & my_spdf@data$NAME!="Russia" , ]
rownames(Europa@data) <- 1:nrow(Europa@data)

Import_mapd <- Europa@data[Europa@data$NAME==gsub("   ", "", rownames(import)[2]),]
for (n in 3:37){
  Import_mapd <- rbind(Import_mapd,Europa@data[Europa@data$NAME==gsub("   ", "", rownames(import)[n]),])
}
Import_mapd

idi<-as.numeric(rownames(Import_mapd))+1
idi

# Palette of 30 colors
library(RColorBrewer)
kolorki <- brewer.pal(9, "YlOrRd") 
kolorki <- colorRampPalette(kolorki)(9)
kolorki
kraje<-c(2:3,5:6,9:29,31,33,35)
import[2,24]
class_of_country <- cut(import[kraje,24],9)
class_of_country[9]
as.numeric(class_of_country)
Kol_Mapa<-vector()
Kol_Mapa[1:50] <- "#FFFFFF"
Kol_Mapa[idi] <- kolorki[as.numeric(class_of_country)]
Kol_Mapa
for (i in 1:30){
# Attribute the appropriate color to each country
class_of_country <- cut(import[kraje,i], 9)
Kol_Mapa[1:50] <- "#FFFFFF"
print(as.numeric(class_of_country))
Kol_Mapa[idi] <- kolorki[as.numeric(class_of_country)]
# Make the plot
png(file=paste(i,"_mapa.png",sep=""))
plot(Europa, col=Kol_Mapa ,  bg = "#A6CAE0", xlab=colnames(import)[i])
dev.off()
}
plot(Europa, col=Kol_Mapa ,  bg = "#A6CAE0", xlab=colnames(import)[i])
