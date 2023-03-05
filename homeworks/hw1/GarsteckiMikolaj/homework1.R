install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
library(tidyr)
auta2012

data(auta2012)

# 1. Rozwa¿aj¹c tylko obserwacje z PLN jako walut¹ (nie zwa¿aj¹c na 
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?


auta2012%>%select(Rodzaj.paliwa)%>%distinct()
# Odp: 18900
auta2012%>%filter(Rodzaj.paliwa=="naped elektryczny",Waluta=="PLN")%>%select(Cena)%>%summarise(across(Cena, median))


# 2. W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001 
# roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
# mechanicznych (KM) jest najwiêksza.



# Odp: Bugatti po 2001 roku

auta2012  %>% select(Marka,Rok.produkcji,KM)%>%
  mutate(nowy=if_else(Rok.produkcji >= 2001,"po2001","stare"))%>%
  select(Marka,nowy,KM)%>%  group_by(Marka,nowy)%>%
  summarise(mediana = median(KM,na.rm=T))%>%arrange(desc(mediana))



# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji
mediana<-auta2012  %>% filter(Kolor=="szary-metallic",!Kraj.aktualnej.rejestracji=="")%>%
  summarise(across(Cena, median))
mediana<-pull(mediana,Cena)
srednia<-auta2012  %>% filter(Kolor=="szary-metallic",!Kraj.aktualnej.rejestracji=="")%>%
  summarise(across(Cena, mean))
srednia<-pull(srednia,Cena)
auta2012%>%filter(Cena>mediana,Cena<srednia,Kolor=="szary-metallic")%>%
  select(Kraj.aktualnej.rejestracji,Kraj.pochodzenia)%>%
  filter(!is.na(Kraj.aktualnej.rejestracji),!Kraj.aktualnej.rejestracji=="")%>%
  filter(as.character(Kraj.aktualnej.rejestracji)!=as.character(Kraj.pochodzenia))%>%
  nrow()
# Odp: 858



# 4. Jaki jest rozstêp miêdzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyn¹ jako rodzajem paliwa?

auta2012%>%filter(Model=="Passat",Wersja=="B6",Rodzaj.paliwa=="benzyna")%>%select(Przebieg.w.km)%>%summarise(kwartal=IQR(Przebieg.w.km,na.rm=T))

# Odp: 75977.5



# 5. Bior¹c pod uwagê samochody, których cena jest podana w koronach czeskich,
# podaj œredni¹ z ich ceny brutto.
# Uwaga: Jeœli cena jest podana netto, nale¿y dokonaæ konwersji na brutto (podatek 2%).

auta2012%>%filter(Waluta=="CZK")%>%mutate(podatek=case_when(Brutto.netto=="brutto"~Cena,Brutto.netto=="netto"~Cena*1.02))%>%select(podatek)%>%summarise(m=mean(podatek))
auta2012%>%filter(Waluta=="CZK")%>%select(Cena)
# Odp: 210678.3



# 6. Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹? Dodatkowo, podaj model, który najczêœciej
# pojawia siê w obu przypadkach.
auta2012%>%filter(Marka=="Chevrolet",Przebieg.w.km>50000)%>%select(Skrzynia.biegow,Model)%>%
  group_by(Skrzynia.biegow)%>%summarise(n=n())
auta2012%>%filter(Marka=="Chevrolet",Przebieg.w.km>50000,Skrzynia.biegow=="manualna")%>%select(Model)%>%
  group_by(Model)%>%summarise(n=n())%>%arrange(-n)
auta2012%>%filter(Marka=="Chevrolet",Przebieg.w.km>50000,Skrzynia.biegow=="automatyczna")%>%select(Model)%>%
  group_by(Model)%>%summarise(n=n())%>%arrange(-n)
# Odp: automatyczna=112 manualna=336 Manualnych wiêcej ni¿ automatycznych Lacetti w manualnych Corvette w automatycznych



# 7. Jak zmieni³a siê mediana pojemnoœci skokowej samochodów marki Mercedes-Benz,
# jeœli weŸmiemy pod uwagê te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012%>%filter(Marka=="Mercedes-Benz")%>%
  mutate(rocznik = if_else(Rok.produkcji<=2003,"stare","nowe"))%>%
  select(rocznik,Pojemnosc.skokowa)%>%group_by(rocznik)%>%
  summarise(med=median(Pojemnosc.skokowa,na.rm = T))
# Odp:Nie zmieni³a siê i wynosi 2200



# 8. Jaki jest najwiêkszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodz¹cych z Niemiec?

auta2012%>%filter(Kraj.aktualnej.rejestracji=="Polska",Kraj.pochodzenia=="Niemcy")%>%
  select(Przebieg.w.km)%>%arrange(desc(Przebieg.w.km))%>%slice_head(n=1)

# Odp:999999999 lub te¿ 1e+09 mo¿liwe ¿e jest to zwyczajnie b³¹d podczas wpisywania do systemu



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodz¹cych z W³och?
auta2012%>%filter(Marka=="Mitsubishi",Kraj.pochodzenia=="Wlochy")%>%
  group_by(Kolor)%>%summarise(n=n())%>%arrange(n)


# Odp: jest to granatowy metallic z 2 autami



# 10. Jaka jest wartoœæ kwantyla 0.25 oraz 0.75 pojemnoœci skokowej dla 
# samochodów marki Volkswagen w zale¿noœci od tego, czy w ich wyposa¿eniu 
# dodatkowym znajduj¹ siê elektryczne lusterka?

auta2012%>%filter(Marka=="Volkswagen")%>%
  mutate(lusterka = if_else(grepl("el. lusterka",Wyposazenie.dodatkowe),"Ma","nie ma"))%>%
  select(lusterka,Pojemnosc.skokowa)%>%group_by(lusterka)%>%
  summarise(kwantyl1 = quantile(Pojemnosc.skokowa, 0.25,na.rm=T),
            kwantyl3 = quantile(Pojemnosc.skokowa, 0.75,na.rm=T))

# Odp:Maj¹ce lusterka elektryczne : 0.25-1892 0.75-1968
#  Nie maj¹ce lusterek elektrycznych : 0.25-1400 0.75-1900


