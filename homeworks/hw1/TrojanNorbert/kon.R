library(PogromcyDanych)
data(auta2012)
library(dplyr)
View(auta2012)
# 1. Rozwa¿aj¹c tylko obserwacje z PLN jako walut¹ (nie zwa¿aj¹c na 
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?

unique(auta2012$Rodzaj.paliwa)

mediana <- auta2012 %>% filter(Rodzaj.paliwa=='naped elektryczny') %>% summarise(mediana = median(Cena.w.PLN))
mediana

# Odp: 19600



# 2. W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001 
# roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
# mechanicznych (KM) jest najwiêksza.

najwieksza_mediana <- auta2012 %>%mutate(nowa=ifelse(Rok.produkcji>=2001,'tak','nie')) %>% 
  group_by(Marka,nowa) %>%   summarise(mediana = median(KM,na.rm=TRUE)) %>% arrange(-mediana) %>% head(1)
najwieksza_mediana

# Odp: Bugatti po 2001



# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.

liczba <- auta2012 %>% filter(Kolor=='srebrny-metallic')
mediana<-summarise(liczba,mediana = median(Cena.w.PLN,na.rm=TRUE))[[1]]
srednia<-summarise(liczba,mediana = mean(Cena.w.PLN,na.rm=TRUE))[[1]]
liczba<- liczba %>% filter((Cena.w.PLN >= mediana) & (Cena.w.PLN <= srednia))
pochodzenie<-select(liczba, Kraj.pochodzenia)[[1]]
aktualna <- select(liczba, Kraj.aktualnej.rejestracji)[[1]]
liczba<-0
for(i in 1:2874){
  if(aktualna[i]!=pochodzenie[i]){
    liczba<-liczba +1
  }
}
liczba
# Odp: 1624



# 4. Jaki jest rozstêp miêdzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyn¹ jako rodzajem paliwa?

odstep<- auta2012 %>% filter(Model=='Passat',Wersja=='B6') %>% summarise(odstep=IQR(Przebieg.w.km,na.rm = TRUE))
odstep

# Odp:69639



# 5. Bior¹c pod uwagê samochody, których cena jest podana w koronach czeskich,
# podaj œredni¹ z ich ceny brutto.
# Uwaga: Jeœli cena jest podana netto, nale¿y dokonaæ konwersji na brutto (podatek 2%).

srednia_ceny<- auta2012 %>% filter(Waluta=='CZK') %>% mutate(nowa_cena=ifelse(Brutto.netto=='brutto',Cena,1.02*Cena)) %>% 
  summarise(srednia=mean(nowa_cena))
srednia_ceny

# Odp: 210678.3 (w koronach czeskich)



# 6. Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹? Dodatkowo, podaj model, który najczêœciej
# pojawia siê w obu przypadkach.

manualne_chevrolety <- auta2012 %>% filter(Marka=='Chevrolet') %>% filter(Skrzynia.biegow=='manualna')%>% summarise(n=n())
automatyczne_chevrolety <- auta2012 %>% filter(Marka=='Chevrolet') %>% filter(Skrzynia.biegow=='automatyczna')%>% summarise(n=n())
# Odp: Wiêcej jest z manualn¹



# 7. Jak zmieni³a siê mediana pojemnoœci skokowej samochodów marki Mercedes-Benz,
# jeœli weŸmiemy pod uwagê te, które wyprodukowano przed lub w roku 2003 i po nim?

przed<- auta2012 %>% filter(Marka=="Mercedes-Benz") %>% filter(Rok.produkcji<=2003) %>% summarise(mediana=median(Pojemnosc.skokowa,na.rm=TRUE))
po<- auta2012 %>% filter(Marka=="Mercedes-Benz") %>% filter(Rok.produkcji>2003) %>% summarise(mediana=median(Pojemnosc.skokowa,na.rm=TRUE))
ruznica<-przed[[1]]-po[[1]]
ruznica

# Odp: nie zmiani³a siê



# 8. Jaki jest najwiêkszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodz¹cych z Niemiec?

najwiekszy <- auta2012 %>% filter(Kraj.aktualnej.rejestracji=='Polska') %>% filter(Kraj.pochodzenia=="Niemcy") %>% 
  arrange(-Przebieg.w.km)%>% head(1) %>% select(Przebieg.w.km)

# Odp:1000000000



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodz¹cych z W³och?

drugi_najmniej<-auta2012 %>% filter(Marka=='Mitsubishi') %>% filter(Kraj.pochodzenia=='Wlochy') %>% 
  group_by(Kolor) %>% summarise(n=n()) %>% arrange(n)

# Odp: srebrny, zielony, czerwony-metalic i grafitowy-metlic s¹ egzekwo namniej popularne, kolejny w kolejce jest granatowy-metalic



# 10. Jaka jest wartoœæ kwantyla 0.25 oraz 0.75 pojemnoœci skokowej dla 
# samochodów marki Volkswagen w zale¿noœci od tego, czy w ich wyposa¿eniu 
# dodatkowym znajduj¹ siê elektryczne lusterka?


oba <- auta2012 %>%filter(Marka == "Volkswagen") %>% mutate(czy_ma=ifelse(str_detect(Wyposazenie.dodatkowe,"el. lusterka"),'tak','nie'))
z_lusterkami<-oba %>% filter(czy_ma=='tak')
bez_lusterek<-oba %>% filter(czy_ma=='nie')
quantile(select(z_lusterkami,Pojemnosc.skokowa),na.rm = TRUE)
quantile(select(bez_lusterek,Pojemnosc.skokowa),na.rm = TRUE)

# Odp: 0.25 dla bez el. lusterek to 1400, a z to 1892,25, natomaist 0.75 bez el. lusterek to 1900, a z to 1968
