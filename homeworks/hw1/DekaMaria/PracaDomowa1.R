install.packages("PogromcyDanych")
library(stats)
library(dplyr)
library(PogromcyDanych)
data(auta2012)
View(auta2012)
# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012%>%
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny")%>%
  summarise(mediana = median(Cena, na.rm = TRUE))

# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012%>%
  mutate(wiek = ifelse(Rok.produkcji < 2001, "starszy", "nowszy"))%>%
  group_by(Marka, wiek)%>%
  summarise(mediana = median(KM, na.rm = TRUE))%>%
  arrange(desc(mediana))%>%
  ungroup(Marka, wiek)%>%
  top_n(1, mediana)

# Odp:Kombinacja, dla której mediana liczby koni mechanicznychh jest największa, 
# to Bugatti wyprodukowane w 2001 roku i później.



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012%>%
  filter(Kolor == "szary-metallic", 
         min(mean(Cena.w.PLN), median(Cena.w.PLN)) <= Cena.w.PLN, Cena.w.PLN <= max(mean(Cena.w.PLN), median(Cena.w.PLN)))%>%
  filter(as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji))%>%
  summarise(n = n())
         
  

# Odp: 1930



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012%>%
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa =="benzyna")%>%
  select(Przebieg.w.km)%>%
  summarise(IQR(Przebieg.w.km, na.rm = TRUE))

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012%>%
  filter(Waluta == "CZK")%>%
  transmute(cena.brutto = case_when(Brutto.netto == "netto" ~ 1.02*Cena,
                                     TRUE ~ Cena))%>%
  summarise(mean(cena.brutto, na.rm = TRUE))
  

# Odp:210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012%>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, (Skrzynia.biegow == "automatyczna"|Skrzynia.biegow == "manualna"))%>%
  group_by(Model, Skrzynia.biegow)%>%
  summarise(n = n())%>%
  arrange(desc(n))%>%
  group_by(Skrzynia.biegow)%>%
  top_n(1, n)
# Odp: Więcej samochodów spełniających te warunki jest z manualną skrzynią biegów.
# w przypadku automatycznej skrzyni najczęściej pojawia się Chevrolet Corvette, 
# a w przypadku manualej Chevrolet Lacetti



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012%>%
  filter(Marka == "Mercedes-Benz")%>%
  mutate(wiek = ifelse(Rok.produkcji > 2003, "nowy", "stary"))%>%
  group_by(wiek)%>%
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: mediana pojemnosi skokowej samochodów z tych grup jest taka sama 



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?
auta2012%>%
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy")%>%
  select(Przebieg.w.km)%>%
  arrange(desc(Przebieg.w.km))%>%
  top_n(1, Przebieg.w.km)


# Odp:1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?
auta2012%>%
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy")%>%
  group_by(Kolor)%>%
  summarise(n = n())%>%
  arrange(desc(n))%>%
  slice(2)


# Odp:niebieski-metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012%>%
  filter(Marka == "Volkswagen")%>%
  mutate(zawiera.el.lusterka = ifelse(grepl("el. lusterka", Wyposazenie.dodatkowe), "tak", "nie"))%>%
  group_by(zawiera.el.lusterka)%>%
  summarise(quantile(Pojemnosc.skokowa, na.rm = TRUE, probs = c(0.25,0.75)))
  


# Odp: Wartosci kwantyli 0.25 i 0.75 dla aut z elektrycznymi lusterkami wynosza odpowiednio
# 1892 i 1968, natomiast bez lusterek 1400 i 1900



