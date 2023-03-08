install.packages("dplyr")
library(dplyr)
install.packages("PogromcyDanych")
library(PogromcyDanych)
data(auta2012)
View(auta2012)
# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(mediana = median(Cena, na.rm = TRUE))

# Odp. 18900


# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  filter(Rok.produkcji >= 2001) %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM, na.rm = TRUE)) %>% 
  top_n(1, mediana)

auta2012 %>% 
  filter(Rok.produkcji < 2001) %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM, na.rm = TRUE)) %>% 
  top_n(1, mediana)

# Odp. Bugatti rok 2001 lub późniejszy.


# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic", Cena.w.PLN > median(Cena.w.PLN, na.rm = TRUE),
         Cena.w.PLN < mean(Cena.w.PLN, na.rm = TRUE)) %>% 
  filter(is.na(Kraj.aktualnej.rejestracji) == FALSE,
         Kraj.aktualnej.rejestracji != "",
         as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) %>%   
  summarise(n=n())

#Odp. 891


# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Marka == "Volkswagen", Model == "Passat", Wersja == "B6") %>% 
  select(Przebieg.w.km) %>% 
  summary()
sum(179000, -109361)

#Odp. 696339


# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(Brutto = ifelse(Brutto.netto == "brutto", Cena, 1.02*Cena)) %>% 
  summarise(mean_price = mean(Brutto, na.rm = TRUE))

#Odp. 210678.3


# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n=n())

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, 
         Skrzynia.biegow == "manualna") %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  top_n(1, n)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, 
         Skrzynia.biegow == "automatyczna") %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  top_n(1, n)

# Odp. Więcej jest tych z manualną skrzynią biegów, w manualnej 
# najczęściej pojawia się model Lacetti, a w automatycznej - Corvette.


# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji <= 2003) %>% 
  summarise(mediana1 = median(Pojemnosc.skokowa, na.rm = TRUE))

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji > 2003) %>% 
  summarise(mediana2 = median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp. Nie zmieniła się.


# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?
 
auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>% 
  summarise(max = max(Przebieg.w.km, na.rm = TRUE))

# Odp. 10^9.

# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi") %>% 
  select(Kolor) %>% 
  group_by(Kolor) %>%
  summarise(n=n()) %>% 
  arrange(n) %>% 
  head(2)

 # Odp. fioletowy


# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen", 
         grepl("el. lusterka", Wyposazenie.dodatkowe) == TRUE) %>% 
  select(Pojemnosc.skokowa) %>% 
  summary()

# Odp. Kwantyl 0.25 - 1892, kwantyl 0.75 - 1968. 











  
  
    
