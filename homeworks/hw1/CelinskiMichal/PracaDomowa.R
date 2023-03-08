install.packages("PogromcyDanych")
library(PogromcyDanych)
data(auta2012)
auta2012
library(dplyr)
library(stats)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>%
  filter(Waluta == "PLN") %>% 
  filter(Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(median(Cena, na.rm = T))

# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

nowsze <- filter(auta2012, Rok.produkcji > 2000) 
nowsze %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM, na.rm = T)) %>% 
  arrange(-mediana) %>% 
  head(1)


starsze <- filter(auta2012, Rok.produkcji <2001)
starsze %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM, na.rm = T)) %>% 
  arrange(-mediana) %>% 
  head(1)

# Odp: Bugatti, 1001 KM, auta z XXI wieku. 



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.


auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  filter(Cena.w.PLN < mean(Cena.w.PLN), Cena.w.PLN > median(Cena.w.PLN)) %>% 
  filter(as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia)) %>% 
  nrow()
  

# Odp: 1331



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>%
  summarise(IQR(Przebieg.w.km, na.rm = T))



# Odp: 75977.5

# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(Cena = ifelse(Brutto.netto == "netto", Cena * 1.02, Cena)) %>% 
  summarise(mean(Cena))

# Odp: 2105678.30 CZK



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  count(Skrzynia.biegow) %>% 
  arrange(-n) %>% 
  head(1)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow, Model) %>% 
  filter(Skrzynia.biegow == "manualna") %>%
  count(Model) %>% 
  arrange(-n) %>% 
  head(1)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow, Model) %>% 
  filter(Skrzynia.biegow == "automatyczna") %>%
  count(Model) %>% 
  arrange(-n) %>% 
  head(1)

# Odp: Więcej razy pojawia się manualna (336 przypadków); w manualnych: Lacetti (94 wystąpień), w automatycznych Corvette (20 razy).



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji <= 2003) %>% 
  summarise(median(Pojemnosc.skokowa, na.rm = T))

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji > 2003) %>% 
  summarise(median(Pojemnosc.skokowa, na.rm = T))

# Odp: w starszych: 2200, w nowszych też 2200. Nie zmieniła się. 



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Niemcy", Kraj.aktualnej.rejestracji == "Polska") %>% 
  arrange(-Przebieg.w.km) %>% 
  select(Przebieg.w.km) %>% 
  head(1)

# Odp: 1e+09 - jakiś miliard kilometrów (chyba to nie możliwe, musiałby przejeżdżać prawie 90k km dziennie)


# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

zad9 <- auta2012
zad9 <- filter(zad9, Marka == "Mitsubishi")
zad9 <- filter(zad9, Kraj.pochodzenia == "Wlochy")
zad9 <- group_by(zad9, Model)
zad9 <- count(zad9, Model)
zad9 <- arrange(zad9, -n)
odpowiedz <- zad9[2,1]

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  count(Kolor) %>% 
  arrange(n) %>% 
  head(10)


# Odp: w sumie to aż 4 kolory występują 1 raz (czerwony-metallic, grafitowy-metallic, srebrny i zielony). Nie da się jednoznacznie odpowiedzieć na to pytanie.  



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(Wyposazenie.dodatkowe = ifelse(grepl("el. lusterka", Wyposazenie.dodatkowe, fixed = TRUE), "ma el.lust", "brak el.lust")) %>% 
  group_by(Wyposazenie.dodatkowe) %>% 
  reframe(quantile(Pojemnosc.skokowa, na.rm = T, probs = c(0.25, 0.75)))

# Odp: auta z elektrycznymi lusterkami -1Q: 1892, 3Q: 1968; auta bez elektrycznych lusterek - 1Q: 1400, 3Q: 1900