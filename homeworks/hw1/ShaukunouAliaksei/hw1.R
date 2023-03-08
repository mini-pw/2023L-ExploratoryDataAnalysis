library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

#   Jak zrozumiałem z postaci ramki, informacja o napędzie elektrycznym
# znajduje się w kolumnie Rodzaj.paliwa:
levels(auta2012$Rodzaj.paliwa)
#   Policzę medianę dla obserwacji, które mają Rodzaj.paliwa = "naped elektryczny" i
# i Waluta = PLN:

auta2012 %>% filter(Rodzaj.paliwa == 'naped elektryczny' & Waluta == 'PLN') %>% 
  pull(Cena) %>% median(na.rm = TRUE)


# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.
auta2012 %>% mutate(before.2001 = (Rok.produkcji < 2001)) %>% 
  group_by(Marka, before.2001) %>% 
  summarise(mediana.KM = median(KM, na.rm = TRUE)) %>% 
  arrange(-mediana.KM) %>% head(1)


# Odp: Bugatti, po lub w 2001



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

# Zauważmy, że w Kraj.aktualnej.rejestracji nie ma NA:
all(!is.na(auta2012$Kraj.aktualnej.rejestracji))

df <- auta2012 %>%  filter(Kolor == 'szary-metallic') %>% 
  select(Cena.w.PLN, Kraj.aktualnej.rejestracji, Kraj.pochodzenia)
sr <- mean(df$Cena.w.PLN)
med <- median(df$Cena.w.PLN)
med <= sr # średnia jest większa

df %>% filter(Cena.w.PLN <= sr, Cena.w.PLN >= med) %>% 
  filter(Kraj.aktualnej.rejestracji != '' & as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia)) %>%
  nrow()
# Na koniec zauważmy, że w uwadze nie jest napisane, że trzeba nie uwzględniać
# obserwacji o pustym Kraj.pochodzenia
# Odp: 635



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?
auta2012 %>% filter(Model == 'Passat'& Wersja == 'B6' & Rodzaj.paliwa == 'benzyna') %>% 
  pull(Przebieg.w.km) %>% IQR(na.rm = TRUE)


# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% filter(Waluta == 'CZK') %>% 
  mutate(cena.brutto = ifelse(Brutto.netto == 'brutto', Cena, 1.02*Cena)) %>% 
  pull(cena.brutto) %>% mean()

# Odp: 210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.
auta2012 %>% filter(Marka == 'Chevrolet') %>% count(Skrzynia.biegow)
auta2012 %>% filter(Marka == 'Chevrolet') %>% 
  count(Model, Skrzynia.biegow) %>%
  group_by(Skrzynia.biegow) %>% 
  summarise(modelmax = Model[which.max(n)])
# wiem, że funkcja which.max zwraca pierwsze wystąpienie maxa, a więc
# rozwiązanie nie byłoby do końca poprawnym, gdyby takich modelów było kilka
# alternatywnie można było po count(Model, Skrzynia.biegow) wyciągnąć
# wszystkie obserwacje, gdzie Skrzynia.biegow = manualna, posortować
# i zwrócić górę, potem tak samo dla automatyczna.

# Odp: z manualną; z automatyczną - Camaro, z manualną - Aveo.



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% filter(Marka == 'Mercedes-Benz') %>%
  mutate(after.2003 = Rok.produkcji > 2003) %>% 
  select(Pojemnosc.skokowa, after.2003) %>%
  group_by(after.2003) %>% 
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: nie zmieniłą się



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?
auta2012 %>% 
  filter(Kraj.pochodzenia == 'Niemcy' & Kraj.aktualnej.rejestracji=='Polska') %>% 
  pull(Przebieg.w.km) %>% max(na.rm = TRUE)

# Odp: 1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?
auta2012 %>% 
  filter(Marka == 'Mitsubishi' & Kraj.pochodzenia == 'Wlochy') %>% 
  count(Kolor) %>% arrange(n)
  


# Odp: granatowy-metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?
library(stringr)
auta2012 %>% 
  filter(Marka == 'Volkswagen') %>% 
  mutate(ma.lusterka = str_detect(Wyposazenie.dodatkowe, 'el. lusterka')) %>% 
  select(Pojemnosc.skokowa, ma.lusterka) %>% 
  group_by(ma.lusterka) %>% 
  summarise(Q1 = quantile(Pojemnosc.skokowa, na.rm = TRUE, probs = 1/4),
            Q3 = quantile(Pojemnosc.skokowa, na.rm = TRUE, probs = 3/4))


# Odp:1400 i 1900, jeżeli nie ma; 1892 i 1968, jeśli są.


