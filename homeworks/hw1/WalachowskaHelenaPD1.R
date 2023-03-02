install.packages("PogromcyDanych")
library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?



# Odp:

auta2012 %>%
  filter(Waluta == "PLN" & Rodzaj.paliwa == "naped elektryczny") %>%
  summarise(mediana = median(Cena))
  


# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.



# Odp:

auta2012 %>%
  mutate(Rok.2001 = ifelse(Rok.produkcji >= 2001, 1, 0)) %>% # tworzymy nową kolumnę
  group_by(Marka, Rok.2001) %>%                              # 1 oznacza rok produkcji >= 2001
  summarise(mediana = median(KM, na.rm = TRUE)) %>%          # 0 oznacza rok produkcji < 2001
  arrange(desc(mediana)) %>%
  head(1)

# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji



# Odp:

auta2012 %>%
  filter(Kolor == "szary-metallic") %>%
  filter(Cena.w.PLN > median(Cena.w.PLN) & Cena.w.PLN < mean(Cena.w.PLN))  %>% # średnia w tym przypadku jest większa od mediany
  filter(as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia)) %>%
  filter(Kraj.aktualnej.rejestracji != "") %>%
  count()

# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?



# Odp:

auta2012 %>%
  filter(Model == "Passat" & Wersja == "B6" & Rodzaj.paliwa == "benzyna") %>%
  summarise(iqr = IQR(Przebieg.w.km, na.rm = TRUE))

# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).



# Odp:

auta2012 %>%
  filter(Waluta == "CZK") %>%
  mutate(Brutto = ifelse(Brutto.netto == "netto", 1.02*Cena, Cena)) %>%
  summarise(Brutto.mean = mean(Brutto))

# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.



# Odp:

auta2012 %>%
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000) %>%
  filter(Skrzynia.biegow != "") %>%
  group_by(Skrzynia.biegow) %>%
  summarise(n = n()) #więcej ze skrzynią manualną 336

auta2012 %>%
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000) %>%
  filter(Skrzynia.biegow != "") %>%
  group_by(Skrzynia.biegow, Model) %>%
  summarise(n = n()) %>%
  top_n(1, n) # automatyczna - Corvette, manualna - Lacetti

# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?



# Odp:

auta2012 %>%
  mutate(Rok.2003 = ifelse(Rok.produkcji <= 2003, 1, 0)) %>% # nowa kolumna, 1 dla <= 2003, 0 dla > 2003
  filter(Marka == "Mercedes-Benz") %>%
  group_by(Rok.2003) %>%
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE)) 
# mediana jest taka sama w obu przypadkach

# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?



# Odp:

auta2012 %>%
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>%
  arrange(-Przebieg.w.km) %>%
  select(Przebieg.w.km) %>%
  head(1)

# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?



# Odp:

auta2012 %>%
  filter(Marka == "Mitsubishi" & Kraj.pochodzenia == "Wlochy") %>%
  group_by(Kolor) %>%
  filter(Kolor != "") %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  head(5) #granatowy-metallic

# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?



# Odp:

auta2012 %>%
  filter(Marka == "Volkswagen") %>% # użycie funkcji grepl() sprawdzającej obecność podanej frazy w napisie
  mutate(Lusterka = ifelse(grepl("el. lusterka", Wyposazenie.dodatkowe), 1, 0)) %>%
  group_by(Lusterka) %>%
  summarise(Q1 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE),
            Q3 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))
