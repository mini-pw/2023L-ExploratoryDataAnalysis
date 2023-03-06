library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Waluta == 'PLN', Rodzaj.paliwa == 'naped elektryczny') %>%
  summarise(median(Cena))


# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  mutate(rok.2001 = as.logical(Rok.produkcji >= 2001)) %>% 
  group_by(Marka, rok.2001) %>% 
  summarise(med = median(KM, na.rm = TRUE)) %>% 
  arrange(-med) %>% 
  View()

# Odp: Bugatti w 2001 lub później



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.

auta2012 %>% 
  filter(Kolor == 'szary-metallic') %>% 
  filter(median(Cena.w.PLN) < Cena.w.PLN, Cena.w.PLN < mean(Cena.w.PLN)) %>% 
  filter(as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia)) %>% 
  summarise(n = n())

# Odp: 1331



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == 'Passat', Wersja == 'B6', Rodzaj.paliwa == 'benzyna') %>% 
  summarise(iqr = IQR(Przebieg.w.km, na.rm = TRUE))

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == 'CZK') %>% 
  mutate(Cena.z.podatkiem = ifelse(Brutto.netto == 'brutto', Cena, Cena * 1.02)) %>% 
  summarise(mean = mean(Cena.z.podatkiem))

# Odp: 210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == 'Chevrolet', Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n())

auta2012 %>% 
  filter(Marka == 'Chevrolet', Przebieg.w.km > 50000, Skrzynia.biegow == 'automatyczna') %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

auta2012 %>% 
  filter(Marka == 'Chevrolet', Przebieg.w.km > 50000, Skrzynia.biegow == 'manualna') %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp: więcej jest ze skrzynią manualna, manualnych najwięcej jest Lacetti, automatów najwięcej jest Corvette



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == 'Mercedes-Benz') %>% 
  mutate(rok.2003 = as.logical(Rok.produkcji <= 2003)) %>% 
  group_by(rok.2003) %>% 
  summarise(median = median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: Nie zmieniła się



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == 'Polska', Kraj.pochodzenia == 'Niemcy') %>% 
  arrange(-Przebieg.w.km) %>% 
  head(1) %>% 
  select(Przebieg.w.km)

# Odp: 1e+09 km



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Kraj.pochodzenia == 'Wlochy', Marka == 'Mitsubishi') %>% 
  group_by(Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  head(2) %>% 
  select(Kolor)

# Odp: grafitowy-metallic


# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

library(stringr)

auta2012 %>% 
  filter(Marka == 'Volkswagen') %>% 
  mutate(ma.lusterka = str_detect(Wyposazenie.dodatkowe, 'el. lusterka')) %>% 
  group_by(ma.lusterka) %>% 
  summarise(kwantyle = quantile(Pojemnosc.skokowa, probs = c(0.25, 0.75), na.rm = TRUE)) %>% 
  View()

# Odp: bez elektrycznych lusterek kwantyle wynoszą odpowiednio 1400 i 1900, z elektrycznymi lusterkami wynoszą 1892.25 i 1968


