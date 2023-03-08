library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Rodzaj.paliwa == 'naped elektryczny', Waluta == 'PLN') %>% 
  summarise(median(Cena, na.rm = TRUE))
  
# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  mutate(Czy.po.2001 = ifelse(Rok.produkcji < 2001, 'nie', 'tak')) %>% 
  group_by(Czy.po.2001, Marka) %>% 
  summarise(med = median(KM, na.rm = TRUE)) %>% 
  arrange(-med)

# Odp: Bugatti, w 2001 lub później



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == 'szary-metallic') %>%  
  summarise(med = median(Cena.w.PLN), mean = mean(Cena.w.PLN))

auta2012 %>% 
  filter(Kolor == 'szary-metallic', 27480 <= Cena.w.PLN, Cena.w.PLN <= 44341.41, !is.na(Kraj.aktualnej.rejestracji), as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia)) %>% 
  count()

# Odp: 635



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == 'Passat', Wersja == 'B6', Rodzaj.paliwa == 'benzyna') %>% 
  summarise(IQR(Przebieg.w.km, na.rm = TRUE))

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == 'CZK') %>% 
  mutate(Cena.brutto = ifelse(Brutto.netto == 'netto', Cena * 1.02, Cena)) %>% 
  summarise(mean(Cena.brutto))

# Odp: 210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>%
  filter(Marka == 'Chevrolet', Przebieg.w.km > 50000) %>% 
  count(Skrzynia.biegow)

auta2012 %>%
  filter(Marka == 'Chevrolet', Przebieg.w.km > 50000) %>% 
  count(Skrzynia.biegow, Model) %>% 
  arrange(-n) %>% 
  head(6)

# Odp: z manualną, dla manualnych Lacetti, dla automatycznych Corvette



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == 'Mercedes-Benz') %>% 
  mutate(Czy.po.2003 = ifelse(Rok.produkcji > 2003, 'tak', 'nie')) %>% 
  group_by(Czy.po.2003) %>% 
  summarise(med = median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: jest taka sama



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == 'Polska', Kraj.pochodzenia == 'Niemcy') %>% 
  arrange(-Przebieg.w.km) %>%
  select(Przebieg.w.km) %>% 
  head(1)

# Odp: 1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>%
  filter(Marka == 'Mitsubishi', Kraj.pochodzenia == 'Wlochy') %>% 
  count(Kolor) %>% 
  arrange(n) %>% 
  head(7)

# Odp: granatowy-metallic, jeśli uznamy że 4 kolory które mają najmniej razem zajmują ostatnie miejsce



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == 'Volkswagen', !is.na(Pojemnosc.skokowa)) %>% 
  mutate(Czy.lusterka = ifelse(grepl('el. lusterka', Wyposazenie.dodatkowe), 'tak', 'nie')) %>% 
  group_by(Czy.lusterka) %>% 
  summarise(Q1 = quantile(Pojemnosc.skokowa, 0.25), Q3 = quantile(Pojemnosc.skokowa, 0.75))

# Odp: gdy nie ma el. lusterka: Q1=1400, Q3=1900, gdy ma: Q1=1892, Q3=1968