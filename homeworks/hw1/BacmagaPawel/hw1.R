library(PogromcyDanych)
library(stringr)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

# summary(auta2012$Rodzaj.paliwa)
auta2012 %>% 
  filter(Waluta == "PLN") %>% 
  filter(Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(mediana.ceny = median(Cena, na.rm = TRUE))

# Odp: Mediana = 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  mutate(Czy.przed.2001 = ifelse(Rok.produkcji < 2001, "Tak", "Nie")) %>% 
  group_by(Marka, Czy.przed.2001) %>% 
  summarise(mediana.KM = median(KM)) %>% 
  arrange(desc(mediana.KM))

# Otrzymujemy różne wyniki omijając wartości nieznane

auta2012 %>% 
  mutate(Czy.przed.2001 = ifelse(Rok.produkcji < 2001, "Tak", "Nie")) %>% 
  group_by(Marka, Czy.przed.2001) %>% 
  summarise(mediana.KM = median(KM, na.rm = T)) %>% 
  arrange(desc(mediana.KM))

# Odp: Bugatti przed 2001, mediana.KM = 560 razem z NA,
# lub Bugatti po 2001, mediana.KM = 1001, omijając NA



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  summarise(mean_Cena.w.PLN = mean(Cena.w.PLN), median_Cena.w.PLN = median(Cena.w.PLN)) -> tmp

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>%
  filter(between(Cena.w.PLN, tmp$median_Cena.w.PLN, tmp$mean_Cena.w.PLN)) %>% 
  mutate(Check.origin = ifelse(as.character(Kraj.aktualnej.rejestracji) == as.character(Kraj.pochodzenia),
                               'Tak', 'Nie')) %>% 
  filter(Check.origin == 'Nie') %>% 
  summarise(n = n())

# Odp: Jest ich 1331 (between stosuje nierówności nieostre)



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>%
  summarise(IQR = IQR(Przebieg.w.km, na.rm = T))

# Odp: IQR = 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(Cena.w.PLN.brutto = ifelse(as.character(Brutto.netto) == "brutto", 
                                       Cena.w.PLN,
                                       1.02*Cena.w.PLN)) %>% 
  summarise(mean.brutto = mean(Cena.w.PLN.brutto))

# Odp: mean.brutto = 36047.06



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Model, Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Odp: Więcej jest tych ze skrzynią manualną,
# Najczęstszy model: manualna - Lacetti 94 razy
#                    automatyczna - Corvette 20 razy



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  mutate(Czy.po.2003 = ifelse(Rok.produkcji > 2003, "Tak", "Nie")) %>% 
  group_by(Czy.po.2003) %>% 
  summarise(median = median(Pojemnosc.skokowa, na.rm=T))

# Odp: Mediana nie zmieniła się, w obu przypadkach wynosi 2200



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", 
         Kraj.pochodzenia == "Niemcy") %>%
  select(Przebieg.w.km) %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  head()
  
# Odp: 999999999, a jeżeli uznać to za błąd danych, to 2950000



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(n)
  
# Odp: granatowy-metallic, wszystkie przed nim mają po 1 aucie, więc są 
# na równi najmniej popularne



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(Czy.ma.elektryczne.lusterka = ifelse(str_detect(Wyposazenie.dodatkowe, "el. lusterka"),
                                       "Tak", "Nie")) %>% 
  group_by(Czy.ma.elektryczne.lusterka) %>% 
  summarise(quant25 = quantile(Pojemnosc.skokowa, probs = 0.25, na.rm = T),
            quant75 = quantile(Pojemnosc.skokowa, probs = 0.75, na.rm = T))
  
# Odp: Volkswageny bez el. lusterek : (kwantyl 0.25 = 1400, kwantyl 0.75 = 1900)
# Volkswageny z el. lusterkami : (kwantyl 0.25 = 1892, kwantyl 0.75 = 1968)


