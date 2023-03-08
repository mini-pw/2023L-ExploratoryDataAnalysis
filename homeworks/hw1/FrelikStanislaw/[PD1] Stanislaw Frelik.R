library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN") %>% 
  filter(Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(mean_price = median(Cena))

# Odp: 18900 PLN

# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  select(c(Marka, Rok.produkcji, KM)) %>% 
  filter(!is.na(KM)) %>% 
  mutate(new_year = case_when(Rok.produkcji >= 2001 ~ "Po 2001",
                              TRUE ~ "Przed 2001")) %>% 
  group_by(Marka, new_year) %>% 
  summarise(m = median(KM)) %>% 
  arrange(desc(m)) %>% 
  head(1)
  
# Odp: Bugatti, po 2001, 1001 KM

# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  mutate(Cena.w.PLN %in% median(Cena.w.PLN):mean(Cena.w.PLN)) %>% 
  filter(Kraj.aktualnej.rejestracji != "") %>% 
  filter(!is.na(Kraj.aktualnej.rejestracji)) %>% 
  filter(as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) %>% 
  count()

# Odp: 2875

# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna", !is.na(Przebieg.w.km)) %>% 
  summarise(iqr = IQR(Przebieg.w.km))

# Odp: 75977.5

# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(hajs = if_else(Brutto.netto == "netto", Cena * 1.02, Cena)) %>% 
  summarise(m = mean(hajs))

# Odp: 210678.3 CZK

# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow != "") %>% 
  group_by(Model, Skrzynia.biegow) %>% 
  count() %>% 
  arrange(desc(n))

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow != "") %>% 
  group_by(Skrzynia.biegow) %>% 
  count()

# Odp: tych z manualną (336>112), z manualną najwięcej jest Lacetti (94)
# a z automatem Corvett (20).

# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", !is.na(Pojemnosc.skokowa)) %>% 
  mutate(kiedy = if_else(Rok.produkcji >= 2003, 1, 0)) %>% 
  select(kiedy, Pojemnosc.skokowa) %>% 
  group_by(kiedy) %>% 
  summarise(m = median(Pojemnosc.skokowa))

# Odp: nie zmieniła się 

# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  head(1)

# Odp: 1e+09, sporo...

# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>% 
  count(Kolor) %>% 
  arrange(n) 

# Odp: granatowy-metallic (2 sztuki)

# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen", !is.na(Pojemnosc.skokowa)) %>% 
  mutate(lusterko = if_else(grepl("el. lusterka", Wyposazenie.dodatkowe), 1, 0)) %>% 
  select(Pojemnosc.skokowa) %>% 
  quantile(na.rm = TRUE)

# Odp: Q1 = 1896, Q3 = 1968
