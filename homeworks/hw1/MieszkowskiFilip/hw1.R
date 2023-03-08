library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Rodzaj.paliwa == "naped elektryczny", Waluta == "PLN") %>% 
  summarize(mediana.ceny = median(Cena,na.rm = TRUE))

# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  group_by(Marka, czas.produkcji = ifelse(Rok.produkcji >= 2001, "nowe", "stare")) %>% 
  summarize(Mediana.Koni.Mechanicznych = median(KM,na.rm = TRUE)) %>% 
  arrange(desc((Mediana.Koni.Mechanicznych))) %>% 
  head(1) %>% 
  select(Marka, czas.produkcji)

# Odp: Bugatti, nowe


# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic"& 
           ((Cena < median(Cena) & Cena > mean(Cena)) | 
           (Cena > median(Cena) & Cena < mean(Cena)))) %>%
  filter(as.character( Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) %>% 
  nrow
  


# Odp: 2114



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>% 
  select(Przebieg.w.km) %>% 
  pull %>% 
  IQR(na.rm = TRUE)

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  transmute(cena.brutto = ifelse(Brutto.netto == "brutto",Cena, Cena * 1.02)) %>%
  pull %>% 
  mean(na.rm = TRUE)


# Odp: 210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  head(1)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow, Model) %>% 
  count %>% 
  filter(Skrzynia.biegow == "manualna") %>% 
  arrange(desc(n)) %>% 
  head(1) %>%
  select(Model)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow, Model) %>% 
  count %>% 
  filter(Skrzynia.biegow == "automatyczna") %>% 
  arrange(desc(n)) %>% 
  head(1) %>%
  select(Model)

# Odp: Wiecej jest ze skrzynia manualna, najpopularniejsze modele: 
# Lacetti (ze skrzynia manualna) oraz Corvette (ze skrzynia automatyczna).



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  select(Pojemnosc.skokowa, Rok.produkcji) %>% 
  group_by(nowe = ifelse(Rok.produkcji > 2003, "nowe", "stare")) %>% 
  summarize(mediana = median(Pojemnosc.skokowa,na.rm = TRUE))

# Odp: Nie zmieniła sie (w obu przypadkach wynosi 2200)



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Niemcy", Kraj.aktualnej.rejestracji == "Polska") %>% 
  select(Przebieg.w.km) %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  head(1) %>% View()

# Odp: 1e09km 
# (to musi być jakiś błąd, nie ma opcji że jakies auto przejechało miliard kilometrów)



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(2)
  
# Odp: niebieski-metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen", grepl("el. lusterka", Wyposazenie.dodatkowe)) %>% 
  select(Pojemnosc.skokowa) %>% 
  pull %>% 
  quantile(na.rm = TRUE)

auta2012 %>% 
  filter(Marka == "Volkswagen", !grepl("el. lusterka", Wyposazenie.dodatkowe)) %>% 
  select(Pojemnosc.skokowa) %>% 
  pull %>% 
  quantile(na.rm = TRUE)


# Odp: W przypadku posiadajacych lusterka elektryczne Q1 = 1892.25, Q3 = 1968
# W przypadku nieposiadajacych lusterek elektrycznych Q1 = 1400, Q3 = 1900