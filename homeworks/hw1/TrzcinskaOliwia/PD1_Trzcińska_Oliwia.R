library("PogromcyDanych")
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN" & Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(Mediana = median(Cena, na.rm = TRUE))

# Odp: 18900 



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  mutate(Wiek = ifelse(auta2012$Rok.produkcji >= 2001, "nowszy", "starszy")) %>% 
  group_by(Marka, Wiek) %>% 
  summarise(Mediana = median(KM, na.rm = TRUE)) %>% 
  arrange(-Mediana) %>% 
  select(Marka, Wiek) %>% 
  head(1)

# Odp: Bugatti, nowszy



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012$Kraj.aktualnej.rejestracji <- factor(auta2012$Kraj.aktualnej.rejestracji, 
                                              levels = levels(auta2012$Kraj.pochodzenia))
auta2012 %>% 
  filter(!is.na(Kraj.aktualnej.rejestracji)) %>%
  filter(Kolor == "szary-metallic") %>% 
  mutate(Średnia = mean(Cena.w.PLN), Mediana = median(Cena.w.PLN)) %>% 
  filter((Średnia <= Cena.w.PLN & Cena.w.PLN <= Mediana) |
         (Mediana <= Cena.w.PLN & Cena.w.PLN <= Średnia)) %>% 
  select(Kraj.pochodzenia, Kraj.aktualnej.rejestracji) %>% 
  filter(Kraj.aktualnej.rejestracji != Kraj.pochodzenia) %>% 
  summarise(n = n())

# Odp: 1331



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>% 
  summarise(IQR = IQR(Przebieg.w.km, na.rm = TRUE))

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(Cena.brutto = case_when(Brutto.netto == "netto" ~ Cena * 1.02,
                                 Brutto.netto == "brutto" ~ Cena)) %>% 
  summarise(Średnia = mean(Cena.brutto))

# Odp: 210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n())

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow, Model) %>% 
  summarise(n = n()) %>% 
  group_by(Skrzynia.biegow) %>% 
  arrange(-n) %>% 
  top_n(1, n)

# Odp: więcej jest ze skrzynią manualną; 
#      najczęściej występujący model ze skrzynią manualną - Lacetti
#      najczęściej występujący model ze skrzynią automatyczną - Corvette



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  select(Rok.produkcji, Pojemnosc.skokowa) %>% 
  mutate(Wiek = ifelse(Rok.produkcji >= 2003, "nowszy", "starszy")) %>% 
  group_by(Wiek) %>% 
  summarise(Mediana = median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: jest taka sama, równa 2200



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Niemcy", Kraj.aktualnej.rejestracji == "Polska") %>% 
  select(Przebieg.w.km) %>%
  arrange(-Przebieg.w.km) %>% 
  head(1)

# Odp: 1e+09 km



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Wlochy" & Marka == "Mitsubishi") %>% 
  group_by(Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(n) 

# Odp: granatowy-metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?
auta2012$Wyposazenie.dodatkowe <- str_split(auta2012$Wyposazenie.dodatkowe, 
                                            pattern = ",") 
auta2012 %>% 
  select(Marka, Pojemnosc.skokowa, Wyposazenie.dodatkowe) %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(el_lusterka = ifelse("el. lusterka" %in% auta2012$Wyposazenie.dodatkowe, 
                              "tak", "nie")) %>% 
  group_by(el_lusterka) %>% 
  summarise(q = quantile(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: z el. lusterkami: Q1 = 1600, Q3 = 1968
#      bez el. lusterek: Q1 = , Q3 = 