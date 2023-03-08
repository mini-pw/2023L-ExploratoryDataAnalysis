install.packages("PogromcyDanych")
library(PogromcyDanych)

View(auta2012)

unique(auta2012["Rodzaj.paliwa"])

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Rodzaj.paliwa == "naped elektryczny", Waluta == "PLN") %>% 
  summarise(mediana = median(Cena.w.PLN, na.rm=TRUE))

# Odp:18900

# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  filter(Rok.produkcji >= 2001) %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM, na.rm=TRUE)) %>% 
  arrange(-mediana) %>% 
  head(1)

auta2012 %>% 
  filter(Rok.produkcji < 2001) %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM, na.rm=TRUE)) %>% 
  arrange(-mediana) %>% 
  head(1)

# Odp: Bugatti, po 2001 roku.

# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i podaj ich liczbę.
library(stringr)

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  summarise(srednia = mean(Cena.w.PLN, na.rm=TRUE),
            mediana = median(Cena.w.PLN, na.rm=TRUE))
auta2012 %>% 
  filter(Kolor == "szary-metallic", 
         Cena.w.PLN > 27480, 
         Cena.w.PLN < 44341.41,
         Kraj.aktualnej.rejestracji != "",
         Kraj.pochodzenia != "") %>% 
  select(Kraj.aktualnej.rejestracji, Kraj.pochodzenia) %>% 
  mutate(czy_ten_sam_kraj = str_equal(Kraj.aktualnej.rejestracji,Kraj.pochodzenia)) %>% 
  filter(czy_ten_sam_kraj == FALSE) %>%
  nrow()

# Odp:356

# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>% 
  summarise(rozstep = IQR(Przebieg.w.km, na.rm=TRUE))


# Odp:75977.5

# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>%
  mutate(ceny_brutto = ifelse(Brutto.netto == "brutto", Cena.w.PLN, Cena.w.PLN*1.02)) %>%
  summarise(srednia_brutto = mean(ceny_brutto, na.rm=TRUE))


# Odp:36047.06

# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow == "manualna") %>% 
  group_by(Model) %>% 
  count() %>%
  arrange(-n) %>% 
  head(1)
  #nrow()
auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow == "automatyczna") %>%
  group_by(Model) %>% 
  count() %>%
  arrange(-n) %>% 
  head(1)
  #nrow()


# Odp: Z manualną (336 vs 112). Wśród tych ze skrzynią manualną najczęściej pojawia się Lacetti, a tych z automatyczną - Corvette.

# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji <= 2003) %>%
  summarise(mediana_przed = median(Pojemnosc.skokowa, na.rm=TRUE))

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji > 2003) %>%
  summarise(mediana_po = median(Pojemnosc.skokowa, na.rm=TRUE))

# Odp: Wcale się nie zmieniła! W przeciwieństwie do średniej, która wzrosła ;).

# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>% 
  arrange(-Przebieg.w.km) %>%
  select(Przebieg.w.km) %>% View
  #head(5)
  

# Odp:Pierwszy wynik - 999999999, wygląda raczej na zakodowanie braku w danych, więc stawiałabym na kolejny - 2950000.

# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>%
  group_by(Kolor) %>% 
  count() %>% 
  arrange(n) %>% 
  head(10)

# Odp: Na drugiej pozycji znajduje się "grafitowy-metallic", ale tak naprawdę razem z kolorami "czerwony-metallic", "srebrny" i "zielony" zajmuje on ostatnie miejsce ex-equo, a drugim co do anty-popularności kolorem (2 samochody) jest "granatowy-metallic".


# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>%
  mutate(czy_el_lusterka = grepl(" el. lusterka", Wyposazenie.dodatkowe)) %>%
  filter(czy_el_lusterka == TRUE) %>%
  summarise(kwantyl1 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE),
            kwantyl3 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))
  
auta2012 %>% 
  filter(Marka == "Volkswagen") %>%
  mutate(czy_el_lusterka = grepl(" el. lusterka", Wyposazenie.dodatkowe)) %>%
  filter(!czy_el_lusterka == TRUE) %>%
  summarise(kwantyl1 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE),
            kwantyl3 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))
        
 


# Odp:Z lusterkami - 0.25: 1896, 0.75: 1968; bez lusterek - 0.25: 1400, 0.75: 1900.