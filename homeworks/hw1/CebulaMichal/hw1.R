library(dplyr)
library(PogromcyDanych)
data(auta2012)



# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>%
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny") %>%
  summarise(Mediana.cen = median(Cena))

# Odp: 18900PLN.



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>%
  filter(Marka != "") %>%
  mutate(Wyprodukowany.od.2001 = (Rok.produkcji >= 2001)) %>%
  group_by(Marka, Wyprodukowany.od.2001) %>%
  summarise(Mediana.KM = median(KM, na.rm = TRUE)) %>%
  ungroup() %>%
  slice_max(Mediana.KM)

# Odp: Bugatti wyprodukowane w 2001 lub później.



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te,
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>%
  filter(Kolor == "szary-metallic",
         Kraj.pochodzenia != "",
         Kraj.aktualnej.rejestracji != "",
         between(Cena.w.PLN, median(Cena.w.PLN), mean(Cena.w.PLN)),
         Kraj.pochodzenia != as.character(Kraj.aktualnej.rejestracji)) %>%
  count(name = "Liczba.modeli")

# Odp: 537.



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>%
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>%
  summarise(IQR = IQR(Przebieg.w.km, na.rm = TRUE))

# Odp: 75977.5km.



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>%
  filter(Waluta == "CZK") %>%
  mutate(Cena.brutto = if_else(Brutto.netto == "netto", 1.02*Cena, Cena)) %>%
  summarise(Srednia.cena.brutto = mean(Cena.brutto))

# Odp: 210678.3CZK



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

filtered6 <- auta2012 %>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow != "")

# Liczba modeli ze względu na skrzynię biegów
filtered6 %>%
  count(Skrzynia.biegow, name = "Liczba modeli")

# Model pojawiający się najczęściej w obu przypadkach
filtered6 %>%
  count(Skrzynia.biegow, Model, name = "Liczba.modeli") %>%
  group_by(Skrzynia.biegow) %>%
  slice_max(Liczba.modeli) %>%
  ungroup()

# Odp: Więcej jest Chevroletów ze skrzynią manualną, przy czym ze skrzynią
# automatyczną najwięcej jest modeli Cervette, zaś ze skrzynią manualną
# najwięcej jest modeli Lacetti.



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>%
  filter(Marka == "Mercedes-Benz") %>%
  mutate(Wyprodukowany.do.2003 = (Rok.produkcji <= 2003)) %>%
  group_by(Wyprodukowany.do.2003) %>%
  summarise(Mediana.pojemnosci.skokowej =
              median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: Mediana nie zmieniła się.



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>%
  filter(Kraj.aktualnej.rejestracji == "Polska",
         Kraj.pochodzenia == "Niemcy") %>%
  slice_max(Przebieg.w.km) %>%
  select(Marka, Model, Kraj.aktualnej.rejestracji, Kraj.pochodzenia,
         Przebieg.w.km)

# Odp: 1e+09km.


# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>%
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy", Kolor != "") %>%
  count(Kolor, name = "Liczba.modeli") %>%
  filter(Liczba.modeli != min(Liczba.modeli)) %>%
  slice_min(Liczba.modeli)

# Odp: Granatowy-metallic.



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>%
  filter(Marka == "Volkswagen") %>%
  mutate(Obecnosc.el.lusterek =
           grepl("el. lusterka", Wyposazenie.dodatkowe)) %>%
  group_by(Obecnosc.el.lusterek) %>%
  summarise(Q1.pojemnosci.skokowej =
              quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE),
            Q3.pojemnosci.skokowej =
              quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))

# Odp: Dla samochodów bez el. lusterkami: Q1 = 1400, Q3 = 1900.
#      Dla samochodów z el. lusterek: Q1 = 1892, Q3 = 1968.


