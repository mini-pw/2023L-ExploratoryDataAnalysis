library(PogromcyDanych)
data(auta2012)
head(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?


# Odp:


auta2012 %>%
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny") %>%
  summarise(mediana_cen = median(Cena.w.PLN))

# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.



# Odp:

auta2012 %>%
  mutate(przed_2001_rokiem = if_else(Rok.produkcji >= 2001, "Nie", "Tak")) %>%
  group_by(przed_2001_rokiem, Marka) %>%
  summarise(mediana_koni = median(KM)) %>%
  arrange(desc(mediana_koni)) %>%
  head(1)

# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te,
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji


# Odp:
mean(auta2012$Cena.w.PLN)
head(auta2012)
auta2012 %>%
  filter(Kolor == "szary-metallic") %>%
  filter((
    median(auta2012$Cena.w.PLN) <= Cena.w.PLN &
      Cena.w.PLN <= mean(auta2012$Cena.w.PLN)
  ) |
    (
      mean(auta2012$Cena.w.PLN) <= Cena.w.PLN &
        Cena.w.PLN <= median(auta2012$Cena.w.PLN)
    )
  ) %>%
  filter(if_else(Kraj.aktualnej.rejestracji == "", FALSE, TRUE)) %>%
  filter(as.vector(Kraj.aktualnej.rejestracji) == as.vector(Kraj.pochodzenia)) %>%
  summarise(n = n())


# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?



# Odp:

auta2012 %>%
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>%
  summarise(
    rozstep = quantile(Przebieg.w.km, 0.75, na.rm = TRUE) - quantile(Przebieg.w.km, 0.25, na.rm = TRUE))


# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).



# Odp:

auta2012 %>%
  filter(Waluta == "CZK") %>%
  mutate(cena_brutto = ifelse(Brutto.netto == "brutto", Cena, Cena * 0.98)) %>%
  summarise(mean = mean(cena_brutto))

# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.



# Odp:

auta2012 %>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>%
  group_by(Skrzynia.biegow) %>%
  summarise(count = n()) %>%
  filter(count == max(count)) %>%
  select(Skrzynia.biegow)

auta2012 %>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>%
  select(Model, Skrzynia.biegow) %>%
  group_by(Model, Skrzynia.biegow) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  group_by(Skrzynia.biegow) %>%
  filter(n == max(n))

# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?



# Odp:
auta2012 %>%
  filter(Marka == "Mercedes-Benz") %>%
  mutate(po_2003_roku = ifelse(Rok.produkcji > 2003, "Tak", "Nie")) %>%
  group_by(po_2003_roku) %>%
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE))


# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?



# Odp:

auta2012 %>%
  filter(Kraj.aktualnej.rejestracji == "Polska",
         Kraj.pochodzenia == "Niemcy") %>%
  select(Przebieg.w.km) %>%
  arrange(desc(Przebieg.w.km)) %>%
  head(1)

# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?



# Odp:

auta2012 %>%
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>%
  group_by(Kolor) %>%
  summarise(Licznosc = n()) %>%
  arrange(desc(Licznosc)) %>%
  head(2) %>%
  arrange(Licznosc) %>%
  head(1)


# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu
# dodatkowym znajdują się elektryczne lusterka?

? select
# Odp:
library("stringr")
auta2012 %>%
  filter(Marka == "Volkswagen") %>%
  mutate(czy_lusterka_el = ifelse(str_detect(Wyposazenie.dodatkowe, "el. lusterka"), "Tak", "Nie")) %>% 
  select(Pojemnosc.skokowa, czy_lusterka_el) %>%
  group_by(czy_lusterka_el) %>%
  summarise(
    q1 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE),
    q3 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE)
  )
