install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
data(auta2012)
View(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN" & Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(median_value = median(Cena.w.PLN, na.rm = TRUE))

# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  mutate(Rok.produkcji_new = ifelse(Rok.produkcji>="2001", "po", "przed")) %>% 
  group_by(Marka, Rok.produkcji_new) %>% 
  summarise(medians = median(KM, na.rm = TRUE))%>% 
  arrange(desc(medians)) %>% 
  head(1)

# Odp: Bugatti, wyprodukowane w 2001 roku i później, 1001



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i podaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

mediana <- auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  select(Cena.w.PLN) %>% 
  summarise(median(Cena.w.PLN, na.rm = TRUE))
mediana <- lapply(mediana, as.numeric)

srednia <- auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  select(Cena.w.PLN) %>% 
  summarise(mean(Cena.w.PLN, na.rm = TRUE))
srednia <- lapply(srednia, as.numeric)

auta2012 %>% 
  filter(Kolor =="szary-metallic" & Cena.w.PLN < srednia & Cena.w.PLN > mediana) %>% 
  filter(Kraj.pochodzenia != "" & Kraj.aktualnej.rejestracji != "") %>% 
  filter(as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) %>% 
  summarise(n=n())


# Odp: 356



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat" & Wersja == "B6" & Rodzaj.paliwa == "benzyna") %>% 
  summarise(IQR(Przebieg.w.km, na.rm = TRUE))

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(Kwota = ifelse(Brutto.netto=="brutto", Cena, Cena*1.02)) %>% 
  summarise(mean(Kwota, na.rm = TRUE))

# Odp: 210678.3 CZK



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000) %>% 
  count(Model, Skrzynia.biegow) %>% 
  arrange(desc(n)) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(sum(n))

# Odp: manualna, manualna - Lacetti (94), automatyczna - Corvette (20)



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  mutate(Rok.produkcji_new = ifelse(Rok.produkcji>"2003", "po", "przed")) %>%
  group_by(Rok.produkcji_new) %>% 
  summarise(median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: nie zmieniła się



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska" & Kraj.pochodzenia == "Niemcy") %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  head(1)

# Odp: 1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Wlochy" & Marka == "Mitsubishi") %>% 
  count(Kolor) %>% 
  arrange(n)

# Odp: Mamy tylko po jednym aucie w kolorach: czerwony-metallic, grafitowy-metallic, srebrny, zielony.



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(lusterka = ifelse(str_detect(Wyposazenie.dodatkowe, "el. lusterka"), "Tak", "Nie")) %>% 
  group_by(lusterka) %>% 
  summarise(quantile(Pojemnosc.skokowa, probs = c(0.25, 0.75), na.rm = TRUE))

# Odp: Q1 - 1892 (lusterka), 1400 (brak), Q3 - 1968 (lusterka), 1900 (brak).