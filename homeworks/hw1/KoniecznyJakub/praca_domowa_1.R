################# PRACA DOMOWA 1 - JAKUB KONIECZNY 320563 ######################

library(PogromcyDanych)
library(stats)
library(dplyr)
library(stringr)
data(auta2012)
options(stringsAsFactors = F)
auta2012

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>%
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(median_of_price = median(Cena.w.PLN)) -> electric

# Odp: 18 900 PLN



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  mutate(po_2001 = Rok.produkcji >= 2001) %>% 
  group_by(Marka, po_2001) %>% 
  summarise(medianaKM = median(KM, na.rm = T)) %>% 
  arrange(-medianaKM) %>% 
  head(1) -> HP_max
  
# Odp: Bugatti, 2001 i później, 1001 KM



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i podaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic", median(Cena.w.PLN, na.rm = T) < Cena.w.PLN,
  Cena.w.PLN < mean(Cena.w.PLN, na.rm = T)) %>% 
  filter(Kraj.aktualnej.rejestracji!= "") %>% 
  filter(as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) -> grey_cars
  nrow(grey_cars)
# # Komentarz: na logikę - jeśli nie znamy kraju aktualnej rejestracji (a zakładam, że
  # samochód jest gdzieś zarejestrowany) to odrzucam wszystkie puste wartości, ponieważ
  # nie wiemy czy kraj pochodzenia jest taki sam jak aktualnej rejestracji, czy inny 
# Odp: 891



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

  auta2012 %>%
    filter(Marka == "Volkswagen", Model == "Passat", Wersja == "B6",
           Rodzaj.paliwa == "benzyna") %>%
    reframe(kwartyle = quantile((Przebieg.w.km), probs = c(0.25, 0.5, 0.75), 
      na.rm = T)) -> kwartyle
  kwartyle[3, 1] - kwartyle[1,1]

  # Odp: 75977.5


# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(brutto_po_konwersji = ifelse(Brutto.netto == "brutto", Cena.w.PLN, Cena.w.PLN*1.02)) %>% 
  summarise(srednia = mean(brutto_po_konwersji), na.rm = T) -> czesi

# Odp: 36047.06 PLN 



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow != "") %>% 
  count(Skrzynia.biegow) -> chevrolety

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow == "manualna") %>%
  count(Model, sort = T)
  
auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow == "automatyczna") %>% 
  count(Model, sort = T)

# Odp: Więcej jest ze skrzynią biegów manualną. W przypadku skrzyni biegów manualnej
# najczęściej występuje model Lacetti, zaś w przypadku skrzyni biegów automatycznej
# najczęściej występuje model Corvette.



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  select(Pojemnosc.skokowa, Rok.produkcji) %>% 
  mutate(wyprodukowane_po_2003 = Rok.produkcji > 2003) %>% 
  group_by(wyprodukowane_po_2003) %>% 
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = T)) -> pojemnosc_skokowa
  diff(pojemnosc_skokowa$mediana)

# Odp: Nie zmieniła się.



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>% 
  arrange(-Przebieg.w.km) %>% 
  select(Przebieg.w.km) %>% 
  top_n(1) -> przebieg_max
  
# Odp: 1e+09 (około 1 000 000 000)



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Wlochy", Marka == "Mitsubishi") %>% 
  select(Kolor) %>% 
  count(Kolor) %>% 
  arrange(n) %>% 
  top_n(-6) -> forza_italia


# Odp: granatowy-metallic




# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(czy_ma_lusterka_el = ifelse(str_detect(Wyposazenie.dodatkowe, "el. lusterka"), "TAK", "NIE")) %>% 
  group_by(czy_ma_lusterka_el) %>% 
  reframe(kwantyle = quantile(Pojemnosc.skokowa, probs = c(0.25, 0.75), na.rm = T)) -> lusterka
  lusterka
# Odp: Pierwszy kwartyl: gdy są lusterka elektryczne - 1892, gdy nie ma - 1400,
# Drugi kwartyl: gdy są lusterka elektryczne - 1968, gdy nie ma - 1900.


