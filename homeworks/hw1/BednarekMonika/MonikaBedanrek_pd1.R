library(PogromcyDanych)
library(stringr)
library(stats)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% filter(Waluta == 'PLN') %>% filter(Rodzaj.paliwa == 'naped elektryczny') %>% 
  summarise(mediana_ceny = median(Cena)) -> sol1


# Odp: Mediana ceny samochodów, które mają napęd elektryczny to 18 900 PLN.



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% mutate(czy_przed_2001 = Rok.produkcji < 2001) %>%  group_by(czy_przed_2001, Marka) %>% 
  summarise(mediana_KM = median(KM, na.rm = TRUE)) %>% arrange(-mediana_KM) -> sol2


# Odp: Mediana liczby koni mechanicznych jest największa dla Bugatti wyprodukowanych po 2001 roku
#      i jest ona równa 1001 KM.



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% filter(Kolor == 'szary-metallic', median(Cena.w.PLN, na.rm = TRUE) <= Cena.w.PLN,
                    mean(Cena.w.PLN, na.rm = TRUE) >=  Cena.w.PLN) %>% 
  filter(as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia), 
         Kraj.pochodzenia != '', Kraj.aktualnej.rejestracji != '')-> sol3

# Odp: 537



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% filter(Model == 'Passat', Wersja == 'B6', Rodzaj.paliwa == 'benzyna') %>% 
  summarise(IQR_przebieg = IQR(Przebieg.w.km, na.rm =TRUE))-> sol4


# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% filter(Waluta == 'CZK') %>% 
  mutate(cena_brutto = ifelse(Brutto.netto == 'brutto', Cena, 1.02 * Cena)) %>%
  summarise(srednia = mean(cena_brutto, na.rm = TRUE)) -> sol5

# Odp: 210 678.3 CZK




# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% filter(Przebieg.w.km > 50000, Marka == 'Chevrolet', Skrzynia.biegow != '') %>%
  group_by(Model, Skrzynia.biegow) %>% summarise(ilosc = n()) %>% 
  arrange(-ilosc) -> sol6
sol6 %>% group_by(Skrzynia.biegow) %>% summarise(ilosc = sum(ilosc)) -> ilosc_podział_na_skrzynie





# Odp: Więcej jest chevroletóW ze skrzynią manualną (112 z automatycnzą i 336 z manualną).
#      Model który pojawia sie najczęściel ze skrzynią manualną to Lacetti, a z automatyczną to Corvette .



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% filter(Marka == 'Mercedes-Benz') %>% mutate(przed_2003 = Rok.produkcji >= 2003) %>% 
  group_by(przed_2003) %>% summarise(mediana_pojemnośći_skokowej = median(Pojemnosc.skokowa, na.rm = TRUE)) -> sol7


# Odp: W obu przypadkach mediana to 2200 zatem nie zmieniła się ona.



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% filter(Kraj.pochodzenia == 'Niemcy', Kraj.aktualnej.rejestracji == 'Polska') %>% 
   top_n(1, Przebieg.w.km) -> sol8


# Odp: 	1 000 000 km





# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% filter(Marka == 'Mitsubishi', Kraj.pochodzenia == 'Wlochy') %>% 
  group_by(Kolor) %>% summarise(liczba = n()) %>% arrange(liczba)-> sol9
  

# Odp: granatowy-metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>%  filter(Marka == 'Volkswagen') %>% 
  mutate(ma_el.lusterka = str_detect(Wyposazenie.dodatkowe, "el. lusterka")) %>% 
  group_by(ma_el.lusterka) %>% 
  summarise(pierwszy_kwartyl = quantile(Pojemnosc.skokowa, probs = c(0.25), na.rm = TRUE), 
            trzeci_kwartyl = quantile(Pojemnosc.skokowa, probs = c(0.75), na.rm = TRUE))-> sol10



# Odp: samochody mające elektryczne lusterka : pierwszy kwartyl = 1892.25, trzeci kwartyl: 1900
#      samochody nie mające elektrycznych lusterek : pierwszy kwartyl = 1400.00, trzeci kwartyl: 1968



