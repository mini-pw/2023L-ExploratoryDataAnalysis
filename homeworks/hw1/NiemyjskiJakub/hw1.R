library(dplyr)
library(PogromcyDanych)
data(auta2012)


str(auta2012)
head(auta2012)
View(auta2012)



# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(median_price = median(Cena, na.rm = TRUE))


# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  filter(!is.na(KM)) %>%
  mutate(po_2001 = ifelse(Rok.produkcji >= 2001, "TAK", "NIE")) %>% 
  group_by(Marka, po_2001) %>% 
  summarise(mediana_KM = median(KM)) %>% 
  arrange(-mediana_KM) %>% 
  select(Marka, po_2001) %>% 
  head(1)



# Odp: Bugatti, w 2001 lub później



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i podaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

# Szukamy mediany i średniej 
auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  summarise(srednia = mean(Cena.w.PLN), mediana = median(Cena.w.PLN))
# srednia jest wieksza od mediany

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  filter(between(Cena.w.PLN, median(Cena.w.PLN), mean(Cena.w.PLN))) %>% 
  filter(!is.na(Kraj.aktualnej.rejestracji)) -> z 
  
filter(z, as.character(z$Kraj.aktualnej.rejestracji) != 
           as.character(z$Kraj.pochodzenia)) %>%
select(all_of(c("Cena.w.PLN", "Kraj.aktualnej.rejestracji", 
                "Kraj.pochodzenia"))) %>% 
summarise(n())
 



# Odp: 1331



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>% 
  summarise(Rozstep = IQR(Przebieg.w.km, na.rm = TRUE))
  
  
  

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).


# założyłem, że średnia cena brutto ma być przedstawiona w dowolnej walucie, 
# zatem ja wybrałem korony czeskie

 auta2012 %>% 
  filter(Waluta == "CZK") %>%
  mutate(CENA_BRUTTO = case_when(Brutto.netto == "brutto" ~ Cena,
                                 Brutto.netto == "netto" ~ Cena * 1.02)) %>% 
  summarise('średnia cena' = mean(CENA_BRUTTO))
  
# Odp: 210678,3 w czeskich koronach



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 5e4) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(Ile = n())
  
auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 5e4) %>% 
  group_by(Skrzynia.biegow, Model) %>% 
  summarise(Ile = n()) %>% 
  group_by(Skrzynia.biegow) %>%   
  filter(Ile == max(Ile))

# Odp: więcej jest Chevroletów ze skrzynią manualną,
# w przypadku skrzyni manualnej najczęściej pojawia się Lacetti, 
# a w przypadku automatycznej Corvette.



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>%
  mutate(po_2003 = ifelse(Rok.produkcji >= 2003, "TAK", "NIE")) %>%
  group_by(po_2003) %>% 
  summarise(Mediana = median(Pojemnosc.skokowa, na.rm = TRUE))
  

# Odp: Nie zmieniła się



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>% 
  select(Przebieg.w.km) %>% 
  arrange(-Przebieg.w.km) %>% 
  head(1)


# Odp: 1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>%  # jest ich 3786
  group_by(Kolor) %>% 
  summarise(Liczba_aut = n()) %>% 
  arrange(Liczba_aut)

# Odp: granatowy-metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?
library(stringr)


auta2012 %>% 
  filter(Marka == "Volkswagen") -> volkswageny
  
mutate(volkswageny, elektryczne_lusterka = str_detect(
  tolower(volkswageny$Wyposazenie.dodatkowe), "el. lusterka")) %>% 
  group_by(elektryczne_lusterka) %>% 
  summarise(kwantyl0_25 = quantile(Pojemnosc.skokowa, na.rm = TRUE, prob = c(0.25)),
            kwantyl0_75 = quantile(Pojemnosc.skokowa, na.rm = TRUE, prob = (0.75)))
  

# Odp: Wartosc kwantyla 0.25 dla samochodów z elektrycznymi lusterkami to 1400, a
# kwantyla 0.75 1900. 
# Wartość kwantyla 0.25 dla samochodów bez elektrycznych lusterek to 1892, a 
# kwantyla 0.75 1968.


