install.packages("PogromcyDanych")
install.packages("stringr")
install.packages("dplyr")
library(PogromcyDanych)
library(dplyr)
library(stringr)
View(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Rodzaj.paliwa == "naped elektryczny", Waluta == "PLN") %>% 
  select(Cena.w.PLN) %>% 
  summarise(mediana = median(Cena.w.PLN))


# Odp: 18900 PLN



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.


auta2012 %>% 
  select(Marka, KM, Rok.produkcji) %>%
  mutate(Rok.produkcji = ifelse(Rok.produkcji > 2000, "Auta nowsze", "Auta starsze")) %>% 
  group_by(Marka, Rok.produkcji) %>% 
  summarise(mediana = median(KM, na.rm = TRUE)) %>% # usunąłem obserwacje NA
  arrange(-mediana) %>% 
  head(1)
  
  
# Odp: Bugatti, Auta nowsze



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji


auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  mutate(średnia = mean(Cena.w.PLN), mediana = median(Cena.w.PLN)) %>%  
  filter(Cena.w.PLN > mediana, Cena.w.PLN < średnia) %>% 
  mutate(Porównanie = if_else(
    as.character(Kraj.aktualnej.rejestracji) == as.character(Kraj.pochodzenia),
    "te same", "inne")) %>%
  filter(Porównanie == "inne") %>%
  summarise(suma = n())


# Odp: 1331 (uwzględniając pusty string)



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Marka == "Volkswagen", Model == "Passat", Wersja == "B6", 
         Rodzaj.paliwa == "benzyna") %>% 
  summarise(rozstęp = IQR(Przebieg.w.km, na.rm = TRUE))
  

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(Cena_nowa = ifelse(Brutto.netto == "brutto", Cena.w.PLN, 1.02 * Cena.w.PLN)) %>% 
  summarise(Średnia = mean(Cena_nowa))


# Odp: 36047.06 PLN 


# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Przebieg.w.km > 50000, Marka == "Chevrolet") %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(ilość = n())

auta2012 %>% 
  filter(Przebieg.w.km > 50000, Marka == "Chevrolet") %>% 
  group_by(Skrzynia.biegow, Model) %>% 
  summarise(ilość = n()) %>% 
  arrange(-ilość)


# Odp:  Manualnych jest więcej, manualna - Lacetti, automatyczna - Corvette 



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji >= 2003, !is.na(Pojemnosc.skokowa)) %>%
  summarise(mediana_nowych = median(Pojemnosc.skokowa))

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji < 2003, !is.na(Pojemnosc.skokowa)) %>%
  summarise(mediana_starych = median(Pojemnosc.skokowa))


# Odp: Nie zmieniła się 



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Niemcy", Kraj.aktualnej.rejestracji == "Polska") %>% 
  select(Przebieg.w.km) %>% 
  arrange(-Przebieg.w.km) %>% 
  head(1)

# Odp: 1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy")  %>% 
  group_by(Kolor) %>% 
  summarise(ilość = n()) %>% 
  arrange(ilość) 


# Odp: granatowy-metallic (pierwsze cztery kolory są w ilości jeden, granatowych - 
# metalliców są dwie sztuki)



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(lusterka = ifelse(str_detect(Wyposazenie.dodatkowe, "el. lusterka"),
                              "są", "brak")) %>% 
  group_by(lusterka) %>%
  reframe(quantile(Pojemnosc.skokowa, na.rm = TRUE))
   


# Odp: Dla braku elektrycznych lusterek 0.25: 1400, 0.75: 1900
#      Dla elektrycznych lusterek 0.25: 1892, 0.75: 1968    