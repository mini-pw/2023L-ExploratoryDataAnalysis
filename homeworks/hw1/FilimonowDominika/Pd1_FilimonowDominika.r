library(PogromcyDanych)
data(auta2012)

summary(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN" & Rodzaj.paliwa == "naped elektryczny") %>%
  summarise(mediana = median(as.numeric(Cena.w.PLN), na.rm = TRUE))
 
# Odp: 18900


# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  mutate(rok_p = ifelse(Rok.produkcji < 2001, "wczesniej", "pozniej")) %>% 
  group_by(rok_p, Marka) %>% 
  summarise(mediana = median(as.numeric(KM), na.rm = TRUE)) %>% 
  arrange(-mediana)
  
# Odp: Mediana liczby koni mechanicznych jest największa przy kombinacji:
# Marka: Bugatti, wyprodukowane w 2001 i później, mediana ta wynosi 1001.0


# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji


auta2012 %>% 
  filter(Kolor == "szary-metallic" & median(Cena.w.PLN) <= Cena.w.PLN & Cena.w.PLN <= mean(Cena.w.PLN)) %>% 
  filter(as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia)) %>% 
  summarise(n = n())

# Odp: Takich samochodów jest 1930


# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat" & Wersja == "B6" & Rodzaj.paliwa == "benzyna") %>% 
  reframe(kwantyle = quantile(Przebieg.w.km, probs = c(0.25, 0.5, 0.75), na.rm = TRUE))
  

?quantile
# Odp: 118250 - 42272.5 = 75977,5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(cena_brutto = ifelse(Brutto.netto == "brutto", Cena, Cena*1.02)) %>%
  summarise(średnia = mean(as.numeric(cena_brutto), na.rm = TRUE))

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(cena_brutto = ifelse(Brutto.netto == "brutto", Cena.w.PLN, Cena.w.PLN*1.02)) %>%
  summarise(średnia = mean(as.numeric(cena_brutto), na.rm = TRUE))

# Odp:Średnia z ceny brutto jest równa 210678.3 (CZK) i 36047.06 (PLN)



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow)%>% 
  summarise(n = n())

auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000 & Skrzynia.biegow != '') %>% 
  group_by(Skrzynia.biegow, Model)%>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp: Tych ze skrzynią manualną. 
# Najczęściej w przypadku skrzyni manualnej pojawia się model Lacetti (94 razy)
# A w przypadku skrzyni automatycznej Corvette (20 razy)



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  mutate(rok_p = ifelse(Rok.produkcji <= 2003, "przed_lub_w", "po" )) %>% 
  group_by(rok_p) %>% 
  summarise(mediana = median(as.numeric(Pojemnosc.skokowa), na.rm = TRUE))

# Odp: Nie zmieniła się (?)



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska" & Kraj.pochodzenia == "Niemcy") %>% 
  select(Przebieg.w.km) %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  head(1)



# Odp: 1e+09 (ok 1,000,000,000)



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Wlochy" & Marka == "Mitsubishi") %>% 
  group_by(Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(n)

# Odp: Drugim najmniej popularnym kolorem jest granatowy-metallic.
#(pierwsze 4 pozycje tj. czerwony-metallic, grafitowy-metallic, 
#srebrny i zielony są eq aequo na pierwszym miejscu)



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla             
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

library (stringr)

auta2012 %>% 
  filter(Marka == "Volkswagen") %>%
  mutate(czy_lusterko = ifelse(str_detect(Wyposazenie.dodatkowe, "el. lusterka"), "tak", "nie")) %>% 
  group_by(czy_lusterko) %>% 
  reframe(kwantyle = quantile(Pojemnosc.skokowa, probs = c(0.25, 0.75), na.rm = TRUE))

# Odp:Pierwszy kwartyl: Jeżeli znajduje się lusterko -> 1892, jeżeli nie -> 1400
#Trzeci kwartyl: Jeżeli znajduje się lusterko -> 1968, jeżeli nie -> 1900

