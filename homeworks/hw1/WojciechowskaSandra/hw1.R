install.packages("dplyr")
library(dplyr)
install.packages("PogromcyDanych")
library(PogromcyDanych)
data(auta2012)
View(auta2012)
# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN", Rodzaj.paliwa=="naped elektryczny") %>% 
  summarise(mediana = median(Cena, na.rm = TRUE))
  
# Odp:18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>%  mutate(rok = ifelse(Rok.produkcji >= 2001, "nowy", "stary")) %>% 
  group_by(rok, Marka) %>% 
  summarise(mediana = median(KM, na.rm =TRUE)) %>% 
  arrange(desc(mediana)) %>% 
  ungroup(rok, Marka) %>% 
  head(1)

# Odp:Bugatti,wyprodukowany po 2001 roku



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

mediana1 <- auta2012 %>% filter(Kolor == "szary-metallic") %>% 
  select(Cena) %>% summarise(medianaszara = median(Cena, na.rm = TRUE))
mediana <-lapply(mediana1,as.numeric)

srednia1 <- auta2012 %>% filter(Kolor == "szary-metallic") %>% 
  select(Cena) %>% summarise(sredniaszara = mean(Cena, na.rm = TRUE))
srednia <-lapply(srednia1,as.numeric)


auta2012 %>% filter(Kolor =="szary-metallic", Cena <= srednia & Cena >= mediana) %>% 
  filter(Kraj.aktualnej.rejestracji != "") %>% 
  filter(Kraj.pochodzenia != "") %>%  filter(as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) %>% 
  summarise(n=n())


# Odp:515



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?
q1 <- auta2012 %>% filter(Model=="Passat", Wersja == "B6",Rodzaj.paliwa == "benzyna") %>% filter(!is.na(Przebieg.w.km)) %>% 
  reframe(q_passat = quantile(Przebieg.w.km))
IQR <- q1[4,1]-q1[2,1]
IQR
# Odp:75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).
auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(nowa_cena = ifelse(Brutto.netto == "brutto",Cena,Cena*0.02 + Cena)) %>% 
  summarise(srednia = mean(nowa_cena, na.rm = TRUE)) 

# Odp:210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.
automat <- auta2012 %>%  filter(Marka == "Chevrolet" , Przebieg.w.km > 50000, Skrzynia.biegow == "automatyczna") %>%  summarise(n=n())
automat <- as.numeric(automat)

manual <- auta2012 %>%  filter(Marka == "Chevrolet" , Przebieg.w.km > 50000, Skrzynia.biegow == "manualna") %>%  summarise(n=n())
manual <- as.numeric(manual)

if(manual >= automat) cat("Więcej samochodów miało skrzynię manualną:", manual) else cat("Więcej samochodów miało skrzynię automatyczną:", automat)

auta2012 %>%  filter(Marka == "Chevrolet" , Przebieg.w.km > 50000, Skrzynia.biegow == "automatyczna") %>% 
  group_by(Model) %>%  summarise(modele_zliczone = n()) %>% filter(modele_zliczone == max(modele_zliczone)) %>% head()

auta2012 %>%  filter(Marka == "Chevrolet" , Przebieg.w.km > 50000, Skrzynia.biegow == "manualna") %>% 
  group_by(Model) %>%  summarise(modele_zliczone = n()) %>% filter(modele_zliczone == max(modele_zliczone)) %>% head()



# Odp: Najwięcej było Chevroletów ze skrzynią manualną. W przypadku aut z manualną skrzynią biegów najczęściej pojawiał się model Lacetti, 
#natomiast aut z automatyczną skrynią biegów był to model Corvette.



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

mediana_starszych <- auta2012 %>% filter(Marka == "Mercedes-Benz", Rok.produkcji <= 2003) %>% 
  summarise(mediana_s = median(Pojemnosc.skokowa, na.rm =TRUE))
mediana_starszych <- as.numeric(mediana_starszych)
mediana_starszych

mediana_nowszych <-  auta2012 %>% filter(Marka == "Mercedes-Benz", Rok.produkcji > 2003) %>% 
  summarise(mediana_n = median(Pojemnosc.skokowa, na.rm =TRUE))
mediana_nowszych <- as.numeric(mediana_nowszych)
mediana_nowszych

# Odp: Mediana się nie zmieniła.



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?
auta2012 %>%  filter(Kraj.pochodzenia == "Niemcy", Kraj.aktualnej.rejestracji == "Polska") %>% summarise(max = max(Przebieg.w.km, na.rm =TRUE))

# Odp:1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?
auta2012 %>% filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy")  %>% select( Kolor) %>% group_by(Kolor) %>%
  summarise(kolory_zliczone = n()) %>% arrange(kolory_zliczone)


# Odp: 



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(lusterka = ifelse(grepl("el. lusterka", Wyposazenie.dodatkowe), "tak", "nie")) %>% 
  group_by(lusterka) %>% 
  reframe(quantile(Pojemnosc.skokowa, na.rm =TRUE, probs = c(0.25,0.75)))

# Odp: Dla aut posiadających lusterka elektryczne kwantyle wynoszą:1892 oraz 1968, 
#natomiast dla aut nieposiadających elektryczny lusterek kwartyle odpowiednio wynoszą: 1400 i 1900.


