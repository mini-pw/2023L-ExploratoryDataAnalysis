library(PogromcyDanych)
library(stringr)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?
auta2012 %>% 
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(mediana = median(Cena))
  
# Odp:18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.
auta2012 %>% 
  mutate(wiek = ifelse(Rok.produkcji >= 2001, "nowy", "stary")) %>% 
  select(Marka, wiek, KM) %>% 
  group_by(Marka, wiek) %>% 
  summarise(mediana = median(KM, na.rm = TRUE)) %>% 
  arrange(desc(mediana)) %>% 
  head(1)
  

# Odp:Bugatti z roku 2001 i późniejszych lat



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  summarise(sr = mean(Cena.w.PLN, na.rm = TRUE),med = median(Cena.w.PLN, na.rm =TRUE))->srednia_med # rozumiem �e �rednia i mediana z tre�ci zadania dotycz� zbioru rekord�w z kolorem szary-metallic
med <- srednia_med[,2]  
sr <- srednia_med[,1]
auta2012 %>% 
  filter(Kolor == "szary-metallic", Cena.w.PLN>=med, Cena.w.PLN<=sr) %>%
  filter(as.character(Kraj.pochodzenia)!=as.character(Kraj.aktualnej.rejestracji),as.character(Kraj.aktualnej.rejestracji)!="" ) %>% 
  summarise(n = n())
  
# Odp:635



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  select(Model, Wersja, Rodzaj.paliwa, Przebieg.w.km) %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>% 
  summarise(kwar1 = quantile(Przebieg.w.km, 0.25, na.rm = TRUE), kwar3 = quantile(Przebieg.w.km, 0.75, na.rm = TRUE)) -> kwartyle
kwar3 <- unname(kwartyle[,2])
kwar1 <- unname(kwartyle[,1])
rozstep <- kwar3 - kwar1
rozstep
# Odp:Wynosi on 75977.5 km


# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(cena_nowa_brutto = ifelse(as.character(Brutto.netto)=="netto", 1.02*Cena, Cena)) %>% 
  summarise(sr = mean(cena_nowa_brutto, na.rm = TRUE)) -> sr
sr
# Odp:Wynosi ona 210678.3 CZK



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Przebieg.w.km > 50000, Marka == "Chevrolet") -> chev 
chev %>%
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  filter(Skrzynia.biegow != "")
chev %>%
  group_by(Model, Skrzynia.biegow) %>% 
  summarise(licz = n())->licz
licz %>% 
  filter(Skrzynia.biegow == "automatyczna") %>% 
  arrange(desc(licz)) %>% 
  head(1) %>% 
  select(Model)
licz %>% 
  filter(Skrzynia.biegow == "manualna") %>% 
  arrange(desc(licz)) %>% 
  head(1) %>% 
  select(Model)

# Odp: Więcej jest ze skrzynią manualną, dla automatycznej Corvette, a dla manualnej Lacetti



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  select(Marka,Rok.produkcji, Pojemnosc.skokowa) %>%
  filter(Marka == "Mercedes-Benz") %>% 
  mutate(wiek = ifelse(Rok.produkcji <= 2003, "stary", "nowy")) %>% 
  group_by(wiek) %>% 
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp:Nie zmieniła się w obu przypadkach wynosi 2200



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia=="Niemcy") %>% 
  top_n(1, Przebieg.w.km) %>% 
  select(Przebieg.w.km)

# Odp:1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  summarise(licz = n()) %>% 
  arrange(licz) -> kolory_pop 
kolory_pop[2, 1]
  
# Odp: Np. grafitowy_metallic bo 4 kolory maj� 1 wyst�pienie, 2 wyst�pienia ma kolor granatowy-metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?
auta2012 %>% 
  filter(Marka == "Volkswagen") %>%
  mutate(z_luster = str_detect(Wyposazenie.dodatkowe, "el. lusterka")) %>% 
  group_by(z_luster) %>% 
  summarise(kwar1 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE), kwar3 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))

# Odp: Warto�ci kwantyla 0.25 to 1400 dla bez el. lusterek i 1892 dla tych z el. lusterkami oraz Warto�ci kwantyla 0.75 to 1900 i 1968 dla odp. bez el.lusterek i z. 

