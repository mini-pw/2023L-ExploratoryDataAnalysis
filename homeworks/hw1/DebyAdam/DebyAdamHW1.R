library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Rodzaj.paliwa == "naped elektryczny" & Waluta == "PLN") %>% 
  summarise(mediana = median(Cena,na.rm = TRUE))
  
  

# Odp: 18 900 PLN



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  mutate(Tysiaclecie = ifelse(Rok.produkcji < 2001, "Drugie", "Trzecie")) %>% 
  group_by(Marka, Tysiaclecie) %>% 
  summarise(mediana = median(KM,na.rm = TRUE)) %>% 
  arrange(-mediana) %>%
  head(3)

# Odp: Bugatti wyprodukowane w 3 tysiacleciu (tzn rok produkcji >= 2001)


# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

Mean_SZ_M <- mean(filter(auta2012, Kolor == "szary-metallic")$Cena.w.PLN, 
                  na.rm = TRUE)
Median_SZ_M <- median(filter(auta2012, Kolor == "szary-metallic")$Cena.w.PLN, 
                  na.rm = TRUE)
                

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  filter(Cena.w.PLN <= Mean_SZ_M & Cena.w.PLN >= Median_SZ_M) %>% 
  mutate(Kar = as.character(Kraj.aktualnej.rejestracji), 
         Kp = as.character(Kraj.pochodzenia)) %>% 
  select(Kar, Kp) %>% 
  filter(Kar != "" & !is.na(Kar)) %>% 
  filter(Kar != Kp) %>% 
  count()
  
  

# (Na w kraju pochodzenia rozumiem jako puste pole)
# Odp: 635



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?


PassatyB6Ben <- auta2012 %>% 
  filter(Model == "Passat" & Wersja == "B6" & Rodzaj.paliwa == "benzyna") 
  
  IQR(PassatyB6Ben[["Przebieg.w.km"]], na.rm = TRUE)

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(CenaBrutto = ifelse(Brutto.netto == "brutto", Cena, Cena*1.02)) %>% 
  summarise(srednia = mean(CenaBrutto))

# Odp: 210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

Chavrolety <- auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000) %>%
  group_by(Skrzynia.biegow)

  count(Chavrolety)
  
  count(Chavrolety, Model) %>% 
    arrange(-n)
    

# Odp: Wiecej z manualna (336 do 112). Co do Modeli dla manualnej Lacetti, a dla
# automatycznej Corvette



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz" & Pojemnosc.skokowa != "") %>% 
  mutate(Rocznik_po_03 = ifelse(Rok.produkcji <= 2003, "Nie", "Tak")) %>% 
  group_by(Rocznik_po_03) %>% 
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: Nie zmieni�a si�, przed i po wynosi 2200



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska" & 
           Kraj.pochodzenia == "Niemcy") %>% 
  summarise(maksimum = max(Przebieg.w.km, na.rm = TRUE))

# Odp: 1e+09 km



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi" & Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  count() %>% 
  arrange(n)

# Po jednym wystepuja: Czerwony-metalic, grafitowy- metallic, srebrny i zielony
# Po dwa egzemokarz sa granatowy metalic

# Odp: Granatowy-Metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

library(stringr)

Volkswageny <- filter(auta2012, Marka == "Volkswagen")
mutate(Volkswageny, 
       LusterkaEl = str_detect(as.character(Volkswageny$Wyposazenie.dodatkowe),
                               pattern = "el. lusterka")) %>% 
  select(Pojemnosc.skokowa, LusterkaEl) %>% 
  group_by(LusterkaEl) %>% 
  summarise(kwartyle = quantile(Pojemnosc.skokowa, na.rm = TRUE,
                                probs = c(0.25, 0.75)))


# Odp: Jezeli sa lusterka elektryczne: 1892 - 1 kwartyl ; 1968 - 2 kwartyl
#      Jezeli nie ma lusterek elektrycznyhc: 1400 - 1 kwartyl; 1900 - 2 kwartyl


