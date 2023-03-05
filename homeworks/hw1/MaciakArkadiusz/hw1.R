library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Rodzaj.paliwa == "naped elektryczny", Waluta == "PLN") %>%
  summarise(median = median(Cena, na.rm = TRUE))

# Odp: 18900


########################

# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>%
  mutate(year_before_2001 = if_else(Rok.produkcji < 2001, "before", "after")) %>% 
  group_by(Marka, year_before_2001) %>% 
  summarise(median = median(KM, na.rm = TRUE)) %>% #UWAGA na.rm=TRUE ma duży wpływ na wynik(bez tego Bugatti, before, 560)
  arrange(desc(median)) %>% 
  head(1)

# Odp: Bugatti after (year 2001)


########################

# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji (pomijamy uwagę)

auta2012 %>% 
  filter(Kolor=="szary-metallic") %>% 
  mutate(mean = mean(Cena.w.PLN, na.rm = TRUE), median = median(Cena.w.PLN, na.rm = TRUE)) %>% 
  filter(ifelse(mean > median, Cena.w.PLN < mean & Cena.w.PLN > median,
                Cena.w.PLN > mean & Cena.w.PLN < median)) %>% # teoretycznie nie wiemy,
  # które z nich jest większe (bez sprawdzenia w tabeli oczywiście; gdybyśmy sprawdzili tabele moglibyśmy
  # wziąć zwyczajnie pierwszą opcje). Dodatkowo rozumiemy "pomiędzy" jako bez wartości granicznych (> nie >=) 
  filter(!is.na(Kraj.aktualnej.rejestracji)) %>% # przy usunięciu pustego stringu:  filter(Kraj.aktualnej.rejestracji!="") %>% 
  filter(as.character(Kraj.aktualnej.rejestracji)!= as.character(Kraj.pochodzenia)) %>%
  count

# wynik byłyby nieco inne gdybyśmy pozbyli się wartości NA z Kraj.pochodzenia
# jednak nie ma wzmianki żeby to robić w UWADZE

# Odp: 1331 z pustymi strignami (635 bez pustych stringów)



#######################

# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Wersja =="B6", Rodzaj.paliwa=="benzyna") %>% 
  summarize(iqr =  IQR(Przebieg.w.km, na.rm = TRUE))
# funkcja IQR liczy rozstęp międzykwartylowy (interquartile range) da danych wartości

# Odp: 75977.5




# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(cena_brutto = ifelse(Brutto.netto =="brutto", Cena, Cena * 1.02)) %>% 
  summarize(mean=mean(cena_brutto))

# Odp: 210678.3 CZK



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  count(sort=TRUE) %>% 
  head(1)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow=="manualna") %>% 
  group_by(Model) %>% 
  count(sort=TRUE) %>% 
  head(1)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow=="automatyczna") %>% 
  group_by(Model) %>% 
  count(sort=TRUE) %>% 
  head(1)

# alternatywne rozwiązanie na drugą część zadania(2 w jednym)
auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow, Model) %>% 
  count(sort=TRUE)

# Odp: więcej manualnych(336), najwięcej manualna: Lacetti(94), najwięcej automat: Corvette(20)
# (bez head(1) w pierwszym poznamy też liczbę automatycznych: 112)



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

# przed lub w roku 2003
med1 <- auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji <= 2003) %>% 
  summarize(median = median(Pojemnosc.skokowa, na.rm = TRUE))

# po roku 2003
med2 <- auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji > 2003) %>% 
  summarize(median = median(Pojemnosc.skokowa, na.rm = TRUE))

med2 - med1

#alterantywne rozwiązanie
auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  group_by(Production.after.2003 = ifelse(Rok.produkcji > 2003, "yes", "no"))%>% 
  summarize(median = median(Pojemnosc.skokowa, na.rm = TRUE))


# Odp: nie zmieniła się - w obu przypakach wynosi 2200



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>% 
  filter(Kraj.pochodzenia=="Niemcy", Kraj.aktualnej.rejestracji=="Polska") %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  select(Przebieg.w.km) %>% 
  head(1)

# Odp: 1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  count %>% 
  arrange(n)#arrange bazowo ustawia rosnąco
#wybieramy pozycje o drugiej najmniejszej wartości

# Odp: granatowy-metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?


# z lusterkami elektrycznymi w dodatkowym wyposażeniu
auta2012 %>% 
  filter(Marka == "Volkswagen", grepl("el. lusterka", Wyposazenie.dodatkowe)) %>% 
  summarize(quantile_025 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE),
            quantile_075 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))

# bez lusterek elektrycznych w dodatkowym wyposażeniu
auta2012 %>% 
  filter(Marka == "Volkswagen", !grepl("el. lusterka", Wyposazenie.dodatkowe)) %>% 
  summarize(quantile_025 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE),
            quantile_075 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))

# Odp: z el. lusterkami: kwantyl 0.25: 1892.25, kwantyl 0.75: 1968;
#      bez el. lusterek: kwantyl 0.25: 1400,    kwantyl 0.75: 1900;


