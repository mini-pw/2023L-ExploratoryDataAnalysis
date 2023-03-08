install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
data(auta2012)
View(auta2012)
# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

# Odp: 19600

auta2012 %>%
  filter(Rodzaj.paliwa == "naped elektryczny") %>%
  summarise(mediana = median(Cena.w.PLN, na.rm=TRUE))

# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

# Odp: Bugatti after 2001

# oznaczenie: after.or.equal.2001 = samochody wyprodukowane
# po 2001 lub w 2001 roku

auta2012 %>%
  mutate(after.or.eq.2001 = ifelse(Rok.produkcji>=2001,
                                    "yes","no")) %>%
  group_by(Marka, after.or.eq.2001) %>%
  summarise(mediana = median(KM, na.rm = TRUE)) %>% 
  arrange(desc(mediana)) %>%
  select(Marka, after.or.eq.2001) %>%
  head(1)

# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

# Odp: 508

statystyka <- auta2012 %>%
  filter(Kolor == "szary-metallic") %>%
  group_by(Marka, Model) %>%
  summarise(mediana = median(Cena.w.PLN, na.rm = TRUE),
            srednia = mean(Cena.w.PLN, na.rm = TRUE))

res <- auta2012 %>%
  filter(Kolor == "szary-metallic") %>%
  select(Marka, Model, Cena.w.PLN, 
         Kraj.pochodzenia, Kraj.aktualnej.rejestracji)

merg <- data_frame(merge(res, statystyka))

merg <- merg %>%
  filter(Cena.w.PLN <= srednia & Cena.w.PLN >= mediana) %>%
  filter(Kraj.aktualnej.rejestracji != "") %>%
  mutate(Kraj.pochodzenia = fct_infreq(Kraj.pochodzenia)) %>%
  mutate(Kraj.aktualnej.rejestracji = fct_infreq(Kraj.aktualnej.rejestracji))

suma<-0
for(i in 1:nrow(merg)){
  if(merg$Kraj.pochodzenia[i] != merg$Kraj.aktualnej.rejestracji[i]){
      suma <- suma + 1
  }
}
print(suma)

# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

# Odp: 75977.5

auta2012 %>%
  filter(Marka == "Volkswagen", Model == "Passat",
         Wersja == "B6", Rodzaj.paliwa == "benzyna") %>%
  summarise(rozstep = IQR(Przebieg.w.km, na.rm = TRUE))


# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

# Odp: 210678.3 (w czeskich koronach)

auta2012 %>%
  filter(Waluta == "CZK")  %>%
  select(Waluta, Cena, Brutto.netto) %>%
  mutate(brutto = case_when(Brutto.netto == "brutto" ~ Cena,
                            Brutto.netto == "netto" ~ 1.02 * Cena)) %>%
  summarise(srednia = mean(brutto, na.rm=TRUE)) 
  
# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

#a)
auta2012 %>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>%
  group_by(Skrzynia.biegow) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(1)
# Odp: manualna 

#b)
auta2012 %>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>%
  group_by(Model, Skrzynia.biegow) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  filter(Skrzynia.biegow == "manualna") %>%
  head(1)

auta2012 %>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>%
  group_by(Model, Skrzynia.biegow) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  filter(Skrzynia.biegow == "automatyczna") %>%
  head(1)
# Odp: Najczęściej w skrzyniach manualnych pojawia się Lacetti (94)
# Najczęściej w skrzyniach automatycznych Corvette (20)


# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?


auta2012 %>%
  filter(Marka == "Mercedes-Benz") %>%
  mutate(after.2003 = ifelse(Rok.produkcji>2003,
                                   "yes","no")) %>%
  group_by(after.2003) %>%
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE)) %>% 
  arrange(desc(mediana))

# Odp: Nie zmieniła się (W obu przypadkach taka sama)

# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

auta2012 %>%
  filter(Kraj.aktualnej.rejestracji=="Polska",
         Kraj.pochodzenia=="Niemcy") %>%
  select(Przebieg.w.km) %>%
  arrange(desc(Przebieg.w.km)) %>%
  head(1)

# Odp: 999999999 km 

# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>%
  filter(Marka == "Mitsubishi" & Kraj.pochodzenia == "Wlochy") %>%
  group_by(Kolor) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  head(2)

# Odp: grafitowy-metalic

# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

auta2012 %>%
  filter(Marka == "Volkswagen") %>%
  select(Model, Pojemnosc.skokowa, Wyposazenie.dodatkowe) %>%
  filter(str_detect(Wyposazenie.dodatkowe, "\\bel. lusterka\\b")) %>%
  summarise(wartosc.kwantyla.25 = quantile(Pojemnosc.skokowa, 0.25, na.rm=TRUE),
            wartosc.kwantyla.75 = quantile(Pojemnosc.skokowa, 0.75, na.rm=TRUE)) %>%
  head(1)

auta2012 %>%
  filter(Marka == "Volkswagen") %>%
  select(Model, Pojemnosc.skokowa, Wyposazenie.dodatkowe) %>%
  summarise(wartosc.kwantyla.25 = quantile(Pojemnosc.skokowa, 0.25, na.rm=TRUE),
            wartosc.kwantyla.75 = quantile(Pojemnosc.skokowa, 0.75, na.rm=TRUE)) %>%
  head(1)
  
# Odp: W przypadku bez lusterek el.: 
#   0.75:  1968,      0.25: 1600

# Odp: W przypadku z lusterkami el.: 
#   0.75:  1968,      0.25: 1892.25


