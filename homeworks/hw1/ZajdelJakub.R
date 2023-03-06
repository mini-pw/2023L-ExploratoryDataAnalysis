
install.packages("PogromcyDanych")
library(PogromcyDanych)
View(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?
str(auta2012)

auta2012 %>% 
  group_by(Rodzaj.paliwa) %>% 
  summarise(n = n())

auta2012 %>% 
  filter(Waluta == "PLN") %>% 
  filter(Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(median = median(Cena.w.PLN))

# Odp: Median - 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  filter(Rok.produkcji >= 2001, is.na(KM) == FALSE) %>% 
  group_by(Marka) %>% 
  summarise(mediana_liczby_koni = median(KM)) %>% 
  mutate(przed_2001 = FALSE) %>% 
  arrange(-mediana_liczby_koni) -> df_1

auta2012 %>% 
  filter(Rok.produkcji < 2001, is.na(KM) == FALSE) %>% 
  group_by(Marka) %>% 
  summarise(mediana_liczby_koni = median(KM)) %>% 
  mutate(przed_2001 = TRUE) %>% 
  arrange(-mediana_liczby_koni) -> df_2

df = union(df_1, df_2)
df %>% 
  arrange(-mediana_liczby_koni) %>% 
  head(1)


# Odp: [Bugatti, 1001km, po 2001]



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  summarise(srednia_cena = mean(Cena.w.PLN)) -> df_srednia_cena
srednia_cena = df_srednia_cena[1, 1]

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  summarise(mediana_cena = median(Cena.w.PLN)) -> df_median_cena
median_cena = df_median_cena[1, 1]

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  filter( min(srednia_cena, median_cena) <= Cena.w.PLN & Cena.w.PLN <= max(srednia_cena, median_cena)) %>% 
  #filter(Kraj.pochodzenia != "", is.na(Kraj.pochodzenia) == FALSE) %>% 
  filter(Kraj.aktualnej.rejestracji != "", is.na(Kraj.aktualnej.rejestracji) == FALSE) -> df_3
  
counter <- 0
for (i in 1:nrow(df_3)) {
  if(df_3[i, 16] != df_3[i, 17]){
    counter <- counter + 1
  }
}

# Odp: counter = 635, a z dodaniem zakomentowanego wiersza kodu := 908



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

auta2012 %>% 
  filter(Marka == "Volkswagen", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>% 
  filter(!is.na(Przebieg.w.km)) %>% 
  select(Przebieg.w.km) ->a

wektor <- a[, 1]
wektor_kwantylowy <- quantile(wektor, probs = seq(0, 1, 1/4))
rozstęp_miedzykwartylowy <- wektor_kwantylowy[4] - wektor_kwantylowy[2]

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(kwota_brutto = ifelse(Brutto.netto == "brutto", Cena, Cena*1.02)) %>% 
  summarise(srednia = mean(kwota_brutto))


# Odp: 210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.


auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n())

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>%
  group_by(Model, Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(-n,by.group = TRUE)

# Odp: 1. Wiecej jest tych z manualna skrzynia biegow. 
#      2. Model najczęściej pojawiający się w grupie z automatyczna skrzynia biegów to: Corvette
#         Model najczesciej pojawiajacy sie w grupie z manualna skrzynia biegow to: Lacetti



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", is.na(Pojemnosc.skokowa) == FALSE, Rok.produkcji > 2003) %>% 
  summarise(mediana = median(Pojemnosc.skokowa))
  
auta2012 %>% 
  filter(Marka == "Mercedes-Benz", is.na(Pojemnosc.skokowa) == FALSE, Rok.produkcji <= 2003) %>% 
  summarise(mediana = median(Pojemnosc.skokowa))
  

# Odp: W obu przypadkach wynosi: 2200, wiec nie zmieniła się :)



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?
# !!! Jako samochód aktualnie zarejestrowany przyjmuję samochody posiadające kraj aktualnej rejestracji. !!! 

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>% 
  arrange(-Przebieg.w.km) %>% 
  select(Przebieg.w.km) %>% 
  head(1)



# Odp: Najwiekszy przebieg: 1e+09 km.



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy", is.na(Kolor) == FALSE, Kolor != "") %>% 
  group_by(Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(n)


# Odp: Drugi najmniej popularny kolor to: "granatowy-metallic"



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

# z el. lusterka
auta2012 %>% 
  filter(Marka == "Volkswagen", grepl("el. lusterka", Wyposazenie.dodatkowe)) %>% 
  summarise(quantile_025 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE), 
            quantile_075 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))

# bez el. lusterka
auta2012 %>% 
  filter(Marka == "Volkswagen", !grepl("el. lusterka", Wyposazenie.dodatkowe)) %>% 
  summarise(quantile_025 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE), 
            quantile_075 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))


# Odp: z el.lusterka:   quantile_025 = 1892.25; quantile_075 = 1968
#      bez el.lusterka: quantile_025 = 1400;    quantile_075 = 1900



