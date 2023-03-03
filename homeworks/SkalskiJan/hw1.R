library(PogromcyDanych)
library(dplyr)
data(auta2012)


# 1. Rozwa¿aj¹c tylko obserwacje z PLN jako walut¹ (nie zwa¿aj¹c na
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?

auta2012 %>% 
  filter(Rodzaj.paliwa == "naped elektryczny" & Waluta == "PLN") %>% 
  select(Cena) %>% 
  pull %>% 
  median
  

# Odp:  18900



# 2.W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001
# roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
# mechanicznych (KM) jest najwiêksza.

auta2012 %>% 
  group_by(Marka, Wyprodukowane.po.2001 = ifelse(Rok.produkcji >= 2001, "tak", "nie")) %>% 
  summarise(Mediana.KM = median(KM), .groups = "keep") %>% 
  arrange(desc(Mediana.KM)) %>% 
  select(Marka, Wyprodukowane.po.2001) %>% 
  head(1)

# Odp: Bugatti , nie



# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  filter(Cena.w.PLN %in% median(Cena.w.PLN):mean(Cena.w.PLN)) %>% 
  filter(!is.na(Kraj.aktualnej.rejestracji)) %>% 
  filter(as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) %>% 
  pull %>% 
  length

# Odp: 1071



# Jaki jest rozstêp miêdzykwartylowy przebiegu (w kilometrach) Passatów w wersji B6 i z benzyn¹ jako rodzajem paliwa?

auta2012 %>% 
  filter(Marka == "Volkswagen" & Model == "Passat" & Wersja == "B6" & Rodzaj.paliwa == "benzyna") %>% 
  select(Przebieg.w.km) %>% 
  pull %>% 
  IQR(na.rm = T)

# Odp: 75977.5



# 5.  Bior¹c pod uwagê samochody, których cena jest podana w koronach czeskich 
# podaj œredni¹ z ich ceny brutto.
# Uwaga: Jeœli cena jest podana netto, nale¿y dokonaæ konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(Cena.brutto = ifelse(Brutto.netto == "netto", 1.23 * Cena, Cena)) %>% 
  pull %>% 
  mean

# Odp: 227796.8 CZK



# 6. Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹? Dodatkowo, podaj model, który najczêœciej
# pojawia siê w obu przypadkach.

DF <- auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow, Model) %>% 
  count %>% 
  arrange(desc(n))

DF %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = sum(n))

# Odp: Wiecej jest Chevroletow ze srzynia manualna (336) niz automatyczna (112). 
# Skrzynia manualna - model Lacetti (94), skrzynia automatyczna - model Corvette (20).



# 7. Jak zmieni³a siê mediana pojemnoœci skokowej samochodów marki Mercedes-Benz,
# jeœli weŸmiemy pod uwagê te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  group_by(Wyprodukowane.przed.2003 = ifelse(Rok.produkcji <= 2003, "tak", "nie")) %>% 
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = T))



# Odp: Nie zmienila sie



# 8. Jaki jest najwiêkszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodz¹cych z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska" & Kraj.pochodzenia == "Niemcy") %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  select(Przebieg.w.km) %>% 
  head(1)

# Odp: 1000000000 km (ciekawe)



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodz¹cych z W³och?

auta2012 %>% 
  filter(Marka == "Mitsubishi" & Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  count %>% 
  arrange(n) 
 
# Odp: granatowy-metallic



# 10. Jaka jest wartoœæ kwantyla 0.25 oraz 0.75 pojemnoœci skokowej dla 
# samochodów marki Volkswagen w zale¿noœci od tego, czy w ich wyposa¿eniu 
# dodatkowym znajduj¹ siê elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen" & grepl("el. lusterka", Wyposazenie.dodatkowe)) %>% 
  select(Pojemnosc.skokowa) %>% 
  pull %>% 
  quantile(na.rm = T)

# Odp: Q1 = 1892.25, Q3 = 1968.0


