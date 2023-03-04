install.packages("dplyr")
library(dplyr)
install.packages("PogromcyDanych")
library(PogromcyDanych)
data(auta2012)
View(auta2012)

# 1. RozwaĹĽajÄ…c tylko obserwacje z PLN jako walutÄ… (nie zwaĹĽajÄ…c na 
# brutto/netto): jaka jest mediana ceny samochodĂłw, ktĂłre majÄ… napÄ™d elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN") %>% 
  filter(Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(mediana = median(Cena.w.PLN, na.rm = TRUE)) 

# Odp: 18900



# 2. W podziale samochodĂłw na marki oraz to, czy zostaĹ‚y wyprodukowane w 2001 
# roku i pĂłĹşniej lub nie, podaj kombinacjÄ™, dla ktĂłrej mediana liczby koni
# mechanicznych (KM) jest najwiÄ™ksza.

auta2012 %>% 
  filter(Rok.produkcji >= 2001) %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM, na.rm = TRUE)) %>% 
  top_n(1, mediana) # 1001, Bugatti, >=2001

auta2012 %>% 
  filter(Rok.produkcji <= 2001) %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM, na.rm = TRUE)) %>% 
  top_n(1, mediana) # 560, Bugatti, <=2001

  
# Odp: Bugatii, 2001 rok i później, 1001 KM



# 3. SpoĹ›rĂłd samochodĂłw w kolorze szary-metallic, ktĂłrych cena w PLN znajduje siÄ™
# pomiÄ™dzy jej Ĺ›redniÄ… a medianÄ… (nie zwaĹĽajÄ…c na brutto/netto), wybierz te, 
# ktĂłrych kraj pochodzenia jest inny niĹĽ kraj aktualnej rejestracji i poodaj ich liczbÄ™.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic", min(median(Cena.w.PLN, na.rm = TRUE), mean(Cena.w.PLN, na.rm = TRUE)) < Cena.w.PLN,
  Cena.w.PLN < max(median(Cena.w.PLN, na.rm = TRUE), mean(Cena.w.PLN, na.rm = TRUE))) %>% 
  filter(Kraj.aktualnej.rejestracji != "", as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) %>%
  summarise(n = n())
  
# Odp: 891


# 4. Jaki jest rozstÄ™p miÄ™dzykwartylowy przebiegu (w kilometrach) PassatĂłw
# w wersji B6 i z benzynÄ… jako rodzajem paliwa?

A <- auta2012 %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna")

IQR(A$Przebieg.w.km, na.rm = TRUE)

# Odp: 75977.5


# 5. BiorÄ…c pod uwagÄ™ samochody, ktĂłrych cena jest podana w koronach czeskich,
# podaj Ĺ›redniÄ… z ich ceny brutto.
# Uwaga: JeĹ›li cena jest podana netto, naleĹĽy dokonaÄ‡ konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  transmute(Brutto = ifelse(Brutto.netto == "brutto", Cena.w.PLN, 1.02*Cena.w.PLN)) %>% 
  summarise(srednia = mean(Brutto, na.rm = TRUE))

# Odp: 36047.06 PLN


# 6. KtĂłrych ChevroletĂłw z przebiegiem wiÄ™kszym niĹĽ 50 000 jest wiÄ™cej: tych
# ze skrzyniÄ… manualnÄ… czy automatycznÄ…? Dodatkowo, podaj model, ktĂłry najczÄ™Ĺ›ciej
# pojawia siÄ™ w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) # Więcej jest Chevroletow z manualna skrzynia biegow (336) niz z automatyczna (112)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow == "automatyczna") %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  top_n(1, n) # Corvette

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow == "manualna") %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  top_n(1, n) # Lacetti


# Odp: Chevroletow z przebiegiem > 50000 jest wiecej z manualna skrzynia biegow.
#      W przypadku automatycznej skrzyni biegow najczesciej pojawia sie model Corvette,
#      natomiast w przypadku manualnej - model Lacetti.



# 7. Jak zmieniĹ‚a siÄ™ mediana pojemnoĹ›ci skokowej samochodĂłw marki Mercedes-Benz,
# jeĹ›li weĹşmiemy pod uwagÄ™ te, ktĂłre wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  summarise(mediana = median(Pojemnosc.skokowa, na.rm=TRUE)) # mediana wszystkich = 2200

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji >= 2003) %>% 
  summarise(mediana = median(Pojemnosc.skokowa, na.rm=TRUE)) # mediana wyprodukowanych w >= 2023 roku = 2200

# Odp: Nie zmieni się (będzie równa 2200)



# 8. Jaki jest najwiÄ™kszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzÄ…cych z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>% 
  arrange(-Przebieg.w.km) %>% 
  select(Przebieg.w.km) %>% 
  top_n(1, Przebieg.w.km)

# Odp: 1e+09 (1 000 000 000 km)



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzÄ…cych z WĹ‚och?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(n)

# Odp: granatowy-metallic (n=2)



# 10. Jaka jest wartoĹ›Ä‡ kwantyla 0.25 oraz 0.75 pojemnoĹ›ci skokowej dla 
# samochodĂłw marki Volkswagen w zaleĹĽnoĹ›ci od tego, czy w ich wyposaĹĽeniu 
# dodatkowym znajdujÄ… siÄ™ elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(lusterka = ifelse(stringr::str_detect(Wyposazenie.dodatkowe, "el. lusterka"), "Tak", "Nie")) %>% 
  group_by(lusterka) %>% 
  reframe(kwantyle = quantile(Pojemnosc.skokowa, c(0.25, 0.75), na.rm = TRUE)) 


# Odp: Gdy lusterka są kwantyl 0.25 wynosi 1892, kwantyl 0.75 - 1968. 
#      Gdy lusterek nie ma kwantyl 0.25 to 1400, a 0.75 - 1900
