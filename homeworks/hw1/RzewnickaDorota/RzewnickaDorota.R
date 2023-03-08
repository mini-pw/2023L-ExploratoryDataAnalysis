# Praca domowa 1, Dorota Rzewnicka 320578

library(PogromcyDanych)
data(auta2012)

library(stringr)

# 1. RozwaĹĽajÄ…c tylko obserwacje z PLN jako walutÄ… (nie zwaĹĽajÄ…c na 
# brutto/netto): jaka jest mediana ceny samochodĂłw, ktĂłre majÄ… napÄ™d elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN" & Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(mediana = median(Cena))
# Odp: Mediana wynosi 18900.


# 2. W podziale samochodĂłw na marki oraz to, czy zostaĹ‚y wyprodukowane w 2001 
# roku i pĂłĹşniej lub nie, podaj kombinacjÄ™, dla ktĂłrej mediana liczby koni
# mechanicznych (KM) jest najwiÄ™ksza.

auta2012 %>% 
  mutate(po_2001 = ifelse(Rok.produkcji<=2001, FALSE, TRUE)) %>% 
  group_by(Marka, po_2001) %>% 
  summarise(mediana_KM = median(KM, na.rm = TRUE)) %>% 
  arrange(desc(mediana_KM)) %>% 
  head(1)
# Odp: Mediana KM jest największa dla samochodów marki Bugatti wyprodukowanych po 2001.


# 3. SpoĹ›rĂłd samochodĂłw w kolorze szary-metallic, ktĂłrych cena w PLN znajduje siÄ™
# pomiÄ™dzy jej Ĺ›redniÄ… a medianÄ… (nie zwaĹĽajÄ…c na brutto/netto), wybierz te, 
# ktĂłrych kraj pochodzenia jest inny niĹĽ kraj aktualnej rejestracji i poodaj ich liczbÄ™.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  mutate(srednia = mean(Cena.w.PLN)) %>% 
  mutate(mediana = median(Cena.w.PLN)) %>% 
  filter(Cena.w.PLN > mediana, Cena.w.PLN < srednia) %>% 
  mutate(kraj = ifelse(as.character(Kraj.aktualnej.rejestracji) == as.character(Kraj.pochodzenia), "taki sam", "rozny")) %>% 
  filter(kraj == "rozny") %>% 
  nrow()
# Odp: Takich samochodów jest 1331.


# 4. Jaki jest rozstÄ™p miÄ™dzykwartylowy przebiegu (w kilometrach) PassatĂłw
# w wersji B6 i z benzynÄ… jako rodzajem paliwa?

auta2012 %>% 
  filter(Marka == "Volkswagen", Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>% 
  summarise(rozstep = IQR(Przebieg.w.km, na.rm = TRUE))
# Odp: Rozstęp międzykwartylowy wynosi 75977.5.


# 5. BiorÄ…c pod uwagÄ™ samochody, ktĂłrych cena jest podana w koronach czeskich,
# podaj Ĺ›redniÄ… z ich ceny brutto.
# Uwaga: JeĹ›li cena jest podana netto, naleĹĽy dokonaÄ‡ konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(brutto = ifelse(Brutto.netto == "netto", 1.02*Cena, Cena)) %>% 
  summarize(srednia_brutto = mean(brutto))
# Odp: Średnia wynosi 210678.3 koron czeskich.


# 6. KtĂłrych ChevroletĂłw z przebiegiem wiÄ™kszym niĹĽ 50 000 jest wiÄ™cej: tych
# ze skrzyniÄ… manualnÄ… czy automatycznÄ…? Dodatkowo, podaj model, ktĂłry najczÄ™Ĺ›ciej
# pojawia siÄ™ w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(1)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow == "manualna") %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(1)

auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow == "automatyczna") %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(1)
# Odp: Więcej jest Chevroletów z manualną skrzynią biegów. Wśród nich najwięcej jest modelu Lacetti, 
# a wśród tych z automatyczną skrzynią biegów najwięcej jest Corvette.


# 7. Jak zmieniĹ‚a siÄ™ mediana pojemnoĹ›ci skokowej samochodĂłw marki Mercedes-Benz,
# jeĹ›li weĹşmiemy pod uwagÄ™ te, ktĂłre wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  mutate(po_2003 = ifelse(Rok.produkcji > 2003, TRUE, FALSE)) %>% 
  group_by(po_2003) %>% 
  summarise(mediana_pojemnosci_skokowej = median(Pojemnosc.skokowa, na.rm = TRUE))
# Odp: Dla wyprodukowanych przed lub w 2003 i po nim mediana pojemności skokowej jest taka sama.


# 8. Jaki jest najwiÄ™kszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzÄ…cych z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>% 
  select(Przebieg.w.km) %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  head(1)
# Odp: Największy przebieg wśród tych samochodów wynosi 1e+09.


# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzÄ…cych z WĹ‚och?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  View() 
# Odp: Drugi najmniej popularny kolor to granatowy-metallic.


# 10. Jaka jest wartoĹ›Ä‡ kwantyla 0.25 oraz 0.75 pojemnoĹ›ci skokowej dla 
# samochodĂłw marki Volkswagen w zaleĹĽnoĹ›ci od tego, czy w ich wyposaĹĽeniu 
# dodatkowym znajdujÄ… siÄ™ elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(el_lusterka = ifelse(str_detect(Wyposazenie.dodatkowe, "el. lusterka"), TRUE, FALSE)) %>% 
  group_by(el_lusterka) %>% 
  reframe(quantile(Pojemnosc.skokowa, na.rm = TRUE))
# Odp: Dla samochodów bez elektrycznych lusterek kwantyle 0.25 i 0.75 to odpowiednio 1400 i 1900,
# a dla tych z lusterkami - 1892 i 1968.

