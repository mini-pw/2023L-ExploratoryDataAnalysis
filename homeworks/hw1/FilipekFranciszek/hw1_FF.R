library(PogromcyDanych)
library(dplyr)
install.packages('stringi')
library('stringi')
data(auta2012)

# W kodzie wypisujê czasami wiêcej wierszy niz (teoretycznie potrzeba), zeby sprawdzic, czy np. 
# nie ma tak naprawdê 2 tak samo popularnych modeli (z jakiejs kategorii)

# 1. Rozwa¿aj¹c tylko obserwacje z PLN jako walut¹ (nie zwa¿aj¹c na 
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?
auta2012 %>%
  filter(Waluta == 'PLN'& Rodzaj.paliwa == 'naped elektryczny') %>%
  summarise(mediana = median(Cena))



# Odp: Ta mediana wynosi 18900 zl.



# 2. W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001 
# roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
# mechanicznych (KM) jest najwiêksza.
auta2012 %>% 
  filter(Rok.produkcji >= 2001) %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM)) %>% 
  arrange(desc(mediana)) %>% 
  head(3)
auta2012 %>% 
  filter(Rok.produkcji < 2001) %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM)) %>% 
  arrange(desc(mediana)) %>% 
  head(3)

# Odp: Mediana  jest najwiêksza (560 KM), gdy bierzemy pod uwagê samochody wyprodukowane przed 2001
#      roku przez firmê Bugatti.



# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.
auta2012 %>% 
  filter(Kolor == 'szary-metallic') %>% 
  summarise(mediana = median(Cena.w.PLN), srednia = mean(Cena.w.PLN))

srednia <- 44341.41
mediana <- 27480

auta2012 %>% 
  filter(Kolor == 'szary-metallic') %>%
  filter(Cena.w.PLN <= srednia , Cena.w.PLN >= mediana) %>% 
  filter(as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia)) %>% 
  count()



# Odp: 1331



# 4. Jaki jest rozstêp miêdzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyn¹ jako rodzajem paliwa?
auta2012 %>% 
  filter(Rodzaj.paliwa == 'benzyna'& Model == 'Passat'& Wersja == "B6") %>% 
  filter(Przebieg.w.km != '') %>% 
  summarise(Rozstep = IQR(Przebieg.w.km))


# Odp: Rostep miedzykwartylowy wynosi 75977.5



# 5. Bior¹c pod uwagê samochody, których cena jest podana w koronach czeskich,
# podaj œredni¹ z ich ceny brutto.
# Uwaga: Jeœli cena jest podana netto, nale¿y dokonaæ konwersji na brutto (podatek 2%).
auta2012 %>% 
  filter(Waluta == 'CZK') %>% 
  mutate(Cena_brutto = ifelse(Brutto.netto == 'brutto', Cena, Cena*1.02)) %>% 
  summarise(srednia_brutto = mean(Cena_brutto))


# Odp: Œrednia cena brutto tych aut to 210678.3 CZK



# 6. Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹? Dodatkowo, podaj model, który najczêœciej
# pojawia siê w obu przypadkach.
auta2012 %>% 
  filter(Marka == 'Chevrolet', Przebieg.w.km > 50000, Skrzynia.biegow == 'manualna') %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  select(Model,n) %>% 
  head(10)

auta2012 %>% 
  filter(Marka == 'Chevrolet', Przebieg.w.km > 50000, Skrzynia.biegow == 'manualna') %>% 
  count()

auta2012 %>% 
  filter(Marka == 'Chevrolet', Przebieg.w.km > 50000, Skrzynia.biegow == 'automatyczna') %>% 
  group_by(Model) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  select(Model,n) %>% 
  head(10)

auta2012 %>% 
  filter(Marka == 'Chevrolet', Przebieg.w.km > 50000, Skrzynia.biegow == 'automatyczna') %>% 
  count()

# Odp: Wiêcej jest tych Chevroletow z przebiegiem wiêkszym od 50000 km, które maj¹ manualna skrzynie biegów.
#      Z tych które mia³y manualn¹ skrzyniê, najczêœciej wyst¹pi³ model Lacetti, a z automatycznych Corvette.



# 7. Jak zmieni³a siê mediana pojemnoœci skokowej samochodów marki Mercedes-Benz,
# jeœli weŸmiemy pod uwagê te, które wyprodukowano przed lub w roku 2003 i po nim?
auta2012 %>% 
  filter(Marka == 'Mercedes-Benz', Rok.produkcji <= 2003) %>%
  filter(!is.na(Pojemnosc.skokowa)) %>% 
  summarise(mediana = median(Pojemnosc.skokowa))

auta2012 %>% 
  filter(Marka == 'Mercedes-Benz', Rok.produkcji > 2003) %>%
  filter(!is.na(Pojemnosc.skokowa)) %>% 
  summarise(mediana = median(Pojemnosc.skokowa))

# Odp: Nie zmieni³a siê.



# 8. Jaki jest najwiêkszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodz¹cych z Niemiec?
auta2012 %>% 
  filter(Kraj.pochodzenia == 'Niemcy', Kraj.aktualnej.rejestracji == 'Polska') %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  select(Przebieg.w.km) %>% 
  head(1)


# Odp: 1e+09 km, no nieŸle 



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodz¹cych z W³och?
auta2012 %>% 
  filter(Marka == 'Mitsubishi', Kraj.pochodzenia == 'Wlochy') %>% 
  group_by(Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  select(Kolor,n) %>% 
  head(10)


# Odp: Okazuje siê, ¿e 4 ro¿ne kolory zajmuj¹ 1 miejsce pod k¹tem najmniejszej popularnoœci,
#      a na 2 miejscu jest 1 kolor. Ten kolor to granatowy-metallic.



# 10. Jaka jest wartoœæ kwantyla 0.25 oraz 0.75 pojemnoœci skokowej dla 
# samochodów marki Volkswagen w zale¿noœci od tego, czy w ich wyposa¿eniu 
# dodatkowym znajduj¹ siê elektryczne lusterka?
auta2012 %>% 
  filter(Marka == 'Volkswagen', stri_detect_fixed(Wyposazenie.dodatkowe, 'el. lusterka') == TRUE) %>%
  na.omit() %>% 
  summarise(q1 = quantile(Pojemnosc.skokowa, probs = 0.25), q3 = quantile(Pojemnosc.skokowa, probs = 0.75))

auta2012 %>% 
  filter(Marka == 'Volkswagen', stri_detect_fixed(Wyposazenie.dodatkowe, 'el. lusterka') == FALSE) %>%
  na.omit() %>% 
  summarise(q1 = quantile(Pojemnosc.skokowa, probs = 0.25), q3 = quantile(Pojemnosc.skokowa, probs = 0.75))  


# Odp: Dla samochodów z el. lusterkami: kwartyl 0.25: 1896, kwartyl 0.75: 1968, bez el. lusterek:
#      kwartyl 0.25: 1391, kwartyl 0.75: 1900.
