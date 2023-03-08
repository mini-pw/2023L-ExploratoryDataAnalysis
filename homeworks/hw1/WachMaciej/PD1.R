library("dplyr")
library("PogromcyDanych")
library("stringi")
data(auta2012)

# 1. Rozwa¿aj¹c tylko obserwacje z PLN jako walut¹ (nie zwa¿aj¹c na 
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(mediana.ceny = median(Cena, na.rm = TRUE))

# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001 
# roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
# mechanicznych (KM) jest najwiêksza.

auta2012 %>% 
  mutate(w.2001.lub.po = ifelse(Rok.produkcji >=2001,"tak","nie")) %>% 
  group_by(Marka, w.2001.lub.po) %>% 
  summarise(mediana.KM = median(KM, na.rm = TRUE)) %>% 
  arrange(-mediana.KM) %>% 
  head(1)

# Odp: Bugatti, wyp. po 2001r, mediana KM: 1001



# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  filter((Cena.w.PLN > median(Cena.w.PLN) & Cena.w.PLN < mean(Cena.w.PLN)) | (Cena.w.PLN < median(Cena.w.PLN) & Cena.w.PLN > mean(Cena.w.PLN))) %>% 
  filter(! Kraj.pochodzenia %s===% Kraj.aktualnej.rejestracji) %>% 
  summarise(n = n())

# Odp: 1331



# 4. Jaki jest rozstêp miêdzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyn¹ jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>% 
  summarise(RQ = quantile(Przebieg.w.km, probs = 0.75, na.rm = T) - quantile(Przebieg.w.km, probs = 0.25, na.rm = T))

# Odp: 75977.5



# 5. Bior¹c pod uwagê samochody, których cena jest podana w koronach czeskich,
# podaj œredni¹ z ich ceny brutto.
# Uwaga: Jeœli cena jest podana netto, nale¿y dokonaæ konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(cena.brutto = ifelse(Brutto.netto == "brutto", Cena, 1.02 * Cena)) %>% 
  summarise(srednia.brutto = mean(cena.brutto))

# Odp:	210678.3



# 6. Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹? Dodatkowo, podaj model, który najczêœciej
# pojawia siê w obu przypadkach.

auta2012 %>% 
  filter(Przebieg.w.km > 50000, Marka == "Chevrolet") %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

auta2012 %>% 
  filter(Przebieg.w.km > 50000, Marka == "Chevrolet") %>% 
  group_by(Model, Skrzynia.biegow) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

# Odp: 1) manualna 2) manualna - Lacetti, automatyczna - Corvette



# 7. Jak zmieni³a siê mediana pojemnoœci skokowej samochodów marki Mercedes-Benz,
# jeœli weŸmiemy pod uwagê te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  mutate(okres.produkcji = ifelse(Rok.produkcji <= 2003, "przed lub w 2003", "po 2003")) %>% 
  group_by(okres.produkcji) %>% 
  summarise(mediana.poj.skokowej = median(Pojemnosc.skokowa, na.rm = T))

# Odp: Taka sama (2200)



# 8. Jaki jest najwiêkszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodz¹cych z Niemiec?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Niemcy", Kraj.aktualnej.rejestracji == "Polska") %>%
  arrange(-Przebieg.w.km) %>% 
  select(Przebieg.w.km) %>% 
  head(1)

# Odp: 1e+09 / 999999999



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodz¹cych z W³och?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Wlochy", Marka == "Mitsubishi") %>%
  group_by(Kolor) %>% 
  summarise(n = n()) %>% 
  arrange(n)

# Odp: granatowy-metallic



# 10. Jaka jest wartoœæ kwantyla 0.25 oraz 0.75 pojemnoœci skokowej dla 
# samochodów marki Volkswagen w zale¿noœci od tego, czy w ich wyposa¿eniu 
# dodatkowym znajduj¹ siê elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(ma.el.lusterka = ifelse(stri_detect_fixed(Wyposazenie.dodatkowe, "el. lusterka")==TRUE, "tak", "nie")) %>% 
  group_by(ma.el.lusterka) %>% 
  summarise(Q1 = quantile(Pojemnosc.skokowa, probs = 0.25, na.rm = T),
            Q3 = quantile(Pojemnosc.skokowa, probs = 0.75, na.rm = T))

# Odp: dla maj¹cych el. lusterka: kwantyl 0.25 = 1892, kwantyl 0.75 = 1968
# dla nie maj¹cych el. lusterek: kwantyl 0.25 = 1400, kwantyl 0.75 = 1900
