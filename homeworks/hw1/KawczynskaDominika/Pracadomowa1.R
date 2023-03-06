library(PogromcyDanych)
data(auta2012)
library(stringr)



# 1. Rozwa¿aj¹c tylko obserwacje z PLN jako walut¹ (nie zwa¿aj¹c na 
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN" & Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(median(Cena))

# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001 
# roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
# mechanicznych (KM) jest najwiêksza.

auta2012 %>% 
  filter(Rok.produkcji != "" & !is.na(KM) &  Marka != "") %>% 
  mutate(po.2001 = ifelse(Rok.produkcji >= 2001, TRUE, FALSE)) %>% 
  group_by(Marka, po.2001) %>% 
  summarise(mediana = median(KM)) %>% 
  arrange(desc(mediana)) %>% 
  head(1) 

# Odp: Mediana liczby koni mechanicznych najwiêksza jak dla Bugatti wyprodukowanym
# w 2021 roku lub póŸniej i wynosi 1001.



# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.

auta2012 %>% 
  filter(Kolor == "szary-metallic" &
           between(Cena.w.PLN, median(Cena.w.PLN), mean(Cena.w.PLN)) &
           as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia)) %>% 
  summarise(n())

# Odp:1930



# 4. Jaki jest rozstêp miêdzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyn¹ jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat" &
           Wersja == "B6" &
           Rodzaj.paliwa == "benzyna") %>% 
  summarise(IQR(Przebieg.w.km, na.rm = TRUE))

# Odp: 75977.5



# 5. Bior¹c pod uwagê samochody, których cena jest podana w koronach czeskich,
# podaj œredni¹ z ich ceny brutto.
# Uwaga: Jeœli cena jest podana netto, nale¿y dokonaæ konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(cena.brutto = ifelse(Brutto.netto == "brutto",
                              Cena.w.PLN, Cena.w.PLN * 1.02)) %>% 
  summarise(mean(cena.brutto))

# Odp: 36047.06



# 6. Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹? Dodatkowo, podaj model, który najczêœciej
# pojawia siê w obu przypadkach.

# Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹?
auta2012 %>% 
  filter(Marka == "Chevrolet" &
           Przebieg.w.km > 50000 &
           Skrzynia.biegow != "") %>%
  group_by(Skrzynia.biegow) %>% 
  summarise(Ilosc = n()) %>% 
  arrange(desc(Ilosc)) %>% 
  slice_head(n = 1L)

# Model, który najczêœciej pojawia siê w obu przypadkach.
auta2012 %>% 
  filter(Marka == "Chevrolet" &
           Przebieg.w.km > 50000 &
           Skrzynia.biegow != "") %>%
  group_by(Skrzynia.biegow, Model) %>% 
  summarise(Ilosc = n()) %>% 
  arrange(desc(Ilosc)) %>% 
  slice_head(n = 1L)

# Odp: Chevroletów z przebiegiem wiêkszym ni¿ 50 000 z manualn¹ skrzyni¹ biegów 
# jest wiêcej. Corvette to model, który pojawia siê najczêœciej jako samochód
# z automatyczn¹ skrzyni¹ biegów, zaœ Lacetti- z manualn¹ skrzyni¹.



# 7. Jak zmieni³a siê mediana pojemnoœci skokowej samochodów marki Mercedes-Benz,
# jeœli weŸmiemy pod uwagê te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz" &
           Pojemnosc.skokowa != "") %>% 
  mutate(przed.2003 = ifelse(Rok.produkcji <= 2003, Pojemnosc.skokowa, NA)) %>% 
  mutate(po.2003 = ifelse(Rok.produkcji > 2003, Pojemnosc.skokowa, NA)) %>% 
  summarise(mediana_przed_2003 = median(przed.2003, na.rm = TRUE),
            mediana_po_2003 = median(po.2003, na.rm = TRUE))

# Odp: Mediana nie zmieni³a siê



# 8. Jaki jest najwiêkszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodz¹cych z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska" &
           Kraj.pochodzenia == "Niemcy" &
           Przebieg.w.km != "") %>% 
  summarise(max(Przebieg.w.km))

# Odp: 1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodz¹cych z W³och?

auta2012 %>% 
  filter(Marka == "Mitsubishi" &
           Kraj.pochodzenia == "Wlochy" &
           Kolor != "") %>% 
  group_by(Kolor) %>% 
  summarise(Ilosc = n()) %>% 
  arrange((Ilosc))

# Odp: Drugi, najmniej popularnym kolorem jest granatowy_metallic.



# 10. Jaka jest wartoœæ kwantyla 0.25 oraz 0.75 pojemnoœci skokowej dla 
# samochodów marki Volkswagen w zale¿noœci od tego, czy w ich wyposa¿eniu 
# dodatkowym znajduj¹ siê elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen" & 
           str_detect(Wyposazenie.dodatkowe, "el. lusterka") == TRUE) %>% 
  summarise(kwantyl_0.25 = quantile(Pojemnosc.skokowa, 0.25, na.rm = TRUE),
            kwantyl_0.75 = quantile(Pojemnosc.skokowa, 0.75, na.rm = TRUE))
  
# Odp:kwantyl_0.25 kwantyl_0.75
#      1892.25         1968