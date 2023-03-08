install.packages("PogromcyDanych")
library(PogromcyDanych)
library(dplyr)
library(stringr)
data(auta2012)

# 1. Rozwa¿aj¹c tylko obserwacje z PLN jako walut¹ (nie zwa¿aj¹c na 
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny") %>% 
  arrange(Cena.w.PLN) %>% 
  select(Cena.w.PLN, Rodzaj.paliwa) %>% 
  summarise(median(Cena.w.PLN))

# Odp: Mediana ceny samochodów, które maj¹ napêd elektryczny wynosi 18900.

# 2. W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001 
# roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
# mechanicznych (KM) jest najwiêksza.

auta2012 %>% 
  mutate(rok = ifelse(Rok.produkcji >= 2001, "po 2001", "przed 2001")) %>%
  select(rok,  Marka, KM) %>% 
  group_by(Marka, rok) %>% 
  summarise(mediana = median(KM, na.rm = TRUE)) %>% 
  arrange(desc(mediana)) %>% 
  head(1)

# Odp: Bugatti po 2001



# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji


auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  select(Kolor, Cena.w.PLN, Kraj.aktualnej.rejestracji,Kraj.pochodzenia) %>% 
  filter(as.character(Kraj.pochodzenia)!= as.character(Kraj.aktualnej.rejestracji)) %>% 
  filter((Cena.w.PLN >= median(Cena.w.PLN) & Cena.w.PLN <= mean(Cena.w.PLN)) | (Cena.w.PLN <= median(Cena.w.PLN) & Cena.w.PLN >= mean(Cena.w.PLN))) %>%
  nrow()


# Odp: 1114



# 4. Jaki jest rozstêp miêdzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyn¹ jako rodzajem paliwa?

auta2012 %>% 
  filter(Marka == "Volkswagen" & Wersja == "B6" & Rodzaj.paliwa == "benzyna") %>% 
  summarise(IQR(Przebieg.w.km, na.rm = TRUE)) 
  

# Odp: 75977.5



# 5. Bior¹c pod uwagê samochody, których cena jest podana w koronach czeskich,
# podaj œredni¹ z ich ceny brutto.
# Uwaga: Jeœli cena jest podana netto, nale¿y dokonaæ konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(cena_ostatecnza = ifelse(Brutto.netto == "brutto", Cena, Cena*1.02)) %>% 
  select(Waluta, cena_ostatecnza) %>%
  summarise(mean(cena_ostatecnza))

# Odp: 210678.3 CZK



# 6. Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹? Dodatkowo, podaj model, który najczêœciej
# pojawia siê w obu przypadkach.


auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000, Skrzynia.biegow!= "") %>% 
  select(Marka, Przebieg.w.km, Skrzynia.biegow, Model) %>% 
  count(Skrzynia.biegow, Model) %>% 
  arrange(-n) %>% 
  head(10)
  

# Odp: z manualn¹, manualna - Lacetti, automatyczna - Corvette



# 7. Jak zmieni³a siê mediana pojemnoœci skokowej samochodów marki Mercedes-Benz,
# jeœli weŸmiemy pod uwagê te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  
  mutate(rok = ifelse(Rok.produkcji >= 2003, "po 2003", "przed 2003")) %>% 
  select(Marka, rok, Pojemnosc.skokowa) %>% 
  group_by(rok) %>% 
  count(rok, median(Pojemnosc.skokowa, na.rm = TRUE)) %>% 
  head()

  
# Odp: Nie zmienia siê i wynosi 2200.



# 8. Jaki jest najwiêkszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodz¹cych z Niemiec?
auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska" & Kraj.pochodzenia == "Niemcy" & !is.na(Przebieg.w.km)) %>% 
  select(Kraj.aktualnej.rejestracji, Kraj.pochodzenia, Przebieg.w.km) %>% 
  arrange(-Przebieg.w.km) %>% 
  head(1)


# Odp: 1e+09 (okoko³o biliona)



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodz¹cych z W³och?

x <- filter(auta2012, Marka == "Mitsubishi"& Kolor != "" & Kraj.pochodzenia == "Wlochy") %>% select(Kolor, Marka, Kraj.pochodzenia)
x <- count(x, Kolor) %>% arrange(n)
x

# Odp: Granatowy- metaliic


# 10. Jaka jest wartoœæ kwantyla 0.25 oraz 0.75 pojemnoœci skokowej dla 
# samochodów marki Volkswagen w zale¿noœci od tego, czy w ich wyposa¿eniu 
# dodatkowym znajduj¹ siê elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen"& !is.na(Pojemnosc.skokowa)) %>% 
  mutate(lusterka = str_detect(Wyposazenie.dodatkowe, "el. lusterka")) %>% 
  group_by(lusterka) %>% summarise(kwantyl25 = quantile(Pojemnosc.skokowa)[2],kwantyl75 = quantile(Pojemnosc.skokowa)[4]) 
  

# Odp: Gdy maj¹ lusterka - 1400, 1900, gdy nie maj¹ 1892,1968