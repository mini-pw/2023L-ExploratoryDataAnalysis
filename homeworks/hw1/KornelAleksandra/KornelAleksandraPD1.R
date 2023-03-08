install.packages("PogromcyDanych")
library(PogromcyDanych)
library(tidyr)



# 1. Rozwa¿aj¹c tylko obserwacje z PLN jako walut¹ (nie zwa¿aj¹c na 
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?

auta2012 %>% 
  filter(Waluta=="PLN",Rodzaj.paliwa=="naped elektryczny" ) %>% 
  summarise(mediana = median(Cena, na.rm = TRUE))
  

# Odp: 18900 (zl)



# 2. W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001 
# roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
# mechanicznych (KM) jest najwiêksza.

auta2012 %>% 
  mutate(wyprodukowano = ifelse(Rok.produkcji<"2001", "przed 2001", "w 2001 lub po")) %>% 
  group_by(Marka, wyprodukowano) %>% 
  summarise(mediana = median(KM, na.rm = TRUE)) %>% 
  arrange(desc(mediana)) %>%
  head(1)
  

# Odp: Bugatti w 2001 lub po



# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji

auta2012 %>% 
  filter(Kolor == "szary-metallic") %>% 
  mutate(mediana=median(Cena.w.PLN, na.rm=TRUE),
         srednia=mean(Cena.w.PLN,na.rm=TRUE)) %>% 
  filter((srednia>Cena.w.PLN & Cena.w.PLN>mediana) |
         (srednia<Cena.w.PLN & Cena.w.PLN<mediana),
         Kraj.pochodzenia!=factor(Kraj.aktualnej.rejestracji, levels(Kraj.pochodzenia))) %>%
  summarize(n=n())

# Odp: 1331



# 4. Jaki jest rozstêp miêdzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyn¹ jako rodzajem paliwa?

auta2012 %>% 
  filter(Model=="Passat",Wersja=="B6",Rodzaj.paliwa=="benzyna") %>%
  select(Przebieg.w.km) %>% 
  summarise(IQR=quantile(Przebieg.w.km, na.rm=TRUE)[4]-quantile(Przebieg.w.km, na.rm=TRUE)[2])
  

# Odp: 75977,5



# 5. Bior¹c pod uwagê samochody, których cena jest podana w koronach czeskich,
# podaj œredni¹ z ich ceny brutto.
# Uwaga: Jeœli cena jest podana netto, nale¿y dokonaæ konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta=="CZK") %>% 
  mutate(cena_brutto = ifelse(Brutto.netto == "brutto", Cena, Cena*1.02)) %>% 
  summarise(srednia = mean(cena_brutto, na.rm = TRUE))

# Odp: 210678.3 (CZK)



# 6. Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹? Dodatkowo, podaj model, który najczêœciej
# pojawia siê w obu przypadkach.

auta2012 %>% 
  filter(Marka=="Chevrolet",Przebieg.w.km>50000,
         Skrzynia.biegow=="automatyczna"|Skrzynia.biegow=="manualna") %>% 
  group_by(Skrzynia.biegow) %>%
  summarise(n=n()) %>%
  arrange(-n)

auta2012 %>% 
  filter(Marka=="Chevrolet",Przebieg.w.km>50000,
         Skrzynia.biegow=="automatyczna"|Skrzynia.biegow=="manualna") %>% 
  group_by(Skrzynia.biegow, Model) %>%
  summarise(maks=n()) %>% 
  top_n(1,maks) 


  

# Odp: wiêcej ze skrzyni¹ manualn¹,
#      dla automatycznej najczêstszym modelem jest Corvette,
#      dla manualnej najczêstszym modelem jest Lacetti



# 7. Jak zmieni³a siê mediana pojemnoœci skokowej samochodów marki Mercedes-Benz,
# jeœli weŸmiemy pod uwagê te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>%
  filter(Marka=="Mercedes-Benz") %>% 
  mutate(wyprodukowano = ifelse(Rok.produkcji<=2003, "przed 2003 lub w 2003", "po 2003")) %>% 
  group_by(wyprodukowano) %>%
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE))



# Odp: nie zmieni³a siê, w obu przypadkach wynosi 2200



# 8. Jaki jest najwiêkszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodz¹cych z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji=="Polska", Kraj.pochodzenia=="Niemcy") %>% 
  arrange(desc(Przebieg.w.km)) %>% 
  head(1) %>% 
  select(Przebieg.w.km)


# Odp: 1e+09 (999999999 km)



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodz¹cych z W³och?

auta2012 %>%
  filter(Marka=="Mitsubishi", Kraj.pochodzenia=="Wlochy") %>% 
  group_by(Kolor) %>% 
  summarise(n=n()) %>% 
  arrange(n) 



# Odp: granatowy-metallic (najmniej popularne s¹ 4 kolory ex aequo, wiêc uznajê,
#      ¿e ten jest drugi najmniej popularny, chociaz w wyniku jest na 5 miejscu)



# 10. Jaka jest wartoœæ kwantyla 0.25 oraz 0.75 pojemnoœci skokowej dla 
# samochodów marki Volkswagen w zale¿noœci od tego, czy w ich wyposa¿eniu 
# dodatkowym znajduj¹ siê elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(elektryczne_lusterka = grepl("el. lusterka",Wyposazenie.dodatkowe)) %>% 
  group_by(elektryczne_lusterka) %>% 
  summarise(kwantyl_0.25 = quantile(Pojemnosc.skokowa, na.rm=TRUE)[2],
            kwantyl_0.75=quantile(Pojemnosc.skokowa,na.rm=TRUE)[4])



# Odp: z lusterkami elektrycznymi: kwantyl 0.25 - 1892, kwantyl 0.75 - 1968
#      bez lusterek elektrycznych: kwantyl 0.25 - 1400, kwantyl 0.75 - 1900
