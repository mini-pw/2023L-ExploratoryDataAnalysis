library(stringr)
library(PogromcyDanych)
data(auta2012)


# 1. Rozwa¿aj¹c tylko obserwacje z PLN jako walutê (nie zwa¿aj¹c na 
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?

auta2012 %>% 
  filter(Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(mediana = median(Cena.w.PLN, na.rm = TRUE))

# Odp: 19600 (hybrydy nie by³y liczone do aut elektrycznych)


# 2. W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001 
# roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
# mechanicznych (KM) jest najwiêksza.

auta2012 %>% 
  mutate(przed_i_po = ifelse(Rok.produkcji <= 2001, "przed", "po")) %>% 
  group_by(Marka) %>% 
  summarise(mediana = median(KM, na.rm = TRUE)) %>% 
    top_n(1, mediana)

# Odp: Bugatti po 2001 (2009 rok) 1001KM


# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.


# srednia wiêksza od mediany

auta2012 %>% 
  filter(Kolor == "szary-metallic",
         median(auta2012$Cena.w.PLN, na.rm = TRUE) <= Cena.w.PLN 
         & Cena.w.PLN <= mean(auta2012$Cena.w.PLN, na.rm = TRUE),
         is.na(Kraj.aktualnej.rejestracji) == FALSE, is.na(Kraj.pochodzenia) == FALSE,
         as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) %>% 
  summarise(n = n())

 
# Odp: 1930

# 4. Jaki jest rozstêp miêdzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyn¹ jako rodzajem paliwa?

auta2012 %>% 
  filter(Marka == "Volkswagen", Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna",
         is.na(Przebieg.w.km) == FALSE ) %>% 
  summarise(kwartyle = IQR(Przebieg.w.km))
  
# Odp: 75977.5


# 5. Bior¹c pod uwagê samochody, których cena jest podana w koronach czeskich,
# podaj œredni¹ z ich ceny brutto.
# Uwaga: Jeœli cena jest podana netto, nale¿y dokonaæ konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(converter = ifelse(Brutto.netto == "brutto", 1, 1.02), prize_brutto = Cena*converter) %>% 
  summarise(srednia_cena_brutto = mean(prize_brutto, na.rm = TRUE))



# Odp: 210678.3 (36047.06 w z³otówkach)



# 6. Których Chevroletów z przebiegiem wiêkszym ni¿ 50 000 jest wiêcej: tych
# ze skrzyni¹ manualn¹ czy automatyczn¹? Dodatkowo, podaj model, który najczêœciej
# pojawia siê w obu przypadkach.

# czy wiecej manualna, czy automatyczna, sposób 1
auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km >= 50000)%>% 
  mutate(automatic = ifelse(Skrzynia.biegow == "automatyczna", 1, 0),
         manual = ifelse(Skrzynia.biegow == "manualna", 1, 0)) %>% 
  summarize(automatyczna = sum(automatic, na.rm = TRUE), manualna = sum(manual, na.rm = TRUE)) 

# czy wiecej manualna, czy automatyczna, sposób 2 (zapewne lepszy)
auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km >= 50000, Skrzynia.biegow != "")%>% 
  group_by(Skrzynia.biegow) %>% summarise(ilosc = n())

# których modeli z automatyczn¹ i manualn¹ najwiêcej
auta2012 %>% 
  filter(Marka == "Chevrolet", Przebieg.w.km >= 50000, 
         Skrzynia.biegow == "manualna" |Skrzynia.biegow == "automatyczna")%>% 
  select(Marka,Model, Skrzynia.biegow, Przebieg.w.km) %>% group_by(Skrzynia.biegow, Model) %>% 
  summarise(n = n()) %>% arrange(-n)


# Odp: wiêcej z manualn¹, najwiêcej ze skrzyni¹ manualn¹ jest Lacetti
# a z automatyczn¹ Corvette



# 7. Jak zmieni³a siê mediana pojemnoœci skokowej samochodów marki Mercedes-Benz,
# jeœli weŸmiemy pod uwagê te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>%
  mutate(przed_i_po = ifelse(Rok.produkcji <= 2003, "przed", "po")) %>% 
  group_by(przed_i_po) %>% 
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE))

# Odp: Mediana pojemnoœci skokowej zwiêkszy³a siê (z 1896 do 1900)


# 8. Jaki jest najwiêkszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodz¹cych z Niemiec?

auta2012  %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska",
         Kraj.pochodzenia == "Niemcy") %>%
  top_n(1, Przebieg.w.km)%>% 
  select(Marka, Przebieg.w.km, Kraj.aktualnej.rejestracji, Kraj.pochodzenia)

# Odp: Najwiêkszy przebieg to 1e+09 (1.000.000.000), w samochodzie marki Mercedes-Benz



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodz¹cych z W³och?

auta2012 %>% 
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy", Kolor != "") %>% 
  group_by(Kolor) %>% 
  summarise(liczba_aut = n()) %>% 
  arrange(liczba_aut)

# Odp: granatowy-metallic


# 10. Jaka jest wartoœæ kwantyla 0.25 oraz 0.75 pojemnoœci skokowej dla 
# samochodów marki Volkswagen w zale¿noœci od tego, czy w ich wyposa¿eniu 
# dodatkowym znajduj¹ siê elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>%
  mutate(czy_sa = ifelse(str_detect(Wyposazenie.dodatkowe, "el. lusterka"), TRUE, FALSE)) %>% 
  group_by(czy_sa) %>% 
  summarise(x = quantile(Pojemnosc.skokowa, probs = seq(0.25, 0.75, 0.5), na.rm = TRUE))

# Odp: gdy nie ma lusterek elektrycznych to wartoœæ kwantyla 0.25, wynosi 1400, 
# a kwantyla 0.75 wynosi 1900, odpowiednio gdy s¹ kwantyl 0.25 to 1892, a kwantyl 
# 0.75 to 1968

