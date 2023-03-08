
library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

auta2012 %>% 
  filter(Waluta == "PLN", Rodzaj.paliwa == "naped elektryczny") %>% 
  summarise(mediana = median(Cena.w.PLN))

# Odp: 18900 PLN

# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

auta2012 %>% 
  mutate(century = if_else(Rok.produkcji > 2000, "XXI", "XX")) %>% 
  filter(!is.na(KM)) %>% 
  group_by(century, Marka) %>% 
  summarise(mediana_KM = median(KM)) %>%  
  arrange(desc(mediana_KM)) %>% 
  select(century, Marka) %>% 
  head(1)
  
# Odp: Póżniej i Bugatti


# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.
# UWAGA: Nie rozpatrujemy obserwacji z NA w kraju aktualnej rejestracji


#Założyłem, że brak wiadomosci o kraju pochodzenia liczy się jako inny kraj niż konkretny kraj
#rejestracji

mediana <- auta2012 %>%
  filter(Kolor == "szary-metallic") %>% 
  summarise(mediana = median(Cena.w.PLN))

srednia <- auta2012 %>%
  filter(Kolor == "szary-metallic") %>% 
  summarise(srednia = mean(Cena.w.PLN))

auta2012 %>% 
  filter(Kolor == "szary-metallic" & Cena.w.PLN < as.numeric(srednia) &
           Cena.w.PLN > as.numeric(mediana)) %>% 
  mutate(Kraj.aktualnej.rejestracji = as.character(Kraj.aktualnej.rejestracji),
         Kraj.pochodzenia = as.character(Kraj.pochodzenia)) %>%
  filter(Kraj.aktualnej.rejestracji != Kraj.pochodzenia) %>% 
  filter(Kraj.aktualnej.rejestracji != "") %>% summarise(n = n())
  
# Odp: 635


# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

przebiegi <- auta2012 %>% 
  filter(Model == "Passat", Rodzaj.paliwa == "benzyna", Wersja == "B6",
         !is.na(Przebieg.w.km)) %>% 
  arrange(Przebieg.w.km) %>% select(Przebieg.w.km)  
  
kwantyle <- quantile(przebiegi[,1])
kwantyle[4] - kwantyle[2]
  

# Odp: 75977.5 (km)


# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(Cena = ifelse(Brutto.netto == "netto", 1.02*Cena, Cena)) %>% 
  summarise(mean = mean(Cena))

# Odp: Średnia ich cena w koronach czeskiech to 210678.3.



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.
library(ggplot2)

auta2012 %>% 
  filter(Przebieg.w.km > 50000) %>% 
  mutate(Skrzynia.biegow = case_when(Skrzynia.biegow == "manualna" ~ "manualna",
                                     Skrzynia.biegow == "automatyczna" ~ "automatyczna",
                                     T ~ "OTHER")) %>% 
  filter(Skrzynia.biegow != "OTHER") %>% 
  ggplot(aes(x= Skrzynia.biegow)) + geom_bar()

auta2012 %>% 
  filter(Przebieg.w.km > 50000) %>% 
  mutate(Skrzynia.biegow = case_when(Skrzynia.biegow == "automatyczna" ~ "automatyczna",
                                     T ~ "OTHER")) %>% 
  filter(Skrzynia.biegow != "OTHER") %>% 
  mutate(Model = fct_infreq(fct_lump(Model, n = 5, other_level = "meh"))) %>% 
  filter(Model != "meh") %>% 
  ggplot(aes(x= Model)) + geom_bar() + facet_grid(cols = vars(Skrzynia.biegow))

auta2012 %>% 
  filter(Przebieg.w.km > 50000) %>% 
  mutate(Skrzynia.biegow = case_when(Skrzynia.biegow == "manualna" ~ "manualna",
                                     T ~ "OTHER")) %>% 
  filter(Skrzynia.biegow != "OTHER") %>% 
  mutate(Model = fct_infreq(fct_lump(Model, n = 5, other_level = "meh"))) %>% 
  filter(Model != "meh") %>% 
  ggplot(aes(x= Model)) + geom_bar() + facet_grid(cols = vars(Skrzynia.biegow))
  



# Odp: Więcej jest samochodów ze skrzynią manualną. Dla manualnej najpopularniejsza jest Astra, a dla
#automatycznej jest to A6



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji > 2002,
         !is.na(Pojemnosc.skokowa)) %>% 
  group_by(Rok.produkcji) %>% 
  summarise(Median = median(Pojemnosc.skokowa)) %>% 
  ggplot(aes(x = Rok.produkcji, y = Median)) + geom_point(size = 3)

# Odp: Pomiędzy rokiem 2005 i 2006 zwiększyła się ona o około 1/3, w latach 2011-12 się najpierw
#zmniejszyła drastycznie (do stanu przed 2006), a nastęnie zwiększyła.



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?


auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>% 
  arrange(desc(Przebieg.w.km)) %>% select(Przebieg.w.km) %>%  head(1) -> k

# Odp: Bilion kilometrów



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

auta2012 %>% 
  filter(Kraj.pochodzenia == "Wlochy", Marka == "Mitsubishi") %>%  
  group_by(Kolor) %>%  summarise(n = n()) %>% 
  arrange(desc(n))


# Odp: Kolor zielony, srebrny, grafitowy-metallic i czerwony-metallic pojawiają się pojedynczo



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

library(stringr)

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(Czy_lusterka = 
    str_detect(Wyposazenie.dodatkowe, "el. lusterka")) %>% 
  filter(!is.na(Pojemnosc.skokowa)) %>% 
  group_by(Czy_lusterka) %>% summarise(kwant25 = quantile(Pojemnosc.skokowa)[2],
                                       kwant75 = quantile(Pojemnosc.skokowa)[4])
  

# Odp: W przypadku gdy elektryczne lusterka nie znajdują się 
#w dodatkowym wyposażeniu wartosc kwantyli wynosi 1400 i 1900,
# a gdy znajdują się odpowiednie kwantyle wynoszą 1892 i 1968.



