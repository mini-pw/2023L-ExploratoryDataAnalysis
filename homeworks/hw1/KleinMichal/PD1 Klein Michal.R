library(dplyr)
library(tidyr)
library(PogromcyDanych)
data(auta2012)

# 1. Rozważając tylko obserwacje z PLN jako walutą (nie zważając na 
# brutto/netto): jaka jest mediana ceny samochodów, które mają napęd elektryczny?

z1 <- auta2012 %>% 
  filter(Rodzaj.paliwa == "naped elektryczny" & Waluta == "PLN")

z1 <- z1$Cena

median(z1, na.rm = TRUE)

# Odp: 18900



# 2. W podziale samochodów na marki oraz to, czy zostały wyprodukowane w 2001 
# roku i później lub nie, podaj kombinację, dla której mediana liczby koni
# mechanicznych (KM) jest największa.

z2 <- auta2012 %>%
  mutate(If2001 = if_else(Rok.produkcji >= 2001, TRUE, FALSE)) %>% 
  group_by(If2001, Marka) %>% 
  summarise(Konie = median(KM, na.rm = TRUE)) %>% 
  arrange(desc(Konie)) %>% 
  head(1)

# Odp: Bugatti, rok >=2001



# 3. Spośród samochodów w kolorze szary-metallic, których cena w PLN znajduje się
# pomiędzy jej średnią a medianą (nie zważając na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny niż kraj aktualnej rejestracji i poodaj ich liczbę.

z3 <- auta2012 %>% 
  filter(Kolor == "szary-metallic")

z3w <- z3$Cena.w.PLN
z3sr <- mean(z3w, na.rm = TRUE)
z3med <- median(z3w, na.rm = TRUE)

z3 <- z3 %>%
  filter(Cena.w.PLN > min(z3sr, z3med) & Cena.w.PLN < max(z3sr, z3med) & as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia))
  
count(z3)  
  
# Odp: 1331



# 4. Jaki jest rozstęp międzykwartylowy przebiegu (w kilometrach) Passatów
# w wersji B6 i z benzyną jako rodzajem paliwa?

z4 <- auta2012 %>% 
  filter(Model == "Passat" & Wersja == "B6" & Rodzaj.paliwa == "benzyna")

z4 <- z4$Przebieg.w.km

z4kw <- quantile(z4, probs = c(0.25, 0.75), na.rm=TRUE)
z4kw[2]-z4kw[1]

# Odp: 75977.5



# 5. Biorąc pod uwagę samochody, których cena jest podana w koronach czeskich,
# podaj średnią z ich ceny brutto.
# Uwaga: Jeśli cena jest podana netto, należy dokonać konwersji na brutto (podatek 2%).

z5 <- auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(Brutto = ifelse(Brutto.netto == "netto", Cena*1.02, Cena))

z5 <- z5$Brutto

z5sr <- mean(z5, na.rm = TRUE)
z5sr

# Odp: 210678.3



# 6. Których Chevroletów z przebiegiem większym niż 50 000 jest więcej: tych
# ze skrzynią manualną czy automatyczną? Dodatkowo, podaj model, który najczęściej
# pojawia się w obu przypadkach.

z6a <- auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000 & Skrzynia.biegow == "manualna")
count(z6a)

z6a <- z6a %>% 
  group_by(Model) %>% 
  count(Model) %>% 
  arrange(desc(n)) %>% 
  head(1)

z6b <- auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000 & Skrzynia.biegow == "automatyczna")
count(z6b)

z6b <- z6b %>% 
  group_by(Model) %>% 
  count(Model) %>% 
  arrange(desc(n)) %>% 
  head(1)

# Odp: Manualna; manualna - Lacetti, automatyczna - Corvette



# 7. Jak zmieniła się mediana pojemności skokowej samochodów marki Mercedes-Benz,
# jeśli weźmiemy pod uwagę te, które wyprodukowano przed lub w roku 2003 i po nim?

z7a <- auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji <= 2003)
z7a <- z7a$Pojemnosc.skokowa
z7amed <- median(z7a, na.rm = TRUE)

z7b <- auta2012 %>% 
  filter(Marka == "Mercedes-Benz", Rok.produkcji > 2003)
z7b <- z7b$Pojemnosc.skokowa
z7bmed <- median(z7b, na.rm = TRUE)

abs(z7bmed-z7amed)

# Odp: Nie zmieniła się.



# 8. Jaki jest największy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzących z Niemiec?

z8 <- auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska" & Kraj.pochodzenia == "Niemcy") %>% 
  top_n(1, Przebieg.w.km)

# Odp: 10^9



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzących z Włoch?

z9 <- auta2012 %>% 
  filter(Marka == "Mitsubishi" & Kraj.pochodzenia == "Wlochy") %>% 
  group_by(Kolor) %>% 
  count(Kolor) %>% 
  arrange(n)

# Odp: granatowy-metallic



# 10. Jaka jest wartość kwantyla 0.25 oraz 0.75 pojemności skokowej dla 
# samochodów marki Volkswagen w zależności od tego, czy w ich wyposażeniu 
# dodatkowym znajdują się elektryczne lusterka?

z10 <- auta2012 %>%
  filter(Marka == "Volkswagen")

ElekLust <- unlist(gregexpr("el. lusterka", z10$Wyposazenie.dodatkowe))

z10$CzyElLusterka <- ElekLust

z10a <- z10 %>% 
  filter(CzyElLusterka != -1)
z10a <- z10a$Pojemnosc.skokowa
quantile(z10a, probs = c(0.25, 0.75), na.rm = TRUE)

z10b <- z10 %>% 
  filter(CzyElLusterka == -1)
z10b <- z10b$Pojemnosc.skokowa
quantile(z10b, probs = c(0.25, 0.75), na.rm = TRUE)

# Odp: Z elektrycznymi lusterkami: 0.25 - 1892.25, 0.75 - 1968; bez elektrycznych lusterek: 0.25 - 1400, 0.75 - 1900)
