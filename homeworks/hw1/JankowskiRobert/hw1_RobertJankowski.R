library(PogromcyDanych)
data(auta2012)

# 1. RozwaÅ¼ajÄ…c tylko obserwacje z PLN jako walutÄ… (nie zwaÅ¼ajÄ…c na 
# brutto/netto): jaka jest mediana ceny samochodÃ³w, ktÃ³re majÄ… napÄ™d elektryczny?

auta2012 %>%
  select(Cena.w.PLN, Rodzaj.paliwa) %>%
  filter(Rodzaj.paliwa == "naped elektryczny") %>%
  summarise(mediana_ceny = median(Cena.w.PLN))

# Odp:
# Mediana ceny samochodów z napêdem elektrycznym wynosi 19600 PLN


# 2. W podziale samochodÃ³w na marki oraz to, czy zostaÅ‚y wyprodukowane w 2001 
# roku i pÃ³Åºniej lub nie, podaj kombinacjÄ™, dla ktÃ³rej mediana liczby koni
# mechanicznych (KM) jest najwiÄ™ksza.

auta2012 %>%
  group_by(Marka,Rok.produkcji >= 2001, Rok.produkcji < 2001) %>%
  summarise(mediana_KM = median(KM, na.rm = TRUE)) %>%
  arrange(-mediana_KM)

# Odp: Bugatti wyprodukowane po lub w 2001 roku maj¹ najwiêksz¹ medianê KM.


# 3. SpoÅ›rÃ³d samochodÃ³w w kolorze szary-metallic, ktÃ³rych cena w PLN znajduje siÄ™
# pomiÄ™dzy jej Å›redniÄ… a medianÄ… (nie zwaÅ¼ajÄ…c na brutto/netto), wybierz te, 
# ktÃ³rych kraj pochodzenia jest inny niÅ¼ kraj aktualnej rejestracji i poodaj ich liczbÄ™.

auta2012 %>%
  mutate(mediana_ceny = median(Cena.w.PLN),srednia_ceny = mean(Cena.w.PLN)) %>%
  filter(Kolor == "szary-metallic", 
         (Cena.w.PLN > mediana_ceny & Cena.w.PLN < srednia_ceny) |
         (Cena.w.PLN > srednia_ceny & Cena.w.PLN < mediana_ceny)) %>%
  mutate(pochodzenie_inne_niz_rejestracja = as.character(Kraj.aktualnej.rejestracji) != as.character(Kraj.pochodzenia)) %>% 
  filter(pochodzenie_inne_niz_rejestracja) %>%
  #select(Marka,Model,Kolor,Cena.w.PLN,mediana_ceny,srednia_ceny,Kraj.aktualnej.rejestracji,Kraj.pochodzenia) %>%
  summarise(n=n())


# Odp: Jest takich aut 1849. (przyj¹³em, ¿e medianê i œredni¹ liczymy ze wszystkich aut, a nie tylko w kolorze szary-metallic)



# 4. Jaki jest rozstÄ™p miÄ™dzykwartylowy przebiegu (w kilometrach) PassatÃ³w
# w wersji B6 i z benzynÄ… jako rodzajem paliwa?

auta2012 %>%
  filter(Model == "Passat", Wersja == "B6", Rodzaj.paliwa == "benzyna") %>%
  summarize(q1 = quantile(Przebieg.w.km, probs = 0.25, na.rm = TRUE), 
            q3 = quantile(Przebieg.w.km, probs = 0.75, na.rm = TRUE)) %>%
  transmute(rozstep_miedzykwartylowy = q3-q1)
  
  
# Odp: Rozstêp miêdzykwartylowy wynosi 75977.5 km.



# 5. BiorÄ…c pod uwagÄ™ samochody, ktÃ³rych cena jest podana w koronach czeskich,
# podaj Å›redniÄ… z ich ceny brutto.
# Uwaga: JeÅ›li cena jest podana netto, naleÅ¼y dokonaÄ‡ konwersji na brutto (podatek 2%).
auta2012 %>%
  filter(Waluta == "CZK") %>%
  mutate(cena_brutto = ifelse(Brutto.netto == "brutto", Cena, 1.02 * Cena)) %>%
  summarize(srednia_ceny_brutto = mean(cena_brutto))


# Odp: Œrednia ceny brutto to 210678.3 CZK.



# 6. KtÃ³rych ChevroletÃ³w z przebiegiem wiÄ™kszym niÅ¼ 50 000 jest wiÄ™cej: tych
# ze skrzyniÄ… manualnÄ… czy automatycznÄ…? Dodatkowo, podaj model, ktÃ³ry najczÄ™Å›ciej
# pojawia siÄ™ w obu przypadkach.


auta2012 %>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>%
  group_by(Skrzynia.biegow) %>%
  summarise(n=n())

auta2012 %>%
  filter(Marka == "Chevrolet", Przebieg.w.km > 50000) %>%
  group_by(Skrzynia.biegow) %>% 
  count(Model) %>%
  top_n(1)

# Odp: Wiêcej jest tych ze skrzyni¹ manualn¹ (336 > 112), najczêstszy model
# z automatyczn¹ szkrzyni¹ to Corvette, a z manualn¹ to Lacetti.



# 7. Jak zmieniÅ‚a siÄ™ mediana pojemnoÅ›ci skokowej samochodÃ³w marki Mercedes-Benz,
# jeÅ›li weÅºmiemy pod uwagÄ™ te, ktÃ³re wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>%
  filter(Marka == "Mercedes-Benz") %>%
  mutate(Wiek = ifelse(Rok.produkcji <= 2003,"starszy","nowszy")) %>%
  group_by(Wiek) %>%
  summarize(mediana_pojemnosci = median(Pojemnosc.skokowa, na.rm = TRUE))
  


# Odp: Mediana pojemnoœci skokowej siê nie zmieni³a i wynosi 2200.



# 8. Jaki jest najwiÄ™kszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzÄ…cych z Niemiec?

auta2012 %>%
  filter(Kraj.aktualnej.rejestracji == "Polska", Kraj.pochodzenia == "Niemcy") %>%
  top_n(3, Przebieg.w.km)

# Odp: Najwiêkszy przebieg to 999999999 km.



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzÄ…cych z WÅ‚och?

auta2012 %>%
  filter(Marka == "Mitsubishi", Kraj.pochodzenia == "Wlochy") %>%
  group_by(Kolor) %>%
  summarise(n = n()) %>%
  arrange(n)

# Odp: Pytanie mo¿na ró¿nie interpretowaæ, poniewa¿ s¹ 4 kolory,
# które s¹ najmniej popularne i ka¿dy z nich ma po jednym reprezentancie,
# przyjmijmy wiêc ¿e drugi najmniej popularny kolor to ju¿ ten który ma wiêcej 
# ni¿ 1 reprezentanta, czyli granatowy-metallic.



# 10. Jaka jest wartoÅ›Ä‡ kwantyla 0.25 oraz 0.75 pojemnoÅ›ci skokowej dla 
# samochodÃ³w marki Volkswagen w zaleÅ¼noÅ›ci od tego, czy w ich wyposaÅ¼eniu 
# dodatkowym znajdujÄ… siÄ™ elektryczne lusterka?


auta2012 %>%
  filter(Marka == "Volkswagen") %>%
  mutate(Lusterka = ifelse(grepl('el. lusterka', Wyposazenie.dodatkowe),"tak","nie")) %>%
  group_by(Lusterka) %>%
  #grepl('pattern',vect) zwraca TRUE, jeœli string vect zawiera 'pattern'
  summarize(kwantyl25 = quantile(Pojemnosc.skokowa, probs = 0.25, na.rm = TRUE), 
            kwantyl75 = quantile(Pojemnosc.skokowa, probs = 0.75, na.rm = TRUE))
  

# Odp: Dla aut z elektrycznymi lusterkami wartoœæ kwantyla 0.25 wynosi 1892,
# wartoœæ kwantyla 0.75 wynosi 1968,
# natomiast dla aut bez elektrycznych lusterek
# wartoœæ kwantyla 0.25 wynosi 1400, wartoœæ kwantyla 0.75 wynosi 1900


