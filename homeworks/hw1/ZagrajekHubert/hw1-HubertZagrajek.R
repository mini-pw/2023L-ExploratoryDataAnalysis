library(PogromcyDanych)
library(stringr)
data(auta2012)

# 1.Rozwa¿aj¹c tylko obserwacje z PLN jako walut¹ (nie zwa¿aj¹c na 
# brutto/netto): jaka jest mediana ceny samochodów, które maj¹ napêd elektryczny?
auta2012 %>%
  filter(Waluta == "PLN" & Rodzaj.paliwa == "naped elektryczny") %>%
  summarise(median_cost = median(Cena.w.PLN, na.rm = TRUE))
  
  
  
# Odp: 18900

# 2.W podziale samochodów na marki oraz to, czy zosta³y wyprodukowane w 2001 
  # roku i póŸniej lub nie, podaj kombinacjê, dla której mediana liczby koni
  # mechanicznych (KM) jest najwiêksza.
auta2012 %>% 
  select(KM, Marka, Rok.produkcji) %>% 
  mutate(date = ifelse(Rok.produkcji >= 2001, "new", "old")) %>% 
  select(Marka, KM, date) %>% 
  summarise(median_km = median(KM, na.rm = TRUE), .by = c("Marka", "date")) %>% 
  top_n(1,median_km)
  



# Odp: Bugatti po 2001 roku



# 3. Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.
auta2012 %>% 
  filter(Kolor == "szary-metallic" &
           Cena.w.PLN > min(median(Cena.w.PLN, na.rm = TRUE),mean(Cena.w.PLN,na.rm = TRUE)) &
           Cena.w.PLN < max(median(Cena.w.PLN,na.rm = TRUE),mean(Cena.w.PLN,na.rm = TRUE))) %>%
  filter(as.character(Kraj.pochodzenia) != as.character(Kraj.aktualnej.rejestracji)) %>%
  summarise(n = n())

  



# Odp: 1849



# 4. Jaki jest rozstÄ™p miÄ™dzykwartylowy przebiegu (w kilometrach) PassatÃ³w
# w wersji B6 i z benzynÄ… jako rodzajem paliwa?

auta2012 %>% 
  filter(Model == "Passat", Rodzaj.paliwa == "benzyna", Wersja == "B6") %>% 
  #IQR to funkcja odpowiadaj¹ca za znalezienie odstêpu mniêdzykwartylowego
  summarise(odstep = IQR(Przebieg.w.km, na.rm = TRUE))


# Odp: 75977.5



# 5. BiorÄ…c pod uwagÄ™ samochody, ktÃ³rych cena jest podana w koronach czeskich,
# podaj Å›redniÄ… z ich ceny brutto.
# Uwaga: JeÅ›li cena jest podana netto, naleÅ¼y dokonaÄ‡ konwersji na brutto (podatek 2%).

auta2012 %>% 
  filter(Waluta == "CZK") %>% 
  mutate(po.podatku = if_else(Brutto.netto == "netto", Cena*1.02, Cena)) %>% 
  summarise(srednia = mean(po.podatku))


# Odp:210678.3



# 6. KtÃ³rych ChevroletÃ³w z przebiegiem wiÄ™kszym niÅ¼ 50 000 jest wiÄ™cej: tych
# ze skrzyniÄ… manualnÄ… czy automatycznÄ…? Dodatkowo, podaj model, ktÃ³ry najczÄ™Å›ciej
# pojawia siÄ™ w obu przypadkach.

auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(n=n())
# wiêcej jest samochodów z manualn¹ skrzyni¹ biegów

auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000 & Skrzynia.biegow == "manualna") %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)
# Z manualna by³o wiêcej Lancetti

auta2012 %>% 
  filter(Marka == "Chevrolet" & Przebieg.w.km > 50000 & Skrzynia.biegow == "automatyczna") %>% 
  group_by(Model) %>% 
  summarise(n=n()) %>% 
  top_n(1,n)
# Z automatyczna bylo wiecej Corvette

# Odp:Wiecej jest aut z manualn¹. Z manualn¹ bylo najwiecej Lancetti. 
# z automatyczna bylo wiecej Corvette




# 7. Jak zmieniÅ‚a siÄ™ mediana pojemnoÅ›ci skokowej samochodÃ³w marki Mercedes-Benz,
# jeÅ›li weÅºmiemy pod uwagÄ™ te, ktÃ³re wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  filter(Marka == "Mercedes-Benz") %>% 
  mutate(czy.przed = ifelse(Rok.produkcji >= 2003, 1, 0)) %>% 
  #podobny efekt mozna otrzyac za pomoca funkcji as.logical
  summarise(mediana = median(Pojemnosc.skokowa, na.rm = TRUE, ),.by = czy.przed)


# Odp: Mediana pojemnosci skokowej przed i po 2003 jest taka sama 



# 8. Jaki jest najwiÄ™kszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzÄ…cych z Niemiec?

auta2012 %>% 
  filter(Kraj.aktualnej.rejestracji == "Polska" & Kraj.pochodzenia == "Niemcy") %>% 
  select(Przebieg.w.km) %>% 
  arrange(-Przebieg.w.km) %>% 
  top_n(1,Przebieg.w.km)


# Odp:1e+09



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzÄ…cych z WÅ‚och?

auta2012 %>% 
  filter(Marka == "Mitsubishi" & Kraj.pochodzenia == "Wlochy") %>%
  summarise(ilosc = n(), .by = Kolor) %>% 
  arrange(ilosc) 


# Odp: Drugi najmniej popularny kolor to granatowy-metalic



# 10. Jaka jest wartoÅ›Ä‡ kwantyla 0.25 oraz 0.75 pojemnoÅ›ci skokowej dla 
# samochodÃ³w marki Volkswagen w zaleÅ¼noÅ›ci od tego, czy w ich wyposaÅ¼eniu 
# dodatkowym znajdujÄ… siÄ™ elektryczne lusterka?

auta2012 %>% 
  filter(Marka == "Volkswagen") %>% 
  mutate(el.lusterka = str_detect(Wyposazenie.dodatkowe, "el. lusterka")) %>% 
  # funkcja str_detect pozwala nam na znalezienie konkretnej informacji ze stringa
  # zwraca wartosc TRUE lub FALSE
  group_by(el.lusterka) %>% 
  summarise(kwant. = quantile(Pojemnosc.skokowa, probs = c(0.25, 0.75), na.rm = TRUE)) %>% 
  View()

# Odp:kwantyle to odpowiednio: bez lusterek - 1400 i 1900
# natomiast z elektrycznymi lusterkami 1892.25, 1968.00


