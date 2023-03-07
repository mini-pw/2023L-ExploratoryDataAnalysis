install.packages("PogromcyDanych")
library(stringi)
library(stringr)
library(PogromcyDanych)
auta2012

#Autor: Pola Moœcicka

# 1. RozwaÅ¼ajÄ…c tylko obserwacje z PLN jako walutÄ… (nie zwaÅ¼ajÄ…c na 
# brutto/netto): jaka jest mediana ceny samochodÃ³w, ktÃ³re majÄ… napÄ™d elektryczny?

auta2012 %>% 
  filter(Waluta=="PLN",Rodzaj.paliwa=="naped elektryczny") %>% 
  select(Cena) %>% 
  summarise(med=median(Cena),na.rm=TRUE)

# Odp: 18900

# 2. W podziale samochodÃ³w na marki oraz to, czy zostaÅ‚y wyprodukowane w 2001 
# roku i pÃ³Åºniej lub nie, podaj kombinacjÄ™, dla ktÃ³rej mediana liczby koni
# mechanicznych (KM) jest najwiÄ™ksza.


auta2012 %>% 
  mutate(podzial = case_when(Rok.produkcji >= "2001" ~ "w 2001/po",
                             Rok.produkcji < "2001" ~ "przed 2001")) %>% 
  filter(!is.na(KM)) %>%                                                
  group_by(Marka, podzial) %>% 
  summarise(med=median(KM)) %>% 
  arrange(-med) 

# Odp: Bugatti, wyprodukowany w/po 2001 (mediana wynosi 1001KM)


#Spoœród samochodów w kolorze szary-metallic, których cena w PLN znajduje siê
# pomiêdzy jej œredni¹ a median¹ (nie zwa¿aj¹c na brutto/netto), wybierz te, 
# których kraj pochodzenia jest inny ni¿ kraj aktualnej rejestracji i poodaj ich liczbê.


auta2012 %>% 
  select(Kolor,Cena.w.PLN,Kraj.aktualnej.rejestracji,Kraj.pochodzenia) %>% 
  filter(Kolor=="szary-metallic") %>% 
  #summarise(srednia=mean(Cena.w.PLN),med=median(Cena.w.PLN)) - mean: 44341.41, mediana:27480, tutaj sobie wyliczy³am medianê i œredni¹ tak dodatkowo
  filter((Cena.w.PLN>median(Cena.w.PLN) & Cena.w.PLN<mean(Cena.w.PLN))|(Cena.w.PLN<median(Cena.w.PLN) & 
                                                                          Cena.w.PLN>mean(Cena.w.PLN)) )%>% 
  filter(!Kraj.pochodzenia%s===%Kraj.aktualnej.rejestracji)  %>% 
  summarize(n=n())

# Odp: 1331



# 4. Jaki jest rozstÄ™p miÄ™dzykwartylowy przebiegu (w kilometrach) PassatÃ³w
# w wersji B6 i z benzynÄ… jako rodzajem paliwa?

auta2012 %>% 
  select(Model, Wersja, Rodzaj.paliwa, Przebieg.w.km) %>% 
  filter(Model=="Passat", Wersja=="B6", Rodzaj.paliwa=="benzyna") %>%
  filter(!is.na(Przebieg.w.km)) %>% 
  summarise(kw1=quantile(Przebieg.w.km,probs = 0.25),kw3=quantile(Przebieg.w.km,probs = 0.75)) %>% 
  mutate(rozstep = abs(kw3-kw1))


# Odp: 75977.5



# 5. BiorÄ…c pod uwagÄ™ samochody, ktÃ³rych cena jest podana w koronach czeskich,
# podaj Å›redniÄ… z ich ceny brutto.
# Uwaga: JeÅ›li cena jest podana netto, naleÅ¼y dokonaÄ‡ konwersji na brutto (podatek 2%).


auta2012 %>% 
  filter(Waluta=="CZK") %>% 
  mutate(Cena.w.PLN.2= case_when(Brutto.netto == "brutto" ~ Cena,Brutto.netto == "netto" ~ Cena*1.02)) %>% 
  summarise(srednia=mean(Cena.w.PLN.2))


# Odp: 210678.3




# 6. KtÃ³rych ChevroletÃ³w z przebiegiem wiÄ™kszym niÅ¼ 50 000 jest wiÄ™cej: tych
# ze skrzyniÄ… manualnÄ… czy automatycznÄ…? Dodatkowo, podaj model, ktÃ³ry najczÄ™Å›ciej
# pojawia siÄ™ w obu przypadkach.

#a)
auta2012 %>% 
  select(Marka, Model, Przebieg.w.km, Skrzynia.biegow) %>% 
  filter(Marka=="Chevrolet", Przebieg.w.km>50000) %>% 
  group_by(Skrzynia.biegow) %>% 
  summarise(count=n())  

#b)
auta2012 %>% 
  select(Marka, Model, Przebieg.w.km, Skrzynia.biegow) %>% 
  filter(Marka=="Chevrolet", Przebieg.w.km>50000) %>% 
  group_by(Model,Skrzynia.biegow) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(10)


# Odp: a)wiêcej jest tych ze skrzyni¹ manualn¹,b)Model ktory pojawia siê najczêœciej - manualne: Lacetti, automatyczne: Corvette




# 7. Jak zmieniÅ‚a siÄ™ mediana pojemnoÅ›ci skokowej samochodÃ³w marki Mercedes-Benz,
# jeÅ›li weÅºmiemy pod uwagÄ™ te, ktÃ³re wyprodukowano przed lub w roku 2003 i po nim?

auta2012 %>% 
  select(Marka, Pojemnosc.skokowa, Rok.produkcji) %>% 
  filter(Marka=="Mercedes-Benz") %>% 
  filter(!is.na(Pojemnosc.skokowa)) %>% 
  summarise(med=median(Pojemnosc.skokowa))
#powy¿ej wyliczona mediana pojemnoœci skokowej równa 2200 marki Mercedes-Benz

auta2012 %>% 
  select(Marka, Pojemnosc.skokowa, Rok.produkcji) %>% 
  filter(Marka=="Mercedes-Benz") %>% 
  filter(!is.na(Pojemnosc.skokowa)) %>% 
  mutate(podzial = case_when(Rok.produkcji > "2003" ~ "po",
                             Rok.produkcji <= "2003" ~ "przed")) %>% 
  group_by(podzial) %>% 
  summarise(med=median(Pojemnosc.skokowa))


# Odp: mediana siê nie zmieni³a 



# 8. Jaki jest najwiÄ™kszy przebieg w samochodach aktualnie zarejestrowanych w
# Polsce i pochodzÄ…cych z Niemiec?

auta2012 %>% 
  select(Marka, Przebieg.w.km, Kraj.aktualnej.rejestracji, Kraj.pochodzenia) %>% 
  filter(Kraj.aktualnej.rejestracji =="Polska", Kraj.pochodzenia=="Niemcy") %>% 
  filter(!is.na(Przebieg.w.km)) %>% 
  arrange(-Przebieg.w.km) %>% 
  head(1)
   


# Odp: 1e+09 (dok³adnie: 999999999)



# 9. Jaki jest drugi najmniej popularny kolor w samochodach marki Mitsubishi
# pochodzÄ…cych z WÅ‚och?

auta2012 %>% 
  select(Marka, Kraj.pochodzenia, Kolor) %>% 
  filter(Marka=="Mitsubishi", Kraj.pochodzenia=="Wlochy") %>% 
  group_by(Kolor) %>% 
  summarise(popularny_kolor=n()) %>% 
  arrange(popularny_kolor) %>% 
  head(10)

# Odp: granatowy-metallic (uzna³am. ¿e te cztery które wystêpuj¹ tylko 1 raz zajmuj¹ ex aequo pierwsze miejsce)



# 10. Jaka jest wartoÅ›Ä‡ kwantyla 0.25 oraz 0.75 pojemnoÅ›ci skokowej dla 
# samochodÃ³w marki Volkswagen w zaleÅ¼noÅ›ci od tego, czy w ich wyposaÅ¼eniu 
# dodatkowym znajdujÄ… siÄ™ elektryczne lusterka?

auta2012 %>% 
  select(Marka, Pojemnosc.skokowa, Wyposazenie.dodatkowe) %>% 
  filter(Marka=="Volkswagen") %>%
  
  #tutaj sprawdza³am czy jest informacja o tych lusterkach i jeœli jest to zamienia³am na informacje tak, wpp nie
  mutate(lusterka=case_when( stri_detect_fixed(Wyposazenie.dodatkowe, "el. lusterka")==TRUE ~ "tak", 
                             stri_detect_fixed(Wyposazenie.dodatkowe, "el. lusterka")==FALSE ~ "nie")) %>% 
  filter(!is.na(Pojemnosc.skokowa)) %>% 
  select(Marka, Pojemnosc.skokowa, lusterka) %>% 
  group_by(lusterka) %>% 
  
  summarise(kw1=quantile(Pojemnosc.skokowa,probs = 0.25),kw2=quantile(Pojemnosc.skokowa,probs = 0.75)) 



# Odp: Wartoœæ kwantyla 0.25 dla samochodów z lusterkami elektrycznymi wynosi 1892, a dla tych bez wynosi 1400.
#      Wartoœæ kwantyla 0.75 dla samochodów z lusterkami elektrycznymi wynosi 1968, a dla tych bez wynosi 1900.


