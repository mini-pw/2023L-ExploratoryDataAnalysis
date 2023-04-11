############################################################################################
#
# Ten plik odnosi się do pierwszego rozdziału na plakcie "Kto wygra Eurowizję 2023?"
# Plik jest podzielony na dwa rozdział. Rozdział 1 zawiera analizę ogólnych wyników na
# Eurowiji (Jakie piosenki były najwyżej oceniane, jakie kraje stworzyły najlepsze piosenki
# , wizualizacja wyników najlepszych krajów na wykresie skrzynkowym) i służy ogólnemu
# zrozumieniu historii konkursu piosenki, aby przygotować się do rozdziału 2.
# Rozdział 2 zawiera kody, które tworzą trzy wykresy na plakat. Następnie na podstawie
# Rozdziału 1 sprawdzone zostało czy wyniki są wiarygodne
#
############################################################################################

library("readxl")
library(dplyr)
library(ggplot2)
library(forcats)
# Data
data = read.csv("ramka2.csv")
data21 = read.csv("ramka3.csv")
to <- read_excel("to_merge.xls")

# __________________ Rozdział 1 - eksploracja __________________

# best songs - exploration
mydata <- data.frame(mydata[complete.cases(mydata), ])
mydata2 <- data.frame(mydata[complete.cases(mydata), ])
mydata <- data %>% select(Year,Country,Points)
data <- vctrs::vec_c(mydata,to_merge)
mydata <- mydata %>% mutate(Points=as.numeric(Points))
res3 <- mydata %>% select(Song,Points) %>% 
  mutate(Points=as.numeric(Points)) %>%
  select(-Points) %>%
  arrange(desc(points)) %>%
  head(10)

ggplot(res, aes(x=Song, y=points)) + 
  geom_col(fill="lightblue") + coord_flip()

## best songs's countries - exploration
data2 <- data %>% select(Year, Country, Points)
data2 <- data2 %>% add_row(Year=as.integer(2022),Country="Ukraine",Points="700")
res3 <- data2 %>% 
  mutate(points=as.numeric(Points), 
         Year=as.integer(Year))
res3 <- data.frame(res3[complete.cases(res3), ])
res33<- data.frame(res3 %>% 
  filter(Year>2016) %>%
  group_by(Country) %>%
  summarize(suma_punktow=sum(points)))
sorted_res <- arrange(res33,desc(suma_punktow))
merged <- merge(res3, sorted_res, by="Country")
merged_sorted <- head(arrange(merged,desc(suma_punktow)),600)
solved <- merged_sorted %>% filter(Year >= 2009) %>%
  mutate(points=as.numeric(Points))

solved2 <- solved %>% 
  mutate(points=if_else(Year < 2016, points*2, points))

# boxplot najlepszych piosenek na eurowizji
ggplot(res2, aes(x=Country, y=points)) + 
  geom_boxplot(fill="lightyellow", outlier.color = "darkgreen") + 
  coord_flip() + theme_minimal() +
  labs(title = "Wyniki poszczególnych krajów w konkursie Eurowizja na przestrzeni całego turnieju",
       y="Liczba zdobytych punktów", x="Kraj")




# ______________ Rozdział 2 - Stworzenie wykresów do plakatu ze zgromadzonej wiedzy ______________

# Wykres 1

# Stworzenie heatmapy
solved2 %>% arrange(desc(points)) %>% head(1)

ht<-ggplot(solved2, aes(factor(Year), reorder(Country,suma_punktow), fill = points)) + 
  geom_tile() +
  scale_fill_gradient2(low = "#F9F5EB", mid="#E4DCCF", high = "#002B5B" ,limits = c(0, 775), midpoint = 200) +
  theme_void() +
  labs(x="Rok", y="Kraj",subtitle = "Najlepsze 13 krajów", 
       title = "Ilość zdobytych głosów w latach 2009-2022", fill="Liczba punktów") +
  theme(legend.position = "right") 
ht

# Wykres 2

# Stworzenie wykresu kołowego, który przedstawia przewidywania bukmacherów na rok 2023
# Dane na podstawie https://eurovisionworld.com

values <- c(39,15,12,11,5,4,3,3,2,2,2)
kolory <- rev(c("#151A6E", "#151ADF", "#1561DF",
            "#1591DF", "#4FCDF6", "#91DFF5",
            "#B0ECF5", "#CBECF5", "#DDF6FF",
            "#F5F6FF", "#FFFFF7"))
labels <- c("Sweden","Finland","Ukraine","Norway","Spain","Israel","Chechia","UK","Austria","France","Others")
df <- data.frame(values = values, labels = labels)

h2 <- ggplot(df, aes(x="", y=values, fill=reorder(labels,values))) + 
  geom_bar(stat = "identity", color = "white", width = 1) +
  coord_polar("y",start=0) +
  geom_text(aes(x = 1.35, y = cumsum(values) - values / 2, label = values), size = 5,color="white") +
  theme(legend.position = "bottom") +
  theme_void() +
  labs(fill="Kraj",title = "Przewidywania bukmacherów na rok 2023",
       subtitle = "(Wynik w procentach)") +
  scale_fill_manual(values = kolory)
h2

# Wykres 3

# Przygotowanie danych do stworzenia wykresu punktowego, który przedstawia
# zależność między przewidywaniami bukmacherów a rzeczywistymi wynikami

all = read.csv("ramka2.csv")
sweden <- all %>% filter(Country == "Sweden") %>% select(Year,Place)
nowe <- data.frame(Year=c(2021,2022),Place=c("14th","4th"))
sweden <- bind_rows(sweden,nowe)
sweden$Place <- as.numeric(gsub("[^0-9]+", "", sweden$Place))
sweden <- sweden %>% filter(Year>=2015)
past <- data.frame(Year=c(2015,2016,2017,2018,2019,2020,2021,2022), 
                   Place=c(1,4,6,5,12,9,6,2))

merged_sweden <- merge(sweden,past, by="Year")

# Stworzenie wykresu punktowego z porzygotowanych danych

ggplot(merged_sweden, 
       aes(x=Place.x, y=Place.y, color=Year, label=Year)) + 
  geom_point() +
  theme_minimal() +
  xlim(0,15) + ylim(0,15) +
  geom_text(size = 3, nudge_y = 0.5) +
  geom_abline(slope = 1, intercept = 0, 
              color = "blue", linetype = "dashed") +
  theme(legend.position = "none") +
  scale_color_gradientn(
    colors = c("darkgreen","darkgreen","darkgreen","darkgreen","red","red","darkgreen"),
    breaks = c(2015,2016,2017,2018,2019,2021,2022)
  ) +
  labs(x="Rzeczywiste miejsce w zawodach", y="Notowania bukmacherów",
       title = "Jak bardzo bukmacherzy mylili się w wybieraniu szwecji
       w ostatnich 8 latach?")



