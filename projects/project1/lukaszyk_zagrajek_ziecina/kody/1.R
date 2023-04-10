# Wykresy radarowe

library(dplyr)
library(fmsb)

df <- read.csv('projekt1/mxmh_survey_results.csv')

# zmieniamy zmienną jakosciową na ilościową w taki sposób, żeby sensownie odzwierciedlała rzeczywistość
# następnie liczymy średnie odpowiedzi
df <- df %>%
  mutate(x = case_when(Frequency..Rap. == 'Never' ~ 0, Frequency..Rap. == 'Rarely' ~ 3,
                                       Frequency..Rap. == 'Sometimes' ~ 8, Frequency..Rap. == 'Very frequently' ~ 15),
         y = case_when(Frequency..Hip.hop. == 'Never' ~ 0, Frequency..Hip.hop. == 'Rarely' ~ 3,
                                       Frequency..Hip.hop. == 'Sometimes' ~ 8, Frequency..Hip.hop. == 'Very frequently' ~ 15),
         Częstotliwość.Klasyczna = case_when(Frequency..Classical. == 'Never' ~ 0, Frequency..Classical. == 'Rarely' ~ 3,
                                             Frequency..Classical. == 'Sometimes' ~ 8, Frequency..Classical. == 'Very frequently' ~ 15),
         Częstotliwość.Rock = case_when(Frequency..Rock. == 'Never' ~ 0, Frequency..Rock. == 'Rarely' ~ 3,
                                             Frequency..Rock. == 'Sometimes' ~ 8, Frequency..Rock. == 'Very frequently' ~ 15),
         Częstotliwość.Pop = case_when(Frequency..Pop. == 'Never' ~ 0, Frequency..Pop. == 'Rarely' ~ 3,
                                             Frequency..Pop. == 'Sometimes' ~ 8, Frequency..Pop. == 'Very frequently' ~ 15),
         Częstotliwość.Metal = case_when(Frequency..Metal. == 'Never' ~ 0, Frequency..Metal. == 'Rarely' ~ 3,
                                             Frequency..Metal. == 'Sometimes' ~ 8, Frequency..Metal. == 'Very frequently' ~ 15)) %>%
  mutate(Częstotliwość.Rap = if_else(x > y, x, y), Fav.genre = if_else(Fav.genre == 'Hip hop', 'Rap', Fav.genre)) %>% 
  select(Ulubiony.gatunek = Fav.genre, Częstotliwość.Metal, Częstotliwość.Pop, Częstotliwość.Rock, Częstotliwość.Klasyczna, Częstotliwość.Rap) %>% 
  filter(Ulubiony.gatunek == 'Rap' | Ulubiony.gatunek == 'Metal' | Ulubiony.gatunek == 'Pop' |
           Ulubiony.gatunek == 'Rock' | Ulubiony.gatunek == 'Classical') %>%
  group_by(Ulubiony.gatunek) %>% 
  summarise(Rap = mean(Częstotliwość.Rap), Rock = mean(Częstotliwość.Rock), Klasyczna = mean(Częstotliwość.Klasyczna),
            Pop = mean(Częstotliwość.Pop), Metal = mean(Częstotliwość.Metal)) 

# wybieramy tylko kolumny niezbędne do poprawnego wygenerowania radarchartow
df_spider <- df %>% 
  select(Klasyczna, Metal, Pop, Rap, Rock)

df[1,1] <- 'Klasyczna' # zmieniam nazwę na polską
rownames(df_spider) <- df$Ulubiony.gatunek # zmieaniemy nazwy rzędów na gatunki do poprawnego wygenerowania radarchartu

# dzielimy ramkę danych na 5 oddzielnych, żeby zrobić 5 wykresów
df_spider_klasyczna <- df_spider[1,]
df_spider_metal <- df_spider[2,]
df_spider_pop <- df_spider[3,]
df_spider_rap <- df_spider[4,]
df_spider_rock <- df_spider[5,]

# ustawiamy zakresy wartości do radarchartów
df_spider_klasyczna <- rbind(rep(15,5) , rep(0,5) , df_spider_klasyczna)
df_spider_metal <- rbind(rep(15,5) , rep(0,5) , df_spider_metal)
df_spider_pop <- rbind(rep(15,5) , rep(0,5) , df_spider_pop)
df_spider_rap <- rbind(rep(15,5) , rep(0,5) , df_spider_rap)
df_spider_rock <- rbind(rep(15,5) , rep(0,5) , df_spider_rock)

# tworzymy 5 radarchartów
radarchart(df_spider_klasyczna, seg=3, pcol='#A3A500', pfcol='#A3A50059', plwd=4, plty=1, cglty = 1)
radarchart(df_spider_metal, seg=3, pcol="#00BF7D", pfcol="#00BF7D59", plwd=4, plty=1, cglty = 1)
radarchart(df_spider_pop, seg=3, pcol="#00B0F6", pfcol="#00B0F659", plwd=4, plty=1, cglty = 1)
radarchart(df_spider_rap, seg=3, pcol="#F8766D", pfcol="#F8766D59", plwd=4, plty=1, cglty = 1)
radarchart(df_spider_rock, seg=3, pcol="#E76BF3", pfcol="#E76BF359", plwd=4, plty=1, cglty = 1)

