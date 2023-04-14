library(dplyr)
library(maps)
library(mapdata)
library(ggplot2)
library(stringr)
library(ggrepel)
library(patchwork)

bramki_lewego <- read.csv(file = 'bramki_lewego.csv')

df1 <- bramki_lewego %>% 
  filter(Venue == "Away") %>% 
  filter(Comp == "Europa Lg" | Comp == "Champions Lg" | Comp == "UEFA Cup") %>% 
  select(Squad, Opponent) %>% 
  group_by(Opponent) %>% 
  summarise(Gole = length(Opponent))

Kraje <- c("Greece", "Netherlands", "Belgium", "UK", "Spain", "Portugal", "UK", "Croatia", 
           "Ukraine", "Norway", "Azerbaijan", "Italy", "France", "UK", "UK", "France", "Greece", "Netherlands",
           "Austria", "Spain", "Serbia", "Italy", "Ukraine", "UK", "Russia") 

df1 <- df1 %>% 
  cbind(Kraje) %>% 
  group_by(Kraje) %>% 
  summarise(ile = sum(Gole))
colnames(df1) <- c("region", "ZdobyteBramki") 
df1$ZdobyteBramki <- as.character(df1$ZdobyteBramki)

country <- map_data("world") 

ramka_do_wykresu <- country %>% 
  left_join(df1)
ramka_do_wykresu$ZdobyteBramki <- replace(ramka_do_wykresu$ZdobyteBramki, is.na(ramka_do_wykresu$ZdobyteBramki), 0)


punkty2 <- bramki_lewego %>% 
  filter(Venue == "Away") %>% 
  filter(Comp == "Europa Lg" | Comp == "Champions Lg" | Comp == "UEFA Cup") %>% 
  select(Opponent) 

#listę drużyn z punkty2 przekazuję do ChatGPT, którego proszę o wygenerowanie ramki danych zawiejących współrzędne geograficzne miast, w których grają te kluby

miasta_goli <- read.csv("miasta_goli.csv")
miasta_goli <- miasta_goli %>% 
  group_by(Miasto) %>% 
  summarise(ile = length(Miasto))

miasta_goli$label = paste0(miasta_goli$Miasto, sep = " - ", miasta_goli$ile)
miasta_goli_pom <- read.csv("miasta_goli.csv") 

miasta_goli_gotowe <- miasta_goli %>% 
  left_join(miasta_goli_pom) %>% 
  select(label, long, lat) %>% 
  unique()

wg_klubow <- read.csv("miasta_goli.csv")
liczba_goli <- wg_klubow %>% 
  group_by(Klub) %>% 
  summarise(ile_goli = length(Klub))
wg_klubow_wspl <- wg_klubow %>% 
  left_join(liczba_goli) %>% 
  unique() %>% 
  mutate(label = paste0("vs. ", Klub, " - ", ile_goli))

#tworzę mapę 
mapa_bramek <- ggplot() + 
  geom_polygon(data = ramka_do_wykresu, aes(x = long, y = lat, group = group, fill = ZdobyteBramki), color = "darkgray") +
  geom_point(data = miasta_goli_gotowe, aes(x = long, y = lat)) +
  geom_label_repel(data = wg_klubow_wspl, aes(x = long, y = lat, label = label), max.overlaps = 30, min.segment.length = 0, box.padding = 0.5, alpha = 0.8) +
  theme_void() +
  coord_fixed(ratio=1.3, xlim = c(-13, 50), ylim = c(34,65)) +
  scale_fill_discrete(type = c('white', '#edf8e9','#c7e9c0','#a1d99b','#74c476','#31a354','#006d2c'),
                      name = "Liczba\nbramek") +
  labs(subtitle = "Z podziałem na kraje, w których strzelał gole", caption = "") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(plot.background = element_rect(fill = "white"),
        legend.position = c(0.95, 0.75),
        legend.margin = margin(10,10,10,10),
        legend.background = element_rect(fill = "white", color = "black"),
        plot.margin = margin(t = -35),
        plot.subtitle = element_text(hjust = 0.5))



##wykres słupkowy

df3 <- bramki_lewego %>% 
  filter(Venue == "Away") %>% 
  filter(Comp == "Europa Lg" | Comp == "Champions Lg" | Comp == "UEFA Cup") %>% 
  select(Date, Squad)

df3a <- str_split(df3$Date, pattern = "-", n = 3, simplify = TRUE)
df3 <- cbind(df3, df3a)
colnames(df3) = c("Data", "Klub", "Rok", "Miesiac", "Dzien")
df3$Rok <- as.numeric(df3$Rok)
df3$Miesiac <- as.numeric(df3$Miesiac)

df3 <- df3 %>% 
  mutate(sezon1 = ifelse(Miesiac < 7, Rok - 1, Rok)) %>% 
  mutate(sezon2 = sezon1 +1)
df3$sezon2 <- substr(df3$sezon2, 3, 4)
df3$sezon <- paste(df3$sezon1, df3$sezon2, sep = "/")
df3 <- df3 %>% 
  select(Klub, sezon)
df3pom <- df3 %>% 
  group_by(sezon) %>% 
  summarise(liczba_goli = length(sezon))

df3 <- df3 %>% 
  left_join(df3pom) %>% 
  unique()

#ręcznie dodaję do ramki danych wiersz zawierający sezon, w którym RL9 nic nie strzelił 
pusty_sezon <- data.frame(Klub = "Dortmund", sezon = "2010/11", liczba_goli = 0)
df3 <- rbind(df3, pusty_sezon)
df3$Klub <- factor(df3$Klub, levels = c("Lech Poznań", "Dortmund", "Bayern Munich", "Barcelona"))

#tworzę wykres słupkowy
wykres_bramek <- df3 %>%  
ggplot(aes(x = sezon, y = liczba_goli, fill = Klub)) +
  geom_col(width = 0.6) + 
  scale_y_continuous(breaks = 1:9, expand = c(0,0), limits = c(0, 10)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values = c("dodgerblue", "#FDE100", "#DC052D", "#004D98")) +
  theme_bw() +
  labs(x = "", y = "Liczba bramek", subtitle = "Z podziałem na sezony i kluby, które reprezentował") +
  theme(panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(size = 12),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title = element_text(size = 11))


#patchwork - łącznie mapy i wykresu słupkowego
ostateczny_wynik <- mapa_bramek / (plot_spacer() + wykres_bramek + plot_spacer() + plot_layout(widths = c(0.15, 0.7, 0.15))) +
  plot_layout(heights = c(0.75, 0.25)) +
  plot_annotation(title = "Bramki Roberta Lewandowskiego w meczach wyjazdowych \neuropejskich pucharów",
                  caption = "Dane: fbref.com, Autor: Michał Celiński",
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")))
