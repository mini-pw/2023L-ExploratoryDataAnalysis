odpowiedzi <- read.csv("responses.csv")

library(dplyr)
library(ggplot2)
library(tidyr)

music_phobia <- odpowiedzi %>% 
  select("Music":"Opera", "Flying":"Fear.of.public.speaking")

klas <- music_phobia %>% 
  filter(Classical.music == 5) 
k <- nrow(klas)
klas <- klas %>% 
  select("Classical.music", "Storm", "Dangerous.dogs", "Ageing") %>% 
  pivot_longer(!Classical.music, names_to = "strach", values_to = "wartosc") %>% 
  filter(wartosc == 5) %>% 
  group_by(strach) %>% 
  summarise(klasyczna = n()/k * 100)

dance <- music_phobia %>% 
  filter(Dance == 5)
k <- nrow(dance)
dance <- dance %>% 
  select("Dance", "Storm", "Dangerous.dogs", "Ageing") %>% 
  pivot_longer(!Dance, names_to = "strach", values_to = "wartosc") %>% 
  filter(wartosc == 5) %>% 
  group_by(strach) %>% 
  summarise(dance = n()/k * 100)

pop <- music_phobia %>% 
  filter(Pop == 5)
k <- nrow(pop)
pop <- pop %>% 
  select("Pop", "Storm", "Dangerous.dogs", "Ageing") %>% 
  pivot_longer(!Pop, names_to = "strach", values_to = "wartosc") %>% 
  filter(wartosc == 5) %>% 
  group_by(strach) %>% 
  summarise(pop = n()/k * 100)

metal <- music_phobia %>% 
  filter(Metal.or.Hardrock == 5)
k <- nrow(metal)
metal <- metal %>% 
  select("Metal.or.Hardrock", "Storm", "Dangerous.dogs", "Ageing") %>% 
  pivot_longer(!Metal.or.Hardrock, names_to = "strach", values_to = "wartosc") %>% 
  filter(wartosc == 5) %>% 
  group_by(strach) %>% 
  summarise(metal = n()/k * 100)

punk <- music_phobia %>% 
  filter(Punk == 5)
k <- nrow(punk)
punk <- punk %>% 
  select("Punk", "Storm", "Dangerous.dogs", "Ageing") %>% 
  pivot_longer(!Punk, names_to = "strach", values_to = "wartosc") %>% 
  filter(wartosc == 5) %>% 
  group_by(strach) %>% 
  summarise(punk = n()/k * 100)

rock <- music_phobia %>% 
  filter(Rock == 5)
k <- nrow(rock)
rock <- rock %>% 
  select("Rock", "Storm", "Dangerous.dogs", "Ageing") %>% 
  pivot_longer(!Rock, names_to = "strach", values_to = "wartosc") %>% 
  filter(wartosc == 5) %>% 
  group_by(strach) %>% 
  summarise(rock = n()/k * 100)

lista <- list(dance, klas, pop, rock, punk, metal)
lacznie <- Reduce(function(x, y) merge(x, y), lista) %>% 
  pivot_longer(!strach, names_to = "gatunek", values_to = "liczba")

ggplot(lacznie, aes(x = strach, y = liczba, fill = gatunek)) +
  geom_col(position = "dodge") + 
  labs(title = "Udział osób bojących się wybranych rzeczy wśród fanów gatunków muzycznych",
       x = "Strachy",
       y = "Procent osób bojących się",
       fill = "Gatunek
muzyczny") +
  scale_fill_manual(labels = c("taneczna", "klasyczna", "metal", "pop", "punk", "rock"), 
                     values = c("#ef4b7e", "#3cb9ac", "#ff7e23", "#a4711e", "#82bfec", "#a6207a")) +
  scale_x_discrete(labels=c("Starość", "Niebezpieczne psy", "Burza")) + 
  theme(plot.background = element_rect(fill = "#FAE3F0"), 
        legend.background = element_rect(fill = "#FAE3F0"),
        panel.background = element_rect(fill = "#FAE3F0"),
        axis.title = element_text(colour = "#3c6ca8"),
        axis.text = element_text(colour="#3c6ca8"),
        title = element_text(colour="#3c6ca8"),
        legend.text = element_text(colour="#3c6ca8"),
        legend.title = element_text(colour="#3c6ca8"),
        axis.ticks = element_blank(),
        panel.grid = element_line(colour = "#3c6ca8"),
        panel.grid.major.x = element_blank())
  




