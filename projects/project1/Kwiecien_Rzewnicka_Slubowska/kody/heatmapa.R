kolumny <- read.csv("columns.csv")
odpowiedzi <- read.csv("responses.csv")

install.packages("extrafont")
install.packages("showtext")
library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont)
library(showtext)


music_hobby <- odpowiedzi %>% 
  select("Music":"Opera", "History":"Pets")

dance_srednie <- music_hobby %>% 
  filter(Dance == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

country_srednie <- music_hobby %>% 
  filter(Country == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

classical_srednie <- music_hobby %>% 
  filter(Classical.music == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

pop_srednie <- music_hobby %>% 
  filter(Pop == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

rock_srednie <- music_hobby %>% 
  filter(Rock == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

metal_srednie <- music_hobby %>% 
  filter(Metal.or.Hardrock == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

punk_srednie <- music_hobby %>% 
  filter(Punk == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

hiphop_srednie <- music_hobby %>% 
  filter(Hiphop..Rap == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

reggae_srednie <- music_hobby %>% 
  filter(Reggae..Ska == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

jazz_srednie <- music_hobby %>% 
  filter(Swing..Jazz == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

latin_srednie <- music_hobby %>% 
  filter(Latino == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)

techno_srednie <- music_hobby %>% 
  filter(Techno..Trance == 5) %>% 
  mutate(sr_mathematics = mean(Mathematics, na.rm = TRUE)) %>% 
  mutate(sr_poetry_writing = mean(Writing, na.rm = TRUE)) %>% 
  mutate(sr_dancing = mean(Dancing, na.rm = TRUE))  %>% 
  mutate(sr_pets = mean(Pets, na.rm = TRUE)) %>% 
  mutate(sr_sport = mean(Passive.sport, na.rm = TRUE)) %>% 
  mutate(sr_celebrity = mean(Celebrities, na.rm = TRUE)) %>% 
  mutate(sr_politics = mean(Politics, na.rm = TRUE)) %>% 
  mutate(sr_playing_instrument = mean(Musical.instruments, na.rm = TRUE)) %>% 
  select("sr_mathematics", "sr_poetry_writing", "sr_dancing", "sr_pets",
         "sr_sport", "sr_celebrity", "sr_politics", "sr_playing_instrument") %>% 
  head(1)


values2 <- c("Taneczna", "Country", "Klasyczna", "Pop", "Rock", "Metal", "Punk",
             "Hiphop", "Reggae", "Jazz", "Latynowska", "Techno")

gatunek <- list()

for (j in 1:12){
  for (k in 1:8){
    gatunek <- append(gatunek, values2[j])
  }
}

gatunek <- as.character(gatunek)


hobby <- list()

values <- c("Matematyka", "Pisanie poezji", "Taniec", "Zwierzęta domowe", "Sport", "Życie celebrytów", "Polityka", "Gra na instrumentach")

for (i in 1:12){
  hobby <- append(hobby, values)
}

hobby <- as.character(hobby)


values3 <- list(dance_srednie, country_srednie, classical_srednie, pop_srednie, rock_srednie, metal_srednie,
                punk_srednie, hiphop_srednie, reggae_srednie, jazz_srednie, latin_srednie, techno_srednie)

srednia <- list()

for (l in 1:12){
  pom <- values3[l]
  pom <- as.data.frame(pom)
  pom <- as.character(pom)
  for (o in 1:8){
    srednia <- append(srednia, pom[o])
  }
}

srednia <- as.character(srednia)


df <- data.frame(gatunek, hobby, srednia )

df$srednia <- as.numeric(df$srednia)

df$srednia[df$srednia < 2.32] <- 1
df$srednia[2.32 < df$srednia & df$srednia < 2.63] <- 2
df$srednia[2.63 < df$srednia   & df$srednia < 3.2] <- 3
df$srednia[3.2 < df$srednia  & df$srednia  < 3.82] <- 4
colnames(df)[3]  <- "poziom lubienia"

ggp <- ggplot(df, aes(gatunek, hobby)) +                         
  geom_tile(aes(fill = `poziom lubienia`)) +
  scale_fill_gradient(low = "#eca7ab", high = "#ab2a7e") + 
  theme(legend.background = element_rect(fill = "#EFEDDF")) + 
  theme(axis.text.x = element_blank()) +
  labs(x = "") +
  labs(y = "") + 
  labs(title = "") + 
  theme(plot.background = element_rect(fill = "#EFEDDF"),
        panel.background = element_rect(fill = "#EFEDDF")) +
  guides(fill = guide_legend(title=""))+
  theme(legend.title = element_text(colour="#3c6ca8"),
        legend.text = element_blank(),
        title = element_text(colour="#3c6ca8"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5))

ggp
