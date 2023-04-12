library(tidyverse)
library(ggplot2)
library(stringr)
library(patchwork)
library(RColorBrewer)
# library(gridExtra)

entire_year_2022 <- read.csv("ramka-caly-2022.csv")

ener_val_point <- ggplot(entire_year_2022) +
  geom_point(aes(x = energy, y = valence, size = streams)) + coord_flip() + 
  labs(title = "All Songs", x = "Energy", y= "Valence") + 
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1)) + 
  geom_vline(xintercept= 0.50, color = "red") +
  geom_hline(yintercept= 0.50, color = "red") +
  theme_classic()

ener_val_point



# str(entire_year_2022$genres)
# unique(entire_year_2022$genres)

genres_count <- entire_year_2022 %>% 
  group_by(genres) %>% 
  count() %>% 
  arrange(desc(n))

ener_val_all_with_genres <- entire_year_2022 %>% 
  select(energy, valence, genres) %>% 
  mutate(simplified_genre = case_when(str_detect(genres, "pop") ~ "pop",
                                      str_detect(genres, "rock") ~ "rock",
                                      str_detect(genres, "r&b") ~ "r&b",
                                      str_detect(genres, "rap") ~ "rap",
                                      .default = "Other")) %>% 
  filter(!is.na(simplified_genre))
  

geom_jitter_ener_val_all_with_genres <- ggplot(ener_val_all_with_genres) +
  geom_jitter(aes(x = valence , y = energy, color = simplified_genre), size = 2) +
  labs(title = "", x = "Valence", y= "Energy") + 
  geom_vline(xintercept= 0.50, color = "red") +
  geom_hline(yintercept= 0.50, color = "red") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        legend.position = "bottom") + 
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1)) +
  scale_color_discrete(name = "Genre")

geom_jitter_ener_val_all_with_genres

png("myplot.png", width=600, height=400, bg = "transparent"); plot(geom_jitter_ener_val_all_with_genres); dev.off()

ener_val_pop <- entire_year_2022 %>% 
  select(energy, valence, genres, streams) %>% 
  mutate(simplified_genre = case_when(str_detect(genres, "pop") ~ "pop",
                                      .default = NA)) %>% 
  filter(!is.na(simplified_genre)) %>%
  ggplot() +
  geom_point(aes(x = energy, y = valence)) +
  labs(title = "Pop", x = "Energy", y= "Valence") + 
  geom_vline(xintercept= 0.50, color = "red") +
  geom_hline(yintercept= 0.50, color = "red") +
  theme_classic() + scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1))

ener_val_pop

# pop_plot <- ggplot(ener_val_pop) +
#   geom_point(aes(x = energy, y = valence))

ener_val_rock <- entire_year_2022 %>% 
  select(energy, valence, genres) %>% 
  mutate(simplified_genre = case_when(str_detect(genres, "rock") ~ "rock",
                                      .default = NA)) %>% 
  filter(!is.na(simplified_genre)) %>%
  ggplot() +
  geom_point(aes(x = energy, y = valence)) +
  labs(title = "Rock", x = "Energy", y= "Valence") + scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1)) + 
  geom_vline(xintercept= 0.50, color = "red") +
  geom_hline(yintercept= 0.50, color = "red") +
  theme()

ener_val_rock

# rock_plot <- ggplot(ener_val_rock) +
#   geom_point(aes(x = energy, y = valence))

ener_val_rb <- entire_year_2022 %>% 
  select(energy, valence, genres) %>% 
  mutate(simplified_genre = case_when(str_detect(genres, "r&b") ~ "r&b",
                                      .default = NA)) %>% 
  filter(!is.na(simplified_genre)) %>% 
  ggplot() +
  geom_point(aes(x = energy, y = valence)) +
  labs(title = "R&B", x = "Energy", y= "Valence") + scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1)) + 
  geom_vline(xintercept= 0.50, color = "red") +
  geom_hline(yintercept= 0.50, color = "red") +
  theme_classic()

ener_val_rb

# rb_plot <- ggplot(ener_val_rb) +
#   geom_point(aes(x = energy, y = valence))

ener_val_hiphop <- entire_year_2022 %>% 
  select(energy, valence, genres) %>% 
  mutate(simplified_genre = case_when(str_detect(genres, "hip hop") ~ "hip hop",
                                      .default = NA)) %>% 
  filter(!is.na(simplified_genre)) %>% 
  ggplot() +
  geom_point(aes(x = energy, y = valence)) +
  labs(title = "Hip hop", x = "Energy", y= "Valence") + scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1)) + 
  geom_vline(xintercept= 0.50, color = "red") +
  geom_hline(yintercept= 0.50, color = "red") +
  theme_classic()

ener_val_hiphop

# hiphop_plot <- ggplot(ener_val_hiphop) +
#   geom_point(aes(x = energy, y = valence))

ener_val_rap <- entire_year_2022 %>% 
  select(energy, valence, genres) %>% 
  mutate(simplified_genre = case_when(str_detect(genres, "rap") ~ "rap",
                                      .default = NA)) %>% 
  filter(!is.na(simplified_genre)) %>% 
  ggplot() +
  geom_point(aes(x = energy, y = valence)) +
  labs(title = "Rap", x = "Energy", y= "Valence") + scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1)) +
  geom_vline(xintercept= 0.50, color = "red") +
  geom_hline(yintercept= 0.50, color = "red") +
  theme_classic()

ener_val_rap

ener_val_pop + ener_val_rock + ener_val_rb + ener_val_hiphop + ener_val_rap

ener_val_pop + coord_flip() + 
  ener_val_rock + coord_flip() + 
  ener_val_rb + coord_flip() + 
  ener_val_hiphop + coord_flip() + 
  ener_val_rap + coord_flip()