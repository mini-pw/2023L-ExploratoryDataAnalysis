library(dplyr)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)

df <- read.csv("mxmh_survey_results.csv")

selected <- df %>% 
  mutate(fav = case_when(Fav.genre == "Metal" ~ "Metal",
                         Fav.genre == "Pop" ~ "Pop",
                         Fav.genre == "Classical" ~ "Classical",
                         Fav.genre == "Rock" ~ "Rock",
                         Fav.genre == "Hip hop" ~ "Hiphop i Rap",
                         Fav.genre == "Rap" ~ "Hiphop i Rap",
                         TRUE ~ "Other")) %>%
  filter(fav %in% c("Pop", "Metal", "Rock", "Classical", "Hiphop i Rap"))

sel_metal <- selected %>% 
  group_by(Frequency..Metal.) %>%
  summarise(mean.depression = mean(Depression),
            mean.insomnia = mean(Insomnia),
            n = n())

sel_rock <- selected %>% 
  group_by(Frequency..Rock.) %>%
  summarise(mean.depression = mean(Depression),
            mean.insomnia = mean(Insomnia),
            n = n())

sel_classical <- selected %>% 
  group_by(Frequency..Classical.) %>%
  summarise(mean.depression = mean(Depression),
            mean.insomnia = mean(Insomnia),
            n = n())

sel_pop <- selected %>% 
  group_by(Frequency..Pop.) %>%
  summarise(mean.depression = mean(Depression),
            mean.insomnia = mean(Insomnia),
            n = n())

sel_hiphop_rap <- selected %>% 
  group_by(Frequency..Hip.hop.) %>%
  summarise(mean.depression = mean(Depression),
            mean.insomnia = mean(Insomnia),
            n = n())

ggplot() +
  geom_point(data = sel_rock, 
             aes(x = mean.depression,
                 y = mean.insomnia,
                 shape = Frequency..Rock.,
                 color = "Rock"),
             size = 5) +
  geom_point(data = sel_metal, 
             aes(x = mean.depression,
                 y = mean.insomnia,
                 shape = Frequency..Metal.,
                 color = "Metal"),
             size = 5) +
  geom_point(data = sel_classical, 
             aes(x = mean.depression,
                 y = mean.insomnia,
                 shape = Frequency..Classical.,
                 color = "Klasyczna"),
             size = 5) +
  geom_point(data = sel_hiphop_rap, 
             aes(x = mean.depression,
                 y = mean.insomnia,
                 shape = Frequency..Hip.hop.,
                 color = "Hiphop i Rap"),
             size = 5) +
  geom_point(data = sel_pop, 
             aes(x = mean.depression,
                 y = mean.insomnia,
                 shape = Frequency..Pop.,
                 color = "Pop"),
             size = 5) +
  scale_shape_manual(values = c(15, 16, 17, 18),
                     labels = c("Nigdy", "Rzadko", "Czasem", "Bardzo często")) +
  geom_hline(aes(yintercept = mean(df$Insomnia)), lty = "dashed") +
  geom_vline(aes(xintercept = mean(df$Depression)), lty = "dashed") +
  labs(title = "",
       x = "Średni poziom depresji",
       y = "Średni poziom insomnii",
       shape = "Częstotliwość",
       color = "Gatunek") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#F6DBC5",
                                        colour = "#F6DBC5",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linetype = 'solid',
                                        colour = "black"), 
        panel.grid.minor = element_line(linetype = 'solid',
                                        colour = "black"),
        plot.background = element_rect(fill = "#F6DBC5",
                                       colour = "#F6DBc5"),
        panel.border = element_blank())