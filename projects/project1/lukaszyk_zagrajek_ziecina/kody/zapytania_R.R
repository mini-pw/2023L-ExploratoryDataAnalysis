library(dplyr)
library(ggplot2)
library(patchwork)
muzyka <- read.csv("mxmh_survey_results.csv")
# muzyka jest ramka danych, z ktorej bedziemy brac rozne zaleznosci

p_violin <- muzyka %>% 
  mutate(fav = case_when(Fav.genre == "Metal" ~ "Metal",
                         Fav.genre == "Pop" ~ "Pop",
                         Fav.genre == "Classical" ~ "Klasyczna",
                         Fav.genre == "Rock" ~ "Rock",
                         Fav.genre == "Hip hop" ~ "Hiphop i Rap",
                         Fav.genre == "Rap" ~ "Hiphop i Rap",
                         TRUE ~ "Other")) %>%
  filter(fav %in% c("Pop", "Metal", "Rock", "Klasyczna", "Hiphop i Rap"))%>% 
  ggplot(aes(x = fav, y = Depression,  group = fav, fill = fav)) +
  geom_violin(adjust = 1, alpha = .5) +
  geom_boxplot(width = 0.1, color = "black") +
  labs(title = "Czy rodzaj muzyki ma wpływ na depresję?", x = "Ulubiony rodzaj muzyki", y = "Poziom depresji")+
  theme(panel.background = element_rect(fill = "#F6DBC5",
                                      colour = "#F6DBC5",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "black"), 
      panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                      colour = "black"),
      plot.background = element_rect(fill = "#F6DBC5",
                                     colour = "#F6DBC5"),
      panel.border = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank())



