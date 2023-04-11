library(dplyr)


library(ggplot2)
library(tidyverse)
library(RColorBrewer)


survey_results <- read.csv("survey.csv")

# ramka danych pokazuj?ca jaki procent os?b z danego przedzia?u godzin s?uchania 
# ma poziom niepokoju wi?kszy ni? 5


anxiety_level <- survey_results %>% 
  group_by(hours_interval) %>% 
  summarise(cnt = n(), anxiety_gt5 = sum(Anxiety >= 6), 
            anxiety_gt5_percent = round((anxiety_gt5 / cnt) * 100, 1 )) %>% 
  mutate(percent_grater_than_50 = case_when(anxiety_gt5_percent >= 50 ~ '>= 50%',
                                            anxiety_gt5_percent < 50 ~ '< 50%'))
# wykres do powy?eszej ramki
anx2 <- anxiety_level %>% 
  ggplot(aes(x = hours_interval, y = anxiety_gt5_percent)) +
  theme_bw()+
  geom_col(width = 0.5, colour = "black", fill = "#C51306") +
  ylab('Procent osób z niepokojem większym niż 5/10')+
  xlab('Czas spędzony na słuchaniu muzyki w ciągu dnia (w godzinach)')+
  scale_y_continuous(limits = c(0,100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     labels = c("0", "10%", "20%", "30%", "40%",
                                "50%", "60%", "70%", "80%", "90%", "100%"), expand = c(0,0) )+
  scale_x_discrete(limits = c('0-2','2-4', '4-6','6-8','8-10','10-12', '> 12')) +
  
  theme(
    legend.position = "none",
    #legend.text = element_text(size = 11),
    #legend.key.size = unit(1.4, 'lines'),  
    
    plot.title = element_text(face = "bold", hjust = 0.5, vjust = 2.5,  size = 24),
    text = element_text(family = "Clear Sans Regular"),
    plot.margin = margin(1, 0.2, 0.5, 1, "cm"),) +
  
  geom_text(aes(label = anxiety_gt5_percent), vjust = -0.5, size = 3.5, colour = "black")

anx2
ggsave(anx2,
       filename = "wkres_anx3.png",
       bg = "transparent",
       width = 8.13, height = 5.19, dpi = 300)


