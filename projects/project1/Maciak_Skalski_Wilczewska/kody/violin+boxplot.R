library(dplyr)
library(ggplot2)

df <- read.csv(file = 'C:/Users/Dell/OneDrive/Dokumenty/R/WdED/Projekt1/data1.csv', 
               stringsAsFactors = FALSE)

df %>% filter(number_of_words < 300) -> df

df %>% 
  ggplot(aes(x = reorder(genre, -number_of_words),
             y = number_of_words,
             fill = factor(genre,
                           levels = c("Hip-hop/Rap",
                                      "R&B",
                                      "Pop",
                                      "Country",
                                      "Rock")),
             color = factor(genre,
                           levels = c("Hip-hop/Rap",
                                      "R&B",
                                      "Pop",
                                      "Country",
                                      "Rock")))) +
  geom_violin(alpha = 0.9, size = 1.2) +
  geom_boxplot(width = 0.22, color = "black", alpha = 0.1, lwd = 1) +
  scale_fill_manual(values = c("#003557", 
                               "#00C377",
                               "#EE564E",
                               "#996035",
                               "#69000C")) +
  scale_color_manual(values = c("#003557", 
                               "#00C377",
                               "#EE564E",
                               "#996035",
                               "#69000C")) +
  scale_y_continuous(breaks = seq(0, ceiling(max(as.numeric(df$number_of_words))), 30),
                     minor_breaks = seq(0, ceiling(max(as.numeric(df$number_of_words))), 15)) +
  labs(x = "", 
       y = "Number of words",
       title = "Number of words in song distribution",
       subtitle = "for different music genres",
       caption = "Sample for each genre consisted of 200 songs") +
  theme(axis.text.x = element_text(size = 20, color = "gray5", face = "bold"),
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 25, color = "gray5"), 
        axis.text.y = element_text(size = 15, color = "gray10"),
        axis.line = element_line(color = "darkgray", size = 1, linetype = "solid", lineend = "round"),
        legend.position = "none",
        plot.title = element_text(size = 30, face = "bold", color = "gray5"),
        plot.subtitle = element_text(size = 20, color = "gray5"),
        plot.caption = element_text(size = 15, face = "italic", color = "gray10"),
        plot.margin = margin(l = 0.5, b = 0.5, r = 1, t = 0.5,  unit = "cm"),
        panel.grid.major.y = element_line(colour="#DCDEDD", size = 1),
        panel.grid.minor.y = element_line(colour = "#DCDEDD"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white",
                                        color = "darkgray",
                                        size = 1,
                                        linetype = "solid"))-> p1

p1
