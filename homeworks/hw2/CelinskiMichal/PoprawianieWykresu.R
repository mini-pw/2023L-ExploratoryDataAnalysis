telewizja <- c("TVP 2", "TVP 1", "TVN", "Polsat")
ogladalnosc <- c("7.85", "7.81", "7.39", "7.32")
ogladalnosc <- as.numeric(ogladalnosc)
Udzialy4Stacji <- data.frame(telewizja, ogladalnosc)


library(ggplot2)
library(forcats)
library(scales)
library(dplyr)


ggplot(Udzialy4Stacji, aes(x = reorder(telewizja, desc(ogladalnosc)), y = ogladalnosc)) +
  geom_col(width = 0.6, fill = "#40AC61") +
  scale_y_continuous(labels = number_format(suffix = "%"), expand = c(0,0, 0.1, 0.1), n.breaks = 10) +
  theme_minimal() +
  geom_text(aes(label = paste(ogladalnosc, "%", sep = "")), 
            nudge_y = -0.15, 
            colour = "white") +
  expand_limits(y = 9) + 
  labs(x = "",
       y = "",
       title = "UDZIAŁY 4 GŁÓWNYCH STACJI W MARCU 2023", 
       subtitle = "Udziały stacji (SHR%, widzowie 4+), wg Nielsen") +
  theme(plot.title = element_text(hjust = 0.5, face="bold" )) +
  theme(plot.subtitle = element_text(hjust = 1)) +
  theme(axis.text = element_text(face="bold"))
    