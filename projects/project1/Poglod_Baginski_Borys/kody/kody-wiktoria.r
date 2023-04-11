library(dplyr)
library(ggplot2)
library(forcats)
library(ggridges)
library(hrbrthemes)

Eurovision <- read.csv("C:\\Users\\wikto\\OneDrive\\Dokumenty\\OneDrive\\Pulpit\\Projekt 1\\merged_table.csv")

Eurovision1 <- Eurovision %>% filter(energy != "NA") %>% select(-c("Contestant", "Song", "R.O", "Host_City", "uri", "mode", "time_signature")) %>% 
  filter(Country %in% c("Ireland", "Sweden", "United Kingdom", "France","Russia")) 

wykres_en <- Eurovision1 %>% ggplot(aes(x = energy, y = Country, fill = factor(..y..))) + theme_minimal() +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_manual(values = c("navyblue","#0b5ca3", "#299fc3", "#63c9dc", "#a3e0ea")) +
  theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) +
  labs(x = "Energia", y = "Kraj") +
  theme(plot.title = element_text(size = 20, face = "bold"))
wykres_en

wykres_d <- Eurovision1 %>% ggplot(aes(x = danceability, y = Country, fill = factor(..y..))) + theme_minimal() +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  
  scale_fill_manual(values = c("navyblue","#0b5ca3", "#299fc3", "#63c9dc", "#a3e0ea")) +
  theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) +
  labs(x = "Taneczność", y = "Kraj") +
  theme(plot.title = element_text(size = 20, face = "bold"))
wykres_d

wykres_s <- Eurovision1 %>% ggplot(aes(x = speechiness, y = Country, fill = factor(..y..))) + theme_minimal() +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_manual(values = c("navyblue","#0b5ca3", "#299fc3", "#63c9dc", "#a3e0ea")) +
  theme(legend.position = "none", panel.spacing = unit(0.1, "lines"), strip.text.x = element_text(size = 8)) +
  labs(x = "Miara mówionych słów", y = "Kraj") +
  theme(plot.title = element_text(size = 20, face = "bold"))
wykres_s



