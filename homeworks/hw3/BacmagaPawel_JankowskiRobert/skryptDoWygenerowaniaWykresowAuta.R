library(dplyr) 
library(ggplot2)
library(forcats)
library(scales)
library(viridis)
library(PogromcyDanych)


auta <- auta2012 %>% 
  group_by(Marka) %>%
  summarise(n = n(), srednia_cena = mean(Cena.w.PLN, na.rm = TRUE),
            sredni_przebieg = mean(Przebieg.w.km, na.rm = TRUE)) %>%
  arrange(-n) %>%
  top_n(10,n)

#sredni przebieg:

auta %>%
  mutate(Marka = fct_reorder(Marka, desc(-sredni_przebieg))) %>%
  ggplot( aes(x=Marka, y = sredni_przebieg, fill = Marka)) +
  geom_col(show.legend = FALSE)  +
  coord_flip() +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3),
                     breaks = round(seq(0, 400000, by = 25000),1)) + 
  ylab("Średni przebieg w tys. kilometrów") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Średni przebieg dla wybranych marek") +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(option = "turbo", discrete = TRUE) 


#srednia cena:

auta %>%
  mutate(Marka = fct_reorder(Marka, desc(-srednia_cena))) %>%
  ggplot( aes(x=Marka, y = srednia_cena, fill = Marka)) +
  geom_col(show.legend = FALSE)  +
  coord_flip() +
  scale_y_continuous(breaks = round(seq(0, 100000, by = 10000),1)) + 
  ylab("Średnia cena w PLN") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Średna cena dla wybranych marek") +
  scale_color_viridis(discrete = TRUE)+
  scale_fill_viridis(option = "inferno", discrete = TRUE) 