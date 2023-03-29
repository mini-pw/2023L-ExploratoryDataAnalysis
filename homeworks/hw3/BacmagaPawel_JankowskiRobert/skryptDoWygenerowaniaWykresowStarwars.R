library(tidyverse)
library(plotrix)

#### Operacje na ramce ####
starwars_eyes <- starwars %>%
  mutate(eye_color = stringr::str_to_title(eye_color),
         eye_color = fct_lump(eye_color, prop = 0.05)) %>%
  count(eye_color, sort = TRUE) %>% 
  ## order factor levels by number, put "Other" to end
  dplyr::mutate(
    eye_color = forcats::fct_rev(forcats::fct_inorder(eye_color)),
    eye_color = forcats::fct_relevel(eye_color, "Other", after = 0)
  )
starwars_eyes <- starwars_eyes %>% 
  dplyr::mutate(perc = paste0(sprintf("%4.1f", n / sum(n) * 100), "%"),
                ## customize label for the first category
                perc = if_else(row_number() == 1, paste(perc, "wszystkich postaci"), perc))


starwars_eyes <- starwars_eyes %>% 
  mutate(
    color = case_when(
      eye_color == "Brown" ~ "saddlebrown",
      eye_color == "Blue" ~ "lightblue",
      eye_color == "Yellow" ~ "yellow",
      eye_color == "Black" ~ "dimgrey",
      eye_color == "Orange" ~ "orange",
      eye_color == "Red" ~ "red",
      eye_color == "Other" ~ "gray85",
    )
  )
kolory_po_polsku = c("Br¹zowy", "Niebieski", "Pozosta³e", "¯ó³ty", "Czarny",
                     "Pomarañczowy", "Czerwony")
kolory_po_polsku_rev = c("Pozosta³e", "Czerwony", "Pomarañczowy", "Czarny", "¯ó³ty",
                         "Niebieski", "Br¹zowy")

starwars_eyes <- starwars_eyes %>% 
  mutate(kolory_po_polsku = kolory_po_polsku)



#### Wykresy #####

## Wszystko ##
ggplot(starwars_eyes, aes(x = n, y = eye_color, fill = color)) + 
  geom_col() +
  geom_text(aes(label = perc), 
            hjust = 1, nudge_x = -.5,
            size = 5, fontface = "bold") +
  scale_fill_identity(guide = "none") +
  theme_void() +
  theme(axis.text.y = element_text(size = 14, hjust = 1),
        plot.margin = margin(rep(15, 4))) +
  labs(title = "Kolor oczu postaci ze Starwars") +
  scale_y_discrete(labels = kolory_po_polsku_rev)

## Bez kolorów ##
ggplot(starwars_eyes, aes(x = n, y = eye_color)) + 
  geom_col() +
  geom_text(aes(label = perc), 
            hjust = 1, nudge_x = -.5,
            size = 5, fontface = "bold") +
  scale_fill_identity(guide = "none") +
  theme_void() +
  theme(axis.text.y = element_text(size = 14, hjust = 1),
        plot.margin = margin(rep(15, 4))) +
  labs(title = "Kolor oczu postaci ze Starwars") +
  scale_y_discrete(labels = kolory_po_polsku_rev)

## Bez etykiet ##
ggplot(starwars_eyes, aes(x = n, y = eye_color, fill = color)) + 
  geom_col() +
  geom_text(aes(label = perc), 
            hjust = 1, nudge_x = -.5,
            size = 5, fontface = "bold") +
  scale_fill_identity(guide = "none") +
  theme_void() +
  theme(plot.margin = margin(rep(15, 4))) +
  labs(title = "Kolor oczu postaci ze Starwars")

## Udzia³ procentowy bez podpisów ##
ggplot(starwars_eyes, aes(x = eye_color, y = n / sum(n), fill = color)) + 
  geom_col() +
  scale_fill_identity(guide = "none") +
  theme_minimal() +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels = kolory_po_polsku_rev) +
  labs(x = "", y="", title = "Kolor oczu postaci ze Starwars") +
  coord_flip()

## Pie ##
starwars_pie <- pie3D(starwars_eyes$n,labels=starwars_eyes$kolory_po_polsku, 
                      radius = 1.1,
                      col= starwars_eyes$color, 
                      main="Kolor oczu postaci ze Starwars (w %)")
