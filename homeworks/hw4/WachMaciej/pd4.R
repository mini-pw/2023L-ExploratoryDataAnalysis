library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(SmarterPoland)
library(stringi)
library(stringr)

theme_nothing <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
  )

alccons <- read.csv("alcohol-consumption.csv") %>% 
  select(country, total_consumption) %>% 
  mutate(country = str_replace(country, "United States", "USA" ))


worldmapdata <- map_data("world") %>% 
  rename(country = region)

worldmapdata %>% 
  left_join(alccons) %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = total_consumption), color = "black") +
  coord_fixed(1.3) +
  theme(panel.background = element_rect(fill = "white")) +
  scale_fill_gradient2(limits = c(0, 17),
                       low = "#81EEE6",
                       high = "#F529E0",
                       mid = "#4A19EC",
                       na.value="#E2FEFF",
                       midpoint = 9,
                       name = "czysty alkohol 
ilość (litry)"
  ) +
  labs(
    title = "Ilość wypitego alkoholu na jednego mieszkanca",
    subtitle = "rok 2016"
  )+
  theme_nothing

  
  
  
  
  
