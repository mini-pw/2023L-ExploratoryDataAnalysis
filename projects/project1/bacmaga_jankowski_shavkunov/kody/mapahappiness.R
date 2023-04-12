library(dplyr)
library(ggplot2)
#library(tidyverse)
library(caret)
library(scales)
library(ggthemes)
library(maps)
library(mapdata)
#options(encoding = "Unicode")
#library(emojifont)
#load.emojifont('OpenSansEmoji.ttf')
#library(fontawesome)
#library(emo)

#setwd("C:/Users/rober/Documents/Mapaswiata")
setwd("C:/Users/rober/Desktop/skrypty_do_plakatu")
valences <- read.csv(file = 'valences.csv')


process <- preProcess(as.data.frame(valences), method=c("range"))
norm_scale <- predict(process, as.data.frame(valences))



xlim <- c(-25, 42)
ylim <- c(33, 76)
worldmap <- map_data("world", xlim = xlim, ylim = ylim) %>% 
  left_join(norm_scale)

smiles <- c("☹️","🙁","😐","🙂","😁")

ggplot(worldmap) +
  #geom_text(family='fontawesome-webfont', size=6) + 
  borders("world")+
  geom_polygon(aes(long, lat, group = group, fill = happiness)) +
  coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_fill_viridis_c(option = "inferno",
                       na.value = "grey",
                       labels = smiles,
                       name = "",
                       guide = guide_colorbar(
                         barheight = unit(10, "cm"),
                         label.position = "right")
  ) +
  theme_map() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text=element_text(size=20)
  )
#ggtitle("Music Happiness Index") +
#ggsave("mapa1.png")


