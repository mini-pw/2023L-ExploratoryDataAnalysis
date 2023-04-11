library(ggplot2)
library(dplyr)
library(showtext)
library(stringr)

font.add(family = "Metropolis", regular = 
           "C:/Users/Uzytkownik/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/METROPOLIS-REGULAR.OTF",
         bold = "C:/Users/Uzytkownik/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/METROPOLIS-SEMIBOLD.OTF")
showtext_auto()

tracks <- read.csv("tracks.csv")
artists <- read.csv("artists.csv")
albums <- read.csv("albums.csv")

tracks2 <- tracks %>% 
  filter(album_id %in% albums$id) %>% 
  filter(artists_ids %in% artists$id)

artists2 <- artists %>% 
  filter(id %in% albums$artists_ids) %>% 
  filter(id %in% tracks$artists_ids)

albums2 <- albums %>% 
  filter(id %in% tracks$album_id) %>% 
  filter(artists_ids %in% artists$id)

artists2 <- artists2 %>% 
  distinct(id, .keep_all = TRUE)

albums2 <- albums2 %>% 
  distinct(id, .keep_all = TRUE)

tracks2 <- tracks2 %>% 
  distinct(id, .keep_all = TRUE)

merged <- merge(tracks2, albums2, by.x = "album_id", by.y = "id")
#View(merged)

date <- str_split(merged$release_date, pattern = "-")
dane <- character(length(date))

for(i in 1:length(date)){
  dane[i] <- paste0(date[[i]][1], "-", date[[i]][2])
}
dane

lata <- str_split(dane, pattern = "-")
danelata <- character(length(lata))

for(i in 1:length(lata)){
  danelata[i] <- lata[[i]][1]
}
danelata

miesiace <- str_split(dane, pattern = "-")
danemiesiace <- character(length(miesiace))

for(i in 1:length(miesiace)){
  danemiesiace[i] <- miesiace[[i]][2]
}
danemiesiace

merged$release_date <- dane
merged$lata <- danelata
merged$miesiace <- danemiesiace

dowykresu <- merged %>% 
  select(lata, miesiace,
         release_date, duration_ms, popularity.x, danceability, energy, tempo,
         loudness, speechiness, acousticness, instrumentalness, liveness, valence) %>% 
  filter(lata > "2002") %>% 
  filter(miesiace != "NA") %>% 
  mutate(duration_ms = duration_ms / 1000)

skrzypce <- dowykresu %>% 
  filter(lata > "2016")

gestosci <- dowykresu %>% 
  filter(lata > "2018") %>% 
  mutate(sekundy = duration_ms / 1000)

najpopularniejsze <- artists2 %>% 
  arrange(-popularity) %>% 
  select(name, popularity, followers) %>% 
  head(15) 

#################################### WYKRES 1 ############################################
wykres_dance <- dowykresu %>% 
  group_by(lata) %>% 
  ggplot(aes(x = lata, y = danceability)) + 
  geom_boxplot(outlier.colour = "#89bc8e",
               color="#70af85", lwd = 1, 
               fill = "#c5e1c6", 
               outlier.size = 1.8, 
               fatten = 1.5, alpha = 0.8) + 
  scale_x_discrete(guide = guide_axis(n = 1)) + 
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) + 
  theme(axis.text.x = element_text(size = 6)) + 
  theme(panel.background = element_rect(fill = "#fdfaf7")) + 
  theme(panel.grid.minor = element_blank()) + 
  ggtitle("Danceability over years") + 
  xlab("Year") + 
  ylab("Danceability")+
  theme(text = element_text(family = "Metropolis")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 55, 
                                  face = "bold", colour = "white")) + 
  theme(axis.text=element_text(size=30, colour = "#aad1ab"),
        axis.title=element_text(size=45, colour = "white")) + 
  theme(axis.text.x=element_text(size=30, colour = "#aad1ab")) + 
  theme(plot.background = element_rect(fill = "#3c3c4c")) + 
  theme(panel.background = element_rect(fill = "#3c3c4c"))+
  theme(panel.grid.major = element_line(size = 0.005, linetype = 'dotted',
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.005, linetype = 'dotted',
                                        colour = "white")) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 10))) + 
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 10, l = 0))) + 
  theme(plot.title = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0))) + 
  theme(plot.margin = margin(t = 0, r = 20, b = 0, l = 0)) + 
  theme(panel.border = element_blank())

wykres_dance

#################################### WYKRES 2 ############################################

ggplot(skrzypce, aes(x = lata, y = tempo)) + 
  geom_violin(color="#70af85", lwd = 1, 
              fill = "#c5e1c6", alpha = 0.8) + 
  theme(axis.text.x = element_text(size = 6)) + 
  theme(panel.background = element_rect(fill = "#fdfaf7")) + 
  theme(panel.grid.minor = element_blank()) + 
  ggtitle("Tempo over years") + 
  xlab("Year") + 
  ylab("Tempo")+
  theme(text = element_text(family = "Metropolis")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 55, 
                                  face = "bold", colour = "white")) + 
  theme(axis.text=element_text(size=30, colour = "#aad1ab"),
        axis.title=element_text(size=45, colour = "white")) + 
  theme(axis.text.x=element_text(size=30, colour = "#aad1ab")) + 
  theme(plot.background = element_rect(fill = "#3c3c4c")) + 
  theme(panel.background = element_rect(fill = "#3c3c4c"))+
  theme(panel.grid.major = element_line(size = 0.005, linetype = 'dotted',
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.005, linetype = 'dotted',
                                        colour = "white")) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 10))) + 
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 10, l = 0))) + 
  theme(plot.title = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0))) + 
  theme(plot.margin = margin(t = 0, r = 20, b = 0, l = 0)) + 
  theme(panel.border = element_blank())

#################################### WYKRES 3 ############################################

ggplot(gestosci, aes(x=duration_ms,fill=lata)) +
  geom_density(color="#70af85", lwd = 1, 
               fill = "#c5e1c6", alpha = 0.8) +
  facet_grid(lata ~ .) + 
  theme(
    strip.text.x = element_text(size = 30),
    strip.text.y = element_text(size = 30))+
  theme(axis.text.x = element_text(size = 6)) + 
  theme(panel.background = element_rect(fill = "#fdfaf7")) + 
  theme(panel.grid.minor = element_blank()) + 
  ggtitle("Duration density curve") + 
  xlab("Duration (seconds)") + 
  ylab("Density")+
  theme(text = element_text(family = "Metropolis")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 55, 
                                  face = "bold", colour = "white")) + 
  theme(axis.text=element_text(size=30, colour = "#aad1ab"),
        axis.title=element_text(size=45, colour = "white")) + 
  theme(axis.text.x=element_text(size=30, colour = "#aad1ab")) + 
  theme(plot.background = element_rect(fill = "#3c3c4c")) + 
  theme(panel.background = element_rect(fill = "#3c3c4c"))+
  theme(panel.grid.major = element_line(size = 0.005, linetype = 'dotted',
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.005, linetype = 'dotted',
                                        colour = "white")) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 10))) + 
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 10, l = 0))) + 
  theme(plot.title = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0))) + 
  theme(plot.margin = margin(t = 0, r = 20, b = 0, l = 0)) + 
  theme(panel.border = element_blank())


#################################### WYKRES 4 ############################################

options(scipen = 999)
library(forcats)
posortowane <- 
  najpopularniejsze %>% mutate(name = forcats::fct_reorder(name, followers))

ggplot(posortowane, aes(x = name, y = followers)) + 
  geom_col(lwd = 1, 
           fill = "#c5e1c6", alpha = 0.8) + 
  scale_x_discrete(guide = guide_axis(n = 1)) + 
  coord_flip() + 
  theme(
    strip.text.x = element_text(size = 25),
    strip.text.y = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 6)) + 
  theme(panel.background = element_rect(fill = "#fdfaf7")) + 
  theme(panel.grid.minor = element_blank()) + 
  ggtitle("Number of followers", 
          subtitle = "for 15 artists with the biggest popularity index") + 
  xlab("Name") + 
  ylab("Number of followers")+
  theme(text = element_text(family = "Metropolis")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 55, 
                                  face = "bold", colour = "white")) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 30, face = "bold",
                                     colour = "white")) +
  theme(axis.text=element_text(size=30, colour = "#aad1ab"),
        axis.title=element_text(size=45, colour = "white")) + 
  theme(axis.text.x=element_text(size=30, colour = "#aad1ab")) + 
  theme(plot.background = element_rect(fill = "#3c3c4c")) + 
  theme(panel.background = element_rect(fill = "#3c3c4c"))+
  theme(panel.grid.major = element_line(size = 0.005, linetype = 'dotted',
                                        colour = "white"),
        panel.grid.minor = element_line(size = 0.005, linetype = 'dotted',
                                        colour = "white")) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 10))) + 
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 10, l = 0))) + 
  theme(plot.title = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) + 
  theme(plot.subtitle = element_text(margin = margin(t = 10, r = 0, b = 20, l = 0)))+
  theme(plot.margin = margin(t = 0, r = 20, b = 0, l = 0)) 


