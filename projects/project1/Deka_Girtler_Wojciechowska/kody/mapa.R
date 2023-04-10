library(dplyr)
library(mapdata)
library(ggplot2)
library(SmarterPoland)
mapaswiata <- map_data("world") %>% 
  rename(country = region)
mapaswiata[mapaswiata == "UK"] <- "United Kingdom"

alc_consumption <- read.csv("data.csv")
alc_consumption <- alc_consumption%>%
  filter(year == 2019) %>% 
  rename(litry = both)

alc_consumption[alc_consumption == "United Kingdom"] <- "UK"
alc_consumption[alc_consumption == "United States"] <- "USA"
alc_consumption[alc_consumption == "Saint Vincent and the Grenadines"] <- "Saint Vincent"
alc_consumption[alc_consumption == "Trinidad and Tobago"] <- "Trinidad"
alc_consumption[alc_consumption == "Saint Kitts and Nevis"] <- "Saint Kitts"
alc_consumption[alc_consumption == "DR Congo"] <- "Democratic Republic of the Congo"

mapapapa <- full_join(mapaswiata, alc_consumption, by = "country")

world_map <- ggplot(data = mapapapa) +
  geom_polygon(aes(x = long, y = lat, fill = litry, group = group),colour = "white")+
  coord_fixed(1.3) +
  labs(title = "Spożycie alkoholu na świecie w 2019",
       subtitle = "litry czystego alkoholu na osobę",
       x = "długość geograficzna",
       y = "szerokość geograficzna") +
  theme_minimal()

world_map


#mapa 2 danceability

dane <- read.csv("df_complete-2.csv", header=TRUE, stringsAsFactors = FALSE, na.string="")
dane2 <- read.csv("charts.csv")

dane <- dane %>% rename("name"="track")


nowa <- merge(dane2, dane, by.x = "name", all.x = FALSE ,all.y= FALSE)

ostateczna <- select(nowa, name, artists, streams, danceability, country, position, year, energy, tempo)

densyy <- arrange(ostateczna, -danceability) 

countries_codes <- read.csv("wikipedia-iso-country-codes.csv")
countries_codes <- countries_codes %>%
  mutate(country_code = tolower(Alpha.2.code))

wszystko <- merge(countries_codes, densyy, by.x = "country_code", by.y = "country", 
                  all.x = FALSE, all.y = TRUE) 
wszystko <- wszystko %>% 
  rename(country = English.short.name.lower.case)
wszystko_kraje <- wszystko %>% 
  distinct(English.short.name.lower.case)
wszystko_razem <- merge(wszystko, countries, by.x = "country", by.y = "country", 
                        all.x = TRUE, all.y = FALSE)

wszystko_razem[wszystko_razem == "Vietnam"] <- "Viet Nam"
wszystko_razem[wszystko_razem == "United States of America"] <- "United States of America"
wszystko_razem[wszystko_razem == "Russian Federation"] <- "Russia"
wszystko_razem[wszystko_razem == "Korea, Republic of"] <- "Korea, Republic of (South Korea)"

marys <- wszystko_razem%>%
  filter(year == 2018)
sumsumsum <- marys %>% group_by(country) %>% summarise(taneczność = sum(danceability) / 1000)



maposz <- full_join(mapaswiata, sumsumsum, by = "country")

dancemap <- ggplot(data = maposz) +
  geom_polygon(aes(x = long, y = lat, fill = dance, group = group),colour = "white")+
  coord_fixed(1.3) +
  scale_fill_distiller(palette = "Purples", direction = 1) +
  labs(title = "Poziom taneczności najpopularniejszych utworów",
       subtitle = "litry czystego alkoholu na osobę",
       x = "długość geograficzna",
       y = "szerokość geograficzna") +
  theme_minimal()

world_map
dancemap


### spozycie alkoholu europa
#mapaswiata <- map_data("world") %>% 
 # rename(country = region)
#mapaswiata[mapaswiata == "UK"] <- "United Kingdom"
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Republic","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden", "United Kingdom", "Ukraine", "Turkey", "Switzerland",
                   "Serbia", "Norway", "Montenegro", "Moldova", "Luxembourg",
                   "Iceland", "Georgia", "Bosnia and Herzegovina", "Belarus", "Azerbaijan", "Armenia",
                   "Andorra", "Albania")

kraje_alc <- alc_consumption %>% distinct(country)
alc_consumption[alc_consumption == "Nigeria"] <- "Czech Republic"
indEU <- which(mapaswiata$country%in%europeanUnion)

europamapa <- mapaswiata[which(mapaswiata$country%in%europeanUnion), ]

mapue <-  full_join(europamapa, alc_consumption, by = "country")

ue_map <- ggplot(data = mapue) +
  geom_polygon(aes(x = long, y = lat, fill = litry, group = group),colour = "white")+
  coord_fixed(1.3) +
  scale_fill_distiller(palette = "BuPu", direction = 1)+
  labs(title = "Spożycie alkoholu na świecie w 2019",
       subtitle = "litry czystego alkoholu na osobę",
       x = "",
       y = "") +
  theme( panel.background = element_rect(fill = "black", colour = "black", size = 0.5, linetype = "solid"),plot.background=element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"), plot.title = element_text(color = "white", size = 17),
        plot.subtitle = element_text(color = "white", size = 13),
        axis.title = element_text(color = "white"),
        axis.text = element_blank(),
        legend.title = element_text(color="white"),
        legend.text = element_text(color="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ue_map

### tanecznosc europa
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Republic","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden", "United Kingdom", "Ukraine", "Turkey", "Switzerland",
                   "Serbia", "Norway", "Montenegro", "Moldova", "Luxembourg",
                   "Iceland", "Georgia", "Bosnia and Herzegovina", "Belarus", "Azerbaijan", "Armenia",
                   "Andorra", "Albania")

dane <- read.csv("df_complete-2.csv", header=TRUE, stringsAsFactors = FALSE, na.string="")
dane2 <- read.csv("charts.csv")

dane <- dane %>% rename("name"="track")


nowa <- merge(dane2, dane, by.x = "name", all.x = FALSE ,all.y= FALSE)

ostateczna <- select(nowa, name, artists, streams, danceability, country, position, year, energy, tempo)

densyy <- arrange(ostateczna, -danceability) 

countries_codes <- read.csv("wikipedia-iso-country-codes.csv")
countries_codes <- countries_codes %>%
  mutate(country_code = tolower(Alpha.2.code))

wszystko <- merge(countries_codes, densyy, by.x = "country_code", by.y = "country", 
                  all.x = FALSE, all.y = TRUE) 
wszystko <- wszystko %>% 
  rename(country = English.short.name.lower.case)

wszystko_razem <- merge(wszystko, countries, by.x = "country", by.y = "country", 
                        all.x = TRUE, all.y = FALSE)



marys <- wszystko_razem%>%
  filter(year == 2018) %>% 
  filter(continent=="Europe")
sumsumsum <- marys %>% group_by(country) %>% summarise(dance = mean(danceability))

europamapa <- mapaswiata[which(mapaswiata$country%in%europeanUnion), ]

maposz <- full_join(europamapa, sumsumsum, by = "country", all.y = TRUE)

maposz$przedziały = cut(maposz$dance, breaks=seq(from=0.667, to=0.778, length.out=8))

dancemap <- ggplot(data = maposz) +
  geom_polygon(aes(x = long, y = lat, fill = przedziały, group = group),colour = "white")+
  coord_fixed(1.5) +
  #scale_fill_brewer(palette = "BuPu", na.value="gray")+
  scale_fill_manual(values = c("#d37bfb", "#b937f7", "#7b04ba", "#3d0459"))+
  labs(title = "Poziom taneczności najpopularniejszych utworów",
       x = "",
       y = "") +
  theme( panel.background = element_rect(fill = "black", colour = "black", size = 0.5, linetype = "solid"),plot.background=element_rect(fill = "black"),
         legend.background = element_rect(fill = "black"), plot.title = element_text(color = "white", size = 15),
         axis.title = element_text(color = "white"),
         axis.text = element_blank(),
         legend.title = element_text(color="white"),
         legend.text = element_text(color="white"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank())

dancemap

mapa_kraje <- mapaswiata %>% distinct(country)

