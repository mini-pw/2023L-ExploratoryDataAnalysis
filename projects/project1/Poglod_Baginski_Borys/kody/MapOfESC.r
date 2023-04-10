library(ggplot2)
library(maps)
library(mapdata)
library(SmarterPoland)
library(dplyr)
library(ggplot2)
library(plyr)
library(ggthemes)
library(dplyr)
europe <- map_data("world") 

capital_coordinates <- read.csv("C:\\Users\\wikto\\Downloads\\country-capitals.csv")

votes <- read.csv("C:\\Users\\wikto\\Downloads\\votes.csv")

#Przydatne linki:
#http://edgebundle.schochastics.net/
#FLOWMAPTUTORIAl
#https://github.com/Spijkervet/eurovision-dataset/releases
#votes %>% filter(from_country == "at", year == 2019, round == "final") %>% View()

#TO CLEAN DATA:


#Zamiana skrótów w ramce danych votes na nazwy kraji
country_shorts <- unique(votes$from_country)
to_full <- c("Austria", "Belgium", "Denmark", "France", "Germany", "Italy",
                  "Luxemburg", "Netherlands", "Switzerland", "United Kingdom",
                  "Sweden", "Monaco", "Norway", "Finland", "Spain", "yu",
                  "Portugal", "Ireland", "Malta", "Iceland", "Greece", "Turkey",
                  "ma", "Cyprus", "Israel", "ba", "Croatia",
                  "Slovenia", "Estonia", "Hungary", "Lithuania", "Poland", "Romania",
                  "Russia", "Slovakia", "North Macedonia", "Latvia", "Ukraine", "Albania",
                  "ad", "Belarus", "cs", "Bulgaria", "Moldova", "Armenia", "Czech Republic",
                  "Georgia", "Montenegro", "Serbia", "Azerbeijan", "San Marino", "Australia")
length(country_shorts)

countries_names <- data.frame(country_shorts, to_full)

votes_improved <- left_join(votes, countries_names, by= c("from_country_id" = "country_shorts"))
votes_improved <- left_join(votes_improved, countries_names, by= c("to_country_id" = "country_shorts"))
head(votes_improved) 

#Wybór lat 2010-2019 oraz zsumowanie punktów danych
votes_in_years <- votes_improved %>% 
  filter(year > 2009, round == "final") %>% mutate(total_points =
                                   ifelse(year<2016, total_points*2, total_points))
votes_in_years <- ddply(votes_in_years, c("from_full", "to_full"), function(x) sum(x$total_points))
#usuwam AUstralie
votes_in_years <- votes_in_years %>% 
  filter(from_full != "Australia", to_full != "Australia")
#Zespolenie 
votes_coordinates <- left_join(votes_in_years, capital_coordinates, by=c("from_full"="CountryName") )
votes_coordinates <- left_join(votes_coordinates, capital_coordinates, by=c("to_full"="CountryName"))

#OD$id <-as.character(c(1:nrow(OD))) # nwm po co to


# europeancontest <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
#                    "Czech Republic","Denmark","Estonia","Finland","France",
#                    "Germany","Greece","Hungary","Ireland","Italy","Latvia",
#                    "Lithuania","Luxembourg","Malta","Netherlands","Poland",
#                    "Portugal","Romania","Slovakia","Slovenia","Spain",
#                    "Sweden", "Ukraine", "Belarus", "Russia",
#                    "Azerbeijan", "Armenia", "Georgia", "Israel", "Norway",
#                    "Switzerland", "Monetnegro", "Libya", "Morocco", "Algeria", "Turkey",
#                    "Bosnia and Herzegovina", "Serbia", "Monaco", "Andorra",
#                    "United Kingdom", "Egypt", "North Macedonia", "Kosovo", "Czechia",
#                    "Moldova", "San Marino")

votes_coordinates <-
  votes_coordinates %>% mutate(CapitalLatitude.x = as.numeric(CapitalLatitude.x),
                               CapitalLatitude.y = as.numeric(CapitalLatitude.y),
                               CapitalLongitude.x = as.numeric(CapitalLongitude.x),
                               CapitalLongitude.y = as.numeric(CapitalLongitude.y))
typeof(votes_coordinates$V1)
library(data.table)

#Norwegia i Grecja

votes_coordinates_from_greece <- votes_coordinates %>% 
  filter(from_full == "Greece", to_full != "Greece") %>% arrange(desc(V1)) %>% head(10) %>% 
  mutate(Score = log(V1))

votes_coordinates_from_norway <- votes_coordinates %>% 
  filter(from_full == "Norway", to_full != "Norway") %>% arrange(desc(V1)) %>% head(10) %>% 
  mutate(Score = log(V1))

View(europe)
europe1 <- europe %>% 
  filter(lat > 25, lat <75, long >-25, long < 50)
ggplot() + 
  geom_polygon(data= rest_europe, aes(long,lat, group=group), fill="#f3e3f4", color = "black") +
  geom_polygon(data= europe_from_norway, aes(long,lat, group=group), fill="#de676c", color = "black") +
  geom_polygon(data= europe_from_greece, aes(long,lat, group=group), fill="#6784de", color = "black") +
  geom_polygon(data= greece, aes(long,lat, group=group), fill="#0d0e58", color = "black") +
  geom_polygon(data= norway, aes(long,lat, group=group), fill="#811212", color = "black") +
  geom_polygon(data= to_both, aes(long,lat, group=group), fill="#ae67de", color = "black") +
  geom_curve(data = votes_coordinates_from_greece, aes(x = CapitalLongitude.x,
        y = CapitalLatitude.x, xend = CapitalLongitude.y, yend = CapitalLatitude.y, color=Score),
             curvature = -0.2, arrow = arrow(type = "closed",length = unit(0.03, "npc"))) +
  scale_colour_gradient(low = "#b6b6b6",
                        high = "#000000") +
  geom_curve(data = votes_coordinates_from_norway, aes(x = CapitalLongitude.x,
        y = CapitalLatitude.x, xend = CapitalLongitude.y, yend = CapitalLatitude.y, color=Score),
             curvature = -0.2, arrow = arrow(type = "closed",length = unit(0.01, "npc")), inherit.aes = FALSE) +
  coord_fixed(1.2) + labs(title = "Norwegia i Grecja")+
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()) -> greece_norway_plot



'%ni%' <- Negate("%in%")

europe_from_norway <- europe1 %>% 
  filter(region %in% unique(votes_coordinates_from_norway$to_full),
         region %ni% unique(votes_coordinates_from_greece$to_full))

europe_from_greece <- europe1 %>% 
  filter(region %in% unique(votes_coordinates_from_greece$to_full), 
         region %ni% unique(votes_coordinates_from_norway$to_full))

rest_europe <- europe1 %>% 
  filter(region %ni% unique(votes_coordinates_from_greece$to_full), 
         region %ni% unique(votes_coordinates_from_norway$to_full),
         region %ni% c("Greece", "Norway"))
greece <- europe1 %>% filter(region == "Greece")
norway <- europe1 %>% filter(region == "Norway")

to_both <- europe1 %>% 
  filter(region %in% unique(votes_coordinates_from_greece$to_full), 
         region %in% unique(votes_coordinates_from_norway$to_full))

# Portugalia i Rosja

votes_coordinates_from_portugal <- votes_coordinates %>% 
  filter(from_full == "Portugal", to_full != "Portugal") %>% arrange(desc(V1)) %>% head(10) %>% 
  mutate(Score = log(V1))

votes_coordinates_from_russia <- votes_coordinates %>% 
  filter(from_full == "Russia", to_full != "Russia") %>% arrange(desc(V1)) %>% head(10) %>% 
  mutate(Score = log(V1))

View(europe)
europe1 <- europe %>% 
  filter(lat > 30, lat <75, long >-20, long < 50)
ggplot() + 
  geom_polygon(data= rest_europe, aes(long,lat, group=group), fill="#f3e3f4", color = "black") +
  geom_polygon(data= europe_from_russia, aes(long,lat, group=group), fill="#de676c", color = "black") +
  geom_polygon(data= europe_from_portugal, aes(long,lat, group=group), fill="#6784de", color = "black") +
  geom_polygon(data= portugal, aes(long,lat, group=group), fill="#0d0e58", color = "black") +
  geom_polygon(data= russia, aes(long,lat, group=group), fill="#811212", color = "black") +
  geom_polygon(data= to_both, aes(long,lat, group=group), fill="#ae67de", color = "black") +
  geom_curve(data = votes_coordinates_from_portugal, aes(x = CapitalLongitude.x,
                                                       y = CapitalLatitude.x, xend = CapitalLongitude.y, yend = CapitalLatitude.y, color=Score),
             curvature = -0.2, arrow = arrow(type = "closed",length = unit(0.03, "npc"))) +
  scale_colour_gradient(low = "#b6b6b6",
                        high = "#000000") +
  geom_curve(data = votes_coordinates_from_russia, aes(x = CapitalLongitude.x,
                                                       y = CapitalLatitude.x, xend = CapitalLongitude.y, yend = CapitalLatitude.y, color=Score),
             curvature = -0.2, arrow = arrow(type = "closed",length = unit(0.01, "npc")), inherit.aes = FALSE) +
  coord_fixed(1.2) + labs(title = "Portugalia i Rosja") +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank())  -> portugal_russia_plot



'%ni%' <- Negate("%in%")

europe_from_russia <- europe1 %>% 
  filter(region %in% unique(votes_coordinates_from_russia$to_full),
         region %ni% unique(votes_coordinates_from_portugal$to_full))

europe_from_portugal <- europe1 %>% 
  filter(region %in% unique(votes_coordinates_from_portugal$to_full), 
         region %ni% unique(votes_coordinates_from_russia$to_full))

rest_europe <- europe1 %>% 
  filter(region %ni% unique(votes_coordinates_from_portugal$to_full), 
         region %ni% unique(votes_coordinates_from_russia$to_full),
         region %ni% c("Portugal", "Russia"))
portugal <- europe1 %>% filter(region == "Portugal")
russia <- europe1 %>% filter(region == "Russia")

to_both <- europe1 %>% 
  filter(region %in% unique(votes_coordinates_from_portugal$to_full), 
         region %in% unique(votes_coordinates_from_russia$to_full))

  

# Polska
votes_coordinates_from_poland <- votes_coordinates %>% 
  filter(from_full == "Poland", to_full != "Poland") %>% arrange(desc(V1)) %>% head(10) %>% 
  mutate(Score = log(V1))

poland <- europe1 %>% filter(region == "Poland")

europe_from_poland <- europe1 %>% 
  filter(region %in% unique(votes_coordinates_from_poland$to_full))

not_poland <- europe1 %>% 
  filter(region %ni% c("Poland"))


ggplot() + 
  geom_polygon(data= not_poland, aes(long,lat, group=group), fill="#f3e3f4", color = "black") +
  geom_polygon(data= europe_from_poland, aes(long,lat, group=group), fill="#6784de", color = "black") +
  geom_polygon(data= poland, aes(long,lat, group=group), fill="#0d0e58", color = "black")  +
  geom_curve(data = votes_coordinates_from_poland, aes(x = CapitalLongitude.x,
    y = CapitalLatitude.x, xend = CapitalLongitude.y, yend = CapitalLatitude.y, color=Score),
             curvature = -0.2, arrow = arrow(type = "closed",length = unit(0.03, "npc"))) +
  scale_colour_gradient(low = "#b6b6b6",
                        high = "#000000") +
  coord_fixed(1.4) + labs(title = "Polska") +
  theme(panel.background = element_rect(fill = 'white', color = 'white'),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())-> poland_plot

