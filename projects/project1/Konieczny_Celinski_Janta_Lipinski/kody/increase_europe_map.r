{
  library(csvread)
  library(csvwr)
  library(dplyr)
  library(data.table)
  library(scales)
  library(ggplot2)
  library(maps)
  library(mapdata)
  library(patchwork)
  library(RColorBrewer)
  library(showtext)
}

font_add("Sheeran", "C:/Users/Kuba/AppData/Local/Microsoft/Windows/Fonts/fletcher_regular.otf")
showtext_auto()

ed_sheeran <- read.csv(file = 'ed_sheeran.csv')
#Zmieniamy typ kolumny date na Date
ed_sheeran$date <- as.Date(ed_sheeran$date)


# W?ochy
ed_italy <- ed_sheeran %>% 
  filter(region == 'Italy')

# ods?uchania tydzie? przed koncertem dla w?och
ed_italy_before <- filter(ed_italy, date == "2017-03-19") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

# ods?uchania w dzie? koncertu dla W?och
ed_italy <- filter(ed_italy, date == "2017-03-16") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_italy <- ed_italy %>% left_join(ed_italy_before)


# Niemcy 
ed_germany <- ed_sheeran %>% 
  filter(region == 'Germany')

ed_germany_before <- filter(ed_germany, date == "2018-07-12") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_germany <- filter(ed_germany, date == "2018-07-19") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_germany <- ed_germany %>% left_join(ed_germany_before)


# Szwajcaria 
ed_switzerland <- ed_sheeran %>% 
  filter(region == 'Switzerland')

ed_switzerland_before <- filter(ed_switzerland, date == "2018-07-28") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_switzerland <- filter(ed_switzerland, date == "2018-08-04") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_switzerland <- ed_switzerland %>% left_join(ed_switzerland_before)


# Wielka Brytania
ed_UK <- ed_sheeran %>% 
  filter(region == 'United Kingdom')

ed_UK_before <- filter(ed_UK, date == "2018-05-18") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_UK <- filter(ed_UK, date == "2018-05-25") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_UK <- ed_UK %>% left_join(ed_UK_before) 

ed_UK[1,1] <- "UK"
# Szwecja
ed_sweden <- ed_sheeran %>% 
  filter(region == 'Sweden')

ed_sweden_before <- filter(ed_sweden, date == "2018-07-04") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_sweden <- filter(ed_sweden, date == "2018-07-11") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_sweden <- ed_sweden %>% left_join(ed_sweden_before)

# Dania
ed_denmark <- ed_sheeran %>% 
  filter(region == 'Denmark')

ed_denmark_before <- filter(ed_denmark, date == "2019-07-21") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_denmark <- filter(ed_denmark, date == "2019-07-28") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_denmark <- ed_denmark %>% left_join(ed_denmark_before)

# Holandia 
ed_netherlands <- ed_sheeran %>% 
  filter(region == 'Netherlands')

ed_netherlands_before <- filter(ed_netherlands, date == "2018-06-22") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_netherlands <- filter(ed_netherlands, date == "2018-06-29") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_netherlands <- ed_netherlands %>% left_join(ed_netherlands_before)

# Belgia 
ed_belgium <- ed_sheeran %>% 
  filter(region == 'Belgium')

ed_belgium_before <- filter(ed_belgium, date == "2018-06-24") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_belgium <- filter(ed_belgium, date == "2018-07-01") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_belgium <- ed_belgium %>% left_join(ed_belgium_before)


# Francja
ed_france <- ed_sheeran %>% 
  filter(region == 'France')

ed_france_before <- filter(ed_france, date == "2018-06-30") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_france <- filter(ed_france, date == "2018-07-07") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_france <- ed_france %>% left_join(ed_france_before)


# Hiszpania
ed_spain <- ed_sheeran %>% 
  filter(region == 'Spain')

ed_spain_before <- filter(ed_spain, date == "2019-06-04") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_spain <- filter(ed_spain, date == "2019-06-11") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_spain <- ed_spain %>% left_join(ed_spain_before)


# Irlandia 
ed_ireland <- ed_sheeran %>% 
  filter(region == 'Ireland')

ed_ireland_before <- filter(ed_ireland, date == "2018-04-28") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_ireland <- filter(ed_ireland, date == "2018-05-05") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_ireland <- ed_ireland %>% left_join(ed_ireland_before)


# Polska 
ed_poland <- ed_sheeran %>% 
  filter(region == 'Poland')

ed_poland_before <- filter(ed_poland, date == "2018-08-05") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_poland <- filter(ed_poland, date == "2018-08-12") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_poland <- ed_poland %>% left_join(ed_poland_before)


# Austria
ed_austria <- ed_sheeran %>% 
  filter(region == 'Austria')

ed_austria_before <- filter(ed_austria, date == "2018-08-01") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_austria <- filter(ed_austria, date == "2018-08-08") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_austria <- ed_austria %>% left_join(ed_austria_before)

# Portugalia 
ed_portugal <- ed_sheeran %>% 
  filter(region == 'Portugal')

ed_portugal_before <- filter(ed_portugal, date == "2019-05-26") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_portugal <- filter(ed_portugal, date == "2019-06-02") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_portugal <- ed_portugal %>% left_join(ed_portugal_before)

# Rumunia 
ed_romania <- ed_sheeran %>% 
  filter(region == 'Romania')

ed_romania_before <- filter(ed_romania, date == "2019-06-26") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_romania <- filter(ed_romania, date == "2019-07-03") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_romania <- ed_romania %>% left_join(ed_romania_before)


# CZechy
ed_czech_republik <- ed_sheeran %>% 
  filter(region == 'Czech Republic')

ed_czech_republik_before <- filter(ed_czech_republik, date == "2019-07-01") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_czech_republik <- filter(ed_czech_republik, date == "2019-07-08") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_czech_republik <- ed_czech_republik %>% left_join(ed_czech_republik_before)


# ?otwa 
ed_latvia <- ed_sheeran %>% 
  filter(region == 'Latvia')

ed_latvia_before <- filter(ed_latvia, date == "2019-07-05") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_latvia <- filter(ed_latvia, date == "2019-07-12") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_latvia <- ed_latvia %>% left_join(ed_latvia_before)


# Finlandia
ed_finland <- ed_sheeran %>% 
  filter(region == 'Finland')

ed_finland_before <- filter(ed_finland, date == "2019-07-17") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_finland <- filter(ed_finland, date == "2019-07-24") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_finland <- ed_finland %>% left_join(ed_finland_before)


# W?gry 
ed_hungary <- ed_sheeran %>% 
  filter(region == 'Hungary')

ed_hungary_before <- filter(ed_hungary, date == "2019-07-31") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_hungary <- filter(ed_hungary, date == "2019-08-07") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_hungary <- ed_hungary %>% left_join(ed_hungary_before)


# Islandia 
ed_iceland <- ed_sheeran %>% 
  filter(region == 'Iceland')

ed_iceland_before <- filter(ed_iceland, date == "2019-08-03") %>% group_by(region) %>% 
  summarise(przed = sum(streams, na.rm = TRUE))

ed_iceland <- filter(ed_iceland, date == "2019-08-10") %>% group_by(region) %>% 
  summarise(po = sum(streams, na.rm = TRUE))

ed_iceland <- ed_iceland %>% left_join(ed_iceland_before)



# Tworzymy ramk? danych z naszymi krajami 

tmp <- rbind(ed_austria,ed_belgium, ed_czech_republik,ed_denmark, ed_finland,
             ed_france, ed_germany, ed_hungary, ed_iceland, ed_ireland,
             ed_italy, ed_latvia, ed_netherlands, ed_poland, ed_portugal,
             ed_romania, ed_spain, ed_sweden, ed_switzerland, ed_UK) %>% 
  mutate(coefficient = (abs(po - przed)/przed)*100)

# Tworzymy map? europy

`%!in%` <- Negate(`%in%`)

theme_nothing <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

world <- map_data("world")
europe <- world %>% 
  filter(long > -10, long < 40) %>% 
  filter(lat > 35, lat < 70) %>% 
  filter(region %!in% c("Russia","Syria", "Algeria","Tunisia", "Morocco"))

europe <- europe %>% 
  left_join(tmp)

europe$color <- ifelse(europe$coefficient == 0 | is.na(europe$coefficient), "grey",
                       ifelse(europe$coefficient > 0 & europe$coefficient < 100, "#11ffff", 
                              ifelse(europe$coefficient >= 100 & europe$coefficient < 300, "#2cc0d6", 
                                     ifelse(europe$coefficient >= 300 & europe$coefficient < 500, "#0684a4", "#004c6d"))))
europe$color <- factor(europe$color, levels = c("grey", "#11ffff", "#2cc0d6", "#0684a4", "#004c6d"))

  europe %>% 
    ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = color), color = "white") +
  coord_fixed(1.4) + 
  scale_fill_manual(values = c("grey", "#11ffff", "#2cc0d6", "#0684a4", "#004c6d"), 
                    breaks = c("grey", "#11ffff", "#2cc0d6", "#0684a4", "#004c6d"),
                    labels =c("no concerts", "0-100 %", "100-300 %", "300-500 %", "over 500 %")) +
  theme_bw() +
    labs(title = expression(bold("Percentage increase of streams at Ed Sheeran's Divide Tour in each country")), 
         x = "", 
         y = "", 
         fill = "Increase of streams",
         caption = "*countries where no concerts were held are marked in grey") +
    theme(plot.title = element_text(hjust = 0.25, size = 25, family = "Sheeran", colour = "white"), 
          legend.title = element_text(size = 18, family = "Sheeran", colour = "white"),
          plot.caption = element_text(size = 15, family = "Sheeran", colour = "white"),
          legend.text = element_text(color = "white")) +
  theme_nothing
  