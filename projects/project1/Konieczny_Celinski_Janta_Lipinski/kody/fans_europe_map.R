library(ggplot2)
library(maps)
library(dplyr)
library(mapdata)
library(showtext)

# Adding custom font
font_add("Sheeran", "C:/Users/Kuba/AppData/Local/Microsoft/Windows/Fonts/fletcher_regular.otf")
showtext_auto()

# Creating useful functions
`%!in%` <- Negate(`%in%`)
theme_nothing <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# Creating 'europe' data frame, that we will use to create Europe map
world <- map_data("world")
world %>% 
  filter(long > -10, long < 40) %>% 
  filter(lat > 35, lat < 70) %>% 
  filter(region %!in% c("Russia","Syria", "Algeria","Tunisia", "Morocco")) -> europe

# Creating data frame with number of fans in each country
fans <- data.frame(region = c("Poland", "Italy", "Switzerland", "Germany", "UK",
                             "Sweden", "Denmark", "Netherlands", "Belgium", "France",
                             "Spain", "Ireland", "Austria", "Portugal", "Romania",
                             "Czech Republic", "Latvia", "Finland", "Iceland"),
                   count = c(104452,137107,109586,775355,1580427,191210,102397,135718,
                             86138,367572,139826,402229,178187,118085,47166,142036,
                             50437,108000,43830))
# Joining data frame with number of fans to 'europe' data frame
europe %>% 
  left_join(fans, by = "region") -> europe

# Division into compartments to create bins in the legend
europe$color <- ifelse(europe$count < 1 | is.na(europe$count), "grey",
                       ifelse(europe$count >= 1 & europe$count < 120000, "#11ffff", 
                              ifelse(europe$count >= 120000 & europe$count < 250000, "#2cc0d6", 
                                     ifelse(europe$count >= 250000 & europe$count < 1000000, "#0684a4",
                                            ifelse(europe$count >= 1000000, "#004c6d", "black")))))

# Creating a factor needed to properly color the map
europe$color <- factor(europe$color, levels = c("grey", "#11ffff", "#2cc0d6", "#0684a4", "#004c6d", "black"))

# Creating map
europe_map <- ggplot() +
  geom_polygon(data = europe, aes(x=long, y = lat, fill = color, group = group), color = "white") + 
  coord_fixed(1.4) +
  theme_bw() +
  theme_nothing +
  scale_fill_manual(values = c("grey", "#11ffff", "#2cc0d6", "#0684a4", "#004c6d", "black"), 
                    breaks = c("grey", "#11ffff", "#2cc0d6", "#0684a4", "#004c6d", "black"),
                    labels = c("no concerts", "0 - 120 000","120 000 - 250 000", "250 000 - 1 000 000","over 1 000 000", ""))+
  labs(title = expression(bold("Total number of fans at Ed Sheeran's Divide Tour concerts by country")), 
       x = "", 
       y = "", 
       fill = "Number of fans",
       caption = "*countries where no concerts were held are marked in grey") +
  theme(plot.title = element_text(hjust = 0.2, size = 23, family = "Sheeran", colour = "white"), 
        legend.title = element_text(size = 18, family = "Sheeran", colour = "white"),
        plot.caption = element_text(size = 15, family = "Sheeran", colour = "white"),
        legend.text = element_text(color = "white"))

europe_map