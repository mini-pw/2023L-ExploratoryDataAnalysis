
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(SmarterPoland)
library(stringi)
library(stringr)


editing_region <- function(str) {
  if(str == "United States") {
    return("USA")
  }
  if(str == "United Kingdom"){
    return("UK")
  }
  if(str == "DR Congo"){
    return("Democratic Republic of the Congo")
  }
  if(str == "Congo"){
    return("Republic of Congo")
  }
  return(str)
}
editing_GDP <- function(gdp) {
  if(gdp > 6e5){
    return(gdp/1000)
  }
  return(gdp)
    
}

world <- map_data("world")
gdp <- read.csv("./gdp_per_capita.csv", encoding="UTF-8")
gdp %>%
  transmute(region = stri_sub(Country.Territory,1,-3),GDP.per.capita = str_replace_all(CIA, ",", ".")) %>%  
  filter(region != "Country/Territo") %>% 
  mutate(GDP.per.capita = 1000 * as.numeric(GDP.per.capita)) -> gdp.per.capita.edited

gdp.per.capita.edited$region <- as.character(lapply(gdp.per.capita.edited$region, editing_region))
gdp.per.capita.edited$GDP.per.capita <- as.numeric(lapply(gdp.per.capita.edited$GDP.per.capita, editing_GDP))



world %>% 
  left_join(gdp.per.capita.edited, by = c("region")) -> world.with.gdp.per.capita

ggplot() +
  geom_polygon(data = world.with.gdp.per.capita, aes(x = long, y = lat, group = group, fill = GDP.per.capita), color = "black") +
  theme_minimal() +
  ggtitle("Produkt krajowy brutto per capita")+
  scale_fill_gradient2(
    low = "red",
    high = "darkblue",
    mid = "white",
    limits = c(0, 100000),
    midpoint = 34900
  ) +
  labs(x = "", y = "", fill = "GDP per capita")

  






