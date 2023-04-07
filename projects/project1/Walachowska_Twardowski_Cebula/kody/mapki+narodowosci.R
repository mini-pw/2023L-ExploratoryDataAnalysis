library(ggmap)
library(mapdata)
library(ggplot2)
library(dplyr)
library(countrycode)



file <- read.csv("Lokacje.csv")
file$year <- as.integer(substr(file$db,1,4))
file$is_new <- ifelse(file$year > 1900, 1, 0)


df1 <- aggregate(is_new~city+lat+lon, file, sum)
df2 <- aggregate(is_new~city+lat+lon, file, length)


df1$wiekszosc <- ifelse(df1$is_new / df2$is_new >= 0.5,1,0)


map_world <- map_data("world")
europe <- subset(map_world, region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                                          "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                          "Croatia", "Cyprus", "Czech Republic","Denmark","Estonia","Finland", 
                                          "France","Georgia", "Germany", "Greece","Hungary","Iceland", 
                                          "Ireland", "Italy","Kazakhstan", "Kosovo", "Latvia","Liechtenstein", 
                                          "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                          "Netherlands","Norway","North Macedonia","Poland","Portugal","Romania",
                                          "Russia","San Marino","Serbia","Slovakia","Slovenia","Spain",
                                          "Sweden","Switzerland","Turkey","Ukraine","UK","Vatican"))


#mapki
map <- ggplot() +
  geom_polygon(data = europe,aes(x = long, y = lat, group = group), color = "black", fill = "#E3A857")+
  geom_point(data = df1, aes(x = lon, y = lat, color = wiekszosc), alpha = 0.9, size = 1.7)+
  coord_map(xlim=c(-30, 32), ylim=c(34, 60)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_gradient(low = "peru", high = "#8D2726")+
  ggtitle("Preferencje oper do wspó³czesnych lub klasycznych kompozytorów")+
  theme(legend.position="none")

map





# wykres z narodowosciami

file$narodowosc = ifelse(file$iso == file$nat, 1,0)
file$country <- countrycode(file$iso, "iso2c", "country.name")

df3 <- aggregate(file$narodowosc, by=list(Category=file$country), FUN=sum)
df4 <- aggregate(file$narodowosc, by=list(Category=file$country), FUN=length)
df3$narodowosc <- df3$x/df4$x
wykres$narodowosc <- percent(wykres$narodowosc)

positions <- c("Germany", "Czechia", "France","Mongolia","Azerbaijan","Thailand","Iceland","Armenia","Russia","Italy")
colnames(df3)[1] <- "kraje"



ggplot(data = df3, aes(x =kraje, y =narodowosc))+
  geom_bar(stat="identity",fill="#8d2726", width = 0.5,)+
  scale_x_discrete(limits = positions,guide = guide_axis(n = 2)) +
  ggtitle("10 krajów które maj¹ najwiêkszy udzia³ grania kompozycji od kompozytorów z swojego kraju")+
  scale_y_continuous(labels = scales::percent)+
  #theme(panel.background = element_blank())+
  ylab("")+
  coord_flip()

