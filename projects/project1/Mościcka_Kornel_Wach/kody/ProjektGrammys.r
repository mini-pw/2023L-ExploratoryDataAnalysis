install.packages(maps)
library(dplyr)
library(ggplot2)
library(stringi)
library(stringr)
library(maps)
library(mapdata)

char<-read.csv("charts.csv") #to jest ta giga ramka 1GB z githuba ze spotify
                             # https://www.kaggle.com/datasets/dhruvildave/spotify-charts?fbclid=IwAR0x9mfBiqjoY3MeHQV4b0klKjEjNyJQtWASGReWGcocUCIaSj31r27bM88


#wykres pude³kowy
c2<-char %>% 
  select(title,artist,rank,region,date,streams) %>% 
  filter(region=="United States") %>% 
  filter(!is.na(streams)) %>%   
  select(title,rank,date,artist) %>% 
  mutate(rok=case_when(stri_detect_fixed(date, "2020")==TRUE ~ "2020", 
                       stri_detect_fixed(date, "2021")==TRUE ~ "2021")) %>% 
  filter((title=="everything i wanted"&rok=="2020")|title=="Leave The Door Open"|
           (rok=="2020"& title=="Don't Start Now")|(title=="Say So"&rok=="2020")|title=="drivers license"|(artist=="Ed Sheeran"&title=="Bad Habits")|
           (title=="Blinding Lights"&rok=="2020")|title=="STAY (with Justin Bieber)")%>% 
  mutate(type=case_when(title=="everything i wanted"~ "winner",
                       title=="Leave The Door Open"~ "winner",
                       title=="Don't Start Now"~ "nominee",
                       title=="Say So"~"nominee",
                       title=="drivers license"~"nominee",
                       title=="Bad Habits"~"nominee",
                       title=="Blinding Lights"~"Spotify Top 5",
                       title=="STAY (with Justin Bieber)"~"Spotify Top 5"
                       
                       )) 

ggplot(c2, aes(x = reorder(title,-rank), y = rank, fill=type) )+
  geom_boxplot(outlier.shape = NA, color = 'mint cream')+
  theme(plot.background = element_rect(fill='#272727', color=NA),
        panel.grid= element_blank(),
        panel.background = element_rect(fill = '#272727'),
        axis.text = element_text(color = "#FFFEC1"),
        axis.title =  element_text(color = "#FFFEC1"),
        legend.background = element_rect(fill ="#272727"),
        legend.title = element_text(color = "#FFFEC1"),
        legend.key = element_rect(fill = '#272727'),
        legend.text = element_text(color = "#FFFEC1"),
        plot.title = element_text(color = "#FFFEC1"))+
  scale_y_continuous(breaks = seq(0,200,10))+
  labs(x=" ",y="position in daily ranking", title='Record Of the Year vs Spotify Top 5')+
  scale_fill_manual(values = c('#FDF62D','#FF2A12', '#FFB83D'))+
  coord_flip()
  

#mapa
giga2 <- char %>% 
  select(region, streams) %>% 
  filter(! (region == "Andorra" | region == "Global" | region == "Luxembourg")) %>% 
  group_by(region) %>% 
  summarise(logstreams = log(sum(streams, na.rm = T))) %>% 
  mutate(region = str_replace(region, "United States", "USA" ))


country <- map_data("world")

index_table <- data.frame(region = unique(giga2$region),
                          index = giga2$logstreams)
theme_nothing <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
mapaspotify <- country %>% 
  left_join(index_table) %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = index), color = "black") +
  coord_fixed(1.3) + 
  scale_fill_gradient2(limits = c(18, 26),
                       mid = "#FFD136",
                       high = "#FF2A12",
                       low = "#FCFC4A",
                       na.value="#FFFEC1",
                       midpoint = 21,
                       name = element_blank()
  ) +
  theme(plot.background = element_rect(fill='#272727', color=NA),
        panel.background = element_rect(fill = '#272727'),
        legend.background = element_rect(fill ="#272727"),
        legend.title = element_text(color = "#FFFEC1"),
        legend.key = element_rect(fill = '#272727'),
        legend.text = element_text(color = "#FFFEC1"))+
  theme(legend.position = "right") +
  labs(fill = "Index") +
  theme_nothing
mapaspotify









#Wykres liniowy

library(dplyr)
library(readr)
library(ggplot2)

#grammy_awards <- read_csv("C:/Users/olako/OneDrive/Pulpit/2 rok/eksploracja danych/projekt1/the_grammy_awards.csv")
Spotify4 <- read_csv("C:/Users/olako/OneDrive/Pulpit/2 rok/eksploracja danych/projekt1/charts.csv")
                    #https://www.kaggle.com/datasets/dhruvildave/spotify-charts?fbclid=IwAR0x9mfBiqjoY3MeHQV4b0klKjEjNyJQtWASGReWGcocUCIaSj31r27bM88


Spotify <- Spotify4 %>%
  filter(region =="United States") %>% 
  select(title, date, streams, artist)

grammy <- Spotify %>%
  filter(title %in% c("Shape of You", "That's What I Like","24K Magic","Feel It Still")) %>% 
  select(title, date, streams, artist)


Spotify2 <- grammy %>%
  arrange(date) %>% 
  mutate(Year=strftime(date,"%Y")) %>% 
  filter(streams!="NA", Year %in% c("2017","2018") )

Sys.setlocale("LC_ALL","English")
ggplot(Spotify2, aes(x = date, y=streams, colour=title, fill=title)) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", guide=guide_axis(angle = 30))+
  geom_smooth(size = 1,alpha=0)+
  scale_color_manual(values = c("#FCA000", "#FCFC4A", "#FF2A12", "#FFFEC1"))+
  scale_fill_manual(values = c( "#FCA000", "#FCFC4A",  "#FF2A12", "#FFFEC1"))+
  theme(plot.background = element_rect(fill="#272727", color=NA),
        panel.background = element_rect(fill = "#272727"),
        panel.grid = element_line(color = "#51514D"),
        axis.text = element_text(color = "#FFFEC1"),
        axis.title.x = element_text(color = "#FFFEC1"),
        axis.title.y = element_text(color = "#FFFEC1"),
        plot.title = element_text(color = "#FFFEC1"),
        legend.background = element_rect(fill ="#272727"),
        legend.title = element_text(color = "#FFFEC1"),
        legend.key = element_rect(fill = '#272727'),
        legend.text = element_text(color = "#FFFEC1"))+
  labs(x="date", 
       y="number of streams",
       color="",)+
  guides(fill = "none")















  