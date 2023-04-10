library(dplyr)
library(ggplot2)
library(readr)
library(fmsb)
library(maps)
library(mapdata)

#Dane
charts_3 <- read_csv("duzy/charts_3.csv") #zmieni³em nazwê z orginalnej 'charts' na 'charts_3' ¿eby siê nie pokrywa³y
charts <- read_csv("duzy/charts.csv")
top10s <- read_csv("duzy/top10s.csv")


charts_z_miesiacami <- mutate(charts,month=substring(date, 6,7 ))
charts_z_miesiacami <- mutate(charts_z_miesiacami,year=substring(date, 1,4 ))


#Mapa
charts_zmodyfikowany <- charts_3 %>% 
  filter(title == "Last Christmas", artist == "Wham!") %>% 
  mutate(month = substring(date, 6,7 ), year = substring(date, 1,4)) %>% 
  filter(year == 2021) %>% 
  group_by(artist, title, region, year) %>% 
  summarise(liczba_wyswietlen = sum(streams))

charts_zmodyfikowany<-mutate(charts_zmodyfikowany, region=ifelse(region=='United States','USA', ifelse(region=='United Kingdom','UK',region)), .keep = "unused")


w2hr <- map_data("world")
mapa_1<-left_join(w2hr,charts_zmodyfikowany,by='region') %>%
  filter(region!='Antarctica') %>% 
  mutate(nowa=ifelse(is.na(liczba_wyswietlen),1,liczba_wyswietlen)) 

theme_nothing <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank())

options(scipen = 999)

ggplot(data = mapa_1) +
  geom_polygon(aes(x = long, y = lat, fill =nowa , group = group), color = "black") +
  coord_fixed(1.3)+
  scale_fill_gradient(low = "#f7f7f7",
                      high = "#0571b0") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(fill = "Index")+
  theme_nothing+
  scale_fill_gradient(low = "#f7f7f7",
                      high = "#0571b0",trans = "log")

#Wykres liniowy
last_chrismas_Wham <- charts_z_miesiacami %>% 
  filter(song %in% c("Last Christmas","Rockin' Around The Christmas Tree","It's The Most Wonderful Time Of The Year",	"All I Want For Christmas Is You")) %>% 
  filter(artist %in% c("Wham!","Brenda Lee","Mariah Carey","Andy Williams")) %>% filter(year %in% c(2018:2022)) %>% select(rank,song,date) %>% slice(1:68)

date<-c(rep(as.Date('2018-11-30'),times=4),
        rep(as.Date('2019-01-20'),times=4),rep(as.Date('2019-11-20'),times=4),
        rep(as.Date('2020-01-20'),times=4),rep(as.Date('2020-11-20'),times=4),
        rep(as.Date('2021-01-20'),times=4))
song<-rep(c("Last Christmas","Rockin' Around The Christmas Tree","It's The Most Wonderful Time Of The Year",	"All I Want For Christmas Is You"), times=6)
rank<-rep(50,times=24)
dodatokwa_ramka <- data.frame(rank,song, date)
last_chrismas_Wham_2<-rbind(last_chrismas_Wham,dodatokwa_ramka)

my_colors <- c(rgb(202,0,32,255,maxColorValue = 255),rgb(244,165,130,255,maxColorValue = 255),rgb(5,113,176,255,maxColorValue = 255),rgb(146,197,222,255,maxColorValue = 255))

ggplot(last_chrismas_Wham_2, aes(y = rank, x = date, group=song,color=song)) + 
  geom_line(size=1.3)+
  scale_x_date(date_labels = "%B",date_breaks = 'month')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=17))+
  theme(axis.title = element_blank(), 
        panel.background = element_blank())+
  scale_color_manual(values = my_colors)+
  labs(color='Tytu³')+
  scale_y_continuous(trans = "reverse" ,limits = c(50,1))


#Wykresy radarowe
#porRównanie all i want fo chrisams i last chrismams


all_i_want_for_chrismas<-c(150,62.7,33.6,-7.4,7,35,241,16.4,3,72)
last_chrismas<-c(107,47,73.5,-12,35,94,262,18,2,73)

dane_all_i_want_i_wake_me_up<-as_data_frame(rbind(all_i_want_for_chrismas,last_chrismas))
colnames(dane_all_i_want_i_wake_me_up)<-c('bpm','energia','tanecznoœæ','dB','¿ywotnoœæ','nastrój','d³ugoœæ','akustycznoœæ','s³ownoœæ','popularnoœæ')
dane_all_i_want_i_wake_me_up <- rbind(c(206,100,100,-2,80,100,424,100,50,100) , c(43,0,0,-15,0,0,134,0,0,0) , dane_all_i_want_i_wake_me_up)


granice_2<- c(rgb(202,0,32,150,maxColorValue = 255),rgb(33,102,172,155,maxColorValue = 255))
granice_1<- c(rgb(202,0,32,222,255,maxColorValue = 255),rgb(33,102,172,255,maxColorValue = 255))

# The default radar chart 
radarchart(dane_all_i_want_i_wake_me_up, axistype=1 , 
           #custom polygon
           pcol=granice_1 , pfcol=granice_2 , plwd=4 , plty=1,
           #custom the grid
           cglcol="#333333", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=8 )
title(main="All I Want For Christmas Is You")


#POrównaie last chrisams i wonderful time



last_chrismas<-c(107,47,73.5,-12,35,94,262,18,2,73)
time<-c(201.6,59.8,24,-8.4,11.7,77.6,151.9,76.6,3.6,61)

dane_all_i_want_i_wake_me_up<-as_data_frame(rbind(time,last_chrismas))
colnames(dane_all_i_want_i_wake_me_up)<-c('bpm','energia','tanecznoœæ','dB','¿ywotnoœæ','nastrój','d³ugoœæ','akustycznoœæ','s³ownoœæ','popularnoœæ')
dane_all_i_want_i_wake_me_up <- rbind(c(206,100,100,-2,80,100,424,100,50,100) , c(43,0,0,-15,0,0,134,0,0,0) , dane_all_i_want_i_wake_me_up)


granice_2<- c(rgb(244,165,130,150,maxColorValue = 255),rgb(33,102,172,155,maxColorValue = 255))
granice_1<- c(rgb(244,165,130,255,maxColorValue = 255),rgb(33,102,172,255,maxColorValue = 255))


# The default radar chart 
radarchart(dane_all_i_want_i_wake_me_up, axistype=1 , 
           #custom polygon
           pcol=granice_1 , pfcol=granice_2 , plwd=4 , plty=1,
           #custom the grid
           cglcol="#333333", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=8)
title(main="It's The Most Wonderful Time Of The Year")


#POrównaie rocking around i last chrismas




rocking_around<-c(67,47.2,58.9,-8.7,50,89.8,126,61, 5,66)
last_chrismas<-c(107,47,73.5,-12,35,94,262,18,2,73)

dane_all_i_want_i_wake_me_up<-as_data_frame(rbind(rocking_around,last_chrismas))
colnames(dane_all_i_want_i_wake_me_up)<-c('bpm','energia','tanecznoœæ','dB','¿ywotnoœæ','nastrój','d³ugoœæ','akustycznoœæ','s³ownoœæ','popularnoœæ')
dane_all_i_want_i_wake_me_up <- rbind(c(206,100,100,-2,80,100,424,100,50,100) , c(43,0,0,-15,0,0,134,0,0,0) , dane_all_i_want_i_wake_me_up)


granice_2<- c(rgb(146,197,222,150,maxColorValue = 255),rgb(5,113,176,155,maxColorValue = 255))
granice_1<- c(rgb(146,197,222,255,maxColorValue = 255),rgb(5,113,176,255,maxColorValue = 255))


# The default radar chart 
radarchart(dane_all_i_want_i_wake_me_up, axistype=1 , 
           #custom polygon
           pcol=granice_1 , pfcol=granice_2 , plwd=4 , plty=1,
           #custom the grid
           cglcol="#333333", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,vlcex=8 )
title(main="Rockin' Around The Christmas Tree")




