library(spotifyr)
library(dplyr)
library(ggplot2)

Sys.setenv(SPOTIFY_CLIENT_ID = '773f667dbddd4b3188f3527afa7a8f7e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'e5e22447130c4685b7d5596bb4ae1ef8')

access_token <- get_spotify_access_token()

### w tej części chcę zbadać jak zmienia się "pozytywność" piosenek na przestrzeni roku, na przykładzie lat 2022 i 2021##

#importujemy ramki i zapisuję jako listę, ramki pochodzą ze strony : https://charts.spotify.com/charts/overview/global

listofdfs <- list(
  read.csv("regional-pl-weekly-2022-01-06.csv"),
  read.csv("regional-pl-weekly-2022-01-13.csv"),
  
  read.csv("regional-pl-weekly-2022-01-20.csv"),
  read.csv("regional-pl-weekly-2022-01-27.csv"),
  # luty
  read.csv("regional-pl-weekly-2022-02-03.csv"),
  read.csv("regional-pl-weekly-2022-02-10.csv"),
  read.csv("regional-pl-weekly-2022-02-17.csv"),
  read.csv("regional-pl-weekly-2022-02-24.csv"),
  # marzec
  read.csv("regional-pl-weekly-2022-03-03.csv"),
  read.csv("regional-pl-weekly-2022-03-10.csv"),
  read.csv("regional-pl-weekly-2022-03-17.csv"),
  read.csv("regional-pl-weekly-2022-03-24.csv"),
  read.csv("regional-pl-weekly-2022-03-31.csv"),
  # kwiecien
  read.csv("regional-pl-weekly-2022-04-07.csv"),
  read.csv("regional-pl-weekly-2022-04-14.csv"),
  read.csv("regional-pl-weekly-2022-04-21.csv"),
  read.csv("regional-pl-weekly-2022-04-28.csv"),
  # maj
  read.csv("regional-pl-weekly-2022-05-05.csv"),
  read.csv("regional-pl-weekly-2022-05-12.csv"),
  read.csv("regional-pl-weekly-2022-05-19.csv"),
  read.csv("regional-pl-weekly-2022-05-26.csv"),
  # czerwiec
  read.csv("regional-pl-weekly-2022-06-02.csv"),
  read.csv("regional-pl-weekly-2022-06-09.csv"),
  read.csv("regional-pl-weekly-2022-06-16.csv"),
  read.csv("regional-pl-weekly-2022-06-23.csv"),
  read.csv("regional-pl-weekly-2022-06-30.csv"),
  # lipiec
  read.csv("regional-pl-weekly-2022-07-07.csv"),
  read.csv("regional-pl-weekly-2022-07-14.csv"),
  read.csv("regional-pl-weekly-2022-07-21.csv"),
  read.csv("regional-pl-weekly-2022-07-28.csv"),
  # sierpien
  read.csv("regional-pl-weekly-2022-08-04.csv"),
  read.csv("regional-pl-weekly-2022-08-11.csv"),
  read.csv("regional-pl-weekly-2022-08-18.csv"),
  read.csv("regional-pl-weekly-2022-08-25.csv"),
  # wrzesien
  read.csv("regional-pl-weekly-2022-09-01.csv"),
  read.csv("regional-pl-weekly-2022-09-08.csv"),
  read.csv("regional-pl-weekly-2022-09-15.csv"),
  read.csv("regional-pl-weekly-2022-09-22.csv"),
  read.csv("regional-pl-weekly-2022-09-29.csv"),
  # październi)k
  read.csv("regional-pl-weekly-2022-10-06.csv"),
  read.csv("regional-pl-weekly-2022-10-13.csv"),
  read.csv("regional-pl-weekly-2022-10-20.csv"),
  read.csv("regional-pl-weekly-2022-10-27.csv"),
  # listopad
  read.csv("regional-pl-weekly-2022-11-03.csv"),
  read.csv("regional-pl-weekly-2022-11-10.csv"),
  read.csv("regional-pl-weekly-2022-11-17.csv"),
  read.csv("regional-pl-weekly-2022-11-24.csv"),
  #grudzien
  read.csv("regional-pl-weekly-2022-12-01.csv"),
  read.csv("regional-pl-weekly-2022-12-08.csv"),
  read.csv("regional-pl-weekly-2022-12-15.csv"),
  read.csv("regional-pl-weekly-2022-12-22.csv"),
  read.csv("regional-pl-weekly-2022-12-29.csv"),
  read.csv("regional-pl-weekly-2023-01-05.csv")
)


# tworzymy funkcję zwracającą średnią pozytywność piosenki w danym tygodniu

mean_positivity_for_week <- function(df){
  uris <- data.frame("track_uri" = 1:200, stringsAsFactors = FALSE)
  for (i in 1:200) {
  uris[[1]][i] <- substr(df[[2]][i], 15, 36)
  }
  # tworzymy kolumnę z valence
  valence_of_song <- 1:200
  for (i in 1:200) {
    valence_of_song[i] <- get_track_audio_features(uris[i,1])$valence
  }
  #ramka z uris, valance i fraction of total streams
  
  week_i <- df %>% 
    select(streams) %>% 
    mutate(uris = uris1$track_uri, valence = valence_of_song, 
           fraction_of_total_streams = df$streams/sum(df$streams),
           mult = valence_of_song * fraction_of_total_streams)
  # średnia ważona (waga to frakcja wśród wszystkich odsłuchań z listy top) pozytywności piosenek w danym tyg
  weighted_mean_pos <- mean(week_i$mult)
  return(weighted_mean_pos)
  
  }
 

#wektor z pozytywnością w ciagu roku, którą będziemy uzupełniać

positivity_whole_year <- rep(0,times = length(listofdfs))

for (i in 1:length(positivity_whole_year)){
  positivity_whole_year[i] = mean_positivity_for_week(listofdfs[[i]])
}

positivity_devided <- positivity_whole_year

# dzielimy przez ilość odsłuchań w danym tygodniu
for (i in 1: length(positivity_whole_year)){
  week_i2 <- listofdfs[[i]] %>% 
    mutate(fraction_of_total_streams = df$streams/sum(df$streams))
  positivity_devided[i]<- positivity_devided[i]*200 / sum(week_i2$fraction_of_total_streams)
}

data_frame_fin <- data.frame(week = 1:53, positivity = positivity_devided)

# tworzymy wektor, który będzie nam służył jako etykieta osi x
tygodnie <- read.csv("tygodnie.csv")
tygodnie <- as.data.frame(tygodnie[1:52,])
tygodnie <- rbind(c("6 sty"), tygodnie)
colnames(tygodnie)<-"data"



sequence <- seq(1,53,2)
tyg <- as.data.frame(tygodnie[sequence,])
colnames(tyg)<-"data"

wykres1 <- data_frame_fin %>% ggplot(aes(x = week, y = positivity))+
  geom_line(size = 1.5, color = "#C51306")+
  ylim(0.5, 0.6)+
  scale_x_continuous(breaks = sequence, labels = tyg$data)+
  theme_minimal()+
  theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45))+
  labs(x ="Tydzień", y = "Średni poziom pozytywności")

wykres1
ggsave(anx,
       filename = "wkres_anx",
       bg = "transparent",
       width = 15, height = 5.2, dpi = 300)

