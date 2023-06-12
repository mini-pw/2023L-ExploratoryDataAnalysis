library(readr)
library(dplyr)

sciezka_do_pliku <- "imdb_top_250_series_episode_ratings.csv"
dane <- read_csv(sciezka_do_pliku)

breaking_bad <- read.csv("breaking_bad.csv")
friends <- read.csv("friends.csv")
game_of_thrones <- read.csv("game_of_thrones.csv")
seinfeld <- read.csv("seinfeld.csv")
simpsons <- read.csv("simpsons.csv")
the_office <- read.csv2("the_office.csv")

needed_colnames <- c("Title", "Season", "Episode", "Directed_by" , "Written_by", "Viewers")

# RAMKA BREAKING BAD

new_colnames_bd <- c("Date", "Season", "Episode", "Title", "Directed_by", 
                  "Written_by", "Duration_mins", "Summary", "Rating_IMDB", 
                  "U.S._viewers_million")

colnames(breaking_bad) <- new_colnames_bd

breaking_bad_1 <- breaking_bad %>% 
  mutate(Viewers = ifelse(is.na(U.S._viewers_million), NA,
                          round(as.double(U.S._viewers_million) * 1e+06)),
         Title = "Breaking Bad")  %>% 
  select(Title, Season, Episode, Directed_by , Written_by, Viewers)

colnames(friends_1) <- needed_colnames

# RAMKA FRIENDS

friends_1 <- friends %>% 
  mutate_all(~gsub("&", ",", .)) %>% 
  mutate(Title = "Friends") %>% 
  select(Title, season, episode_num_in_season, directed_by, written_by, us_viewers)

colnames(friends_1) <- needed_colnames

# RAMKA GAME OF THRONES

game_of_thrones_1 <- game_of_thrones %>%  
  mutate(Viewers = ifelse(is.na(`U.S. Viewers (Millions)`), NA,
                          round(as.double(`U.S. Viewers (Millions)`) * 1e+06)),
         Title = "Game of Thrones") %>% 
  select( Title, Season, `No. of Episode (Season)`, `Directed by`, `Written by`,
         Viewers)

colnames(game_of_thrones_1) <- needed_colnames

# RAMKA SEINDELD

seinfeld_1 <- seinfeld %>% 
  select(season, episode_num_in_season, directed_by, written_by, us_viewers) %>% 
  mutate_all(~gsub("&", ",", .)) %>% 
  mutate(Title = "Seinfeld")

colnames(seinfeld_1) <- needed_colnames
  
# RAMKA SIMPSONS

simpsons_1 <- simpsons %>% 
  mutate(Viewers = episode_votes * 100,
         Title = "The Simpsons",
         Directed_by = NaN ,
         Written_by = NaN) %>% 
  select(Title, season_no, episode_no, Directed_by, Written_by, Viewers)

colnames(simpsons_1) <- needed_colnames

# RAMKA THE OFFICE

the_office_1 <- the_office %>% 
  mutate(Viewers = Viewership * 1e+06,
         Title = "The Office",
         ) %>% 
  select(Title, Season, Episode, Director, Writers, Viewers) 

colnames(the_office_1) <- needed_colnames

# TWORZENIE JEDNEJ RAMKI DANYCH

all_series <- rbind(friends_1, breaking_bad_1, game_of_thrones_1, 
                    the_office_1, simpsons_1, seinfeld_1)

all_series$Season <- as.numeric(all_series$Season)
all_series$Episode <- as.numeric(all_series$Episode)

# £¥CZENIE RAMEK DANYCH

dane_1 <- dane %>%  
  filter(Title %in% c("Breaking Bad", "Friends", "Game of Thrones", 
                            "Seinfeld", "The Simpsons", "The Office" )) %>% 
  select(Season, Episode, Rating, Title)

dane_2 <- left_join(dane_1, all_series, by = c("Title", "Season", "Episode") )

# Zapisanie do pliku csv
library(readr)
write_csv(dane_2, path = "gotowe_dane_2.csv")
