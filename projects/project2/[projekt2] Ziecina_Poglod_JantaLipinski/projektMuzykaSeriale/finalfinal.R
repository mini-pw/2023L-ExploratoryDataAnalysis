library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(shiny)
library(palmerpenguins)
library(ggplot2)
library(plotly)
library(bslib)
library(stringi)
library(stringr)
library(ggrepel)
library(shinycssloaders)
library(data.table)

# Zakładka 1 dane
data <- read.csv("ViewingActivity.csv")
exploring <- data %>% mutate(Profile.Name = ifelse(Profile.Name=="Magda Papierz",
                                                   "Magda", Profile.Name)) %>%
  mutate(Country = ifelse(Country=="KR (Korea, Republic of)",
                          "KR (Korea)", Country)) %>%
  mutate(Profile.Name = ifelse(Profile.Name=="Pasożyt", "Janek", Profile.Name)) %>%
  mutate(Duration = str_sub(Duration,end=-4)) %>%
  mutate(hours = as.numeric(substr(Duration,1,2)),
         minutes = as.numeric(substr(Duration,4,5))) %>%
  mutate(NewDuration = hours * 60 + minutes) %>%
  mutate(datetime = str_sub(Start.Time,end=-13)) %>%
  mutate(Year = substr(datetime,1,4),
         Month = substr(datetime,4,4)) %>%
  mutate(licznik_filmow=ifelse(!grepl(":",Title),1,0)) %>%
  mutate(licznik_serialow=ifelse(grepl(":",Title),1,0)) %>%
  mutate(Title=gsub(":.*", "", Title)) 

country <- map_data("world")

# Zadładka 2 dane

df_netflix <- read.csv('netflix_titles.csv')
df_amazon <- read.csv('amazon_prime_titles.csv')
df_disney <- read.csv('disney_plus_titles.csv')
df_hbo <- read.csv('hbo_max_titles.csv')

df_netflix$vod <- 'Netflix'
df_amazon$vod <- 'Amazon Prime'
df_disney$vod <- 'Disney+'
df_hbo$vod <- 'HBO Max'

df <- bind_rows(df_netflix, df_amazon, df_disney, df_hbo)

X <- gsub("'", "", df[,8])
Y <- gsub("\\[|\\]", "", X)
df$genres <- Y

x <- gsub("'", "", df[,9])
y <- gsub("\\[|\\]", "", x)
df$production_countries <- y

df <- df %>% 
  select(title, type, year = release_year, countries = production_countries, seasons, genres,
         imdb_score, imdb_votes, tmdb_score, tmdb_popularity, vod) %>% 
  arrange(-imdb_votes)

df <- df %>% mutate(imdb_pom = case_when(imdb_votes > 200 ~ imdb_score,
                                         TRUE ~ NA),
                    tmdb_pom = case_when(tmdb_popularity > 2.5 ~ tmdb_score,
                                         TRUE ~ NA),
                    dif = imdb_score - tmdb_score,
                    dif1 = imdb_pom - tmdb_pom,
                    rating = case_when(is.na(imdb_pom) & is.na(tmdb_pom) ~ NA,
                                       is.na(imdb_pom) & tmdb_popularity < 12 & tmdb_pom > 9.4 ~ NA,
                                       is.na(imdb_pom) & tmdb_pom > 9.4 ~ tmdb_pom + dif * 1/2,
                                       is.na(imdb_pom) ~ tmdb_pom,
                                       is.na(tmdb_pom) ~ imdb_pom,
                                       TRUE ~ (tmdb_pom + imdb_pom)/2)) %>% 
  select(title, type, year, countries, seasons, genres, rating, vod)

df <- df %>% 
  mutate(type = case_when(type == "SHOW" ~ "Serial",
                          TRUE ~ "Film",))

genre_mapping <- c(
  "documentation" = "Dokument", "drama" = "Dramat", "sport" = "Sport", "romance" = "Romans",
  "comedy" = "Komedia", "crime" = "Kryminał", "music" = "Muzyczny", "fantasy" = "Fantasy",
  "european" = "Europejski", "thriller" = "Thriller", "action" = "Akcja", "history" = "Historyczny",
  "family" = "Familijny", "war" = "Wojenny", "animation" = "Animowany", "scifi" = "Science fiction",
  "reality" = "Reality", "western" = "Western", "horror" = "Horror"
  )

country_mapping <- c(
  "US" = "Stany Zjednoczone", "GB" = "Wielka Brytania", "EG" = "Egipt", "IN" = "Indie",
  "DE" = "Niemcy", "SU" = "Związek Radziecki", "DZ" = "Algieria", "CA" = "Kanada",
  "FR" = "Francja", "LB" = "Liban", "JP" = "Japonia", "AR" = "Argentyna",
  "IE" = "Irlandia", "AU" = "Australia", "ET" = "Etiopia", "GH" = "Ghana",
  "BF" = "Burkina Faso", "HK" = "Hongkong", "MX" = "Meksyk", "CN" = "Chiny",
  "ES" = "Hiszpania", "PS" = "Palestyna", "CO" = "Kolumbia", "BE" = "Belgia",
  "NO" = "Norwegia", "IT" = "Włochy", "TR" = "Turcja", "NZ" = "Nowa Zelandia",
  "DK" = "Dania", "HU" = "Węgry", "CZ" = "Czechy", "TW" = "Tajwan",
  "KR" = "Korea Południowa", "RU" = "Rosja", "NG" = "Nigeria", "CH" = "Szwajcaria",
  "MY" = "Malezja", "KW" = "Kuwejt", "PH" = "Filipiny", "ZA" = "Republika Południowej Afryki",
  "RW" = "Rwanda", "MA" = "Maroko", "AT" = "Austria", "SE" = "Szwecja",
  "NL" = "Holandia", "SG" = "Singapur", "KE" = "Kenia", "CL" = "Chile",
  "SA" = "Arabia Saudyjska", "BR" = "Brazylia", "ID" = "Indonezja", "IS" = "Islandia",
  "AE" = "Zjednoczone Emiraty Arabskie", "IL" = "Izrael", "PL" = "Polska", "FI" = "Finlandia",
  "CD" = "Demokratyczna Republika Konga", "RO" = "Rumunia", "UA" = "Ukraina", "BG" = "Bułgaria",
  "QA" = "Katar", "IR" = "Iran", "JO" = "Jordania", "SY" = "Syria",
  "GL" = "Grenlandia", "VE" = "Wenezuela", "BY" = "Białoruś", "VN" = "Wietnam",
  "TN" = "Tunezja", "TH" = "Tajlandia", "GE" = "Gruzja", "IQ" = "Irak",
  "KH" = "Kambodża", "AL" = "Albania", "CU" = "Kuba", "PR" = "Portoryko",
  "RS" = "Serbia", "UY" = "Urugwaj", "PE" = "Peru", "LU" = "Luksemburg",
  "PY" = "Paragwaj", "PK" = "Pakistan", "VA" = "Watykan", "GR" = "Grecja",
  "NP" = "Nepal", "BD" = "Bangladesz", "TZ" = "Tanzania", "CM" = "Kamerun",
  "KG" = "Kirgistan", "MC" = "Monako", "SN" = "Senegal", "BT" = "Bhutan",
  "LK" = "Sri Lanka", "CY" = "Cypr", "PT" = "Portugalia", "AO" = "Angola",
  "ZW" = "Zimbabwe", "MW" = "Malawi", "GT" = "Gwatemala", "MU" = "Mauritius",
  "IO" = "Brytyjskie Terytorium Oceanu Indyjskiego", "AF" = "Afganistan", "KN" = "Saint Kitts i Nevis",
  "EE" = "Estonia", "DO" = "Dominikana", "PA" = "Panama", "FO" = "Wyspy Owcze",
  "YU" = "Jugosławia", "XC" = "Czechosłowacja", "LI" = "Liechtenstein", "CI" = "Wybrzeże Kości Słoniowej",
  "AN" = "Antyle Holenderskie", "SK" = "Słowacja", "SZ" = "Eswatini", "JM" = "Jamajka",
  "BO" = "Boliwia", "LT" = "Litwa", "EC" = "Ekwador", "KZ" = "Kazachstan",
  "MT" = "Malta", "CF" = "Republika Środkowoafrykańska", "GD" = "Grenada", "SO" = "Somalia",
  "TT" = "Trynidad i Tobago", "XK" = "Kosowo", "CR" = "Kostaryka", "LV" = "Łotwa",
  "SV" = "Salwador", "TC" = "Turks i Caicos", "MN" = "Mongolia", "NI" = "Nikaragua",
  "SB" = "Wyspy Salomona", "VU" = "Vanuatu", "AQ" = "Antarktyda", "FM" = "Mikronezja",
  "UZ" = "Uzbekistan", "NA" = "Namibia", "AZ" = "Azerbejdżan", "BM" = "Bermudy",
  "SI" = "Słowenia", "BA" = "Bośnia i Hercegowina", "HR" = "Chorwacja", "PF" = "Polinezja Francuska",
  "FJ" = "Fidżi", "HN" = "Honduras", "NC" = "Nowa Kaledonia", "OM" = "Oman",
  "MK" = "Macedonia Północna", "UG" = "Uganda", "KI" = "Kiribati", "BW" = "Botswana", "BS" = "Bahamy"
)
    

df <- df %>%
  mutate(
    genres_split = strsplit(genres, ",\\s*"),
    countries_split = strsplit(countries, ",\\s*")) %>%
  mutate(
    genres_pl = sapply(genres_split, function(genres) {
      translated_genres <- sapply(genres, function(genre) {
        if (genre %in% names(genre_mapping))
          genre_mapping[[genre]]
        else
          genre
      })
      paste(translated_genres, collapse = ", ")
    }),
    countries_pl = sapply(countries_split, function(countries) {
      translated_countries <- sapply(countries, function(country) {
        if (country %in% names(country_mapping))
          country_mapping[[country]]
        else
          country
      })
      paste(translated_countries, collapse = ", ")
    })
  )

df <- df %>% 
  mutate(genres = genres_pl, countries = countries_pl) %>% 
  select(title, type, year, countries, seasons, genres, rating, vod)

country_frequency <- table(unlist(strsplit(as.character(df$countries), ", ")))
country_frequency <- country_frequency[order(-country_frequency)]

# Zakładka 3 dane

# Pobieranie ramek danych
data_disney_actors <- read.csv("credits.csv")
data_disney <- read.csv("disney_plus_titles.csv")
data_amazon_actors <- read.csv("credits1.csv")
data_amazon <- read.csv("amazon_prime_titles.csv")
data_HBO_actors <- read.csv("credits2.csv")
data_HBO <- read.csv("hbo_max_titles.csv")
data_Netflix_actors <- read.csv("credits3.csv")
data_Netflix <- read.csv("netflix_titles.csv")

data_VOD <- rbind(data_amazon, data_disney, data_HBO, data_Netflix)
# Tworzenie ramki z gatunkami filmowymi dla netflixa
documentary_net <- subset(data_Netflix, grepl("\\bdocumentation\\b", genres, ignore.case = TRUE))
documentary_net <- documentary_net %>% mutate(z = "Dokument")
drama_net <- subset(data_Netflix, grepl("\\bdrama\\b", genres, ignore.case = TRUE))
drama_net <- drama_net %>% mutate(z = "Dramat")
sport_net <- subset(data_Netflix, grepl("\\bsport\\b", genres, ignore.case = TRUE))
sport_net <- sport_net %>% mutate(z = "Sport")
romance_net <- subset(data_Netflix, grepl("\\bromance\\b", genres, ignore.case = TRUE))
romance_net <- romance_net %>% mutate(z = "Romans")
comedy_net <- subset(data_Netflix, grepl("\\bcomedy\\b", genres, ignore.case = TRUE))
comedy_net <- comedy_net %>% mutate(z = "Komedia")
crime_net <- subset(data_Netflix, grepl("\\bcrime\\b", genres, ignore.case = TRUE))
crime_net <- crime_net %>% mutate(z = "Kryminał")
music_net <- subset(data_Netflix, grepl("\\bmusic\\b", genres, ignore.case = TRUE))
music_net <- music_net %>% mutate(z = "Muzyczny")
fantasy_net <- subset(data_Netflix, grepl("\\bfantasy\\b", genres, ignore.case = TRUE))
fantasy_net <- fantasy_net %>% mutate(z = "Fantasy")
thriller_net <- subset(data_Netflix, grepl("\\bthriller\\b", genres, ignore.case = TRUE))
thriller_net <- thriller_net %>% mutate(z = "Thriller")
action_net <- subset(data_Netflix, grepl("\\baction\\b", genres, ignore.case = TRUE))
action_net <- action_net %>% mutate(z = "Akcja")
history_net <- subset(data_Netflix, grepl("\\bhistory\\b", genres, ignore.case = TRUE))
history_net <- history_net %>% mutate(z = "Historyczny")
family_net <- subset(data_Netflix, grepl("\\bfamily\\b", genres, ignore.case = TRUE))
family_net <- family_net %>% mutate(z = "Familijny")
war_net <- subset(data_Netflix, grepl("\\bwar\\b", genres, ignore.case = TRUE))
war_net <- war_net %>% mutate(z = "Wojenny")
european_net <- subset(data_Netflix, grepl("\\beuropean\\b", genres, ignore.case = TRUE))
european_net <- european_net %>% mutate(z = "Europejski")
animation_net <- subset(data_Netflix, grepl("\\banimation\\b", genres, ignore.case = TRUE))
animation_net <- animation_net %>% mutate(z = "Animowany")
scifi_net <- subset(data_Netflix, grepl("\\bscifi\\b", genres, ignore.case = TRUE))
scifi_net <- scifi_net %>% mutate(z = "Sci-fi")
reality_net <- subset(data_Netflix, grepl("\\breality\\b", genres, ignore.case = TRUE))
reality_net <- reality_net %>% mutate(z = "Reality show")
western_net <- subset(data_Netflix, grepl("\\bwestern\\b", genres, ignore.case = TRUE))
western_net <- western_net %>% mutate(z = "Western")
horror_net <- subset(data_Netflix, grepl("\\bhorror\\b", genres, ignore.case = TRUE))
horror_net <- horror_net %>% mutate(z = "Horror")

movie_genres_net <- rbind(documentary_net, drama_net, sport_net, romance_net, 
                          comedy_net, crime_net, music_net,fantasy_net, thriller_net,
                          action_net, history_net, family_net, war_net, european_net, 
                          animation_net, scifi_net, reality_net, western_net, horror_net)

movie_genres_net <- movie_genres_net %>% mutate(VOD = "Netflix")


# Tworzenie ramki z gatunkami filmowymi dla HBO
documentary_HBO <- subset(data_HBO, grepl("\\bdocumentation\\b", genres, ignore.case = TRUE))
documentary_HBO <- documentary_HBO %>% mutate(z = "Dokument")
drama_HBO <- subset(data_HBO, grepl("\\bdrama\\b", genres, ignore.case = TRUE))
drama_HBO <- drama_HBO %>% mutate(z = "Dramat")
sport_HBO <- subset(data_HBO, grepl("\\bsport\\b", genres, ignore.case = TRUE))
sport_HBO <- sport_HBO %>% mutate(z = "Sport")
romance_HBO <- subset(data_HBO, grepl("\\bromance\\b", genres, ignore.case = TRUE))
romance_HBO <- romance_HBO %>% mutate(z = "Romans")
comedy_HBO <- subset(data_HBO, grepl("\\bcomedy\\b", genres, ignore.case = TRUE))
comedy_HBO <- comedy_HBO %>% mutate(z = "Komedia")
crime_HBO <- subset(data_HBO, grepl("\\bcrime\\b", genres, ignore.case = TRUE))
crime_HBO <- crime_HBO %>% mutate(z = "Kryminał")
music_HBO <- subset(data_HBO, grepl("\\bmusic\\b", genres, ignore.case = TRUE))
music_HBO <- music_HBO %>% mutate(z = "Muzyczny")
fantasy_HBO <- subset(data_HBO, grepl("\\bfantasy\\b", genres, ignore.case = TRUE))
fantasy_HBO <- fantasy_HBO %>% mutate(z = "Fantasy")
thriller_HBO <- subset(data_HBO, grepl("\\bthriller\\b", genres, ignore.case = TRUE))
thriller_HBO <- thriller_HBO %>% mutate(z = "Thriller")
action_HBO <- subset(data_HBO, grepl("\\baction\\b", genres, ignore.case = TRUE))
action_HBO <- action_HBO %>% mutate(z = "Akcja")
history_HBO <- subset(data_HBO, grepl("\\bhistory\\b", genres, ignore.case = TRUE))
history_HBO <- history_HBO %>% mutate(z = "Historyczny")
family_HBO <- subset(data_HBO, grepl("\\bfamily\\b", genres, ignore.case = TRUE))
family_HBO <- family_HBO %>% mutate(z = "Familijny")
war_HBO <- subset(data_HBO, grepl("\\bwar\\b", genres, ignore.case = TRUE))
war_HBO <- war_HBO %>% mutate(z = "Wojenny")
european_HBO <- subset(data_HBO, grepl("\\beuropean\\b", genres, ignore.case = TRUE))
european_HBO <- european_HBO %>% mutate(z = "Europejski")
animation_HBO <- subset(data_HBO, grepl("\\banimation\\b", genres, ignore.case = TRUE))
animation_HBO <- animation_HBO %>% mutate(z = "Animowany")
scifi_net <- subset(data_HBO, grepl("\\bscifi\\b", genres, ignore.case = TRUE))
scifi_HBO <- scifi_net %>% mutate(z = "Sci-fi")
reality_HBO <- subset(data_HBO, grepl("\\breality\\b", genres, ignore.case = TRUE))
reality_HBO <- reality_HBO %>% mutate(z = "Reality show")
western_HBO <- subset(data_HBO, grepl("\\bwestern\\b", genres, ignore.case = TRUE))
western_HBO <- western_HBO %>% mutate(z = "Western")
horror_HBO <- subset(data_HBO, grepl("\\bhorror\\b", genres, ignore.case = TRUE))
horror_HBO <- horror_HBO %>% mutate(z = "Horror")

movie_genres_HBO <- rbind(documentary_HBO, drama_HBO, sport_HBO, romance_HBO, 
                          comedy_HBO, crime_HBO, music_HBO,fantasy_HBO, thriller_HBO,
                          action_HBO, history_HBO, family_HBO, war_HBO, european_HBO, 
                          animation_HBO, scifi_HBO, reality_HBO, western_HBO, horror_HBO)

movie_genres_HBO <- movie_genres_HBO %>% mutate(VOD = "HBO MAX")

# Tworzenie ramki z gatunkami filmowymi dla netflixa
documentary_dis <- subset(data_disney, grepl("\\bdocumentation\\b", genres, ignore.case = TRUE))
documentary_dis <- documentary_dis %>% mutate(z = "Dokument")
drama_dis <- subset(data_disney, grepl("\\bdrama\\b", genres, ignore.case = TRUE))
drama_dis <- drama_dis %>% mutate(z = "Dramat")
sport_dis <- subset(data_disney, grepl("\\bsport\\b", genres, ignore.case = TRUE))
sport_dis <- sport_dis %>% mutate(z = "Sport")
romance_dis <- subset(data_disney, grepl("\\bromance\\b", genres, ignore.case = TRUE))
romance_dis <- romance_dis %>% mutate(z = "Romans")
comedy_dis <- subset(data_disney, grepl("\\bcomedy\\b", genres, ignore.case = TRUE))
comedy_dis <- comedy_dis %>% mutate(z = "Komedia")
crime_dis <- subset(data_disney, grepl("\\bcrime\\b", genres, ignore.case = TRUE))
crime_dis <- crime_dis %>% mutate(z = "Kryminał")
music_dis <- subset(data_disney, grepl("\\bmusic\\b", genres, ignore.case = TRUE))
music_dis <- music_dis %>% mutate(z = "Muzyczny")
fantasy_dis <- subset(data_disney, grepl("\\bfantasy\\b", genres, ignore.case = TRUE))
fantasy_dis <- fantasy_dis %>% mutate(z = "Fantasy")
thriller_dis <- subset(data_disney, grepl("\\bthriller\\b", genres, ignore.case = TRUE))
thriller_dis <- thriller_dis %>% mutate(z = "Thriller")
action_dis <- subset(data_disney, grepl("\\baction\\b", genres, ignore.case = TRUE))
action_dis <- action_dis %>% mutate(z = "Akcja")
history_dis <- subset(data_disney, grepl("\\bhistory\\b", genres, ignore.case = TRUE))
history_dis <- history_dis %>% mutate(z = "Historyczny")
family_dis <- subset(data_disney, grepl("\\bfamily\\b", genres, ignore.case = TRUE))
family_dis <- family_dis %>% mutate(z = "Familijny")
war_dis <- subset(data_disney, grepl("\\bwar\\b", genres, ignore.case = TRUE))
war_dis <- war_dis %>% mutate(z = "Wojenny")
european_dis <- subset(data_disney, grepl("\\beuropean\\b", genres, ignore.case = TRUE))
european_dis <- european_dis %>% mutate(z = "Europejski")
animation_dis <- subset(data_disney, grepl("\\banimation\\b", genres, ignore.case = TRUE))
animation_dis <- animation_dis %>% mutate(z = "Animowany")
scifi_dis <- subset(data_disney, grepl("\\bscifi\\b", genres, ignore.case = TRUE))
scifi_dis <- scifi_dis %>% mutate(z = "Sci-fi")
reality_dis <- subset(data_disney, grepl("\\breality\\b", genres, ignore.case = TRUE))
reality_dis <- reality_dis %>% mutate(z = "Reality show")
western_dis <- subset(data_disney, grepl("\\bwestern\\b", genres, ignore.case = TRUE))
western_dis <- western_dis %>% mutate(z = "Western")
horror_dis <- subset(data_disney, grepl("\\bhorror\\b", genres, ignore.case = TRUE))
horror_dis <- horror_dis %>% mutate(z = "Horror")

movie_genres_dis <- rbind(documentary_dis, drama_dis, sport_dis, romance_dis, 
                          comedy_dis, crime_dis, music_dis,fantasy_dis, thriller_dis,
                          action_dis, history_dis, family_dis, war_dis, european_dis, 
                          animation_dis, scifi_dis, reality_dis, western_dis, horror_dis)

movie_genres_dis <- movie_genres_dis %>% mutate(VOD = "Disney+")

# Tworzenie ramki z gatunkami filmowymi dla netflixa
documentary_ama <- subset(data_amazon, grepl("\\bdocumentation\\b", genres, ignore.case = TRUE))
documentary_ama <- documentary_ama %>% mutate(z = "Dokument")
drama_ama <- subset(data_amazon, grepl("\\bdrama\\b", genres, ignore.case = TRUE))
drama_ama <- drama_ama %>% mutate(z = "Dramat")
sport_ama <- subset(data_amazon, grepl("\\bsport\\b", genres, ignore.case = TRUE))
sport_ama <- sport_ama %>% mutate(z = "Sport")
romance_ama <- subset(data_amazon, grepl("\\bromance\\b", genres, ignore.case = TRUE))
romance_ama <- romance_ama %>% mutate(z = "Romans")
comedy_ama <- subset(data_amazon, grepl("\\bcomedy\\b", genres, ignore.case = TRUE))
comedy_ama <- comedy_ama %>% mutate(z = "Komedia")
crime_ama <- subset(data_amazon, grepl("\\bcrime\\b", genres, ignore.case = TRUE))
crime_ama <- crime_ama %>% mutate(z = "Kryminał")
music_ama <- subset(data_amazon, grepl("\\bmusic\\b", genres, ignore.case = TRUE))
music_ama <- music_ama %>% mutate(z = "Muzyczny")
fantasy_ama <- subset(data_amazon, grepl("\\bfantasy\\b", genres, ignore.case = TRUE))
fantasy_ama <- fantasy_ama %>% mutate(z = "Fantasy")
thriller_ama <- subset(data_amazon, grepl("\\bthriller\\b", genres, ignore.case = TRUE))
thriller_ama <- thriller_ama %>% mutate(z = "Thriller")
action_ama <- subset(data_amazon, grepl("\\baction\\b", genres, ignore.case = TRUE))
action_ama <- action_ama %>% mutate(z = "Akcja")
history_ama <- subset(data_amazon, grepl("\\bhistory\\b", genres, ignore.case = TRUE))
history_ama <- history_ama %>% mutate(z = "Historyczny")
family_ama <- subset(data_amazon, grepl("\\bfamily\\b", genres, ignore.case = TRUE))
family_ama <- family_ama %>% mutate(z = "Familijny")
war_ama <- subset(data_amazon, grepl("\\bwar\\b", genres, ignore.case = TRUE))
war_ama <- war_ama %>% mutate(z = "Wojenny")
european_ama <- subset(data_amazon, grepl("\\beuropean\\b", genres, ignore.case = TRUE))
european_ama <- european_ama %>% mutate(z = "Europejski")
animation_ama <- subset(data_amazon, grepl("\\banimation\\b", genres, ignore.case = TRUE))
animation_ama <- animation_ama %>% mutate(z = "Animowany")
scifi_ama <- subset(data_amazon, grepl("\\bscifi\\b", genres, ignore.case = TRUE))
scifi_ama <- scifi_ama %>% mutate(z = "Sci-fi")
reality_ama <- subset(data_amazon, grepl("\\breality\\b", genres, ignore.case = TRUE))
reality_ama <- reality_ama %>% mutate(z = "Reality show")
western_ama <- subset(data_amazon, grepl("\\bwestern\\b", genres, ignore.case = TRUE))
western_ama <- western_ama %>% mutate(z = "Western")
horror_ama <- subset(data_amazon, grepl("\\bhorror\\b", genres, ignore.case = TRUE))
horror_ama <- horror_ama %>% mutate(z = "Horror")

movie_genres_ama <- rbind(documentary_ama, drama_ama, sport_ama, romance_ama, 
                          comedy_ama, crime_ama, music_ama,fantasy_ama, thriller_ama,
                          action_ama, history_ama, family_ama, war_ama, european_ama, 
                          animation_ama, scifi_ama, reality_ama, western_ama, horror_ama)

movie_genres_ama <- movie_genres_ama %>% mutate(VOD = "Amazon Prime Video")

# Tworzymy całą ramkę danych

movie_genres <- rbind(movie_genres_ama, movie_genres_net, movie_genres_HBO, movie_genres_dis)
movie_data_actors <- rbind(data_Netflix_actors, data_amazon_actors, data_disney_actors, data_HBO_actors)
movie_data_actors <- filter(movie_data_actors, role == "ACTOR")
dt <- inner_join(movie_data_actors, movie_genres, by = "id", multiple = "all")

# aktorzy
best_actors <- inner_join(movie_data_actors, data_VOD,by = "id", multiple = "all")
best_actors <- best_actors %>%  
  distinct() %>% 
  filter(imdb_votes > 1000) %>%
  mutate(score = (imdb_score + tmdb_score)/2) 
best <-best_actors %>%  group_by(name) %>% 
  summarise(Filmy = n()) %>% filter( Filmy >= 20)
aktorzy <- inner_join(best, dt,by = "name", multiple = "all") %>% select(name, z)


#################################### UI ####################################



ui1 <- fluidPage(
  theme = bs_theme(
    bg = "#101010", 
    fg = "#FDF7F7", 
    primary = "red", 
    base_font = font_google("Prompt"),
    code_font = font_google("JetBrains Mono")
  ),
  
  tags$head(
    tags$style("
      .title {
        font-family: 'Netflix Sans', Arial, sans-serif;
        text-align: center;
        font-size: 30px;
      }
      .input1 {
        font-family: 'Netflix Sans', Arial, sans-serif;
        width: 200px;
        text-align:center;
        margin: 0 auto;
      }
      .input2 {
        width: 200px;
        text-align:center;
        margin: 0 auto;
      }
      .distPlot{
        font-size:16px;
        width: 700px;
        margin-top:20px;
        margin: 0 auto;
        border: 1px solid silver; 
        background: white; padding: 4px;
        border-radius: 8px;
        box-shadow: 1px 1px 3px silver; 
      }
      .tabelka{
        font-size:16px;
        width: 300px;
        margin: 0 auto;
        border-radius: 8px;
      }
      .tabelka2{
        font-size:16px;
        width: 200px;
        margin: 0 auto;
        border-radius: 8px;
      }
      .slupkowy{
        font-size:16px;
        color: black;
        width:400px;
        padding-right:20px;
        height: 490px;
        margin: 0 auto;
        border: 1px solid silver; 
        background: white; padding: 4px;
        border-radius: 8px;
        box-shadow: 1px 1px 3px silver; 
      }
      
      .mapka{
        padding-top:50px;
        width: 640px;
        height: 450px;
        margin: 0 auto;
        margin-top:50px;
        border: 1px solid silver; 
        background: white; padding: 4px;
        border-radius: 8px;
        box-shadow: 1px 1px 3px silver; 
      }
      
      .okno1{
      font-size:16px;
      margin: 0 auto;
      width:800px;
      padding-top: 20px;
      margin-bottom: 20px;
      height:540px;
      border: 8px solid white; 
      }
      
      .okno2{
      font-size:16px;
      margin: 0 auto;
      width: 1200px;
      margin-top: 20px;
      margin-bottom: 40px;
      height: 500px;
      border: 8px solid white; 
      }
      
      .okno3{
      font-size:16px;
      margin: 0 auto;
      width: 1200px;
      margin-top: 60px;
      margin-bottom: 20px;
      height: 600px;
      border: 8px solid white; 
      }
      
    ")
  ),
  
  div(
    class = "title",
    "Analiza trendów u znajomych na Netflixie"
  ),
  
  
  fluidRow(
    column(12, 
           
           div(
             class = "input1",
             selectInput("Osoba","Wybierz osobę:",unique(exploring$Profile.Name))
           ),
           
           div(
             class = "okno1",
             div(
               class = "distPlot",
               plotlyOutput("distPlot"), 
             ),
             
             
             div(
               class = "input2",
               sliderInput("Zakres","Wybierz rok:", c(2019,2023),
                           min = 2019,
                           max = 2023,
                           sep = "",
                           step=1)
             )
           )
    ),
    
    fluidRow(
      div(
        class="okno2",
        fluidRow(
          
          column(6, 
                 div(
                   class = "tabelka",
                   HTML("<br>"),
                   textOutput("opis1"),
                   HTML("<br>"),
                   tableOutput("tabelka")
                 )
                 
          ),
          column(6, 
                 
                 div(
                   class = "slupkowy",
                   textOutput("opis2"),
                   HTML("<br>"),
                   plotlyOutput("slupkowy")
                 )
          )
          
        ),
        
        fluidRow(
          div(class="okno3",
              fluidRow(
                
                column(4,
                       div(
                         class="tabelka2",
                         HTML("<br><br>"),
                         "Kraje w których były oglądane filmy:",
                         HTML("<br><br>"),
                         tableOutput("tabelka2")
                       )
                ),
                column(8,
                       div(
                         class="mapka",
                         plotlyOutput("mapka")
                       )
                )
              ),
          )
        )
        
      ))
  ))


ui2 <- fluidPage(
  theme = bs_theme(
    bg = "#101010", 
    fg = "#FDF7F7", 
    primary = "red", 
    base_font = font_google("Prompt"),
    code_font = font_google("JetBrains Mono")
  ),
  
  tags$head(
    tags$style("
      .okno1{
      font-size:16px;
      margin: 0 auto;
      width:1000px;
      padding-top: 20px;
      margin-bottom: 20px;
      height:540px;
      border: 8px solid white; 
      }
      .tytul {
        margin-top: 12px;
        font-family: 'Netflix Sans', Arial, sans-serif;
        text-align: center;
        font-size: 30px;
        height: 100px;
      .text1 {
        text-align: center;
        margin: 0 auto;
      }
      ")),
  
  fluidRow(
    div(
      class = "tytul",
      column(12,
             "Analiza występowania aktorów w filmach określonych gatunków z podziałem na portale streamingowe"
      )
    ),
    div(
      class="okno1",
      fluidRow(
        div(
          class="text1",
          column(
            width = 12,
            selectInput("ktore_VOD",
                        "Platforma streamingowa:",
                        choices = c("Netflix", "HBO MAX", "Amazon Prime Video", "Disney+"))
          )
        )),
      
      fluidRow(
        column(12,
               plotlyOutput("barPlot")
        )),
    )),
  
  fluidRow(
    column(3,
           tableOutput("tabela_aktorow")
    )
    ,
    
    column(
      width = 9,
      selectInput(inputId = "input_text1", label = "Wprowadź imię i nazwisko aktora:",
                  choices=unique(aktorzy$name)),
      plotlyOutput("barplotaktor")
    )
    
  )
)


ui3 <- fluidPage(
  
  theme = bs_theme(
    bg = "#101010", 
    fg = "#FDF7F7", 
    primary = "red", 
    base_font = font_google("Prompt"),
    code_font = font_google("JetBrains Mono")
  ),
  
  tags$style(HTML("
    .bottom-space {
      padding-bottom: 50px;
    }
    .tekst {
      font-size: 30px;
      margin-top: 12px;
      text-align: center;
      font-family: 'Netflix Sans', Arial, sans-serif;
      padding-bottom: 10px;
    }
  ")),
  
  div(
    class = "tekst",
    column(width = 12,
           "Porównywarka serwisów streamingowych")
  ),
  
  # Side panel z filtrowaniem
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Filtr typu (film/serial)
      
      selectInput(
        "type_filter",
        "Typ:",
        choices = c("Wszystko", "Film", "Serial"),
        selected = "Wszystko"
      ),
      
      # Filtr gatunków
      
      selectInput(
        "genres_filter",
        "Gatunki:",
        choices = c("", unique(unlist(strsplit(as.character(df$genres), ", ")))),
        multiple = TRUE
      ),
      
      # Filtr kraju produkcji
      
      selectInput(
        "countries_filter",
        "Kraj produkcji:",
        choices = c("", names(country_frequency)),
        multiple = TRUE
      ),
      
      # Filtr ocen
      
      numericInput(
        "rating_filter",
        "Minimalna ocena:",
        value = "",
        step = 0.1
      ),
      
      # Filtr roku wydania
      
      sliderInput(
        "year_filter",
        "Rok wydania:",
        min = min(df$year, na.rm = TRUE),
        max = max(df$year, na.rm = TRUE),
        value = c(min(df$year, na.rm = TRUE), max(df$year, na.rm = TRUE)),
        step = 1,
        sep = "",
        ticks = FALSE
      ),
      
      # Filtr liczby sezonów
      
      conditionalPanel(
        condition = "input.type_filter == 'Serial'",
        sliderInput(
          "seasons_filter",
          "Liczba sezonów:",
          min = 1,
          max = max(df$seasons, na.rm = TRUE),
          value = c(min(df$seasons, na.rm = TRUE), max(df$seasons, na.rm = TRUE)),
          step = 1,
          ticks = FALSE
        )
      )
      
    ),
    
    # Główny panel z wskaźnikami i histogramem
    mainPanel(
      fluidRow(
        
        # netflix
        
        column(
          width = 3,
          wellPanel(
            h4("Netflix"),
            "Ocena:",
            p(textOutput("netflix_average_rating")),
            "Liczba tytułów:",
            p(textOutput("netflix_total_titles")),
            "Stosunek do wszystkich:",
            p(textOutput("netflix_ratio"))
          )
        ),
        
        # disney
        
        column(
          width = 3,
          wellPanel(
            h4("Disney+"),
            "Ocena:",
            p(textOutput("disney_average_rating")),
            "Liczba tytułów:",
            p(textOutput("disney_total_titles")),
            "Stosunek do wszystkich:",
            p(textOutput("disney_ratio"))
          )
        ),
        
        # hbo
        
        column(
          width = 3,
          wellPanel(
            h4("HBO Max"),
            ("Ocena:"),
            p(textOutput("hbo_average_rating")),
            "Liczba tytułów:",
            p(textOutput("hbo_total_titles")),
            "Stosunek do wszystkich:",
            p(textOutput("hbo_ratio"))
          )
        ),
        
        # amazon
        
        column(
          width = 3,
          wellPanel(
            h4("Amazon Prime"),
            "Ocena:",
            p(textOutput("prime_average_rating")),
            "Liczba tytułów:",
            p(textOutput("prime_total_titles")),
            "Stosunek do wszystkich:",
            p(textOutput("prime_ratio"))
          )
        ),
        
        # histogram
        
        column(
          width = 9,
          plotlyOutput("histogram")
        ),
        column(
          width = 3,
          selectInput(
            "vod_filter",
            "VOD:",
            choices = c("Wszystkie", "Netflix", "Disney+", "HBO Max", "Amazon Prime"),
            selected = "Wszystkie"
          ),
          selectInput(
            "hist_variable",
            "Zmienna histogramu:",
            choices = c("Ocena", "Rok wydania"),
            selected = "Ocena"
          ),
        ),
        
        # tabela
        
        column(
          class = "bottom-space",
          width = 12,
          DT::dataTableOutput("filtered_table")
        )
      )
    )
  )
)

appui <- navbarPage(
  theme = bs_theme(
    bg = "#101010", 
    fg = "#FDF7F7", 
    primary = "red", 
    base_font = font_google("Prompt"),
    code_font = font_google("JetBrains Mono")
  ),
  title = "Filmy I Seriale",
  tabPanel("Trendy znajomych", ui1),
  tabPanel("Porównywarka VOD", ui3),
  tabPanel("Analiza aktorów", ui2)
)

# SERVER

server <- function(input, output, session) {
  output$barPlot <- renderPlotly({
    gatunki <- dt
    plot_ly(gatunki %>%
              filter(gatunki$VOD == input$ktore_VOD) %>%
              distinct() %>%
              group_by(z) %>%
              summarise(Liczba_filmow = n()),
            x = ~z,
            y = ~Liczba_filmow) %>%
      add_trace(type="bar",marker=list(color="red")) %>%
      layout(title = "Liczba aktorów w zależności od gatunku", font=list(color="white"),
             xaxis = list(title = 'Gatunek', tickangle = 45),
             yaxis = list(title = 'Liczba aktrów'),
             plot_bgcolor = "black",
             paper_bgcolor = "black")
    
    
    
  })
  
  
  
  output$tabela_aktorow <- renderTable({
    
    
    
    
    s1 <-best_actors %>%  group_by(name) %>% 
      summarise(Filmy = n())
    
    s2 <- best_actors %>%  group_by(name) %>% 
      summarise(Ocena = mean(score))
    
    score <- inner_join(s1, s2, by = "name")
    score <- filter(score, Filmy >= 5) %>% arrange(desc(Ocena))
    colnames(score)[colnames(score) == "name"] <- "Aktor" 
    
    head(score, 10)
  })
  
  output$barplotaktor <- renderPlotly({
    gatunki_aktor <- aktorzy
    plot_ly(gatunki_aktor %>%
              filter(gatunki_aktor$name == input$input_text1) %>%
              group_by(z) %>%
              summarise(Liczba_filmow = n()),
            x = ~z,
            y = ~Liczba_filmow) %>%
      add_trace(type = "scatter", mode = "lines+markers", marker = list(color = "red"), line = list(color = "red")) %>%
      layout(title = "Liczba wystąpień aktora w filmie ze względu na gatunek",
             font = list(color = "white"),
             xaxis = list(title = 'Gatunek', tickangle = 45),
             yaxis = list(title = 'Liczba filmów'),
             plot_bgcolor = "black",
             paper_bgcolor = "black")
    
  })
  
  
  
  # Wskaźniki dla poszczególnych serwisów streamingowych
  
  # netflix
  output$netflix_average_rating <- renderText({
    filtered <- filter_data()
    sprintf("%.1f", round(mean(filtered$rating[filtered$vod == "Netflix"], na.rm = TRUE), 1))
  })
  
  output$netflix_total_titles <- renderText({
    filtered <- filter_data()
    nrow(filtered[filtered$vod == "Netflix", ])
  })
  
  output$netflix_ratio <- renderText({
    filtered <- filter_data()
    total_titles <- nrow(df[df$vod == "Netflix", ])
    netflix_titles <- nrow(filtered[filtered$vod == "Netflix", ])
    paste(sprintf("%.1f", round(netflix_titles/total_titles, 3) * 100), "%", sep = "")
  })
  
  # disney
  output$disney_average_rating <- renderText({
    filtered <- filter_data()
    sprintf("%.1f", round(mean(filtered$rating[filtered$vod == "Disney+"], na.rm = TRUE), 1))
  })
  
  output$disney_total_titles <- renderText({
    filtered <- filter_data()
    nrow(filtered[filtered$vod == "Disney+", ])
  })
  
  output$disney_ratio <- renderText({
    filtered <- filter_data()
    total_titles <- nrow(df[df$vod == "Disney+", ])
    disney_titles <- nrow(filtered[filtered$vod == "Disney+", ])
    paste(sprintf("%.1f", round(disney_titles/total_titles, 3) * 100), "%", sep = "")
  })
  
  # hbo
  output$hbo_average_rating <- renderText({
    filtered <- filter_data()
    sprintf("%.1f", round(mean(filtered$rating[filtered$vod == "HBO Max"], na.rm = TRUE), 1))
  })
  
  output$hbo_total_titles <- renderText({
    filtered <- filter_data()
    nrow(filtered[filtered$vod == "HBO Max", ])
  })
  
  output$hbo_ratio <- renderText({
    filtered <- filter_data()
    total_titles <- nrow(df[df$vod == "HBO Max", ])
    hbo_titles <- nrow(filtered[filtered$vod == "HBO Max", ])
    paste(sprintf("%.1f", round(hbo_titles/total_titles, 3) * 100), "%", sep = "")
  })
  
  # prime
  output$prime_average_rating <- renderText({
    filtered <- filter_data()
    sprintf("%.1f", round(mean(filtered$rating[filtered$vod == "Amazon Prime"], na.rm = TRUE), 1))
  })
  
  output$prime_total_titles <- renderText({
    filtered <- filter_data()
    nrow(filtered[filtered$vod == "Amazon Prime", ])
  })
  
  output$prime_ratio <- renderText({
    filtered <- filter_data()
    total_titles <- nrow(df[df$vod == "Amazon Prime", ])
    prime_titles <- nrow(filtered[filtered$vod == "Amazon Prime", ])
    paste(sprintf("%.1f", round(prime_titles/total_titles, 3) * 100), "%", sep = "")
  })
  
  # pojawiamy opcję Sezon przy wyborze Serial
  
  observeEvent(input$type_filter, {
    if (input$type_filter == "Serial") {
      updateSelectInput(
        session,
        "hist_variable",
        choices = c("Ocena", "Rok wydania", "Liczba sezonów"),
        selected = "Ocena"
      )
    }
  })
  
  # Histogram
  output$histogram <- renderPlotly({
    filtered <- filter_histogram_data()
    filtered <- filtered[!is.na(filtered$rating) & is.finite(filtered$rating), ]
    
    hist_variable <- switch(
      input$hist_variable,
      "Ocena" = {
        breaks <- seq(round(min(filtered$rating), 1), round(max(filtered$rating), 1), by = 0.1) # Adjust the number of bins here
        hist_values <- filtered$rating
        #name <- "Ocena"
      },
      "Rok wydania" = {
        breaks <- seq(min(filtered$year), max(filtered$year), by = 1) # Adjust the number of bins here
        hist_values <- filtered$year
        #name <- "Rok wydania"
      },
      "Liczba sezonów" = {
        breaks <- seq(min(filtered$seasons), max(filtered$seasons), by = 1) # Adjust the number of bins here
        hist_values <- filtered$seasons
        #name <- "Liczba sezonów"
      }
    )

    plot_ly(filtered, x = ~hist_variable) %>%
      add_trace(type = "histogram", marker = list(color = "red"),
                xbins = list(start = min(breaks) - 1, end = max(breaks) + 1, size = (max(breaks) + 1 - min(breaks))/length(breaks))) %>%
      layout(title = "", font=list(color="white"),
             xaxis = list(title = ''),
             yaxis = list(title = 'Liczba tytułów'),
             plot_bgcolor = "black",
             paper_bgcolor = "black")
  })
  
  # Tabela
  output$filtered_table <- DT::renderDataTable(DT::datatable({
    filter_data() %>% 
      select('Tytuł oryginalny' = title, Typ = type, 'Rok wydania' = year, 'Kraje produkcji' = countries,
             Sezony = seasons, Gatunki = genres, Ocena = rating, Serwis = vod) %>% 
      mutate(Ocena = ifelse(Ocena=="","",sprintf("%.2f", round(Ocena, 2))))
  }))
  
  # Filtrowanie danych
  filter_data <- reactive({
    filtered <- df
    
    if (!is.null(input$genres_filter) && length(input$genres_filter) > 0) {
      filtered <- filtered[sapply(filtered$genres, function(x) any(input$genres_filter %in% strsplit(as.character(x), ", ")[[1]])), ]
    }
    
    if (!is.null(input$type_filter) && input$type_filter != "" && input$type_filter != "Wszystko") {
      filtered <- filtered[filtered$type == input$type_filter, ]
    }
    
    if (!is.null(input$seasons_filter)) {
      if (!is.na(input$seasons_filter[1]) && !is.na(input$seasons_filter[2]) && input$seasons_filter[1] <= input$seasons_filter[2]) {
        filtered <- filtered[(!is.na(filtered$seasons) & filtered$seasons >= input$seasons_filter[1] & filtered$seasons <= input$seasons_filter[2]) | is.na(filtered$seasons), ]
      }
    }
    
    if (!is.null(input$year_filter) && input$year_filter[1] <= input$year_filter[2]) {
      filtered <- filtered[filtered$year >= input$year_filter[1] & filtered$year <= input$year_filter[2], ]
    }
    
    if (!is.null(input$countries_filter) && length(input$countries_filter) > 0) {
      filtered <- filtered[sapply(filtered$countries, function(x) any(input$countries_filter %in% strsplit(as.character(x), ", ")[[1]])), ]
    }
    
    if (!is.null(input$rating_filter) && input$rating_filter != "" && !is.na(as.numeric(input$rating_filter))) {
      filtered <- filtered[!is.na(filtered$rating) & filtered$rating >= as.numeric(input$rating_filter), ]
    }
    
    filtered
  })
  
  # Dane dla histogramu
  filter_histogram_data <- reactive({
    filtered <- filter_data()
    
    if (!is.null(input$vod_filter) && input$vod_filter != "" && input$vod_filter != "Wszystkie") {
      filtered <- filtered[filtered$vod == input$vod_filter, ]
    }
    
    filtered
  })
  
  
  output$distPlot <- renderPlotly({
    
    plot_ly(data = exploring %>% filter(Profile.Name == input$Osoba 
                                        ,exploring$Year >= input$Zakres[1],
                                        exploring$Year <= input$Zakres[2]), 
            x = ~datetime, y=~NewDuration,
            type = "bar",marker=list(color="red")) %>%
      layout(yaxis=list(range=c(0,8000), title = "Czas oglądania w minutach"),
             xaxis = list(title = "Oś czasu"),
             title = list(text = 
                            paste0("Ile czasu ", input$Osoba, " spędzał/a na netflixie w latach ",
                                   input$Zakres[1], "-", input$Zakres[2]), 
                          font = list(color = "white")),
             plot_bgcolor = "black",
             paper_bgcolor = "black")
  })
  
  output$tabelka <- renderTable({
    t <- exploring %>% filter(
      exploring$Year >= input$Zakres[1],
      exploring$Year <= input$Zakres[2]) %>%
      group_by(Profile.Name) %>% 
      count(Title) %>%
      filter(Profile.Name == input$Osoba) %>%
      arrange(desc(n)) %>%
      head(8) %>% select(Title)
    t
  })
  
  output$slupkowy <- renderPlotly({
    res <- exploring %>% filter(
      exploring$Year >= input$Zakres[1],
      exploring$Year <= input$Zakres[2],
      Profile.Name == input$Osoba)
    suma_filmow <- sum(res$licznik_filmow)/(sum(res$licznik_filmow)+sum(res$licznik_serialow))
    suma_seriali <- sum(res$licznik_serialow)/(sum(res$licznik_filmow)+sum(res$licznik_serialow))
    df <- data.frame(Film = (suma_filmow*100), Serial = (suma_seriali*100))
    
    pl <- plot_ly(data=df, x=~names(df), y=~unlist(df))
    pl %>% add_trace(type="bar",marker=list(color="red")) %>%
      layout(plot_bgcolor = "black", paper_bgcolor = "black", xaxis=list(title="Rodzaj"),
             yaxis=list(title="Liczba w procentach"))
  })
  
  output$opis1 <- renderText({
    paste0("Osiem najpopularniejszych filmów i seriali dla użytkownika ",
           input$Osoba," w latach ", input$Zakres[1]," - ",input$Zakres[2]," to:")
  })
  
  output$opis2 <- renderText({
    paste0("Stosunek oglądania filmów do seriali dla ",
           input$Osoba," w latach ", input$Zakres[1]," - ",input$Zakres[2],":")
  })
  
  output$mapka <- renderPlotly({
    kraje <- unique(exploring %>% filter(
      exploring$Year >= input$Zakres[1],
      exploring$Year <= input$Zakres[2],
      Profile.Name == input$Osoba) %>% 
        select(Country))
    kraje_fixed <- eval(parse(text = (gsub("[A-Z]{2} \\((.*?)\\)", "\\1", kraje))))
    
    country %>% 
      ggplot(aes(x = long, y = lat, group = group)) + 
      geom_polygon(fill=ifelse(country$region %in% kraje_fixed, "red","black"), 
                   color="white", size="0.1") +
      coord_fixed(1.3) +
      theme_void() 
  })
  
  output$tabelka2 <- renderTable({
    kraje <- unique(exploring %>% filter(
      exploring$Year >= input$Zakres[1],
      exploring$Year <= input$Zakres[2],
      Profile.Name == input$Osoba) %>% 
        select(Country))
    kraje_fixed <- eval(parse(text = (gsub("[A-Z]{2} \\((.*?)\\)", "\\1", kraje))))
    kraje_fixed
  })
  
}

shinyApp(appui, server)
