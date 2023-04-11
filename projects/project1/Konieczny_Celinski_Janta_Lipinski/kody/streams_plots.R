# BIBLIOTEKI
{
library(csvread)
library(csvwr)
library(dplyr)
library(tidyr)
library(data.table)
library(scales)
library(ggplot2)
library(showtext)
  
font_add("Sheeran", "C:/Users/Kuba/AppData/Local/Microsoft/Windows/Fonts/fletcher_regular.otf")
showtext_auto()

}
# TWORZYMY RAMKĘ DANYCH DLA ARTYSTY, KTÓREGO BĘDZIEMY ANALIZOWAĆ
{
# spotify <- read.csv("charts.csv")
# spotify %>%
# top_n(-1)
# 
# #Ed Sheeran
# ed_sheeran <- spotify %>%
#   filter(artist == "Ed Sheeran")
# 
# write.csv(ed_sheeran, "ed_sheeran.csv", row.names=FALSE)
}
# WCZYTANIE RAMKI DANYCH STWORZONEJ POWYŻEJ
{
  ed_sheeran <- read.csv(file = 'ed_sheeran.csv')
}
# KONCERTY EDA SHEERANA - 11/12.08.2018 PGE NARODOWY, WARSZAWA
{
# Dane
ed_sheeran <- read.csv(file = 'ed_sheeran.csv')
  
# Zmieniamy typ kolumny date na Date
ed_sheeran$date <- as.Date(ed_sheeran$date)

# Zamieniamy remix jednej z popularnych piosenek na jej oryginalny tytuł
ed_sheeran$title[ed_sheeran$title == 'Perfect Duet (Ed Sheeran & BeyoncĂ©)'] <- 'Perfect'

  # Wykres liczby odsłuchań piosenek Eda Sheerana z TOP 200 w Polsce w okresie koncertów na PGE Narodowym w Warszawie

# Tworzymy pomocniczą ramkę danych, której użyjemy do stworzenia wykresu
ed_sheeran %>% 
  filter(date >= '2018-07-11' & date <= '2018-09-11', region == "Poland", streams != "") %>% 
  group_by(date, title) %>% 
  summarise(sum_of_streams = sum(streams)) -> ed_sheeran_poland


poziomy <- c(
  "Dive",
  "Photograph",
  "Supermarket Flowers",
      "Thinking out Loud" ,
      "Castle on the Hill",
      "Galway Girl" ,
      "Happier",
      "Perfect",
      "Shape of You")

ed_sheeran_poland$title <- factor(ed_sheeran_poland$title, levels = poziomy)

# Usuwamy NA
ed_sheeran_poland <- na.omit(ed_sheeran_poland)

# Wykres
  ed_sheeran_poland %>% 
  ggplot(aes(x = date, y = sum_of_streams, fill = title)) + 
  geom_col() + 
  labs(x = "Days", y = "Streams", 
    title = "Streams of Ed Sheeran's songs from TOP 200 Spotify in Poland during the concert period",
    caption = "*date of the concert - 11/08/2018 marked with a white line") +
  scale_x_date(date_labels = "%d/%m/%Y", expand = c(0,0)) + 
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3), expand = c(0,0), limits = c(0,150000)) +
  guides(fill=guide_legend(title="Song")) +
  theme_bw() +
  #theme(panel.grid = element_blank()) +
  scale_fill_manual(values = rev(c("#bd7ebe", "#ffb55a", "#ffee65", "#beb9db", "#fdcce5", "#8bd3c7","#fd7f6f", "#7eb0d5", "#b2e061")))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Sheeran", size = 20, color = "white"),
          legend.title = element_text(size = 15, family = "Sheeran", color = "white"),
          plot.caption = element_text(size = 15, family = "Sheeran", color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.border =  element_rect(color = "white"),
          legend.text = element_text(color = "white"))+
  geom_vline(xintercept = as.Date("2018-08-12"), color = "white", size = 1.2)
}
# KONCERTY EDA SHEERANA - 28/29.06.2018 AMSTERDAM, HOLANDIA
{
 # Dane
  ed_sheeran <- read.csv(file = 'ed_sheeran.csv')
  
 # Zmieniamy typ kolumny date na Date
  ed_sheeran$date <- as.Date(ed_sheeran$date)
  
 # Zamieniamy remix jednej z popularnych piosenek na jej oryginalny tytuł
  ed_sheeran$title[ed_sheeran$title == 'Perfect Duet (Ed Sheeran & BeyoncĂ©)'] <- 'Perfect'
  
  
 # Wykres liczby odsłuchań piosenek Eda Sheerana z TOP 200 w Holandii w okresie koncerów w Amsterdamie"
  
 #Tworzymy pomocniczą ramkę danych, której użyjemy do stworzenia wykresu
  ed_sheeran %>% 
    filter(date >= '2018-05-28' & date <= '2018-07-28', region == "Netherlands", streams != "") %>% 
    group_by(date, title) %>% 
    summarise(sum_of_streams = sum(streams)) -> ed_sheeran_netherlands
  
  poziomy <-
    c(
      "Thinking out Loud" ,
      "Castle on the Hill",
      "Galway Girl" ,
      "Happier",
      "Perfect",
      "Shape of You"
    )

ed_sheeran_netherlands$title <- factor(ed_sheeran_netherlands$title, levels = poziomy)

ed_sheeran_netherlands <- na.omit(ed_sheeran_netherlands)

 # Wykres
  ed_sheeran_netherlands %>%
      ggplot(aes(x = date, y = sum_of_streams, fill = title)) + 
    geom_col() + 
    labs(x = "Days", y = "Streams", 
         title = "Streams of Ed Sheeran's songs from TOP 200 in Netherlands during the concert period",
         caption = "*date of the concert - 28/06/2018 marked with a white line") +
    scale_x_date(date_labels = "%d/%m/%Y", expand = c(0,0)) + 
    scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3), expand = c(0,0), limits = c(0,280000))+
    guides(fill=guide_legend(title="Song")) +
    scale_fill_manual(values = rev(c("#bd7ebe", "#ffb55a", "#ffee65", "#beb9db", "#fdcce5", "#8bd3c7"))) +
     theme_bw() +
     theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Sheeran", size = 20, color = "white"),
           legend.title = element_text(size = 15, family = "Sheeran", color = "white"),
           plot.caption = element_text(size = 15, family = "Sheeran", color = "white"),
           axis.title = element_text(color = "white"),
           axis.text = element_text(color = "white"),
           panel.border =  element_rect(color = "white"),
           legend.text = element_text(color = "white"))+
     geom_vline(xintercept = as.Date("2018-06-28"), color = "white", size = 1.18)
}
# KONCERT EDA SHEERANA - 01.07.2018 - WERCHTER, BELGIA
{
  # Dane
  ed_sheeran <- read.csv(file = 'ed_sheeran.csv')
  
  # Zmieniamy typ kolumny date na Date
  ed_sheeran$date <- as.Date(ed_sheeran$date)
  
  # Zamieniamy remix jednej z popularnych piosenek na jej oryginalny tytuł
  ed_sheeran$title[ed_sheeran$title == 'Perfect Duet (Ed Sheeran & BeyoncĂ©)'] <- 'Perfect'
  
  # Wykres liczby odsłuchań piosenek Eda Sheerana z TOP 200 w Belgii w okresie koncertów
  
  #Tworzymy pomocniczą ramkę danych, której użyjemy do stworzenia wykresu
  ed_sheeran %>% 
    filter(date >= '2018-06-01' & date <= '2018-08-01', region == "Belgium", streams != "") %>% 
    group_by(date, title) %>% 
    summarise(sum_of_streams = sum(streams)) -> ed_sheeran_belgium
  
  poziomy <- c(
    "Dive",
    "Photograph",
    "Supermarket Flowers",
    "Thinking out Loud" ,
    "Castle on the Hill",
    "Galway Girl" ,
    "Happier",
    "Perfect",
    "Shape of You")
  
  ed_sheeran_belgium$title <- factor(ed_sheeran_belgium$title, levels = poziomy)
  
  ed_sheeran_belgium <- na.omit(ed_sheeran_belgium)
  
  # Wykres
  ed_sheeran_belgium %>%
    ggplot(aes(x = date, y = sum_of_streams, fill = title)) + 
    geom_col() + 
    labs(x = "Days", y = "Streams", 
         title = "Streams of Ed Sheeran's songs from TOP 200 in Belgium during the concert period",
         caption = ("*date of the concert - 01/07/2018 marked with a black line"))+
    scale_x_date(date_labels = "%d/%m/%Y", expand = c(0,0)) + 
    scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3), expand = c(0,0), limits = c(0,100000))+
    guides(fill=guide_legend(title="Song")) +
    scale_fill_manual(values = rev(c("#bd7ebe", "#ffb55a", "#ffee65", "#beb9db", "#fdcce5", "#8bd3c7","#fd7f6f", "#7eb0d5", "#b2e061")))+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Sheeran", size = 20, color = "white"),
          legend.title = element_text(size = 15, family = "Sheeran", color = "white"),
          plot.caption = element_text(size = 15, family = "Sheeran", color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          panel.border =  element_rect(color = "white"),
          legend.text = element_text(color = "white"))+
    geom_vline(xintercept = as.Date("2018-07-01"), color = "white", size = 1.08)
}

