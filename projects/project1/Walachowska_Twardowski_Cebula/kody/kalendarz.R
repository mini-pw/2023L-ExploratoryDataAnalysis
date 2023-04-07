library(dplyr)
library(ggplot2)
library(lubridate)
library(showtext)
library(calendR)
library(paletteer)

opera <- read.csv("Opera.csv")
View(opera)

# opery z ilością wystawień i datą premiery w danym roku
cal <- modern_premiers %>%
  mutate(start_d = ifelse(year(first_performance) == 2017,
                          yday(first_performance)-243,
                          yday(first_performance)+122),
         id = 1:n()) %>%
  select(id, composer, birth_y, work, city, start_d)
View(cal)
cal$id <- 14:1

events <- rep(0, 365)
events[cal$start_d] <- cal$id

font_add_google(name = "Old Standard TT", family = "oper")
showtext_auto()

Sys.setlocale("LC_ALL", "English")

calendar <- calendR(start_date = "2017-09-01",
        end_date = "2018-08-31",
        start = "M",
        title = "Modern opera",
        title.size = 45,
        title.col = "#d9c19f",
        subtitle = "2017/2018",
        subtitle.size = 25,
        subtitle.col = "#f5f0e6",
        special.days = events,
        gradient = TRUE,
        special.col = c("#EECF6D", "#EECF6D", "#EECF6D",
                        "#D5AC4E", "#D5AC4E", "#D5AC4E", 
                        "#8B6220", "#8B6220", "#8B6220",
                        "#720E07", "#720E07", "#720E07",
                        "#45050C", "#45050C"
                        ),
        low.col = "#1f1313",
        day.size = 5,
        days.col = "#f5f0e6",
        col = "#f5f0e6",
        lwd = 0.5,
        lty = 1,
        weeknames.col = "#f5f0e6",
        weeknames.size = 5,
        months.size = 15,
        months.col = "#d9c19f",
        months.pos = 0.5,
        mbg.col = "#491c1a",
        orientation = "p",
        bg.col = "transparent")
calendar
ggsave(calendar,
       filename="kalendarz_plot.png",
       width=8,
       height=12,
       bg="transparent")
