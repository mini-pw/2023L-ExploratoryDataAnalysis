library(dplyr)
library(ggplot2)

opera <- read.csv("Opera.csv")

composers_birth <- opera %>%
  select(composer, db, performances) %>%
  mutate(birth_y = as.numeric(substr(as.character(db), 1, 4))) %>%
  filter(birth_y >= 1900) %>%
  group_by(composer, birth_y) %>%
  summarise(popularity_cm = sum(performances)) %>%
  arrange(-popularity_cm) %>%
  head(15)

opers_popularity <- opera %>%
  group_by(composer, work) %>%
  summarise(popularity_w = sum(performances)) %>%
  arrange(-popularity_w)

modern_operas <- left_join(composers_birth, opers_popularity, join_by(composer)) %>%
  arrange(-popularity_cm) %>%
  group_by(composer) %>%
  top_n(1, popularity_w)

premiers <- opera %>%
  filter(season == "1718") %>%
  select(work, city, start.date, performances) %>%
  mutate(first_performance = as.Date(as.character(start.date), format='%Y%m%d'))

modern_premiers <- left_join(modern_operas, premiers, join_by(work)) %>%
  group_by(composer, birth_y, work) %>%
  top_n(1, performances) %>%
  filter(!(composer == "Glass" & city == "London") & !(composer == "Piazzolla" & city == "Halle"))

View(modern_premiers)
