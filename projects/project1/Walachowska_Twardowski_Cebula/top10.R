library(dplyr)
library(ggplot2)

opera <- read.csv("../Data/Opera.csv")
View(opera)

top_composers_world <- opera %>%
  group_by(composer) %>%
  summarise(popularity_c = sum(performances)) %>%
  arrange(-popularity_c)

top_opers_world <- opera %>%
  group_by(composer, work) %>%
  summarise(popularity_w = sum(performances)) %>%
  filter(work != "Carmen (adaptation)") %>%
  arrange(-popularity_w)

View(top_opers_world)

city <- opera %>%
  group_by(work, city) %>%
  summarise(popularity_oc = sum(performances)) %>%
  group_by(work) %>%
  top_n(1, popularity_oc) %>%
  arrange(-popularity_oc)

top_composer_with_3_top_operas <- left_join(top_composers_world, top_opers_world, join_by(composer)) %>%
  arrange(-popularity_c) %>%
  group_by(composer) %>%
  top_n(3, popularity_w)

# tabela z top 10 kompozytorami ze świata, dla każdego jego top 3 opery i miasto,
# w którym były najczęściej wystawiane
# popularity_c (popularność kompozytora)
# popularity_w (popularność opery)
# popularity_oc (liczba wystawień danej opery w wybranym mieście)
top10_composers_top3_operas_city <- left_join(top_composer_with_3_top_operas,
                                              city, join_by(work)) %>%
  head(31)

View(top10_composers_top3_operas_city)
