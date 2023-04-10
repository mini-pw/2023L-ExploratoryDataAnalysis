install.packages("wordcloud")
library(wordcloud)
install.packages("wordcloud2")
library(wordcloud2)

class(data_wc$EPİK1) <- "character"
class(data_wc$SATİRİK1) <- "character"
class(data_wc$LİRİKROM1) <- "character"
class(data_wc$DİDAKTİK1) <- "character"
class(data_wc$PASTORAL1) <- "character"
class(data_wc$LİİRKHÜZÜN1) <- "character"
data_wc[data_wc == "1"] <- "Meat"
data_wc[data_wc == "2"] <- "Fast-food"
data_wc[data_wc == "3"] <- "Milky desserts"
data_wc[data_wc == "4"] <- "Sherbet desserts"
data_wc[data_wc == "5"] <- "Alcohols"
data_wc[data_wc == "6"] <- "Water"
data_wc[data_wc == "7"] <- "Tea"
data_wc[data_wc == "8"] <- "Coffee"
data_wc[data_wc == "9"] <- "Bakery"
data_wc[data_wc == "10"] <- "Fruits"
data_wc[data_wc == "11"] <- "Vegetables"
data_wc[data_wc == "12"] <- "Tobacco products"
data_wc[data_wc == "13"] <- "Carbonated drinks"

data_wc %>% group_by(LİRİKROM1) %>% 
  summarise(observed_sum=n()) %>% 
  arrange(desc(observed_sum)) -> data_wc_lirikrom

par(bg="#FAE3F0")
wordcloud(words = data_wc_lirikrom$LİRİKROM1, freq = data_wc_lirikrom$observed_sum, 
          min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = c("#ef4b7e", "#3cb9ac", "#ff7e23", "#a4711e", "#82bfec", "#a6207a")) -> wc_liryk

data_wc %>% group_by(PASTORAL1) %>% 
  summarise(observed_sum=n()) %>% 
  arrange(desc(observed_sum)) -> data_wc_pastoral

wordcloud(words = data_wc_pastoral$PASTORAL1, freq = data_wc_pastoral$observed_sum, 
          min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = c("#ef4b7e", "#3cb9ac", "#ff7e23", "#a4711e", "#82bfec", "#a6207a"), scale=c(4,0.25)) -> wc_idyl

data_wc %>% group_by(EPİK1) %>% 
  summarise(observed_sum=n()) %>% 
  arrange(desc(observed_sum)) -> data_wc_epik

wordcloud(words = data_wc_epik$EPİK1, freq = data_wc_epik$observed_sum, 
          min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = c("#ef4b7e", "#3cb9ac", "#ff7e23", "#a4711e", "#82bfec", "#a6207a"), scale=c(4,0.25)) -> wc_epik
