library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(htmltools)
library(wordcloud)
library(wordcloud2)
library(tm)
library(stringr)
library(igraph)
library(hexbin)
library(GGally)
library(forcats)
library(ggridges)
library(viridis)

DANE <- read.csv("DANE.csv")

# macierz korelacji

DANE_MK <- DANE %>% 
  filter(rank < 51) %>% 
  select(danceability, loudness, acousticness,
         energy, speechiness, duration_min,
         num_uniq_words, liveness, valence) %>% 
  filter(!is.na(danceability), !is.na(num_uniq_words)) %>% 
  filter(num_uniq_words > 0) %>% 
  rename("#UNIQUE WORDS" = "num_uniq_words",
         "DURATION" = "duration_min",
         "SPEECHINESS" = "speechiness",
         "ENERGY" = "energy",
         "LOUDNESS" = "loudness",
         "VALENCE" = "valence",
         "DANCEABILITY" = "danceability",
         "LIVENESS" = "liveness",
         "ACCOUSTICNESS" = "acousticness") 

DANE_MK <- round(cor(DANE_MK), 1)
ggcorrplot(DANE_MK,
           hc.order = TRUE,
           lab = TRUE,
           lab_col = "#24192d",
           lab_size = 8,
           type = "upper",
           outline.color = "#24192d",
           colors = c("#40e0d0", "#DED1F7", "#890087")) +
  theme(legend.position="none",
        axis.text.x = element_text(face="bold", color="gold", 
                                   size=18),
        axis.text.y = element_text(face="bold", color="gold", 
                                   size=14))

# wordcloud

pattern <- c("like","yeahgot", "can", "gonna","said","gotta",
             "need", "just", "come", "know", "get","tell", "cant",
             "cause", "look", "every", "ill", "aint", "hold", "call",
             "don", "see", "make", "belong", "say", "youre", "theres", "saw", 
             "keep", "good", "times","turnin", "bring", "find", "didnt",
             "along","theres", "dont", "now", "got", "take", "well", "still",
             "ooh", "will", "back", "gone", "wanna", "ever", "thats", "must",
             "shes","ive","watching","watch","youll","try","end","maybe","wont",
             "next","gave","going","taking","gettin","put", "yes", "another", 
             "want", "let")

docs <- DANE %>% pull(lyrics)
docs <- Corpus(VectorSource(docs))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, pattern)
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq = words) %>% 
  filter(!word %in% pattern) %>% 
  filter(freq > 9)

wordcloud2(df, color = rep_len(c("#40e0d0", 
                                 "#B469BF",
                                 "#CDA7E1",
                                 "#8FD9E4",
                                 "#DED1F7", 
                                 "#B469BF",
                                 "#890087"), nrow(df)),
                               backgroundColor = "#24192d")


# ridge linesy

DANE %>% 
  ggplot(aes(x = energy, y = decade, group = decade, fill = factor(decade))) +
  geom_density_ridges(rel_min_height = 0.01, scale = 5) +
  scale_fill_manual(values = rev(c("#75DBDD", 
                                   "#A9D6EA",
                                   "#DED1F7", 
                                   "#C28BD2",
                                   "#A546AC",
                                   "#890087"))) + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

# valance geomline

dt2 <- read.csv("C:\\Users\\User\\Downloads\\tracks_features.csv")

dt2 %>% 
  mutate(valence2 = 400*(valence-0.5)) %>% 
  group_by(year) %>% 
  summarise(median_valence = median(valence2)) %>% 
  mutate(sign = ifelse(median_valence > 0,1,-1)) %>% 
  ggplot(aes(x = year, y = median_valence, fill = as.factor(sign))) +
  geom_col(width=0.6)+
  scale_fill_manual(values = c("#A546AC", "#40E0D0")) + 
  scale_x_continuous(n.breaks = 20, limits = c(1950, 2020),guide = guide_axis(n = 1)) +
  scale_y_continuous(n.breaks = 16, limits = c(-100, 50),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Percentagewise valence of songs across last 70 years",x = "",y = "Negative Valence                                  Positive Valence") +
  theme(
    plot.title = element_text(hjust = 0.5, face="bold", color = "#FFD700", size = 15),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face="bold",color= "#FFD700", size = 10.5),
    axis.text.y = element_text(vjust = 0.5, hjust=1, face="bold", size=12, color= "#FFD700"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#47115A"),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(color = "darkslategray2", linewidth = 0.000001, linetype = 3),
    legend.position="none",
    plot.background = element_rect(fill = "#47115A"),
    axis.title = element_text(size = 12, color = "#FFD700", face="bold")) +
  geom_vline(xintercept = 1985.5, linetype="longdash", size = 1, color = "#FFD700") +
  annotate("text", x = 1992, y = -65, label = "Rise of hip-hop", color = "#FFD700", size = 4.5)

# kod wizualizacji na spotkania projektowe:

dt2 %>% 
  group_by(year) %>% 
  summarise(median_energy = median(energy)) %>% 
  ggplot(aes(x = year, y = median_energy)) +
  scale_x_continuous(n.breaks = 14, limits = c(min_year,max_year)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line(color='blue', size=1.5) +
  #  geom_point(shape=21, color='black', fill='red', size=2) +
  scale_y_continuous(n.breaks = 12) +
  theme(panel.background = element_rect(fill = 'lightblue', color = 'purple')) +
  labs(title = "Energiczność Piosenek przez ostatnie 70 lat",
       x = "Rok",
       y = "Mediana energiczności")

dt2 %>% 
  filter(year >= min_year, speechiness > 0) %>% 
  group_by(year) %>% 
  summarise(median_energy = median(energy), median_speech = median(speechiness)) %>% 
  ggplot(mapping = aes(x = median_energy, y = median_speech, color = year)) +
  geom_point(size = 2, position=position_jitter(h=0.003,w=0.003)) +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=12,face="bold")) +
  scale_color_gradient(low = "red", high = "green") +
  scale_y_continuous(n.breaks = 15, limits = c(0.035, 0.05)) +
  labs(title = "Zależność energii i ilości tekstu od roku",
       x = "Energiczność",
       y = "ilość tekstu", color = "Rok") +
  theme(legend.title = element_text(color = "blue", size = 15),
        legend.text = element_text(color = "red", face = "bold"))

# num words

data<-read.csv("billboard-lyrics-spotifyv2.csv")

wykres3<- data%>%filter(!is.na(words_per_sec))%>%group_by(year)%>%
  summarise(mean_words=mean(words_per_sec),
            .groups = 'drop') %>%as.data.frame()

ggplot(data = wykres3%>%filter(year<2023)) +
  geom_line(aes(x = year, y = mean_words),color="#40e0d0") +
  geom_point(aes(x = year, y = mean_words),colour="#40e0d0") +
  labs(title = "Mean number of words in each year", colour = "gold")+
  ylab("Number of words per second")+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1945,2023,2))+
  annotate(geom="text",x=1981, xmin=1980,xmax =1984, y=2.0, 
           label="Beginning of\ngolden age hip-hop",colour="gold",size=5) +
  geom_vline(xintercept=1986, linetype="dashed", color="gold", size=1)+
  
  annotate(geom="text",x=2013, xmin=1980,xmax =1984, y=1.2, 
           label="Hip-hop is the most\nlistened genre",colour="#fcc5c0",size=5) +
  
  
  geom_vline(xintercept=2017, linetype="dashed", color="#fcc5c0", size=1)+
  theme(
    line = element_line(color ="#40e0d0" ),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size=20,color="gold",face="bold"),
    axis.text.x  = element_text(size=10,color="gold",angle = 90),
    axis.text.y  = element_text(size=14,color="gold"),
    panel.grid.major  = element_line(linetype="dotted",colour ="#EA698B",size = 0.2 ),
    strip.text.x = element_text(face="bold"),
    plot.title = element_markdown(hjust=.5,size=34,lineheight=.8, face="bold", margin=margin(20,0,30,0),colour = "gold"),
    plot.subtitle = element_markdown(hjust=.5,size=18,lineheight = 1, margin=margin(10,0,30,0)),
    plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color="gold", lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(10,10,10,10),
    legend.position = "none",
    legend.title = element_text(face="bold"))