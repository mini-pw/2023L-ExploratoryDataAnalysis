library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(ggridges)
library(forcats)

Zdrowie <- read.csv("C:/Users/LEGION/OneDrive - Politechnika Warszawska/Pulpit/Projekt/mxmh_survey_results.csv")



###################   Do plakatu ##################################

Zdrowie15 <- Zdrowie %>% 
  filter(Age >= 14 & Age <= 30)

### Wykres gestosci ###

czeste <- Zdrowie %>% 
  mutate(freq2_classical = ifelse(Frequency..Classical. %in% c('Sometimes', 'Very frequently'), 1, 0), 
         freq2_hiphop = ifelse(Frequency..Hip.hop. %in% c('Sometimes', 'Very frequently'), 1, 0), 
         freq2_metal = ifelse(Frequency..Metal. %in% c('Sometimes', 'Very frequently'), 1, 0),
         freq2_pop = ifelse(Frequency..Pop. %in% c('Sometimes', 'Very frequently'), 1, 0), 
         freq2_rap = ifelse(Frequency..Rap. %in% c('Sometimes', 'Very frequently'), 1, 0), 
         freq2_rock = ifelse(Frequency..Rock. %in% c('Sometimes', 'Very frequently'), 1, 0), 
         freq2_rb = ifelse(Frequency..R.B. %in% c('Sometimes', 'Very frequently'), 1, 0)) 

sklad_class <- czeste %>% 
  filter(freq2_classical == 1) %>% 
  mutate(duzo_sluch = 'Klasyczna') %>% 
  select(duzo_sluch, Insomnia)

sklad_hiphop <- czeste %>% 
  filter(freq2_hiphop == 1) %>% 
  mutate(duzo_sluch = 'Hip Hop') %>% 
  select(duzo_sluch, Insomnia)

sklad_metal <- czeste %>% 
  filter(freq2_metal == 1) %>% 
  mutate(duzo_sluch = 'Metal') %>% 
  select(duzo_sluch, Insomnia)

sklad_pop <- czeste %>% 
  filter(freq2_pop == 1) %>% 
  mutate(duzo_sluch = 'Pop') %>% 
  select(duzo_sluch, Insomnia)

sklad_rap <- czeste %>% 
  filter(freq2_rap == 1) %>% 
  mutate(duzo_sluch = 'Rap') %>% 
  select(duzo_sluch, Insomnia)

sklad_rock <- czeste %>% 
  filter(freq2_rock == 1) %>% 
  mutate(duzo_sluch = 'Rock') %>% 
  select(duzo_sluch, Insomnia)

sklad_rb <- czeste %>% 
  filter(freq2_rb == 1) %>% 
  mutate(duzo_sluch = 'R&B') %>% 
  select(duzo_sluch, Insomnia)

gotowa <- rbind(sklad_class, sklad_hiphop, sklad_metal, sklad_pop, sklad_rap, 
                sklad_rock, sklad_rb)


wykr1 <- ggplot(gotowa, aes(x = Insomnia, y = fct_inorder(duzo_sluch), group = duzo_sluch, fill = duzo_sluch)) + 
  geom_density_ridges_gradient(scale = 2, alpha = 0.7, quantile_lines = TRUE, quantiles = c( 0.25, 0.5, 0.75), colour = "black", size = 0.2) +
  scale_fill_cyclical(name = "Cycle", guide = "legend",values = c("red2", 'red2',"orange", 'red2',  "red2", "red2", "red2")) +
  labs(x = 'Poziom bezsenności', y = "", title = "Rozkład bezsenności w zależności od gatunku muzyki") +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white", size = 9),
        plot.title = element_text(color = "white", hjust = 0.5, vjust = 4),
        legend.title = element_text(color = "white"),
        legend.position = "None",
        text = element_text(family = "Impact"))
wykr1

#### INSOMIA mediana ####

# Kolumna Srednia zawiera mediany

# Dla Classical
DepClassic <- Zdrowie15 %>% 
  group_by(Frequency..Classical.) %>% 
  summarise(Srednia = median(Insomnia)) %>% 
  merge(rep("Klasyczna")) %>% 
  rename(Freq = Frequency..Classical.)

# Dla Hip Hop
DepHipHop <- Zdrowie15 %>% 
  group_by(Frequency..Hip.hop.) %>% 
  summarise(Srednia = median(Insomnia)) %>% 
  merge(rep("Hip Hop")) %>% 
  rename(Freq = Frequency..Hip.hop.)

# Dla Rap
DepKpop <- Zdrowie15 %>% 
  group_by(Frequency..Rap.) %>% 
  summarise(Srednia = median(Insomnia)) %>% 
  merge(rep("Rap")) %>% 
  rename(Freq = Frequency..Rap.)

# Dla Metal
DepMetal <- Zdrowie15 %>% 
  group_by(Frequency..Metal.) %>% 
  summarise(Srednia = median(Insomnia)) %>% 
  merge(rep("Metal")) %>% 
  rename(Freq = Frequency..Metal.)

# Dla Pop
DepPop <- Zdrowie15 %>% 
  group_by(Frequency..Pop.) %>% 
  summarise(Srednia = median(Insomnia)) %>% 
  merge(rep("Pop")) %>% 
  rename(Freq = Frequency..Pop.)

# Dla R&B
DepR.B <- Zdrowie15 %>% 
  group_by(Frequency..R.B.) %>% 
  summarise(Srednia = median(Insomnia)) %>% 
  merge(rep("R&B")) %>% 
  rename(Freq = Frequency..R.B.)

# Dla Rock
DepRock <- Zdrowie15 %>% 
  group_by(Frequency..Rock.) %>% 
  summarise(Srednia = median(Insomnia)) %>% 
  merge(rep("Rock")) %>% 
  rename(Freq = Frequency..Rock.)


DepresjaHeatMapa <- rbind(DepClassic, DepHipHop, DepMetal,
                          DepPop, DepR.B, DepKpop, DepRock)

DepresjaHeatMapa <- DepresjaHeatMapa %>% 
  mutate(Nazwy = case_when(Freq == "Very frequently" ~ "Bardzo często", 
                           Freq == "Sometimes" ~ "Czasami",
                           Freq == "Rarely" ~ "Rzadko",
                           TRUE ~ "Nigdy"))

ggplot(DepresjaHeatMapa, aes(x = fct_inorder(y), y = fct_inorder(Nazwy), fill = Srednia)) +
  labs(title = "Mediana bezsenności względem częstotliwości słuchania danego 
         gatunku muzycznego",
       y = "", x = "", fill = "Mediana") +
  geom_tile() +
  theme_minimal() +
  scale_fill_gradient2(high =  "black",low = "red", mid = "red4", midpoint = 4) +
  theme(plot.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white", hjust = 0.5, vjust = 4),
        legend.title = element_text(color = "white", hjust = 0.5),
        text = element_text(family = "Impact"))

#### Violin Plot dla Insomia Metal #####

Zdrowie15 %>%
  mutate(Nazwy = case_when(Frequency..Metal. == "Very frequently" ~ "Bardzo często", 
                           Frequency..Metal. == "Sometimes" ~ "Czasami",
                           Frequency..Metal. == "Rarely" ~ "Rzadko",
                           TRUE ~ "Nigdy")) %>%
  
  ggplot(aes(x = fct_relevel(Nazwy, "Nigdy", "Rzadko", "Czasami", 
                             "Bardzo często"), y= Insomnia, fill = "red")) +
  scale_fill_manual(values = "red2")+
  labs(title = "Rozkład bezsenności względem częstotliwości słuchania metalu",
       x = "", y = "Poziom bezsenności", fill = "") + 
  theme_minimal() +
  theme(legend.position="none",
        plot.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white", size = 9),
        plot.title = element_text(color = "white", hjust = 0.5, vjust = 4),
        legend.title = element_text(color = "white", hjust = 0.5),
        text = element_text(family = "Impact")) +
  geom_violin() 

################# Dodatkowo, przy szukaniu ##########################
# Sprawdzmy jak wplywa muzyka na polepszenie zamopoczucia
Zdrowie %>% 
  filter(Music.effects != "") %>% 
  group_by(Music.effects) %>% 
  summarise(suma = n()) %>% 
  ggplot(aes(x = Music.effects, y = suma)) +
  labs(x = "Efekt", y = "",
       title = "Czy muzyka wp?yne?a na polepszenie Twojego samopoczucia?")+
  geom_col()

# Sprawdzmy, kt?ra muzyka wp?yne?a na zdecydowane polepszenie samopoczucia 

Polepszenie <- Zdrowie15 %>% 
  filter(Music.effects != "" & Music.effects == "Improve") %>%
  group_by(Fav.genre) %>% 
  summarise(sumaPolepszenia = n())

LicznoscGatunku <- Zdrowie15 %>% 
  group_by(Fav.genre) %>% 
  summarise(suma = n())
  
merge(Polepszenie, LicznoscGatunku) %>% 
  mutate(stusunek = sumaPolepszenia/suma) %>% 
ggplot(aes(x = reorder(Fav.genre, -stusunek), y = stusunek)) +
  labs(x = "Ulubiony gatunek muzyczny", y = "", 
       title = "Stosunek ulubionego gatunku na poprawe samopoczucia") +
  geom_col()

# Wsrod tych, ktorzy polepszyli samopoczucie jak sluchali gospelu

Zdrowie %>% 
  filter(Music.effects == "Improve") %>% 
  group_by(Frequency..Gospel.) %>% 
  summarise(suma = n()) %>% 
  ggplot(aes(x = Frequency..Gospel., y = suma)) +
  labs(x = "Czestosc", y ="",
  title = "Czestosc sluchania gospelu przez osoby, ktore wykazaly poprawe samopoczucia") +
  geom_col()

# Wsrod tych, ktorzy polepszyli samopoczucie jak sluchali Metalu

Zdrowie15 %>% 
  filter(Music.effects == "Improve") %>% 
  group_by(Frequency..Metal.) %>% 
  summarise(suma = n()) %>% 
  ggplot(aes(x = Frequency..Metal., y = suma)) +
  labs(x = "Czestosc", y ="",
       title = "Czestosc sluchania Metalu przez osoby, ktore wykazaly poprawe samopoczucia") +
  geom_col()

# Wsrod tych, ktorzy polepszyli samopoczucie jak sluchali Hip-hop

Zdrowie %>% 
  filter(Music.effects == "Improve") %>% 
  group_by(Frequency..Hip.hop.) %>% 
  summarise(suma = n()) %>% 
  ggplot(aes(x = Frequency..Hip.hop., y = suma)) +
  labs(x = "Czestosc", y ="",
       title = "Czestosc sluchania Hip-Hopu przez osoby, ktore wykazaly poprawe samopoczucia") +
  geom_col()

# Wsrod tych, ktorzy polepszyli samopoczucie jak sluchali Rocku

Zdrowie %>% 
  filter(Music.effects == "Improve") %>% 
  group_by(Frequency..Rock.) %>% 
  summarise(suma = n()) %>% 
  ggplot(aes(x = Frequency..Rock., y = suma)) +
  labs(x = "Czestosc", y ="",
       title = "Czestosc sluchania Rock przez osoby, ktore wykazaly poprawe samopoczucia") +
  geom_col()

# Wsrod tych, ktorzy polepszyli samopoczucie jak sluchali Latina

Zdrowie %>% 
  filter(Music.effects == "Improve") %>% 
  group_by(Frequency..Latin.) %>% 
  summarise(suma = n()) %>% 
  ggplot(aes(x = Frequency..Latin., y = suma)) +
  labs(x = "Czestosc", y ="",
       title = "Czestosc sluchania Latin przez osoby, ktore wykazaly poprawe samopoczucia") +
  geom_col()

# Wsrod tych, ktorzy polepszyli samopoczucie jak sluchali Video music

Zdrowie %>% 
  filter(Music.effects == "Improve") %>% 
  group_by(Frequency..Video.game.music.) %>% 
  summarise(suma = n()) %>% 
  ggplot(aes(x = Frequency..Video.game.music., y = suma)) +
  labs(x = "Czestosc", y ="",
       title = "Czestosc sluchania muzyki z gier przez osoby, ktore wykazaly poprawe samopoczucia") +
  geom_col()


# Teraz zbadajmy osoby u ktorych wyskoczylo pogorszenie

Pogorszenie <- Zdrowie %>% 
  filter(Music.effects != "" & Music.effects == "Worsen") %>%
  group_by(Fav.genre) %>% 
  summarise(sumaPogorszenia = n())

LicznoscGatunku <- Zdrowie %>% 
  group_by(Fav.genre) %>% 
  summarise(suma = n())

merge(Pogorszenie, LicznoscGatunku) %>% 
  mutate(stusunek = sumaPogorszenia/suma) %>% 
  ggplot(aes(x = reorder(Fav.genre, -stusunek), y = stusunek)) +
  labs(x = "Ulubiony gatunek muzyczny", y = "", 
       title = "Stosunek ulubionego gatunku na pogorszenie samopoczucia") +
  geom_col()


# Wsrod tych, ktorzy pogorszyli samopoczucie jak sluchali muzyka z gier

Zdrowie %>% 
  filter(Music.effects == "Worsen") %>% 
  group_by(Frequency..Video.game.music.) %>% 
  summarise(suma = n()) %>% 
  ggplot(aes(x = Frequency..Video.game.music., y = suma)) +
  labs(x = "Czestosc", y ="",
       title = "Czestosc sluchania Video games przez osoby, ktore wykazaly zmniejszenie samopoczucia") +
  geom_col()

# Teraz zbadajmy osoby u ktorych nie ma zmiany

Brak <- Zdrowie %>% 
  filter(Music.effects != "" & Music.effects == "No effect") %>%
  group_by(Fav.genre) %>% 
  summarise(sumaBrak = n())

LicznoscGatunku <- Zdrowie %>% 
  group_by(Fav.genre) %>% 
  summarise(suma = n())

merge(Brak, LicznoscGatunku) %>% 
  mutate(stusunek = sumaBrak/suma) %>% 
  ggplot(aes(x = reorder(Fav.genre, -stusunek), y = stusunek)) +
  labs(x = "Ulubiony gatunek muzyczny", y = "", 
       title = "Stosunek ulubionego gatunku na brak wplywu") +
  geom_col()

# Sprawdzmy, dla ludzi ktorzy sluchaja Latina czasami i czesto

Zdrowie %>% 
  filter(Frequency..Latin. == "Sometimes" | Frequency..Latin. == "Very frequently") %>% 
  mutate(Lek = ifelse(Anxiety >= 6, 1, 0)) %>% 
  mutate(Depresja = ifelse(Depression >= 6, 1, 0)) %>% 
  mutate(Bezsennosc = ifelse(Insomnia >= 6, 1, 0)) %>% 
  mutate(Zaburzenia = ifelse(OCD >= 6, 1, 0)) %>% 
  summarise(SumaLeku = sum(Lek), SumaDepresja = sum(Depresja), 
            SumaBezsennosc=sum(Bezsennosc), SumaZanurz = sum(Zaburzenia)) 
  ChorobyLatin <- data.frame( Choroby =c("SumaLeku", "SumaDepresja", "SumaBezsennosc", "SumaZanurz"), 
             Liczba = c(78, 66, 46, 19))

  ggplot(ChorobyLatin, aes(x = Choroby, y = Liczba)) +
    geom_col()
  
  # Sprawdzmy, dla ludzi ktorzy sluchaja Gospal czasami i czesto
  
  Zdrowie %>% 
    filter(Frequency..Gospel. == "Sometimes" | Frequency..Gospel. == "Very frequently") %>% 
    mutate(Lek = ifelse(Anxiety >= 6, 1, 0)) %>% 
    mutate(Depresja = ifelse(Depression >= 6, 1, 0)) %>% 
    mutate(Bezsennosc = ifelse(Insomnia >= 6, 1, 0)) %>% 
    mutate(Zaburzenia = ifelse(OCD >= 6, 1, 0)) %>% 
    summarise(SumaLeku = sum(Lek), SumaDepresja = sum(Depresja), 
              SumaBezsennosc=sum(Bezsennosc), SumaZanurz = sum(Zaburzenia)) 
  ChorobyGospel <- data.frame( Choroby =c("SumaLeku", "SumaDepresja", "SumaBezsennosc", "SumaZanurz"), 
                              Liczba = c(34, 28, 18, 9))
  
  ggplot(ChorobyGospel, aes(x = Choroby, y = Liczba)) +
    ylim(0, 80) +
    geom_col()
  
  
  # Spróbujmy z naszą grupa wiekową
  

  Zdrowie15 %>% 
    subset(Fav.genre %in% PopularneGat$Fav.genre) %>% 
  ggplot(aes(x = Fav.genre, y= Depression)) + 
    geom_violin()  
  
  PopularneGat <- Zdrowie15 %>% 
    group_by(Fav.genre) %>% 
    summarise(SumaGatunku = n()) %>% 
    filter(SumaGatunku >= 26)

  # Insomnia (*)
  Zdrowie15 %>% 
    subset(Fav.genre %in% PopularneGat$Fav.genre) %>% 
    ggplot(aes(x = Fav.genre, y= Insomnia)) + 
    geom_violin() 
  
  # Anxiety
  Zdrowie15 %>% 
    subset(Fav.genre %in% PopularneGat$Fav.genre) %>% 
    ggplot(aes(x = Fav.genre, y= Anxiety)) + 
    geom_violin() 

  
  ## Boxploty
  Zdrowie15 %>% 
    subset(Fav.genre %in% PopularneGat$Fav.genre) %>% 
    ggplot(aes(x = Fav.genre, y= Depression)) + 
    geom_boxplot()
  
  # Inosmnia
  Zdrowie15 %>% 
    subset(Fav.genre %in% PopularneGat$Fav.genre) %>% 
    ggplot(aes(x = Fav.genre, y= Insomnia)) + 
    geom_boxplot()   
  
  #Anxiety
  Zdrowie15 %>% 
    subset(Fav.genre %in% PopularneGat$Fav.genre) %>% 
    ggplot(aes(x = Fav.genre, y= Anxiety)) + 
    geom_boxplot() 
  
  # Heatmapa
  
  # Dla Classical
  DepClassic <- Zdrowie15 %>% 
    group_by(Frequency..Classical.) %>% 
    summarise(Srednia = mean(Depression)) %>% 
    merge(rep("Classic")) %>% 
    rename(Freq = Frequency..Classical.)
  
  # Dla EDM
  DepEDM <- Zdrowie15 %>% 
    group_by(Frequency..EDM.) %>% 
    summarise(Srednia = mean(Depression)) %>% 
    merge(rep("EDM"))%>% 
    rename(Freq = Frequency..EDM.)

  # Dla Hip Hop
  DepHipHop <- Zdrowie15 %>% 
    group_by(Frequency..Hip.hop.) %>% 
    summarise(Srednia = mean(Depression)) %>% 
    merge(rep("Hip Hop")) %>% 
    rename(Freq = Frequency..Hip.hop.)
  
  # Dla Kpop
  DepKpop <- Zdrowie15 %>% 
    group_by(Frequency..K.pop.) %>% 
    summarise(Srednia = mean(Depression)) %>% 
    merge(rep("Kpop")) %>% 
    rename(Freq = Frequency..K.pop.)
  
  # Dla Metal
  DepMetal <- Zdrowie15 %>% 
    group_by(Frequency..Metal.) %>% 
    summarise(Srednia = mean(Depression)) %>% 
    merge(rep("Metal")) %>% 
    rename(Freq = Frequency..Metal.)
  
  # Dla Pop
  DepPop <- Zdrowie15 %>% 
    group_by(Frequency..Pop.) %>% 
    summarise(Srednia = mean(Depression)) %>% 
    merge(rep("Pop")) %>% 
    rename(Freq = Frequency..Pop.)
  
  # Dla R&B
  DepR.B <- Zdrowie15 %>% 
    group_by(Frequency..R.B.) %>% 
    summarise(Srednia = mean(Depression)) %>% 
    merge(rep("R&B")) %>% 
    rename(Freq = Frequency..R.B.)
  
  # Dla Rock
  DepRock <- Zdrowie15 %>% 
    group_by(Frequency..Rock.) %>% 
    summarise(Srednia = mean(Depression)) %>% 
    merge(rep("Rock")) %>% 
    rename(Freq = Frequency..Rock.)
  
  # Dla Video Games
  DepGames <- Zdrowie15 %>% 
    group_by(Frequency..Video.game.music.) %>% 
    summarise(Srednia = mean(Depression)) %>% 
    merge(rep("Video game music"))%>% 
    rename(Freq = Frequency..Video.game.music.)
  
  DepresjaHeatMapa <- rbind(DepClassic, DepEDM, DepHipHop, DepKpop, DepMetal,
                            DepPop, DepR.B, DepRock, DepGames)

  ggplot(DepresjaHeatMapa, aes(x = y, y = Freq, fill = Srednia)) +
    geom_tile()
  
  
  ### Z mediana
  
  # Dla Classical
  DepClassic <- Zdrowie15 %>% 
    group_by(Frequency..Classical.) %>% 
    summarise(Srednia = median(Depression)) %>% 
    merge(rep("Classic")) %>% 
    rename(Freq = Frequency..Classical.)
  
  # Dla EDM
  DepEDM <- Zdrowie15 %>% 
    group_by(Frequency..EDM.) %>% 
    summarise(Srednia = median(Depression)) %>% 
    merge(rep("EDM"))%>% 
    rename(Freq = Frequency..EDM.)
  
  # Dla Hip Hop
  DepHipHop <- Zdrowie15 %>% 
    group_by(Frequency..Hip.hop.) %>% 
    summarise(Srednia = median(Depression)) %>% 
    merge(rep("Hip Hop")) %>% 
    rename(Freq = Frequency..Hip.hop.)
  
  # Dla Kpop
  DepKpop <- Zdrowie15 %>% 
    group_by(Frequency..K.pop.) %>% 
    summarise(Srednia = median(Depression)) %>% 
    merge(rep("Kpop")) %>% 
    rename(Freq = Frequency..K.pop.)
  
  # Dla Metal
  DepMetal <- Zdrowie15 %>% 
    group_by(Frequency..Metal.) %>% 
    summarise(Srednia = median(Depression)) %>% 
    merge(rep("Metal")) %>% 
    rename(Freq = Frequency..Metal.)
  
  # Dla Pop
  DepPop <- Zdrowie15 %>% 
    group_by(Frequency..Pop.) %>% 
    summarise(Srednia = median(Depression)) %>% 
    merge(rep("Pop")) %>% 
    rename(Freq = Frequency..Pop.)
  
  # Dla R&B
  DepR.B <- Zdrowie15 %>% 
    group_by(Frequency..R.B.) %>% 
    summarise(Srednia = median(Depression)) %>% 
    merge(rep("R&B")) %>% 
    rename(Freq = Frequency..R.B.)
  
  # Dla Rock
  DepRock <- Zdrowie15 %>% 
    group_by(Frequency..Rock.) %>% 
    summarise(Srednia = median(Depression)) %>% 
    merge(rep("Rock")) %>% 
    rename(Freq = Frequency..Rock.)
  
  # Dla Video Games
  DepGames <- Zdrowie15 %>% 
    group_by(Frequency..Video.game.music.) %>% 
    summarise(Srednia = median(Depression)) %>% 
    merge(rep("Video game music"))%>% 
    rename(Freq = Frequency..Video.game.music.)
  
  DepresjaHeatMapa <- rbind(DepClassic, DepEDM, DepHipHop, DepKpop, DepMetal,
                            DepPop, DepR.B, DepRock, DepGames)
  
  ggplot(DepresjaHeatMapa, aes(x = y, y = Freq, fill = Srednia)) +
    geom_tile()
  
  
  #### INSOMIA ####
  
  # Dla Classical
  DepClassic <- Zdrowie15 %>% 
    group_by(Frequency..Classical.) %>% 
    summarise(Srednia = mean(Insomnia)) %>% 
    merge(rep("Classic")) %>% 
    rename(Freq = Frequency..Classical.)
  
  # Dla EDM
  DepEDM <- Zdrowie15 %>% 
    group_by(Frequency..EDM.) %>% 
    summarise(Srednia = mean(Insomnia)) %>% 
    merge(rep("EDM"))%>% 
    rename(Freq = Frequency..EDM.)
  
  # Dla Hip Hop
  DepHipHop <- Zdrowie15 %>% 
    group_by(Frequency..Hip.hop.) %>% 
    summarise(Srednia = mean(Insomnia)) %>% 
    merge(rep("Hip Hop")) %>% 
    rename(Freq = Frequency..Hip.hop.)
  
  # Dla Kpop
  DepKpop <- Zdrowie15 %>% 
    group_by(Frequency..K.pop.) %>% 
    summarise(Srednia = mean(Insomnia)) %>% 
    merge(rep("Kpop")) %>% 
    rename(Freq = Frequency..K.pop.)
  
  # Dla Metal
  DepMetal <- Zdrowie15 %>% 
    group_by(Frequency..Metal.) %>% 
    summarise(Srednia = mean(Insomnia)) %>% 
    merge(rep("Metal")) %>% 
    rename(Freq = Frequency..Metal.)
  
  # Dla Pop
  DepPop <- Zdrowie15 %>% 
    group_by(Frequency..Pop.) %>% 
    summarise(Srednia = mean(Insomnia)) %>% 
    merge(rep("Pop")) %>% 
    rename(Freq = Frequency..Pop.)
  
  # Dla R&B
  DepR.B <- Zdrowie15 %>% 
    group_by(Frequency..R.B.) %>% 
    summarise(Srednia = mean(Insomnia)) %>% 
    merge(rep("R&B")) %>% 
    rename(Freq = Frequency..R.B.)
  
  # Dla Rock
  DepRock <- Zdrowie15 %>% 
    group_by(Frequency..Rock.) %>% 
    summarise(Srednia = mean(Insomnia)) %>% 
    merge(rep("Rock")) %>% 
    rename(Freq = Frequency..Rock.)
  
  # Dla Video Games
  DepGames <- Zdrowie15 %>% 
    group_by(Frequency..Video.game.music.) %>% 
    summarise(Srednia = mean(Insomnia)) %>% 
    merge(rep("Video game music"))%>% 
    rename(Freq = Frequency..Video.game.music.)
  
  DepresjaHeatMapa <- rbind(DepClassic, DepEDM, DepHipHop, DepKpop, DepMetal,
                            DepPop, DepR.B, DepRock, DepGames)
  
  ggplot(DepresjaHeatMapa, aes(x = y, y = Freq, fill = Srednia)) +
    geom_tile()+
  labs(title = "
       Średnia bezsennosci względem częstotliwości sluchania danego gatunku muzycznego",
                    y = "Częstotliwość", x = "Gatunek muzyczny") +
    scale_fill_gradient2(high =  "black",low = "red3", mid = "red4", midpoint = 4)+
    theme_minimal()
    
  
 
  
  

  ### Wykres gestosci ###
  
  czeste <- Zdrowie %>% 
    mutate(freq2_classical = ifelse(Frequency..Classical. %in% c('Sometimes', 'Very frequently'), 1, 0), 
           freq2_hiphop = ifelse(Frequency..Hip.hop. %in% c('Sometimes', 'Very frequently'), 1, 0), 
           freq2_metal = ifelse(Frequency..Metal. %in% c('Sometimes', 'Very frequently'), 1, 0),
           freq2_pop = ifelse(Frequency..Pop. %in% c('Sometimes', 'Very frequently'), 1, 0), 
           freq2_rap = ifelse(Frequency..Rap. %in% c('Sometimes', 'Very frequently'), 1, 0), 
           freq2_rock = ifelse(Frequency..Rock. %in% c('Sometimes', 'Very frequently'), 1, 0), 
           freq2_rb = ifelse(Frequency..R.B. %in% c('Sometimes', 'Very frequently'), 1, 0)) 
  
  sklad_class <- czeste %>% 
    filter(freq2_classical == 1) %>% 
    mutate(duzo_sluch = 'Klasyczna') %>% 
    select(duzo_sluch, Insomnia)
  
  sklad_hiphop <- czeste %>% 
    filter(freq2_hiphop == 1) %>% 
    mutate(duzo_sluch = 'Hip Hop') %>% 
    select(duzo_sluch, Insomnia)
  
  sklad_metal <- czeste %>% 
    filter(freq2_metal == 1) %>% 
    mutate(duzo_sluch = 'Metal') %>% 
    select(duzo_sluch, Insomnia)
  
  sklad_pop <- czeste %>% 
    filter(freq2_pop == 1) %>% 
    mutate(duzo_sluch = 'Pop') %>% 
    select(duzo_sluch, Insomnia)
  
  sklad_rap <- czeste %>% 
    filter(freq2_rap == 1) %>% 
    mutate(duzo_sluch = 'Rap') %>% 
    select(duzo_sluch, Insomnia)
  
  sklad_rock <- czeste %>% 
    filter(freq2_rock == 1) %>% 
    mutate(duzo_sluch = 'Rock') %>% 
    select(duzo_sluch, Insomnia)
  
  sklad_rb <- czeste %>% 
    filter(freq2_rb == 1) %>% 
    mutate(duzo_sluch = 'R&B') %>% 
    select(duzo_sluch, Insomnia)
  
  gotowa <- rbind(sklad_class, sklad_hiphop, sklad_metal, sklad_pop, sklad_rap, 
                  sklad_rock, sklad_rb)
  
  
  wykr1 <- ggplot(gotowa, aes(x = Insomnia, y = fct_inorder(duzo_sluch), group = duzo_sluch, fill = duzo_sluch)) + 
    geom_density_ridges_gradient(scale = 2, alpha = 0.7, quantile_lines = TRUE, quantiles = c( 0.25, 0.5, 0.75), colour = "black", size = 0.2) +
    scale_fill_cyclical(name = "Cycle", guide = "legend",values = c("red2", 'red2',"orange", 'red2',  "red2", "red2", "red2")) +
    labs(x = 'Poziom bezsenności', y = "", title = "Rozkład bezsenności w zależności od gatunku muzyki") +
    theme_minimal()+
    theme(plot.background = element_rect(fill = "black"),
          legend.text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white", size = 9),
          plot.title = element_text(color = "white", hjust = 0.5, vjust = 4),
          legend.title = element_text(color = "white"),
          legend.position = "None",
          text = element_text(family = "Impact"))
  wykr1
  
  sklad_metal_wiek <- czeste %>% 
    filter(freq2_metal == 1) %>% 
    mutate(poziom_bezsennosci = case_when(Insomnia %in% c(0, 1, 2, 3) ~'niski poziom',
                                          Insomnia %in% c(4, 5, 6, 7) ~'sredni poziom',
                                          TRUE ~'wysoki poziom')) %>% 
    select(poziom_bezsennosci, Age)
  
  wykr2 <- ggplot(sklad_metal_wiek, aes(x = Age, y = poziom_bezsennosci, group = poziom_bezsennosci, fill = poziom_bezsennosci)) + 
    geom_density_ridges_gradient(scale = 1.3, alpha = 0.7, quantile_lines = TRUE, quantiles=c(0.5, 0.9), colour = "white", size = 0.2) +
    labs(x = "wiek", y = "poziom bezsennosci", title = "Wykres rozk?adu wieku w zale?no?ci od bezsenno?ci w?r?d os?b co najmniej czasami s?uchaj?cych metalu") +
    theme_minimal()
  wykr2

  
  
  