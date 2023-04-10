# 1

dane %>% 
  filter(Age <= 40 & Age >= 18) %>% 
  filter(Fav.genre == "Metal" | Fav.genre == "EDM" | Fav.genre == "Country" | Fav.genre == "R&B") %>% 
  ggplot(aes(x=Fav.genre, y=Insomnia, fill = Fav.genre, color = Fav.genre)) +
  geom_violin()+
  scale_color_manual(values = c("#e2d4ff", "#e2d4ff", "#fced60", "#e2d4ff"))+
  scale_fill_manual(values = c("#e2d4ff", "#e2d4ff", "#fced60", "#e2d4ff"))+
  labs(
    x = "Genre",
    y = "Insomnia Rate")+
  theme(
    plot.background = element_rect(fill = "#333333", colour = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    legend.background = element_rect(fill = "#333333"),
    legend.position = "none",
    axis.title = element_text(color = "white", size = 19),
    axis.text = element_text(color = "white", size = 17),
    panel.grid.major = element_line(color = "#BBBBBB"),
    panel.grid.minor = element_blank())+
  stat_summary(fun=mean, geom="point", shape=21, size=3.6, color = "black")+
  stat_summary(fun=mean, geom="point", shape=20, size=3.5, color = "#333333")+
  scale_x_discrete(limits=c("Metal", "EDM", "Country", "R&B"))

# 2

dane %>% 
  filter(Age <= 40 & Age >= 18) %>% 
  filter(Fav.genre %in% c("Metal", "R&B")) %>% 
  ggplot(aes(x = Age, color = Fav.genre, fill = Fav.genre))+
  geom_density(alpha = 0.15 , size = 1.6)+
  scale_color_manual(values = c("#fce300", "#e2d4ff"))+
  scale_fill_manual(values = c("#fce300", "#e2d4ff"))+
  labs(y = "Density")+
  theme(
    plot.background = element_rect(fill = "#333333", colour = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    legend.background = element_rect(fill = "#333333"),
    legend.text = element_text(color = "white", size = 17,),
    legend.key = element_blank(),
    legend.title = element_text(color = "white", size = 19),
    axis.title = element_text(color = "white", size = 19),
    axis.text = element_text(color = "white", size = 17),
    panel.grid.major = element_line(color = "#BBBBBB"),
    panel.grid.minor = element_blank()
  )

??legend.tittle
# 3

mean_BPM <- dane %>% 
  filter(BPM <= 250) %>% 
  mutate(Metal = ifelse(Fav.genre == "Metal" | Frequency..Metal. == "Very frequently", "Yes", ifelse(Frequency..Metal. == "Never", "No", ""))) %>% 
  filter(Metal == "No" | Metal == "Yes") %>% 
  group_by(Metal) %>% 
  summarise(mean = mean(BPM))

options(scipen = 999)
dane %>% 
  filter(BPM <= 250) %>% 
  mutate(Metal = ifelse(Fav.genre == "Metal" | Frequency..Metal. == "Very frequently", "Yes", ifelse(Frequency..Metal. == "Never", "No", ""))) %>% 
  filter(Metal == "No" | Metal == "Yes") %>% 
  ggplot(aes(x  = BPM, color = Metal, fill = Metal))+
  geom_density(linetype = "dotted", size = 1, alpha = 0.07)+
  geom_vline(data = mean_BPM, aes(xintercept = mean, 
                                  color = Metal), size=0.7)+
  scale_color_manual(values = c("#e2d4ff", "#fce300"))+
  scale_fill_manual(values = c("#e2d4ff", "#fce300"))+
  labs(y = "Density")+
  theme(
    plot.background = element_rect(fill = "#333333", colour = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    legend.background = element_rect(fill = "#333333"),
    legend.key = element_blank(),
    legend.text = element_text(color = "white", size = 17),
    legend.title = element_text(color = "white", size = 19),
    axis.title = element_text(color = "white", size = 19),
    axis.text = element_text(color = "white", size = 17),
    panel.grid.major = element_line(color = "#BBBBBB"),
    panel.grid.minor = element_blank()
  )
# 4 
dane %>% 
  mutate(Depression_level = case_when(Depression <= 3 ~ "low",
                                      Depression >= 7 ~ "high",
                                      TRUE ~ "medium")) %>% 
  mutate(Metal = ifelse(Fav.genre == "Metal" | Frequency..Metal. == "Very frequently", "Yes", ifelse(Frequency..Metal. == "Never", "No", ""))) %>% 
  filter(Metal %in% c("No", "Yes")) %>% 
  group_by(Metal, Depression_level) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(percent = ifelse(Metal == "No", n/264, n/152)) %>% 
  ggplot(aes(x = Depression_level, y = percent, fill = Metal)) +
  geom_col(position = "dodge2")+
  scale_x_discrete(limits=c("low", "medium", "high"))+
  scale_fill_manual(values = c("#e2d4ff","#fced60"))+
  labs(y = "Percentage",
       x = "Depression level")+
  theme(
    plot.background = element_rect(fill = "#333333", colour = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    legend.background = element_rect(fill = "#333333"),
    legend.text = element_text(color = "white", size = 17),
    legend.title = element_text(color = "white", size = 19),
    axis.title = element_text(color = "white", size = 19),
    axis.text = element_text(color = "white", size = 17),
    panel.grid.major = element_line(color = "#BBBBBB"),
    panel.grid.minor = element_blank()
  )+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()
