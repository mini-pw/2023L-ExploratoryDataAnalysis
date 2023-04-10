library("dplyr")
library(SmarterPoland)
library(ggplot2)
library(tidyr)

#dane do heatmapy
top_2010_2019 <- read.csv("top10s.csv")

dane_wykres_2 <- top_2010_2019 %>% 
  select(dnce, nrgy, val, acous, pop) %>% 
  rename(taneczność = dnce, energia = nrgy, pozytywność = val, akustyczność = acous, popularność = pop)

#macierz korelacji
corr_matrix <- cor(dane_wykres_2)

corr_df <- as.data.frame(corr_matrix)
#indeksy i kolumnt z nazwami wierszy i kolumn
corr_df <- corr_df %>%
  mutate(corr_df, var1 = rownames(corr_df)) %>% 
  pivot_longer(-var1, names_to = "var2", values_to = "Współczynnik") %>% 
  mutate(Współczynnik = scale(Współczynnik)) 

#skalowanie wspólczynnika korelacji
corr_df$Współczynnik <- scale(corr_df$Współczynnik, center = TRUE, scale = max(abs(corr_df$Współczynnik)))
corr_df$Współczynnik <- round(corr_df$Współczynnik,2)
  
# Tworzenie heatmapy
ggplot(corr_df, aes(var1, var2, fill = Współczynnik)) +
  geom_tile() +
  scale_fill_gradient2(low = "#192c85", mid = "white", high = "#7c00ca", midpoint = 0) +
  labs(title = "Mapa ciepła przedstawiająca korelację różnych własciwości", 
       x = "", y = "") +
  theme( panel.background = element_rect(fill = "black", colour = "black", size = 0.5, linetype = "solid"),plot.background=element_rect(fill = "black"),
         legend.background = element_rect(fill = "black"),
         plot.title = element_text(color = "white",size=20),
         axis.title = element_text(color = "white"),
         axis.text = element_text(color = "white"),
         legend.title = element_text(color="white"),
         legend.text = element_text(color="white"))+
  coord_fixed(1) +
  geom_text(aes(label = Współczynnik),color="black")


  
  
  

  
