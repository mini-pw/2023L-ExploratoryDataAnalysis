dane <- mxmh_survey_results
library(fmsb)

df <- dane %>% 
  group_by(`Fav genre`) %>% 
  filter(!is.na(`Fav genre`)) %>%
  select(Anxiety, Depression, Insomnia, OCD) %>% 
  summarise_at(vars("Anxiety","Depression","Insomnia", "OCD"),median)



# Wcześniej wykonana ramka danych z gatunkami i medianą dla odpowiednich zaburzeń
Dane_gatunki <- as.data.frame(df)

{

  Depresja <- Dane_gatunki %>% select(`Fav genre`, Depression)
  #Modyfikuje ramkę danych, tworzę transpozycję
  Depresja_radar <- as.data.frame(Depresja[,-1])
  rownames(Depresja_radar) <- Depresja[,1]
  Depresja_radar <- as.data.frame(t(Depresja_radar))
  # Tutaj wartości minimalne i maksymalne wykresu jakie chcemy
  Depresja_radar <- rbind(rep(10,16) , rep(0,16) , Depresja_radar)
  #Funkcja tworząca wykres
  Depresja_radar <- radarchart(Depresja_radar, axistype = 0, seg = 10, cglwd=1,cglty=1, plwd=2, vlcex=0.8, title = "Depresja",
                               axislabcol = "grey40", cglcol = "grey40", pcol = "#C51306")
}

{
Niepokoj <- Dane_gatunki %>% select(`Fav genre`, Anxiety)
Niepokoj_radar <- as.data.frame(Niepokoj[,-1])
rownames(Niepokoj_radar) <- Niepokoj[,1]
Niepokoj_radar <- as.data.frame(t(Niepokoj_radar))
Niepokoj_radar <- rbind(rep(10,16) , rep(0,16) , Niepokoj_radar)
Niepokoj_radar <-radarchart(Niepokoj_radar, axistype = 0, seg = 10, cglwd=1,cglty=1, plwd=2, vlcex=0.8, title = "Niepokój", 
                            axislabcol = "grey30", cglcol = "grey30", pcol = "#C51306")

}


{
  Bezsennosc <- Dane_gatunki %>% select(`Fav genre`, Insomnia)
  Bezsennosc_radar <- as.data.frame(Bezsennosc[,-1])
  rownames(Bezsennosc_radar) <- Bezsennosc[,1]
  Bezsennosc_radar <- as.data.frame(t(Bezsennosc_radar))
  Bezsennosc_radar <- rbind(rep(10,16) , rep(0,16) , Bezsennosc_radar)
  Bezsennosc_radar <- radarchart(Bezsennosc_radar, axistype = 0, seg = 10, cglwd=1,cglty=1, plwd=2, vlcex=0.8, title = "Bezsenność",
                                 axislabcol = "grey40", cglcol = "grey40", pcol = "#C51306")
}

