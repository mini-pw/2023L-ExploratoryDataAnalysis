
library(dplyr)
library(ggplot2)
library("viridis")


piosenki <- read.csv('C:/Users/julia/Desktop/SpotifyFeatures.csv',header=TRUE,stringsAsFactors = FALSE,
                       na.string = "")

colnames(piosenki)[1] <- "gatunek muzyczny"
piosenki <- piosenki %>% 
  select(`gatunek muzyczny`,danceability)%>% 
  filter(`gatunek muzyczny`%in% c("Alternative", "Dance","Folk","Blues","Opera","Electronic","Hip_Hop","Rap","Indie","Classical","Pop","Reggea","Jazz","Rock","Soul"))
View(piosenki)
piosenki[piosenki == "Alternative"] <- "Alternatywna"
piosenki[piosenki == "Electronic"] <- "Elektroniczna"
piosenki[piosenki == "Hip_Hop"] <- "Hip Hop"
piosenki[piosenki == "Classical"] <- "Klasyczna"
piosenki[piosenki == "Alternative"] <- "Alternatywna"

boxes<- ggplot(piosenki, aes(x= `gatunek muzyczny`,y= danceability, fill =`gatunek muzyczny`)) +
  geom_boxplot(color="white", fill="#6c049c", alpha=1, linewidth = 1.1) +
  ggtitle("Taneczno??, a gatunek muzyczny")+
  ylab("Poziom taneczno?ci") + xlab("")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2, title = ""))+
  stat_summary(fun.y=mean, geom = "point", size = 5, color = "black")+
  theme(legend.position="none") +
    theme(
      panel.border = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "black"),
      plot.title = element_text(color="white", size=23, face="bold.italic"),
      axis.title.x = element_text(color="white", size=14, face="bold"),
      axis.title.y = element_text(color="white", size=20, face="bold"),
      panel.background = element_rect(fill = "black", colour = "black", size = 0.5, linetype = "solid"),
      plot.background=element_rect(fill = "black"),
      legend.background = element_rect(fill = "black"),
      axis.title = element_text(color = "white"),
      axis.text = element_text(color = "white",size = 20),
      legend.title = element_text(color="white"),
      legend.text = element_text(color="white"))
 
boxes

p <- p + stat_summary(fun.y=mean, geom = "point", size = 5, color = "black")
p+ geom_boxplot(color="white", fill="7b04ba", alpha=1, linewidth = 1.1)
