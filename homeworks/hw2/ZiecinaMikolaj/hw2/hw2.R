library(ggplot2)
library(forcats)

df <- data.frame(x = c('TVP2', 'TVP1', 'tvn', 'polsat'), y = c(0.0786, 0.0782 ,0.074, 0.0733))
ggplot(df, aes(x = fct_rev(x), y = y))+
  geom_col(fill = 'darkgreen', width = 0.6)+
  labs(x = 'Stacja telewizyjna', y = 'Udział stacji', title = 'Udziały 4 głównych stacji w marcu 2023')+
  scale_y_continuous(labels = scales::percent)+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white'))
