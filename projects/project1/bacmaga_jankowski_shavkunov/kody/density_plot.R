library(reticulate)
library(ggplot2)
library(ggridges)
library(dplyr)
library(stringi)
library(viridis)
library(hrbrthemes)
library(forcats)
library(thematic)
library(extrafont)
font_import()
loadfonts()
Sys.setlocale("LC_TIME", "English")

path <- paste(getwd(), 'data_aggregated_global', sep='/')
files <- list.files(path = path)
full_paths <- paste(path, files, sep = '/')
full_df <- read.csv(full_paths[1])
full_df$week <- as.Date(stri_sub(files[1], from = 24, to = 33))
full_df$month <- format(full_df$week, '%B')
for (i in 2:53) {
  temp <- read.csv(full_paths[i])
  temp$week <- as.Date(stri_sub(files[i], from = 24, to = 33))
  temp$month <- format(temp$week, '%B')
  full_df <- rbind(full_df, temp)
}
full_df <- full_df %>% mutate(month = as_factor(month))





plot_a <- ggplot(full_df, aes(y = fct_rev(month), x = valence, fill = ..x..)) + 
  stat_density_ridges(geom = "density_ridges_gradient", quantile_lines = TRUE, quantiles = 2, color = "coral") +
  scale_fill_gradient(low = "white", high = "#ff7700") +
  labs(title = 'Distribution of Positiveness',
       x = 'Positiveness', y ='') +
  xlim(0, 1)
plot_final <- plot_a +
  theme(legend.position = "none",
  plot.background = element_rect(fill = "#FFF6ED"),
  text = element_text(family = "Montserrat", size = 12, colour = "black"),
  plot.title = element_text(family = "Montserrat", size = 16, colour = "black"),
  plot.subtitle = element_text(family = "Montserrat", size = 14, colour = "black"),  
  axis.text = element_text(family = "Montserrat", size = 12, colour = "black"), 
  axis.title = element_text(family = "Montserrat", size = 14, colour = "black"),  
  panel.spacing = unit(0.2, "lines"),  
  plot.margin = unit(c(1, 1, 1, 1), "lines"),  
  panel.background = element_rect(fill = "#FFF6ED"),  
  plot.title.position = "plot",
  panel.spacing.y = unit(0.2, "lines"),  
  panel.spacing.x = unit(0.2, "lines"),
  panel.grid.minor.x = element_line(color = "white", linetype = "dashed", size = 0.8),
  panel.grid.major.x = element_line(color = "white", linetype = "dashed", size = 0.8),
  axis.ticks.x  = element_line(color = "white", linetype = "dashed", size = 0.8),
  axis.ticks.y = element_line(color = '#FFF6ED'))










