library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(RColorBrewer)

data_poster <- read_xlsx("poster_graph.xlsx")

class(data_poster$Music) <- "character"

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

data_poster %>% 
  filter(Gender == "female") %>% 
  drop_na() %>% 
  ggplot(aes(x = Music, y = Shopping, fill = Type)) +
  geom_split_violin() +
  scale_fill_manual(values = c("#ff7e23", "#a6207a"), labels = c("", "", "", "", "", ""),) +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme(plot.background = element_rect(fill = "#EFEDDF"), 
        legend.background = element_rect(fill = "#EFEDDF"),
        panel.background = element_rect(fill = "#EFEDDF"),
        axis.title = element_text(colour = "#551E00"),
        axis.text = element_text(colour="#551E00"),
        title = element_text(colour="#551E00"),
        legend.text = element_text(colour="#551E00"),
        legend.title = element_text(colour="#551E00"),
        axis.ticks = element_blank(),
        panel.grid = element_line(colour = "#551E00"),
        panel.grid.major.x = element_blank())
  




   




