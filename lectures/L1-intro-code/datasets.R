#########################################
###    WSTĘP DO EKSPLORACJI DANYCH    ###
###              WYKŁAD 1             ###
#########################################


# Anscombe's Quartet ------------------------------------------------------

data("anscombe")
head(anscombe)

library(ggplot2)
library(patchwork)

plots <- list()

for (i in 1:4){
  p <- ggplot(anscombe, aes_string(x = paste0("x", i),
                                   y = paste0("y", i))) +
    geom_point(size = 4) + 
    geom_abline(intercept = 3, slope = 0.5) + 
    theme_minimal() + 
    xlim(c(3, 19)) + 
    ylim(c(3, 13))
  plots[[i]] <-  p
}

(plots[[1]] + plots[[2]])/(plots[[3]] + plots[[4]])


# The Datasaurus Dozen ----------------------------------------------------

df <- read.csv("../The Datasaurus Dozen/DatasaurusDozen-wide.tsv", 
               sep = "\t",
               skip = 1)

names(df)[1:2] <- c("x.0", "y.0")

i <- 0
p <- ggplot(df, aes_string(x = paste0("x.", i),
                           y = paste0("y.", i))) +
  geom_point(size = 4) +
  theme_minimal()
p
i <- i + 1



