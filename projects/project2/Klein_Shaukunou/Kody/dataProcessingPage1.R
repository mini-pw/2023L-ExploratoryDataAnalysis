library('dplyr')
library('jsonlite')
library('stringr')
library('wordcloud')
library('wordcloud2')
library('plotly')
library('shiny')
library('shinydashboard')
library('shinyWidgets')
library('bslib')
library('htmlwidgets')
library('shinyjs')
library('slickR')
library('highcharter')
library('flexdashboard')

pathToDfs <- 'D:\\project\\project\\data\\movie_dataset_public_final\\'

tagdl <- read.csv(paste0(pathToDfs,'scores\\tagdl.csv'))
tagdl <- tagdl %>% 
  mutate(score_std = (score - min(score))/(max(score)-min(score)))

threshold <- 0.6
tagdlFiltered <- tagdl %>% filter(score_std >= threshold)

metadata <- stream_in(file(paste0(pathToDfs,"raw\\metadata.json")))
metadata <- metadata %>% select(-c('dateAdded', 'imdbId'))

full_df <- metadata %>% inner_join(tagdlFiltered)

whichQuantile <- 0.75
mostPopularTags <- full_df %>% count(tag)
q <- mostPopularTags %>% pull(n) %>% quantile(whichQuantile)
mostPopularTags <- mostPopularTags %>% filter(n >= q) %>% select(tag)

full_df <- full_df %>% inner_join(mostPopularTags)

full_df <- full_df %>% 
  mutate(year = as.numeric(str_extract(title, "(?<=\\()\\d{4}(?=\\))")))
rm(metadata)
rm(tagdl)
rm(tagdlFiltered)




getFilmsByCriterions <- function(full_df, 
                                 tags = unique(full_df$tag), 
                                 AreAllTagsIncluded = FALSE,
                                 yearMin = min(full_df$year), 
                                 yearMax = max(full_df$year),
                                 directors = unique(full_df$directedBy)) {
  library('dplyr')
  
  if (AreAllTagsIncluded) {
    titles <- full_df %>% group_by(title) %>% filter(all(tags%in%tag)) %>% pull(title)
  } else {
    titles <- full_df %>% filter(tag %in% tags) %>% pull(title) %>% unique 
  }
  df <- full_df %>% filter(title %in% titles & 
                             between(year,yearMin, yearMax) & 
                             directedBy %in% directors) %>% 
    arrange(-avgRating)
  df
}
