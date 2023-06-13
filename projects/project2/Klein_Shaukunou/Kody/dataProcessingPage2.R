
pathToDfs <- 'D:\\project\\project\\data\\movie_dataset_public_final\\'
reviews <- readLines(file(paste0(pathToDfs,"raw\\reviews.json")), n = 100000)
reviews <- stream_in(textConnection(as.vector(reviews)))
temp <- full_df %>% pull(item_id) %>% unique
reviews <- reviews %>% filter(item_id %in% temp)

zmien_tytuly <- function(ramka_danych) {
  tytuly_z_the <- grepl("^The ", ramka_danych$Title)
  ramka_danych$Title[tytuly_z_the] <- sub("^The (.*)( \\(\\d+\\))$", "\\1, The\\2", ramka_danych$Title[tytuly_z_the])
  
  return(ramka_danych)
}

images <- read.csv(paste0(pathToDfs,"\\MovieGenre.csv"))
images <- zmien_tytuly(images)
