
kolumny <- c("valence","danceability               ",
             "energy","tempo","               speechiness")

i=5 #to jest numer artysty od 1 do 5
artist <- top_artists[i,]
artist_name = artist[1]
print(artist_name)
artist <- rbind(maxima,minima,artist[,-1])
#png(filename =paste(artist_name,"-chart.png"))
create_radarchart(artist,title = NULL, color = '#f5a442',vlabels = kolumny,
                  vlcex = 4)

