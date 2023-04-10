# -*- coding: utf-8 -*-
"""
Created on Sat Mar 18 12:37:39 2023

@author: admin
"""


import pandas as pd
import numpy as np
import string
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.colors import ListedColormap
plt.style.use('seaborn')

#To plot the graphsfrom wordcloud import WordCloud
from wordcloud import WordCloud
import lyricsgenius 
from requests.exceptions import Timeout

import nltk
import spacy
import nltk.data
from nltk.stem import WordNetLemmatizer
from nltk.corpus import stopwords

from PIL import Image

from matplotlib import font_manager
font_dir = ['C:/Users/Dell/Downloads/DIN-BlackAlternate']
for font in font_manager.findSystemFonts(font_dir):
    font_manager.fontManager.addfont(font)

nltk.download('stopwords')
nltk.download('vader_lexicon')
nltk.download('punkt')
nltk.download('wordnet')
nlp = spacy.load("en_core_web_sm")

####################################################################################

access_token = "QrJvVIxGDJdz93NoZ9RpB4Pj1uY-ER0zrCDz1XBf9gExnLqwidrV6Kze4dQrHkWH"

pop_artists = ["Madonna", "Rihanna", "Taylor Swift", "Celine Dion", "Ed Sheeran",
       "Justin Bieber", "Katy Perry", "Lady Gaga", "Adele", "Britney Spears"]
rock_artists = ["Queen", "Led Zeppelin", "Pink Floyd", "Eagles", "The Rolling Stones",
        "Bruce Springsteen", "U2", "Phil Collins", "Paul McCartney", "The Beatles"]
hiphop_artists = ["Eminem", "Drake", "Kanye West", "Jay-Z", "Lil Wayne",
          "Nicki Minaj", "Flo Rida", "Tupac Shakur", "Snoop Dogg", "DMX"]
rb_artists = ["Mariah Carey", "Beyonce", "Whitney Houston", "Alicia Keys", "Janet Jackson",
      "Christina Aguilera", "Usher", "The Weeknd", "R.Kelly", "Bruno Mars"]
country_artists = ["Garth Brooks", "George Strait", "Kenny Rogers", "Johnny Cash", "Tim McGraw",
           "Shania Twain", "Alabama", "Alan Jackson", "John Denver", "Reba McEntire"]

genres = ["Pop", "Rock", "Hip-hop/Rap", "R&B", "Country"]

dic = {"Pop": pop_artists, 
       "Rock": rock_artists, 
       "Hip-hop/Rap": hiphop_artists, 
       "R&B": rb_artists, 
       "Country": country_artists}

alphabet = list(string.ascii_lowercase)

####################################################################################

def search_data(id, token):
    
    genius = lyricsgenius.Genius(token, timeout = 15)
    genre = dic[genres[id]]
    
    list_lyrics = []
    list_artist = []
    list_title = []
    list_genre = [genres[id]] * 200
    
    for name in genre:
        retries = 0
        while retries < 5:           
            try:
                artist = genius.search_artist(name, max_songs = 20, sort = 'popularity')
                songs = artist.songs
            except Timeout as e:
                retries += 1
                continue           
            for song in songs:
                list_lyrics.append(song.lyrics)
                list_artist.append(song.artist)
                list_title.append(song.title)
            break
        
    df = pd.DataFrame({'genre' :list_genre,
                       'artist': list_artist, 
                       'title': list_title,
                       'lyric': list_lyrics})
    return df

def clean_lyrics(df, column):
  
    df = df
    df[column] = df[column].str.lower()
    df[column] = df[column].str.replace(r"verse |[1|2|3]|chorus|bridge|outro|reprise|embed|lyrics", "").str.replace("[", "").str.replace("]", "").str.replace("'","")
    df[column] = df[column].str.lower().str.replace(r"instrumental|intro|guitar|solo", "")
    df[column] = df[column].str.replace("\n", " ").str.replace(r"[^\w\d'\s]+", "").str.replace("efil ym fo flah", "")
    df[column] = df[column].str.strip()

    return df

def lyrics_to_words(document):

    stop_words = set(stopwords.words('english'))
    exclude = set(string.punctuation)
    lemma = WordNetLemmatizer()
    stopwordremoval = " ".join([i for i in document.lower().split() if i not in stop_words])
    punctuationremoval = ''.join(ch for ch in stopwordremoval if ch not in exclude)
    normalized = " ".join(lemma.lemmatize(word) for word in punctuationremoval.split())
    
    return normalized

def unique(list1):
    unique_list =[]
    for elem in list1:
        if elem not in unique_list:
            unique_list.append(elem)
    return unique_list

def plot_wordcloud(id, df):
    theme = list_theme[id]
    mask = list_mask[id]
    genre = genres[id]
    
    wc = WordCloud(background_color = None, colormap = theme, max_font_size = 170,
                   mask = mask, min_word_length = 3, scale = 5, max_words = 500,
                   mode = "RGBA", font_path = 'C:/Users/Dell/Downloads/DIN-BlackAlternate.ttf',
                   relative_scaling=.2, min_font_size = 6, prefer_horizontal = .93)
    plt.figure(figsize=(30,20))
    
    dic = dict(zip(df["Word"].tolist(), df[genre].tolist()))
    wc.generate_from_frequencies(dic)
    plt.imshow(wc,interpolation="bilinear")
    plt.axis("off")
    plt.savefig('wordcloud_%s' % str(id) + '.png', bbox_inches = 'tight')
    
    plt.show()
    
################################################################################
        
df0 = search_data(0, access_token)
df1 = search_data(1, access_token)
df2 = search_data(2, access_token)
df3 = search_data(3, access_token)
df4 = search_data(4, access_token)

frames = [df0, df1, df2, df3, df4]
df = pd.concat(frames)

df = clean_lyrics(df, 'lyric')
df = df[df['lyric'].notnull()]

my_stop_words = ["im", "youve", "youre", "oh", "dont","doesnt","didnt", "cause", "hey", "might",
                 "also", "like","couldnt","know", "isnt", "havent", "hadnt", "will", "would",
                 "cant", "te", "pre", "thats", "em", "gotta", "uh", "yall", "gon", "go", "got",
                 "bout", "as", "wanna","gonna", "ah","ho", "one", "outta", "yeah", "ooh", "aint",
                 "let","ive", "ill", "u", "shes", "ya", "yo", "huh", "id", "likepre", "youll", 
                 "tryna", "ima", "wont", "til", "whats", "lil", "youd", "theyre","interlude",
                 "wasnt", "theyll", "werent", "hasnt", "shell", "them", "ayo", "ayy", "nah"]

nlp_stop_words = nlp.Defaults.stop_words 
        
words = []

df = df.reset_index(drop = True)
for lyric in df['lyric'].tolist():
    ws = unique(lyrics_to_words(lyric).split())
    del ws[0]
    ws = [el for el in ws if el not in my_stop_words and
          el not in nlp_stop_words and
          el.isalpha() and
          len(el) > 2 and len(el) < 15]
    words.append(ws)
    
df['words'] = words

df.to_csv('data1.csv', index=False)
df = pd.read_csv('C:/Users/Dell/OneDrive/Dokumenty/Phyton/WdED/Projekt1/data.csv')

df_artists = df.copy
df_artists['number_of_words'] = df_artists["words"].str.len()

df_genres = df_artists.groupby("genre")["number_of_words"].mean().reset_index()
df_genres.columns = ["genre","avg_number_of_words"]
df_genres = df_genres.sort_values(by="avg_number_of_words", ascending=False).reset_index(drop=True)
df_genres.to_csv('genres.csv', index=False)

df_artists = df_artists.groupby(["artist", "genre"])["number_of_words"].mean().reset_index()
df_artists.columns = ["artist", "genre", "avg_number_of_words"]
df_artists = df_artists.sort_values(by="avg_number_of_words", ascending=False).reset_index(drop=True)
df_artists.to_csv('artists.csv', index=False)

set_words = []
set_genres = []

for i in df.index:
   for word in df['words'].iloc[i]:
       set_words.append(word)
       set_genres.append(df['genre'].iloc[i])
       
words_df = pd.DataFrame({'words':set_words,'genre':set_genres})

words_df = words_df.groupby(["words", "genre"]).size().reset_index(name = 'counts')
words_df = words_df.iloc[186:]

words_df["pop"] = np.where(words_df.genre == "Pop", words_df.counts, 0)
words_df["rock"] = np.where(words_df.genre == "Rock", words_df.counts, 0)
words_df["hip-hop/rap"] = np.where(words_df.genre == "Hip-hop/Rap", words_df.counts, 0)
words_df["r&b"] = np.where(words_df.genre == "R&B", words_df.counts, 0)
words_df["country"] = np.where(words_df.genre == "Country", words_df.counts, 0)

words_df = words_df.groupby(["words"]).agg(pop=("pop", max),
                                           rock=("rock", max),
                                           hiphop=("hip-hop/rap", max),
                                           rb=("r&b", max),
                                           country=("country", max)).reset_index()

words_df["Total"] = words_df.loc[:,["pop",
                                    "rock",
                                    "hiphop",
                                    "rb",
                                    "country"]].sum(axis=1)

words_df.columns = ["Word", "Pop", "Rock", "Hip-hop/Rap", "R&B", "Country", "Total"]

words_df = words_df.replace({"Word" : {"nigga" : "ni**a", "fuck" : "f**k", "fuckin" : "f**kin",
                              "fucked" : "f**ked", "motherfucker" : "motherf**ker",
                              "shit" : "sh*t", "dick" : "d*ck", "motherfuckin" : "motherf**kin",
                              "bitch" : "b*tch"}})

words_df.to_csv('words.csv', index = False)
words_df = pd.read_csv('C:/Users/Dell/OneDrive/Dokumenty/Phyton/WdED/Projekt1/words.csv')

letters_df = words_df.copy()
letters_df["first_letter"] = letters_df["Word"].str[0]
letters_df = letters_df.loc[letters_df["first_letter"].isin(alphabet)]
letters_df = letters_df.sort_values(by = "Total", 
                                   ascending = False).reset_index(drop = True)

letters_df = letters_df.groupby(["first_letter"], as_index = False).agg({"Word" : "first",
                                                                         "Total" : "max"})
                                                                         
letters_df.to_csv('C:/Users/Dell/OneDrive/Dokumenty/Phyton/WdED/Projekt1/letters.csv', index = False)

###############################################################################

flatui = ["#9a0200", "#840000","#4a0100", "#4b0101", "#410200", "#1d0200", "#000000"]
my_cmap = ListedColormap(sns.color_palette(flatui).as_hex())

mask0 = np.array(Image.open("C:/Users/Dell/OneDrive/Dokumenty/Phyton/WdED/Projekt1/masks/sluchawki.png"))
mask1 = np.array(Image.open("C:/Users/Dell/OneDrive/Dokumenty/Phyton/WdED/Projekt1/masks/gitara.png"))
mask2 = np.array(Image.open("C:/Users/Dell/OneDrive/Dokumenty/Phyton/WdED/Projekt1/masks/czapka.png"))
mask3 = np.array(Image.open("C:/Users/Dell/OneDrive/Dokumenty/Phyton/WdED/Projekt1/masks/micro3.png"))
mask4 = np.array(Image.open("C:/Users/Dell/OneDrive/Dokumenty/Phyton/WdED/Projekt1/masks/kapelusz.png"))

list_theme = ["spring", my_cmap, "cividis", "winter", "copper"]
list_mask = [mask0, mask1, mask2, mask3, mask4]

plot_wordcloud(0, words_df)
plot_wordcloud(1, words_df)
plot_wordcloud(2, words_df)
plot_wordcloud(3, words_df)
plot_wordcloud(4, words_df)







    
