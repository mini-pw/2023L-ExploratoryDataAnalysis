from dotenv import load_dotenv
import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import csv
import time

load_dotenv()
spotify = spotipy.Spotify(client_credentials_manager=SpotifyClientCredentials())



# id od ktorego zaczynamy wyszukiwanie
initial_id = "0iBTVnJ1Sff92zCDujfvyJ" #Young Leosia

# liczba tworcow, ktorych kontakty zostana dodane (rozmiar grafu)
number_of_nodes = 150

# np w kolumnie genres jest wiele wyników, czym mają być separowane
# nie moze byc to przecinek "," ani ", " gdyż te sa uzywane do kodowania odzielenia kolejnych kolumn
separator = "; "




def concatenate(iterable, separator = "; "):
    out = ""
    for item in iterable:
        out += item + separator
    return out[:-len(separator)]

def load_artists(initial_id = "0iBTVnJ1Sff92zCDujfvyJ", number_of_nodes = 10, separator = "; "):
    queue = [initial_id]
    already_visited_ids = set()
    for i in range(number_of_nodes):
            for artist in spotify.artist_related_artists(queue[i])["artists"]:
                if not artist["id"] in already_visited_ids:
                    queue.append(artist["id"])
                    already_visited_ids.add(artist["id"])

    with open('artists.csv', 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(["id",
            "name",
            "followers",
            "genres",
            "popularity",
            "realted_artists_id"])
        for artist_id in queue:
            artist = spotify.artist(artist_id)
            realted_artists_id = [art["id"] for art in spotify.artist_related_artists(artist_id)["artists"]]

            # genres
            genres = concatenate(artist["genres"])

            #related artists
            realted_artists_id = concatenate(realted_artists_id)
            writer.writerow([
                artist["id"],
                artist["name"],
                artist["followers"]["total"],
                genres, 
                artist["popularity"],
                realted_artists_id])

def read_album_ids():
    visited_album_ids = set()
    filename = 'artists.csv'
    with open(filename, 'r') as csvfile:
        datareader = csv.reader(csvfile)
        for row in datareader:
            if row[0] == "id":
                continue
            albums = spotify.artist_albums(row[0])
            for album in albums["items"]:
                if not album["id"] in visited_album_ids:
                    visited_album_ids.add(album["id"])
                    yield album["id"]

def read_tracks_ids():
    visited_tracks_ids = set()
    filename = 'albums.csv'
    with open(filename, 'r') as csvfile:
        datareader = csv.reader(csvfile)
        for row in datareader:
            if row[0] == "id":
                continue
            track_ids = row[7].split("; ")
            for track_id in track_ids:
                if not track_id in visited_tracks_ids:
                    yield track_id
                    visited_tracks_ids.add(track_id)



def load_albums(album_ids):
    with open('albums.csv', 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['id', 
            'artists_ids',
            'label',
            'name',
            'popularity', 
            'release_date',
            'total_tracks',
            'tracks_ids',
            'available_markets'
            ])
        for album_id in album_ids:
            album = spotify.album(album_id)

            # artists
            artist_ids = concatenate(artist["id"] for artist in album["artists"])
        
            # available markets
            available_markets = concatenate(album["available_markets"])

            #track ids
            track_ids = concatenate(track["id"] for track in album["tracks"]["items"])
            try:
                writer.writerow([album_id, 
                    artist_ids, 
                    album["label"],
                    album["name"], 
                    album["popularity"], 
                    album["release_date"], 
                    album['total_tracks'], 
                    track_ids, 
                    available_markets
                    ])
            except:
                print(f"One album with id '{ album_id }' not taken due to an error.")


def load_tracks(track_ids):
    with open('tracks.csv', 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['id', 
            'album_id', 
            'artists_ids',  
            'duration_ms', 
            'name', 
            'popularity', 
            'danceability', 
            'energy', 
            'key', 
            'loudness', 
            'mode', 
            'speechiness', 
            'acousticness', 
            'instrumentalness', 
            'liveness', 
            'valence', 
            'tempo',
            'available_markets',
            ])
        print(f"Numbers of tracks to be loaded: {len(track_ids)}")
        it = 0
        for track_id in track_ids:
            if it % 10 == 0:
                print(f"Loaded {it} tracks")
            it += 1
            try:
                track = spotify.track(track_id)
                track_extra_info = spotify.audio_features(track_id)[0]
            

                # artists
                artist_ids = concatenate(artist["id"] for artist in track["artists"])
        
                # available markets
                available_markets = concatenate(track["available_markets"])

            
                writer.writerow([track_id, 
                    track["album"]["id"],
                    artist_ids,
                    track["duration_ms"],
                    track["name"],
                    track["popularity"],
                    track_extra_info["danceability"],
                    track_extra_info["energy"],
                    track_extra_info["key"],
                    track_extra_info["loudness"],
                    track_extra_info["mode"],
                    track_extra_info["speechiness"],
                    track_extra_info["acousticness"],
                    track_extra_info["instrumentalness"],
                    track_extra_info["liveness"],
                    track_extra_info["valence"],
                    track_extra_info["tempo"],
                    available_markets
                    ])
            except:
                print(f"One track with id '{ track_id }' not taken due to an error.")





if __name__ == "__main__":
    start_time = time.time()
    #load_artists(initial_id, number_of_nodes, separator)
    #load_albums(read_album_ids())
    load_tracks(list(read_tracks_ids())[19889:])
    print(f"time consumed: { time.time() - start_time }s")


