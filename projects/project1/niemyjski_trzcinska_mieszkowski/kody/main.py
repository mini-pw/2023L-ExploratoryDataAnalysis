from dotenv import load_dotenv
import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import json
load_dotenv()

spotify = spotipy.Spotify(client_credentials_manager=SpotifyClientCredentials())
print("ready to load")
album = spotify.album("4WQA0C2nwJvBMTdsjf5JOm")
"""
artist = spotify.artist("4nPxrGG7k7aEKmNLsfX4cd")
albums = spotify.artist_albums("4nPxrGG7k7aEKmNLsfX4cd")
tracks = spotify.album_tracks("4WQA0C2nwJvBMTdsjf5JOm")
track = spotify.track("0t7CPCaPyna0OKeQlbxJms")
features = spotify.audio_features("0t7CPCaPyna0OKeQlbxJms")
related_artists = spotify.artist_related_artists("4nPxrGG7k7aEKmNLsfX4cd")
analysis = spotify.audio_analysis("2ASuuiVsmKssdkeeNkgCKv")
"""
print(album.keys())

with open("sample.json", "w") as outfile:
    json.dump(album, outfile)
"""
for segment in analysis["segments"]:
    assert max(segment["pitches"]) == 1
        
"""
#[0.755, 1.0, 0.616, 0.517, 0.383, 0.435, 0.496, 0.528, 0.71, 0.608, 0.496, 0.432]