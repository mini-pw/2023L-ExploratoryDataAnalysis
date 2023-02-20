# P1: Muzyka

Pierwszy projekt poświęcony jest eksploracji danych dotyczących muzyki. Jego celem jest przygotowanie plakatu w formacie A2 (+ .pdf), który przedstawi graficznie ciekawe informacje.

Wykresy mogą być wykonane w dowolnym narzędziu i złożone w plakat z użyciem dowolnej techniki. Podczas *Wykładu 8 (11-04-2023)* zespoły przedstawiają krótkie prezentacje swojej pracy.

Plakat powinien składać się ze zbioru przynajmniej trzech spójnych tematycznie wykresów oraz komentarzy/opisów do wykresów. Projekt wykonuje się w zespole 3 osobowym. Kody źródłowe wykresów i plakat w postaci elektronicznej należy umieścić na GitHubie.

## Zajęcia projektowe

Zajęcia projektowe to głównie wspólne dyskusje, praca w grupie, prezentacje kolejnych etapów, konsultacje.

<table style="undefined;table-layout: fixed; width: 526px">
<colgroup>
<col style="width: 59.116667px">
<col style="width: 82.116667px">
<col style="width: 331.116667px">
<col style="width: 54.116667px">
</colgroup>
<thead>
  <tr>
    <th>Tydzień</th>
    <th>Data</th>
    <th>Zakres</th>
    <th>Punkty</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>1</td>
    <td>21-02-2023</td>
    <td>Wprowadzenie do projektu, podział na zespoły.</td>
    <td></td>
  </tr>
  <tr>
    <td>2</td>
    <td>28-02-2023</td>
    <td>Praca w zespołach, burza mózgów, określenie tematyki plakatu.</td>
    <td>1</td>
  </tr>
  <tr>
    <td>3</td>
    <td>21-03-2023</td>
    <td>Na zajęcia należy przygotować pomysły oraz pierwsze wizualizacje bazujące na znalezionych danych.</td>
    <td>2</td>
  </tr>
  <tr>
    <td>4</td>
    <td>04-04-2023</td>
    <td>Na zajęciach zespoły prezentują swoje zaawansowane wizualizacje oraz  prezentują prototyp plakatu. </td>
    <td>2</td>
  </tr>
 
</tbody>
</table>

## Ocena

Za projekt można otrzymać od 0 do 24 punktów, z czego:

-   5p (1 x 1p, 2 x 2p) uzyskuje się za przedstawienie postępu prac w danym tygodniu
-   5p uzyskuje się za przygotowanie estetycznych wykresów (dwa lub więcej)
-   5p uzyskuje się, jeżeli przygotowane wykresy mają wszystkie niezbędne elementy do poprawnego odczytania danych (tytuł, podtytuł, adnotacje na osiach, legenda, jednostki, opis jak czytać wykres)
-   5p uzyskuje się za estetykę i pomysłowość aranżacji wykresów i opisów w jedną całość
-   4p uzyskuje się za prezentację projektu

Podczas prezentacji z każdego zespołu musi być obecna co najmniej jedna osoba. Nieobecność całego zespołu podczas sesji plakatowej skutkuje brakiem punktów za prezentację (-4 pkt) oraz możliwością zdobycia maksymalnie 80% za plakat (12 pkt).

## Przykłady danych

- wyniki ankiety dotyczącej muzyki i zdrowia psychicznego:
[https://www.kaggle.com/datasets/catherinerasgaitis/mxmh-survey-results](https://www.kaggle.com/datasets/catherinerasgaitis/mxmh-survey-results)
- zbiór utworów na Spotify (porównanie gatunków muzycznych na podstawie danych zbieranych przez serwis Spotify
[https://www.kaggle.com/datasets/vicsuperman/prediction-of-music-genre](https://www.kaggle.com/datasets/vicsuperman/prediction-of-music-genre)
- podejście do klasyfikacji gatunku na podstawie metryk dotyczących dźwięku
[https://www.kaggle.com/datasets/insiyeah/musicfeatures](https://www.kaggle.com/datasets/insiyeah/musicfeatures)
- dane z magazynu billboard od 1958 roku (najpopularniejsze piosenki): [](https://www.kaggle.com/datasets/dhruvildave/billboard-the-hot-100-songs)[https://www.kaggle.com/datasets/dhruvildave/billboard-the-hot-100-songs](https://www.kaggle.com/datasets/dhruvildave/billboard-the-hot-100-songs)
- top piosenek ze Spotify 2010-2019
[https://www.kaggle.com/datasets/leonardopena/top-spotify-songs-from-20102019-by-year](https://www.kaggle.com/datasets/leonardopena/top-spotify-songs-from-20102019-by-year) (trochę mało obserwacji ~600)
- gigantyczny zbiór danych ze Spotify (1GB!) który zawiera dane o top200 i viral50 utworach z podziałem na kraj, liczbę odtworzeń, miejsce w analizowanym rankingu itd.
https://www.kaggle.com/datasets/leonardopena/top-spotify-songs-from-20102019-by-year](https://www.kaggle.com/datasets/leonardopena/top-spotify-songs-from-20102019-by-year)
- inspiracja - plakat dotyczący konkretnego gatunku muzycznego, np. metalu
[https://www.kaggle.com/datasets/mrpantherson/metal-by-nation](https://www.kaggle.com/datasets/mrpantherson/metal-by-nation)
- dane nt. 50 piosenek wykonanych przez profesjonalną koreańską piosenkarkę w języku angielskim i koreańskim. [https://zenodo.org/record/4785016#.YYkpOtZBxqv](https://zenodo.org/record/4785016#.YYkpOtZBxqv)
- 40 Open-Source Audio Datasets for ML
[https://towardsdatascience.com/40-open-source-audio-datasets-for-ml-59dc39d48f06](https://towardsdatascience.com/40-open-source-audio-datasets-for-ml-59dc39d48f06)
- million song dataset
[http://millionsongdataset.com](http://millionsongdataset.com)
- FMA: A Dataset For Music Analysis
[https://github.com/mdeff/fma](https://github.com/mdeff/fma)



**Uwagi**

-   Duża część danych na Kaggle'u to bardzo proste i małe zbiorki, z których być trudno będzie wyciągnąć coś ciekawego. Na pewno warto poświęcić trochę czasu na eksplorację większych zbiorów (lub też poszukać innych) i wybrać z nich pewien ciekawy do zwizualizowania podzbiór.
-   Niektóre zbiory danych nie są w plikach `.csv`. W przypadku problemów z odczytaniem danych warto zwrócić uwagę, czy na Kaggle'u lub w dokumentacji odpowiednich pakietów nie ma notatników/winietek, które mogą posłużyć za pomoc.

## Oddanie projektu

Czas na wykonanie projektu jest do **10-04-2023** - do tego dnia (włącznie) będą przyjmowane Pull Requests na GitHub.

W PR o nazwie `[projekt1] Nazwisko1_Nazwisko2_Nazwisko3` należy zamieścić folder o nazwie `nazwisko1_nazwisko2_nazwisko3` zawierający:

-   plakat w formacie .pdf o nazwie `nazwisko1_nazwisko2_nazwisko3`,
-   wszystkie kody służące do odtworzenia wykresów (na ile to możliwe) w podfolderze `kody`.

PR robi jedna osoba z zespołu. Folder należy umieścić w [../projects/project1](https://github.com/MI2-Education/2023L-ExploratoryDataAnalysis/tree/main/projects/project1).

Należy wydrukować plakat i przynieść go na Wykład **8**. 

Uwagi:

-  na plakacie powinien znaleźć się podpis identyfikujący autorów oraz źródło/a danych

## Materiały

Przykłady narzędzi do tworzenia plakatów:

-   PowerPoint
-   [](https://www.canva.com/)[https://www.canva.com/](https://www.canva.com/) (Pro with [](https://education.github.com/pack)[https://education.github.com/pack](https://education.github.com/pack))
-   Inkscape

Plakaty z poprzednich lat:
-  [Posters about sports enter the game!](https://medium.com/responsibleml/posters-about-sports-enter-the-game-4cd77e659afe)
-   [Posters that change the perspective on climate and the environment](https://medium.com/responsibleml/posters-that-change-the-perspective-on-climate-and-the-environment-c3682c0f6c39)
-   [poster::make([movie | book | series])](https://medium.com/responsibleml/poster-make-movie-book-series-3ac2c8a01180)
-   [COVID-19 Data Visualization](https://medium.com/responsibleml/covid-19-data-visualization-bc0732c19d46)
- [więcej...](https://github.com/MI2-Education/posters)
