### Zadanie 1

Utwórz nową aplikację R Shiny.

R Studio > File > New File > Shiny Web App

### Zadanie 2

Zmień wykres (histogram) z bazowego na wykres z pakietu `ggplot2`.

### Zadanie 3

Zmieniamy źródło danych naszej aplikacji, będzie to ramka danych `serialeIMDB` z pakietu `PogromcyDanych`. Aplikacja powinna zawierać wykres skrzynkowy dla zmiennej ocena.

### Zadanie 4

Zastosuj w aplikacji `selectInput()`, aby umożliwić wybór serialu.

### Zadanie 5

Dla wybranego serialu z listy rozwijanej (`selectInput()`) dodajemy wykres punktowy, który przyjmuje na osi x odcinki, na osi y ocenę a kolorem oznaczamy sezon.
Dodaj do wykresu tytuł "Oceny dla serialu XXX", gdzie XXX to wybrany serial z listy rozwijanej.

### Zadanie 6

Do panelu aplikacji dodajemy `checkboxInput()`, który będzie odpowiedzialny za dodanie linii trendu. Chcemy mieć jedną linię trendu dla wszystkich sezownów wybranego serialu.

### Zadanie 7\*

Do aplikacji dodaj tabelę z podsumowaniem ocen dla każdego sezonu. Kolumny tej tabeli to kolejne sezony, w wierszach umieszczamy kolejno informacje takie jak:
- minimalna ocena
- Q1
- mediana
- średnia
- Q3
- maksymalna ocena 
 