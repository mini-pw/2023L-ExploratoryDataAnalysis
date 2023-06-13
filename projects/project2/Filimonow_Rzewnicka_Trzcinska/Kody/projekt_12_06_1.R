library(plotly)
library(shiny)
library(shinyWidgets)
library(stringr)
library(dplyr)
library(shinydashboard)


ui <- dashboardPage(skin = "black", 

  
  dashboardHeader(title = "The Walt Disney Company", titleWidth = 450),
  dashboardSidebar(  sidebarMenu(
    menuItem("Disney na przestrzeni lat", tabName = "dashboard", icon = icon("crown")),
    menuItem("Charakerystyka filmów Disneya", icon = icon("video"), tabName = "widgets")
  )
  ),
  dashboardBody(
    tags$style(HTML("

.box.box-solid.box-danger>.box-header {
  color: #FFFFFF;
  background:#EFBEB7
                    }

.box.box-solid.box-danger{
border-bottom-color:#EFBEB7;
border-left-color:#EFBEB7;
border-right-color:#EFBEB7;
border-top-color:#EFBEB7;
}

.box.box-danger>.box-header {
  color:#000000;
  background:#EFBEB7
                    }

.box.box-danger{
border-bottom-color:#EFBEB7;
border-left-color:#EFBEB7;
border-right-color:#EFBEB7;
border-top-color:#EFBEB7;
}
                              ")),
    tags$style(HTML("

.box.box-solid.box-primary>.box-header {
  color: #FFFFFF;
  background:#F3CC64
                    }

.box.box-solid.box-primary{
border-bottom-color:#F3CC64;
border-left-color:#F3CC64;
border-right-color:#F3CC64;
border-top-color:#F3CC64;
}

.box.box-primary>.box-header {
  color:#000000;
  background:#F3CC64
                    }

.box.box-primary{
border-bottom-color:#F3CC64;
border-left-color:#F3CC64;
border-right-color:#F3CC64;
border-top-color:#F3CC64;
}
                              ")),
    
tabItems(
  
      tabItem(tabName = "dashboard",
              h2("Disney na przestrzeni lat", style = 'font-size:42px;'),
              fluidRow(
                box(status = "danger", solidHeader = FALSE, checkboxInput("animation", label = "Filmy animowane", value = TRUE), checkboxInput("films", label = "Pozostałe filmy", value = TRUE)),

                box(status = "danger", solidHeader = FALSE,
                  setSliderColor("#B12228", 1),
                  sliderInput("years", "Rok",
                              min = 1937, max = 2020,
                              value = c(2000,2020),
                              step = 1, sep = "")
                )),
              fluidRow(
                box(status = "danger", solidHeader = FALSE, plotlyOutput("plot1")), 
                box(title = h3("Jak zmieniała się długość filmów na przestrzeni lat?", style = 'font-size:27px;'), height = 425, width = 6,style="text-align: justify;font-size:16px;",status = "danger", solidHeader = FALSE,
                                                 "Czas trwania filmów zmieniał się przez ostatnie kilkadziesiąt lat. W latach 60. produkcje Disneya stały się nieco dłuższe, potem się skróciły, by w XXI wieku znów się wydłużyć. 
                                                  Zdecydowanie więcej jest filmów bardzo długich. Kiedyś takie trwające ponad 140 minut były rzadkością, a teraz nie są niczym wyjątkowym.
                                                  Również czas trwania filmów animowanych wyraźnie wzrósł w ciągu kilku dekad.")),
              fluidRow(box(status = "danger", solidHeader = FALSE, title = h3("Jak oceniane są filmy z poszczególnych lat?", style = 'font-size:27px;'), height = 425,
                           width = 6,style="text-align: justify;font-size:16px;",
                           "IMDb (Internet Movie Database) jest największą na świecie internetową bazą danych na temat filmów i ludzi z nimi związanych. Zarejestrowani użytkownicy serwisu mogą ocenić każdy film w skali od 1–10. Jak w tym rankingu wypadają produkcje Disneya?
                           Wykres pokazuje, że mediana ocen filmów wyprodukowanych w danym roku prawie zawsze mieści się między 6 a 7. Możemy też zauważyć, że bardzo mało filmów ma niskie oceny. 
                           Jak dotąd, najwyższy wynik to 8,6. Możemy więc nadal czekać na film, który zasłuży na 10!"), 
                       box(status = "danger", solidHeader = FALSE, plotlyOutput("plot2"))
              ),
              fluidRow(
                box(status = "danger", solidHeader = FALSE, plotlyOutput("plot4")),
                box(status = "danger", solidHeader = FALSE, plotlyOutput("linePlot"))
              ),

                
                fluidRow(
                  box(status = "danger", solidHeader = FALSE, title = h3("Przychody ze sprzedaży biletów", style = 'font-size:30px;'), height = 425, width = 6,style="text-align: justify;font-size:16px;",
                      "Wykres przedstawia średnią wartość przychodu ze sprzedaży biletów na filmy wyprodukowane w poszczególnych latach. Można zauważyć, że wraz z upływem lat znacznie on wzrasta, osiągając naprawdę ogromne wartości.
                      Jednak filmy z każdego roku osiągały sukcesy, ponieważ nawet najmniejsze liczby na wykresie są rzędu milionów dolarów. Widać również znaczny spadek przychodów w 2020 roku, co może być spowodowane pandemią. Mamy więc podstawy podejrzewać, że filmy obecnie produkowane będą generować jeszcze większe zyski."
             ),
             box(status = "danger", solidHeader = FALSE, title = h3("Przychód z produkcji filmowych ogółem", style = 'font-size:30px;'), height = 425, width = 6,style="text-align: justify;font-size:16px;", " Wykres przedstawia dochody brutto wytwórni z wyprodukowanych filmów. Jak można zauważyć, w latach osiemdziesiątych i dziewiędziesiątych nastąpił duży wzrost w dochodach, 
                 to właśnie wtedy wydane zostały najbardziej kultowe animacje, a sam okres ten bywa określany Renesansem Disneya. 
                 Obecnie dochody te są nieco wyższe, może być to związane z dużymi produkcjami filmowymi takimi jak np. filmy Filmowego Universum Marvela bądź Gwiezdnych Wojen."
             )
             
                )
      ),
      
      tabItem(tabName = "widgets",
              h2("Charakterystyka filmów Disneya", style = 'font-size:42px;'),
              fluidRow(
                box(status = "primary", solidHeader = FALSE, title = h3("Od Królewny Śnieżki do Gwiezdnych Wojen", style = 'font-size:30px;'), checkboxInput("inflation", "Uwzględniona inflacja", value = TRUE), height = 425, tags$head(tags$style(HTML('* {font-family: "Arial"};'))),style="text-align: justify;font-size:16px;",
                    "Nie da się zaprzeczyć, że Królewna Śnieżka w ogromnej mierze przyczyniła się do tego jaką obecnie wytwórnią jest Walt Disney. 
                    Osiągnięte wyniki sprzedażowe były nieporównywalne do innych filmów z tamtych czasów.
                    Uzyskane fundusze pozwoliły na rozbudowę firmy, a w dalekiej przyszłości na wykupienie praw do franczyzy Gwiezdnych Wojen. 
                    W 2015 swoją premierę miał VII film kultowej serii przynosząc tym samym wytwórni ogromny dochód. Ten sprzedażowy sukces zdaje się przewyższać dochody wszystkich innych produkcji Disneya, jednakże gdyby uwzględnić do tego inflację okazuje się, że Królewna Śnieżka była tym prawdziwym i największym sukcesem w dziejach The Walt Disney Company."),
                    
                    
                    box(status = "primary", solidHeader = FALSE, plotlyOutput("barPlot"))
                ),
              fluidRow(
                box(status = "primary", solidHeader = FALSE, plotlyOutput("plot3")),
                box(status = "primary", solidHeader = FALSE, title = h3("Które gatunki filmów są najczęściej produkowane?", style = 'font-size:30px;'), height = 425, "Przez niecałe 90 lat od premiery swojej pierwszej animacji Disney wyprodukował bardzo wiele kultowych filmów. Dzisiaj można przyporządkować je do różnych gatunków. Jednak które z nich są najbardziej popularne? Na wykresie możemy zauważyć, że wyraźnie
                    przodują trzy gatunki filmowe: komedia, film przygodowy i dramat. Liczba przedstawicieli pozostałych gatunków jest już zdecydowanie mniejsza. Jednak zbiór filmów Disneya jest tak bogaty, że każdy może w nim odnaleźć coś, co lubi najbardziej.", style="text-align: justify;font-size:16px;"
                    )
                
              ),
              fluidRow(
                box(status = "primary", solidHeader = FALSE, title = "Źródła", height = 100, width = 12, "https://www.kaggle.com/datasets/prateekmaj21/disney-movies;", "https://www.kaggle.com/datasets/maricinnamon/walt-disney-character-dataset;", "https://www.kaggle.com/datasets/maricinnamon/walt-disney-character-dataset")),
              fluidRow(
                box(status = "primary", solidHeader = FALSE, title = "Autorki", height = 100, width = 12, "Oliwia Trzcińska, Dorota Rzewnicka, Dominika Filimonow")
              )
              
              
              
      )
      
    )
    
    
  ), 
  tags$head(tags$style(HTML('* {font-family: "Arial"};'))))

    
    
      

    
    
    server <- function(input, output) {
      
      output$plot2 <- renderPlotly({
        
        
        data <- read.csv("walt_disney_movies.csv")
        
        if (input$animation == TRUE & input$films == TRUE) {
          data <- data
          
        } else if (input$animation == TRUE) {
          
          wybor <- read.csv("disney-characters.csv")[,2]
          wybor <- str_replace_all(wybor, "[\r\n]" , "")
          data <- data %>% filter(title %in% wybor)
          
        } else if (input$films == TRUE) {
          
          wybor <- read.csv("disney-characters.csv")[,2]
          wybor <- str_replace_all(wybor, "[\r\n]" , "")
          data <- data %>% filter(! title %in% wybor)
          
        } else if (input$animation == FALSE & input$films == FALSE) {
          
          data["Release.date"] <- NA
          
        }
        #dodajemy kolumnę "rok" z samym rokiem
        
        date <- str_split(data$Release.date, pattern = "-")
        dane <- character(length(date))
        
        for(i in 1:length(date)){
          dane[i] <- paste0(date[[i]][1], "-", date[[i]][2])
        }
        dane
        
        lata <- str_split(dane, pattern = "-")
        danelata <- character(length(lata))
        for(i in 1:length(lata)){
          danelata[i] <- lata[[i]][1]
        }
        danelata
        
        data$rok <- danelata
        data <- data %>% 
          filter(rok != "")
        
        df <- data %>% 
          select("rok", "imdb") %>% 
          filter(rok != "") %>% 
          filter(imdb != "") %>% 
          filter(imdb != "N/A") %>%
          filter(rok >= input$years[1] & rok <= input$years[2])
        plot_ly(data = df, x = ~rok, y = ~imdb, type = "box", fillcolor = "#EFBEB7", color = I("#B12228")) %>% 
          layout (
            yaxis = list(title = "Ocena filmu w serwisie IMDb"),
            xaxis = list(title = "Rok"), title = "Oceny filmów w kolejnych latach"
          )
        
      })
      
      
      output$plot3 <- renderPlotly({
        disney <- read.csv("disney_movies.csv")
        
        
        #dodajemy kolumnę z samym rokiem
        disney$year <- as.POSIXct(disney$release_date)
        disney$year <- format(disney$year, format = "%Y")
        
        df <- disney %>% 
          group_by(genre) %>% 
          summarise(ile = n()) %>% 
          arrange(-ile)
        
        #rysujemy liczbę filmów z poszczególnych gatunków
        plot_ly(df, y = ~genre, x = ~ile, type = 'bar',
                orientation = 'h', marker = list(color = "#393E8F")) %>%
          layout(title = 'Liczba filmów poszczególnych gatunków', plot_bgcolor = "#FFFFFF",
                 xaxis = list(title = ''), 
                 yaxis = list(title = '')) %>%
          layout(yaxis = list(categoryorder = "total ascending"))
        
      })
      
      
      
      
      
      output$plot4 <- renderPlotly({
        data <- read.csv("walt_disney_movies.csv")
        
        if (input$animation == TRUE & input$films == TRUE) {
          data <- data
          
        } else if (input$animation == TRUE) {
          
          wybor <- read.csv("disney-characters.csv")[,2]
          wybor <- str_replace_all(wybor, "[\r\n]" , "")
          data <- data %>% filter(title %in% wybor)
          
        } else if (input$films == TRUE) {
          
          wybor <- read.csv("disney-characters.csv")[,2]
          wybor <- str_replace_all(wybor, "[\r\n]" , "")
          data <- data %>% filter(! title %in% wybor)
          
        }
        
        else if (input$animation == FALSE & input$films == FALSE) {
          
          data["Release.date"] <- NA
          
        }
        
        #dodajemy kolumnę "rok" z samym rokiem
        
        date <- str_split(data$Release.date, pattern = "-")
        dane <- character(length(date))
        
        for(i in 1:length(date)){
          dane[i] <- paste0(date[[i]][1], "-", date[[i]][2])
        }
        dane
        
        lata <- str_split(dane, pattern = "-")
        
        danelata <- character(length(lata))
        for(i in 1:length(lata)){
          danelata[i] <- lata[[i]][1]
        }
        danelata
        
        data$rok <- danelata
        data <- data %>% 
          filter(rok != "")
        
        
        
        df <- data %>% 
          select("rok", "Box.office") %>% 
          filter(rok != "") %>% 
          filter(Box.office != "") %>% 
          filter(rok >= input$years[1] & rok <= input$years[2]) %>% 
          group_by(rok) %>% 
          summarise(srednia_box_office = mean(Box.office))
        plot_ly(data = df, x = ~rok, y = ~srednia_box_office, type = "scatter", 
                mode = "lines+markers", color = I("#B12228"))%>% 
          layout (
            yaxis = list(title = "Średni przychód ($)"),
            xaxis = list(title = "Rok"), 
            title = "Przychód ze sprzedaży biletów"
          )
        
      })
      
      output$plot1 <- renderPlotly({
        disney2 <- read.csv("walt_disney_movies.csv")
        
        if (input$animation == TRUE & input$films == TRUE) {
          disney2 <- disney2
          
        } else if (input$animation == TRUE) {
          
          wybor <- read.csv("disney-characters.csv")[,2]
          wybor <- str_replace_all(wybor, "[\r\n]" , "")
          disney2 <- disney2 %>% filter(title %in% wybor)
          
        } else if (input$films == TRUE) {
          
          wybor <- read.csv("disney-characters.csv")[,2]
          wybor <- str_replace_all(wybor, "[\r\n]" , "")
          disney2 <- disney2 %>% filter(! title %in% wybor)
          
        } else if (input$animation == FALSE & input$films == FALSE) {
          
          disney2["Release.date"] <- NA
          
        } 
        df <- disney2 %>% 
          mutate(year = str_sub(Release.date, 1,4)) %>% 
          select("year", "Running.time") %>% 
          filter(year != "") %>% 
          filter(year >= input$years[1] & year <= input$years[2])
        plot_ly(data = df, x = ~year, y = ~Running.time, type = "box", fillcolor = "#EFBEB7", color = I("#B12228")) %>% 
          layout (
            title = "Czas trwania filmów w kolejnych latach",
            yaxis = list(title = "Czas trwania filmu (w minutach)"),
            xaxis = list(title = "Rok")
          )
        
      })
      
      
      
      output$linePlot <- renderPlotly({
        disney <- read.csv("disney_movies.csv")
        
        
        
        if (input$animation == TRUE & input$films == TRUE) {
          disney <- disney
          
        } else if (input$animation == TRUE) {
          
          wybor <- read.csv("disney-characters.csv")[,2]
          wybor <- str_replace_all(wybor, "[\r\n]" , "")
          disney <- disney %>% filter(movie_title %in% wybor)
        } else if (input$films == TRUE) {
          
          wybor <- read.csv("disney-characters.csv")[,2]
          wybor <- str_replace_all(wybor, "[\r\n]" , "")
          disney <- disney %>% filter(! movie_title %in% wybor)

        } else if (input$animation == FALSE & input$films == FALSE) {
          
          disney["total_gross"] <- NA
          disney["inflation_adjusted_gross"] <- NA
          
        }
        
        year_1 <- input$years[1]
        year_2 <- input$years[2]
        
        if (input$years[2] > 2016)  {year_2 <- 2016}
        
        
        
        data <- disney %>% filter(disney$release_date >= year_1,
                                  disney$release_date <= year_2 ) %>% select( release_date, inflation_adjusted_gross, total_gross)
        data_inflation <- aggregate(data$inflation_adjusted_gross, by = list(rok = str_sub(data$release_date, 1,4)), FUN = sum)
        data_no_inflation <- aggregate(data$total_gross, by = list(rok = str_sub(data$release_date, 1,4)), FUN = sum)
        colnames(data_inflation)[2] <- "dochod"
        colnames(data_no_inflation)[2] <- "dochod"
        
        
        plot_ly() %>% 
          add_trace(data_inflation, x = ~data_inflation$rok,
                    y = ~data_inflation$dochod, 
                    type = 'scatter', mode = 'lines', name = "Dochód (z inflacją)", line = list(color = "#efbeb7", width = 2, dash = "dot")) %>% 
          add_trace(data_no_inflation, x = ~data_no_inflation$rok,
                    y = ~data_no_inflation$dochod, 
                    type = 'scatter', mode = 'lines', name = "Dochód", line = list(color = "#b12228" )) %>% 
          layout (
            yaxis = list(title = "Dochód (w $)"),
            xaxis = list(title = "Rok"),
            legend = list(x = 0.05, y = 1),
            title = "Dochód brutto z produkcji filmowych "
          )
      })
      output$barPlot <- renderPlotly({
        disney <- read.csv("disney_movies.csv")

        
        
        data <- disney %>% arrange(-inflation_adjusted_gross) %>% head(10) 
        
        if (input$inflation == TRUE) {
          
          data <- data %>% select(movie_title, inflation_adjusted_gross)
          colnames(data)[2] <- "dochod"
          
        } else {
          
          data <- data %>% select(movie_title, total_gross)
          colnames(data)[2] <- "dochod"
        }
        
        data$movie_title <- factor(data$movie_title, levels = data$movie_title[order(data$dochod, decreasing = FALSE)])
        
        plot_ly(data,
                y =~ movie_title,
                x = ~dochod,
                type = "bar",
                orientation = "h", 
                marker = list(color = "#393E8F")) %>% 
          layout(yaxis = list(title = "", side = "right"),
                 xaxis = list(title = "Dochód"),
                 title = "Najbardziej dochodowe produkcje The Walt Disney Company")
        
        
      })
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    
