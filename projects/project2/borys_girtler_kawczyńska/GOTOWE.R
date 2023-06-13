library(shiny)
library(readr)
library(ggplot2)
library(tm)
library(wordcloud)
library(shinythemes)
library(stringr)
library(shinyjs)
library(readr)
library(RColorBrewer)
library(wordcloud2)
library(leaflet)
library(bslib)
library(plotly)
library(dplyr)

"iconv file.pcl -f UTF-8 -t ISO-8859-1 -c"

# Wczytanie danych
{
  data <- read.csv("gotowe_dane_2.csv")
  data2 <- read.csv("data2.csv")
}

# Dane dotyczace seriali - potrzebne do obrazkow
{
  seriale <- data.frame(
    Title = c("Breaking Bad", "Game of Thrones", "The Office", "Friends", "The Simpsons"),
    ImageURL = c(
      "https://fwcdn.pl/fpo/06/68/430668/7730445.6.jpg",
      "https://fwcdn.pl/fpo/68/48/476848/7880020.6.jpg",
      "https://fwcdn.pl/fpo/28/87/202887/7972309.6.jpg",
      "https://fwcdn.pl/fpo/39/93/33993/7929978.6.jpg",
      "https://fwcdn.pl/fpo/79/60/87960/8030267.6.jpg"
    ),
    stringsAsFactors = FALSE
  )
}

# Zakladka 1 - twoj ulubiony serial
{
  ui_1 <- fluidPage(
    
    # Stylizacja intefejsu - ladnie wygladajece obrazki
    {
      tags$head(
        tags$style(HTML("
      .serial-image {
        display: inline-block;
        margin: auto;
      }
      .serial-image.selected {
        border-color: #f8d61e;
        margin: auto;
      }
      "))
      )
    },
  
    # Tytul zakladki
    titlePanel("Twój ulubiony serial"),
    
    # Wyswietla nam obrazki w ktore mozemy kliknac
    uiOutput("serial_images"), 
    
    fluidRow(
      # Kolumna po lewo 
      column(width = 3,
             
             # Wyswietla nam slider dla liczby sezonow
             uiOutput("seasons_slider")
      ),
      
      # Kolumna po prawo
      column(width = 9,
             
             # Wyswietla nam wykres liniowy ogladalnosci
             plotOutput("line_plot")
      ),
      
      fluidRow(
        column(width = 12,
               # Wyswietla nam boxplot oceny
               plotOutput("boxplot")
        )
      ),
      
      fluidRow(
        column(width = 12,
               
               # Wyswietla nam word cloud
               plotOutput("word_cloud", width = "800px", height = "500px")
        ),
      )
      
    )
  )
}

# Zakladka 2 - porownanie seriali
{
ui_2 <- fluidPage(
  
  # Tytul zakladki
  titlePanel("Porównaj swoich ulubieñców"),
  
  fluidRow(
    # Kolumna po lewo 
    column(width = 3,
           
           # Wyswietla nam checkboxa
           checkboxGroupInput("selected_titles", "Wybierz tytu³y", choices = unique(data$Title), selected = NULL),
           
           # Slider
           uiOutput("seasons_range"),
    ),
    
    # Kolumna po prawo
    column(width = 9,
           
           # Wyswietla nam boxplot
           plotOutput("boxplot2")
    )
  )
)
}

# Zakladka 3 - wszystkie seriale
{
ui_3 <- fluidPage(
  
  # Tytul zakladki
  titlePanel("A jakie seriale lubi¹ ogl¹daæ inni"),
  
  fluidRow(
    column(width = 6,
           
           # Wybor platformy
            selectInput("platform", "Wybierz platformê:", choices = unique(data2$platform))
    ),
    column(width = 6,
           
           # Wybor wyswietlanego wykresu
            radioButtons("option", "Jaki wykres chcesz zobaczyæ:", choices = c("Œrednia iloœæ sezonów", 
                                                                       "Liczba serialii z uwglêdnieniem iloœæi sezonów"))
    )
  ),
    fluidRow(
      # Wyswietlanie wykresu
      plotlyOutput("map")
    )
  )

}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Wykresy potrzebne do ui_1
  {
  # Generowanie obrazkow do wyboru seriali
  {
    output$serial_images <- renderUI({
      image_inputs <- lapply(seq_len(nrow(seriale)), function(i) {
        div(
          class = "serial-image",
          tags$img(
            src = seriale$ImageURL[i],
            height = 183,
            style = "cursor: pointer;",
            onclick = sprintf("Shiny.setInputValue('selected_serial', '%s');", seriale$Title[i])
          )
        )
      })
      div(
        class = "row",
        div(
          class = "col-md-12",
          style = "text-align: center;",
          tags$h5("Wybierz swój ulubiony serial",style = "font-size: 28px;")
        ),
        
        do.call(tagList, image_inputs)
      )
    })
  }
  
  # Generowanie slidera dla sezonow w zaleznosci od wybranego serialu
  {
    output$seasons_slider <- renderUI({
      title_data <- subset(data, Title == input$selected_serial)
      max_season <- max(title_data$Season)
      
      sliderInput("seasons", "Wybierz sezony",
                  min = 1, max = max_season,
                  value = c(1, max_season), step = 1, width = "200px")
  })
  }
  
  # Generowanie boxplotu na podstawie wybranego serialu i liczby sezonow
  {
  output$boxplot <- renderPlot({
    filtered_data <- subset(data, Title == input$selected_serial & Season >= input$seasons[1] & Season <= input$seasons[2])
    ggplot(filtered_data, aes(x = factor(Season), y = Rating)) +
      geom_boxplot(fill = "#b73525", color = "#a41313") +
      labs(x = "Sezon", y = "Ocena") +
      ggtitle(paste("Boxplot oceny wybranych sezonów")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  }
  
  # Generowanie wykresu liniowego ogladalnosci w zaleznosci od sezonu
  {
  output$line_plot <- renderPlot({
    filtered_data <- subset(data, Title == input$selected_serial & Season >= input$seasons[1] & Season <= input$seasons[2])
    filtered_data$Viewers = filtered_data$Viewers / 1000000
    ggplot(filtered_data, aes(x = Episode, y = Viewers, group = Season, color = factor(Season))) +
      geom_line() +
      labs(x = "Odcinki", y = "Widzowie w milionach", color = "Sezon") +
      ggtitle("Liczba ods³on odcinków w zale¿nosci do sezonu") +
      scale_color_brewer(palette = "Reds") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  }
  
  # Generowanie wykresu Word Cloud dla rezyserow konkretnych tytu³ów seriali w zale¿noœci od wybranego sezonu
  {
  output$word_cloud <- renderPlot({
    
    if (input$selected_serial == "The Simpsons") {
      plot_text <- "Daliœcie siê oszukaæ nie istnieje re¿yser"
      wordcloud(words = plot_text, min.freq = 1, scale=c(4,0.5), colors = "#a41313", random.order =FALSE)
      title("Spróbuj odczytaæ, a dowiesz siê kto wyre¿yserowa³ ten film", cex.main = 2)
    } else {
      
    filtered_data <- subset(data, Title == input$selected_serial & Season == input$seasons[1])
    directors <- filtered_data$Directed_by
    
    # Podzia³ nazwisk re¿yserów oddzielonych przecinkami
    director_names <- unlist(strsplit(directors, ";"))
    
    # Usuniêcie ewentualnych spacji na pocz¹tku i koñcu nazwisk
    director_names <- trimws(director_names)
    
    # Generowanie wykresu Word Cloud na podstawie nazwisk re¿yserów
    wordcloud(names(table(director_names)), table(director_names),
              colors = brewer.pal(length(unique(director_names)), "Reds"))
    
    # Dodanie tytu³u do wykresu Word Cloud
    title("Re¿yserowie", cex.main = 2)
    }
      
  })
  }
  }
  
  # Wykresy potrzebne do ui_2
  {
  # Generowanie slidera dla wyboru sezonow
  {
  output$seasons_range <- renderUI({
    selected_titles <- input$selected_titles
    max_seasons <- aggregate(Season ~ Title, data = data[data$Title %in% selected_titles, ], FUN = function(x) max(x))
    max_season <- min(max_seasons$Season)
    sliderInput("selected_seasons", "Wybierz sezony, które chcesz porównaæ", min = 1, max = max_season, value = c(1, max_season), step = 1)
  })
  }
  
  # Generowanie boxplotu oceny dla wybranych tytu³ow i sezonow
  {
  output$boxplot2 <- renderPlot({
    selected_titles <- input$selected_titles
    selected_seasons <- input$selected_seasons[1]:input$selected_seasons[2]
    
    filtered_data <- subset(data, Title %in% selected_titles & Season %in% selected_seasons)
    
    ggplot(filtered_data, aes(x = factor(Season), y = Rating, fill = Title)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Reds") +
      labs(x = "Sezon", y = "Ocena", fill = "Tytu³ serialu") +
      ggtitle("Boxplot oceny sezonów dla wybranych seriali") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  }
  }
  
  # Wykresy potrzebne do ui_3
  {
    output$map <- renderPlotly({
      
      # Generowanie mapki
      if (input$option == "Œrednia iloœæ sezonów") {
        filtered_data <- reactive({
          data2 %>%
            filter(platform %in% input$platform) %>%
            group_by(country, duration) %>%
            summarise(n = n())
        })
        
        mean_duration <- filtered_data() %>%
          group_by(country) %>%
          summarise(Œredni_czas = mean(as.numeric(duration)))
        
        plot_ly(data = mean_duration, type = "choropleth",
                locations = ~country,
                locationmode = "country names",
                z = ~Œredni_czas,
                colorscale = "Reds",
                text = ~paste("Kraj:", country, "<br>Œredni czas:", Œredni_czas)) %>%
          layout(title = "<br> Œrednia iloœæ sezonów w serialach z podzia³em na kraje",
                 geo = list(showframe = FALSE, showcoastlines = FALSE)) %>%
          colorbar(title = "Œrednia sezonów")
        
      } else if (input$option == "Liczba serialii z uwglêdnieniem iloœæi sezonów") {
        seriale_filtered <- subset(data2, platform == input$platform)
        
        # Generowanie slupkow
        ggplot(seriale_filtered, aes(x = as.factor(duration))) + 
          geom_bar(fill = "#b73525") +
          labs(x = "Liczba sezonów", y = "Liczba seriali") +
          ggtitle("Liczba seriali na wybranej platformie z podzai³em na iloœæ sezonów") +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold"))
      }
    })
  }
}

app_ui <- navbarPage(
  title = "Serialowe potyczki",
  tabPanel("Twój ulubiony serial", ui_1),
  tabPanel("Porównaj swoich ulubieñców", ui_2),
  tabPanel("Co ogl¹daj¹ inni", ui_3)
  )

# Run the application 
shinyApp(ui = app_ui, server = server)
