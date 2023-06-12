# Biblioteki
library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Przetwarzanie danych

## Dane Marvel vs DC
db <- read_excel("C:/Users/zofia/OneDrive/Pulpit/Czwarty semestr/Wstęp do eksploracji danych/P2/MCUvsDC.xlsx")
db <- db %>% 
  mutate(Budget_NEW = Budget/1000000) %>% 
  select("Original Title", "Company", "Rate", "Metascore", "Minutes", "Release", "Budget_NEW")
gender <- c("man", "man", "man", "man", "man", "", "man", "man", "man", "",
           "", "man", "man", "man", "", "man", "man", "man", "", "",
           "woman", "", "man", "woman", "man", "man", "man", "man", "man", "man",
           "man", "man", "man", "", "woman", "", "man", "man", "man")
db <- data.frame(db, gender)

## Dane Platformy streamingowe
movies <- read.csv("C:/Users/zofia/OneDrive/Pulpit/Czwarty semestr/Wstęp do eksploracji danych/P2/MoviesOnStreamingPlatforms.csv")
movies <- movies %>% 
  select(-c(Type, X, ID))
movies$Rotten.Tomatoes <- substr(movies$Rotten.Tomatoes, 1, nchar(movies$Rotten.Tomatoes) - 4)
movies$Rotten.Tomatoes <- as.integer(movies$Rotten.Tomatoes)
movies <- na.omit(movies)

OnlyHulu <- movies %>% 
  filter(Hulu == 1 & !(Netflix == 1 | Disney. == 1 | Prime.Video == 1))
movies <- movies %>% 
  filter(!Title %in% OnlyHulu$Title) %>% 
  select(-Hulu) %>% 
  rename(PrimeVideo = Prime.Video,
         DinseyPlus = Disney.) %>% 
  filter(Year >= 2000)
rownames(movies) <- seq(nrow(movies))

df <- read.csv("C:/Users/zofia/OneDrive/Pulpit/Czwarty semestr/Wstęp do eksploracji danych/P2/imdb_top_1000.csv")
df <- df %>% 
  select(c(Poster_Link, Series_Title, Released_Year, Runtime, Genre, IMDB_Rating, Overview)) %>% 
  na.omit(df) %>% 
  mutate(Released_Year = as.numeric(Released_Year)) %>% 
  mutate(Runtime = substr(Runtime, 1, 3)) %>% 
  mutate(Runtime = as.numeric(Runtime))

df1 <- df %>%
  select(Genre) %>% 
  separate_rows(Genre, sep = ", ") %>%
  distinct(Genre) 
  


ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Projekt 2",
                  # Zakładki 
                  tabPanel("Platformy Streamingowe",
                           titlePanel("Platformy Streamingowe"),
                           sidebarPanel(
                             
                             selectInput("Platform", "Platforma:",
                                         c("Netflix" = "NF",
                                           "Prime Video" = "PV",
                                           "Disney+" = "D+"),
                                         selected = "NF"),
                             
                             selectInput("Age", "Kategoria wiekowa:",
                                         c("18+" = "18",
                                           "13+" = "13",
                                           "7+" = "7",
                                           "wszystkie" = "all"),
                                         selected = "all"),
                             
                             sliderInput(inputId = "year1",
                                         label = "Rok:",
                                         min = 2000,
                                         max = 2021,
                                         value = c(2010,2020),
                                         step = 1,
                                         sep = ""),
                           ),
                           mainPanel(
                             plotOutput("Platf"),
                             textOutput("tekst3")
                           )
                  ),
                  tabPanel("Marvel vs DC", 
                           titlePanel("Marvel vs DC"),
                           sidebarPanel(
                             checkboxGroupInput("company", "Producent:",
                                                c("Marvel" = "marvel",
                                                  "DC" = "dc"),
                                                selected = c("marvel", "dc")),
                             checkboxGroupInput("character", "Główna postać:",
                                                c("Mężczyzna" = "man",
                                                  "Kobieta" = "woman"),
                                                selected = c("man", "woman")),
                             sliderInput(inputId = "minutes",
                                         label = "Długość trwania filmu w minutach:",
                                         min = 80,
                                         max = 190,
                                         value = c(100,150),
                                         step = 10),
                             sliderInput(inputId = "year",
                                         label = "Rok emisji:",
                                         min = 2004,
                                         max = 2019,
                                         value = c(2010,2015),
                                         step = 1,
                                         sep = ""),
                             sliderInput(inputId = "budget",
                                         label = "Budżet liczony w milionach dolarów:",
                                         min = 40,
                                         max = 360,
                                         value = c(160,260),
                                         step = 20)
                           ),
                           mainPanel(
                             plotOutput("IMD1"),
                             plotOutput("meta1")
                           )
                  ),
                  tabPanel("Oceny IMDB",
                           titlePanel("Oceny IMDB"),
                           sidebarPanel(
                             sliderInput(inputId = "years",
                                         label = "Rok emisji:",
                                         min = 1920,
                                         max = 2020,
                                         value = c(1920,2020),
                                         step = 1,
                                         sep = ""),
                             sliderInput(inputId = "time",
                                         label = "Czas trwania w minutach:",
                                         min = 45,
                                         max = 321,
                                         value = c(45,321),
                                         step = 1,
                                         sep = ""),
                           ),
                           mainPanel(
                             plotOutput("wyk_imdb"),
                             uiOutput("tekst1"),
                             uiOutput("tekst2")
                           
                           )
                )
))


server <- function(input, output){
  
  data <- reactive({
    minminutes <- input$minutes[1]
    maxminutes <- input$minutes[2]
    minyear <- input$year[1]
    maxyear <- input$year[2]
    minbudget <- input$budget[1]
    maxbudget <- input$budget[2]
    
    db <- db %>% 
      filter(
        Minutes >= minminutes,
        Minutes <= maxminutes,
        Release >= minyear,
        Release <= maxyear,
        Budget_NEW >= minbudget,
        Budget_NEW <= maxbudget,
      )
    
    if ("marvel" %in% input$company & !"dc" %in% input$company){
      db <- db %>% 
        filter(Company == "Marvel")
    }
    
    if (!"marvel" %in% input$company & "dc" %in% input$company){
      db <- db %>% 
        filter(Company == "DC")
    }
    
    if ("man" %in% input$character  & !"woman" %in% input$character){
      db <- db %>% 
        filter(gender == "man")
    }
    
    if (!"man" %in% input$character  & "woman" %in% input$character){
      db <- db %>% 
        filter(gender == "woman")
    }
    
    if (!"man" %in% input$character  & !"woman" %in% input$character){
      db <- db %>% 
        filter(gender == "")
    }
    
    db <- as.data.frame(db)
    
    db
  })
  
  data1 <- reactive({
    
    minyear1 <- input$year1[1]
    maxyear1 <- input$year1[2]
    
    movies <- movies %>%
      filter(
        
        Year >= minyear1,
        Year <= maxyear1,
      )
    
    if ("NF" %in% input$Platform){
      movies <- movies %>%
        filter(Netflix == 1)
    }
    
    if ("PV" %in% input$Platform){
      movies <- movies %>%
        filter(PrimeVideo == 1)
    }
    
    if ("D+" %in% input$Platform){
      movies <- movies %>%
        filter(DinseyPlus == 1)
    }
    
    if ("18" %in% input$Age){
      movies <- movies %>%
        filter(Age == "18+")
    }
    
    if ("all" %in% input$Age){
      movies <- movies 
    }
    
    if ("13" %in% input$Age){
      movies <- movies %>%
        filter(Age == "13+")
    }
    
    if ("7" %in% input$Age){
      movies <- movies %>%
        filter(Age == "7+")
    }
    
    movies <- as.data.frame(movies)
    
    movies
  })
  
  data3 <- reactive({
    mintime <- input$time[1]
    maxtime <- input$time[2]
    minyears <- input$years[1]
    maxyears <- input$years[2]
    
    oceny_imdb <- df %>% 
      filter(
        Runtime >= mintime,
        Runtime <= maxtime,
        Released_Year >= minyears,
        Released_Year <= maxyears,
      )
      
    oceny_imdb
    
  })
  
  data_text <- reactive({
    maxrating <- max(data3()$IMDB_Rating)
    IMDB_text <- data3() %>% 
      filter(IMDB_Rating == maxrating)
    IMDB_text
  })
    
  output$IMD1 <- renderPlot({
    
    ggplot(data(), aes(x = Original.Title, y = Rate)) + 
      geom_point()+
      labs(title = "Ocena IMDB",
      x = "Film",
      y = "IMDB")
    
  })
  
  output$meta1 <- renderPlot({
    
    ggplot(data(), aes (x = Original.Title, y = Metascore))+
      geom_point()+
      labs(title = "Ocena Metacritic",
      x = "Film",
      y = "Metacritic")
    
  })
  
  output$Platf <- renderPlot({
    
    if ("NF" == input$Platform){
      ggplot(data1(), aes(x = Rotten.Tomatoes)) +
        geom_histogram(fill = "#b3050f") +
        scale_x_continuous(limits = c(0, 100)) +
        theme_minimal() +
        labs(
          x = "Oceny Rotten Tomatoes",
          y = "Liczba ocen"
        )
    }
    else{
      if ("PV" == input$Platform){
        ggplot(data1(), aes(x = Rotten.Tomatoes)) +
          geom_histogram(fill = "#00aae4") +
          scale_x_continuous(limits = c(0, 100)) +
          theme_minimal() +
          labs(
            x = "Oceny Rotten Tomatoes",
            y = "Liczba ocen"
          )
      }
      else{
        ggplot(data1(), aes(x = Rotten.Tomatoes)) +
          geom_histogram(fill = "#020e40") +
          scale_x_continuous(limits = c(0, 100)) +
          theme_minimal() +
          labs(
            x = "Oceny Rotten Tomatoes",
            y = "Liczba ocen"
          )
      }
    }
    
  })
   
  output$wyk_imdb <- renderPlot({
    
    ggplot(data3(), aes(x = IMDB_Rating,fill="#0099FF")) +
      geom_histogram(color="white") +
      scale_x_continuous(limits = c(6.5,9.5),breaks = seq(6.5,9.5,by=0.5))+
      labs(x = "Oceny", y = "Ilość", title = "Rozkład ocen filmów z portalu IMDB")+
      theme(plot.title = element_text(size = 16, face = "bold"), 
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            legend.position = "none")+
      scale_fill_manual(values = "#0099FF")
    
    
    
  })
  
  output$tekst1 <- renderUI({
    bold1 <- paste("<b>",data_text()$Series_Title[1],"</b>")
    bold2 <- paste("<b>",data_text()$IMDB_Rating[1],"</b>")
    HTML(paste("Jeden z najlepiej ocenianych filmów dla Twojego wyboru to ",bold1," z oceną",bold2,"."))
    
  })
  output$tekst2 <- renderUI({
    bold3 <- paste("<b>",data_text()$Overview[1],"</b>")
    HTML(paste("Opis tego filmu w języku angielskim:",bold3))
    
  })
  
  output$tekst3 <- renderText({
    paste("liczba filmów:", nrow(data1()))
    
  })
}

shinyApp(ui = ui, server = server)
