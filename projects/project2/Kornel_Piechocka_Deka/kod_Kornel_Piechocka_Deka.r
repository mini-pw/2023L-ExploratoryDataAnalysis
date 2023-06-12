library(shiny)
library(tidyverse)
library(readr)
library(data.table)


dane1 <- read_csv("titles.csv")
dane2 <- read_csv("netflix_titles.csv")



netflix_data <- inner_join(dane1, dane2, by = "title")

library(bslib)

theme <- bs_theme(
  bg = "#2B4950", fg = "#C6B7AD", primary = "#E1742B", 
  secondary = "#6F5B51", warning = "#B27B2C", font_scale = NULL, 
  `enable-gradients` = TRUE, `enable-shadows` = TRUE, bootswatch = "superhero"
)



# Funkcja zwracająca lise gatunków
podziel_napis <- function(napis){
  return(strsplit(napis, ","))
}


# Funkcja do filtrowania danych
filter_data <- function(data, type, genre, Country, year_range, rate) {
  filtered_data <- data
  
  if (type != "All") {
    filtered_data <- filtered_data %>% filter(type.x == type)
  }
  
  if (genre != "All") {
    filtered_data <- filtered_data %>% filter(str_detect(listed_in, genre))
  }
  
  if (Country != "All") {
    filtered_data <- filtered_data %>% filter(str_detect(country,Country))
  }
  
  if (!is.null(year_range)) {
    filtered_data <- filtered_data %>% filter(release_year.y >= year_range[1] & release_year.y <= year_range[2])
  }
  filtered_data <- filtered_data %>% filter(as.numeric(imdb_score) >= as.numeric(rate[1]) & as.numeric(imdb_score) <= as.numeric(rate[2]))
  
  
  return(filtered_data)
}


ui <- fluidPage(
  theme = theme,
  
  
  titlePanel("Netflix Movie Recommender"),
  div(h5("Find a perfect movie to watch")),
  
  tabsetPanel(
    tabPanel("Filter",
             sidebarLayout(
               sidebarPanel(
                 selectInput("typeInput", "Select type:", c("All", unique(netflix_data$type.x))),
                 selectInput("genreInput", "Select genre:", c("All", unique(unlist(lapply(as.list(netflix_data$listed_in), podziel_napis))))),
                 selectInput("countryInput", "Select country:", c("All", unique(unlist(lapply(as.list(netflix_data$country), podziel_napis))))),
                 sliderInput("yearInput", "Select release year range:",
                             min = min(netflix_data$release_year.y),
                             max = max(netflix_data$release_year.y),
                             value = c(min(netflix_data$release_year.y), max(netflix_data$release_year.y))),
                 sliderInput("rangeRate", "Select rate range:", 
                             min = 1,
                             max = 10,
                             value = c(1, 10))
               ),
               
               mainPanel(
                 selectInput("plotTypeInput","Compare films in terms of:", c("Duration", "Rating")),
                 plotOutput("moviesPlot")
                 
               )
             )),
    tabPanel("Film Details",
             
             mainPanel(
               dataTableOutput("moviesTable"))
    ),
    tabPanel("Actors",
             sidebarPanel(
               selectInput("Actor", "Choose actor:", unique(unlist(lapply(as.list(netflix_data$cast), podziel_napis))))
             ),
             mainPanel(
               plotOutput("Plot3"),
               dataTableOutput("actorsTable")
             ))
  )
  
)



server <- function(input, output) {
  # bs_themer() <- jak to tu bedzie to pokaze sie okienko do zmieniania kolorow
  
  filtered_data <- reactive({
    filter_data(netflix_data, input$typeInput, input$genreInput, input$countryInput, input$yearInput, input$rangeRate) 
  })
  
  output$moviesPlot <- renderPlot({
    sorted_data <- filtered_data()%>%
      arrange(runtime)
    if (input$plotTypeInput == "Duration") {
      ggplot(sorted_data, aes(x = reorder(title,runtime), y = runtime)) +
        geom_bar(stat = "identity", fill = "#E1742B") +
        labs(x = "Title", y = "Duration (minutes)") +
        theme(axis.text.x = element_text(color = "#C49C81", angle = 90, hjust = 1),
              axis.text.y = element_text(color = "#C49C81"),
              plot.background = element_rect(fill = "#bad1d6", colour = "#bad1d6"),
              panel.background = element_rect(fill = "#bad1d6"),
              legend.background = element_rect(fill = "#bad1d6"),
              legend.position = "none",
              axis.title = element_text(color = "#C49C81"),
              panel.grid.major = element_line(color = "#C8B5A9"),
              panel.grid.minor = element_blank())
    } else if (input$plotTypeInput == "Rating") {
      ggplot(sorted_data, aes(x = reorder(title,imdb_score), y = imdb_score)) +
        geom_bar(stat = "identity", fill = "#E1742B") +
        labs(x = "Title", y = "IMDb Rating")+
        theme(axis.text.x = element_text(color = "#C49C81", angle = 90, hjust = 1),
              axis.text.y = element_text(color = "#C49C81"),
              plot.background = element_rect(fill = "#bad1d6", colour = "#bad1d6"),
              panel.background = element_rect(fill = "#bad1d6"),
              legend.background = element_rect(fill = "#bad1d6"),
              legend.position = "none",
              axis.title = element_text(color = "#C49C81"),
              panel.grid.major = element_line(color = "#C8B5A9"),
              panel.grid.minor = element_blank())
    } 
  })
  
  filtered_data <- reactive({
    filter_data(netflix_data, input$typeInput, input$genreInput, input$countryInput, input$yearInput, input$rangeRate) %>%
      arrange(imdb_score)
  })
  
  filtered_data_table <- reactive({
    filter_data(netflix_data, input$typeInput, input$genreInput, input$countryInput, input$yearInput,input$rangeRate) %>%
      arrange(imdb_score) %>% 
      select(title, description.y, age_certification, release_year.y) %>% 
      rename(c(description = description.y, "age certification" = age_certification, "release year" = release_year.y))
  })
  
  
  
  # output$moviesPlot2 <- renderPlot({
  #   sorted_data <- filtered_data()%>%
  #     arrange(runtime)
  #   imdb_data <- sorted_data 
  #   imdb_data$star_rating <- ceiling(imdb_data$imdb_score)
  #   
  #   ggplot(imdb_data, aes(x = reorder(title,runtime), y = star_rating)) +
  #     geom_bar(stat = "identity", fill = "#E1742B", width = 0.8)+
  #     labs(x = "Title", y = "Star Rating") +
  #     theme(axis.text.x = element_text(color = "#C49C81", angle = 90, hjust = 1),
  #           axis.text.y = element_text(color = "#C49C81"),
  #           plot.background = element_rect(fill = "#bad1d6", colour = "#bad1d6"),
  #           panel.background = element_rect(fill = "#bad1d6"),
  #           legend.background = element_rect(fill = "#bad1d6"),
  #           legend.position = "none",
  #           axis.title = element_text(color = "#C49C81"),
  #           panel.grid.major = element_line(color = "#C8B5A9"),
  #           panel.grid.minor = element_blank())
  # })
  
  
  
  #output$moviesTable <- renderDataTable({
  #  data.table(filtered_data_table(), options = list(pageLength = 10, lengthMenu = c(10, 20, 30)))
  #})
  
  output$moviesTable <- renderDataTable({
    data.table(netflix_data %>% 
                 select(title, description.x, release_year.x, imdb_score)) %>% 
      setnames(new = c("title", "description", "release year", "imdb score"), skip_absent = TRUE)
  })
  
  
  
  
  output$Plot3<- renderPlot(({
    netflix_data %>% 
      filter(str_detect(cast, input$Actor)) %>% 
      select(title, description.x, release_year.x,listed_in, imdb_score, country, cast) %>% 
      ggplot(aes(x = title, y = imdb_score))+
      geom_bar(stat = "identity", fill = "#E1742B", width = 0.8)+
      labs(x = "Title", y = "Star Rating") +
      theme(axis.text.x = element_text(color = "#C49C81", angle = 90, hjust = 1),
            axis.text.y = element_text(color = "#C49C81"),
            plot.background = element_rect(fill = "#bad1d6", colour = "#bad1d6"),
            panel.background = element_rect(fill = "#bad1d6"),
            legend.background = element_rect(fill = "#bad1d6"),
            legend.position = "none",
            axis.title = element_text(color = "#C49C81"),
            panel.grid.major = element_line(color = "#C8B5A9"),
            panel.grid.minor = element_blank())
    
  }))
  
  output$actorsTable <- renderDataTable(({
    data.table(
      netflix_data %>% 
        filter(str_detect(cast, input$Actor)) %>% 
        select(title, description.x, release_year.x,listed_in, imdb_score, country, cast)
    ) %>% 
      setnames(new = c("title", "description", "release year", "listed in", "imdb score", "country", "cast"), skip_absent = TRUE)
  }))
  
  
}



shinyApp(ui = ui, server = server)
