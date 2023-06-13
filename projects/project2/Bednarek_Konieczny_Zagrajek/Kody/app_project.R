
library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(readxl)
my_data <- read_excel("oscars.xlsx")

m <- my_data %>% filter(my_data$year_ceremony == 2018)
# ######## ramka potrzebna do histPlot1
years <- 1928:2020
years <- years[-7]

year_counts <- my_data[my_data$gender == 'Female' & my_data$winner == 'TRUE',]
year_counts <- as.data.frame(table(year_counts$year_ceremony))
colnames(year_counts) <- c("year", "count")
year_counts <- arrange(year_counts, -desc(year))
year_counts$year <- years

############# ramka do linePlot1

oscarsRace <- my_data %>% 
  group_by(year_ceremony, Race) %>% 
  summarise(count = n()) %>% 
  arrange(-year_ceremony) 
  colnames(oscarsRace) <- c("rok", "rasa", "liczba")
  
  

#################

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  
  titlePanel("Analiza danych o Oskarach na przestrzeni lat 1928-2020"),

  textOutput("text"),
  
  fluidRow(
    column(6, 
          
           sliderInput("zakres",
                       "Wybierz przedział czasu:",
                       value = c(min(year_counts$year), max(year_counts$year)),
                       min = min(year_counts$year, na.rm = T),
                       max = max(year_counts$year, na.rm = T),
                       step = 1,
                       sep = ''
                       )
        
          ),
    column(6,
           checkboxGroupInput("rasy",
                              "Które rasy wybirasz?",
                              unique(my_data$Race)
                              )
           )
  
  ),
  fluidRow(
    column(6,

           plotlyOutput("histPlot1")
           
          ),
    column(6,
           plotlyOutput("linePlot1"))
    
  ),
  fluidRow(
    column(3, 
           textOutput("text2"),
           
           selectInput("year",
                       "Wybierz rok:",
                       years),
           selectInput("number",
                       "Wybierz ile pozycji chcesz pokazać",
                       c(1:10)
                       )
           
           ),
    column(8,
           plotlyOutput("barPlot1")
    )
  )
)




server <- function(input, output) {
  
  output$text <- renderText(({
    
   "1. Oskary wśród kobiet"
    
  }))
 
  
  output$histPlot1 <- renderPlotly({
    
    
    
    plot_ly(year_counts %>% 
              filter(
                     year_counts$year >= input$zakres[1],
                     year_counts$year <= input$zakres[2]), 
            x = ~year,
            y = ~count,
            type = "bar") %>% 
      layout(title = "Liczba oskarów zdobytych przez kobiety w wybranym przedziale czasowym", 
             xaxis = list(title = 'przedział czasu'), 
             yaxis = list(title = 'liczba wygranych oskarów'))
    
  
  })
  
  output$linePlot1 <- renderPlotly({
    df <- oscarsRace %>% filter(rasa %in% input$rasy)
    
    
    # plot_ly(data = as.data.frame(oscarsRace %>% filter(oscarsRace$rasa %in% input$rasy)),
    #         x = ~rok,
    #         y = ~liczba,
    #         color = ~rasa,
    #         colors = "Set1",
    #         type = "point") %>% 
    #   layout(title = "Liczba osób zdobywających oskary w podziale na rasy", 
    #          xaxis = list(title = 'rok'), 
    #          yaxis = list(title = 'liczba')
    #          )
    
    ggplot(df, aes(x = rok, y = liczba, color = rasa, colors = "Set1")) +
      geom_point()+
      theme_minimal() + 
      labs(title = "Liczba przyznawanych Oskarów w podziale na rasy na przestrzeni lat", xlab = "rok", ylab = "liczba przyznanych Oskarów")
    
    
  })
  
  output$barPlot1 <- renderPlotly({
    
    films_year <- my_data %>% filter(year_ceremony == input$year) %>% select(film)
    film_counts <- as.data.frame(table(films_year))
    colnames(film_counts) <- c("film", "count")
    film_counts <- arrange(film_counts, desc(count))
    film_counts_plot <- top_n(film_counts, input$number)
    
   
    
    ggplot(data = film_counts_plot, aes(x = reorder(film, count), y = count)) +
      geom_bar(stat = "identity", fill = "aquamarine4") +
      theme_bw() +
      coord_flip() +
      labs(title = paste("Top", input$number, "najczęściej nominowane filmy w roku", input$year),
            x = "Film", 
            y = "Liczba")
    
  })
  output$text2 <- renderText(({
    
    "Najczęściej nominowane filmy w poszczególnych latach"
    
  }))

  
 
}


shinyApp(ui = ui, server = server)