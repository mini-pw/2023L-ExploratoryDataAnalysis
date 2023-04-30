library(PogromcyDanych)
library(shiny)


ui <- fluidPage(
  titlePanel("Seriale IMDB"),
  sidebarLayout(
    sidebarPanel(
      ### Zadanie 4 ###
      selectInput("serial",
                  "Wybierz serial:",
                  unique(serialeIMDB$serial)),
      ### Koniec - Zadanie 4 ###
      ### Zadanie 6 ###
      checkboxInput("czyLiniaTrendu",
                    "Czy linia trendu?",
                    FALSE)
      ### Koniec - Zadanie 6 ###
      ),
    mainPanel(
      ### Zadanie 5 ###
      plotOutput("wykresPunktowy"),
      ### Koniec - Zadanie 5 ###
      ### Zadanie 7 ###
      tableOutput("tabela")
      ### Koniec - Zadanie 7 ###
      )
    )
  )



server <- function(input, output) {

  ### Zadanie 3 ###
  output$boxplot <- renderPlot({
    
    ggplot(serialeIMDB, aes(x = ocena)) + 
      geom_boxplot()
    
  })
  ### Koniec - Zadanie 3 ###
  
  ### Zadanie 5 ###
  output$wykresPunktowy <- renderPlot({
    
    wykres <-
      ggplot(serialeIMDB[serialeIMDB$serial == input$serial, ]) +
      geom_point(aes(x = id, y = ocena, color = sezon)) +
      labs(
        x = "Odcinek",
        y = "Ocena",
        color = "Sezon",
        title = paste("Oceny dla serialu", input$serial)
      )
    ### Zadanie 6 ###
    if(input$czyLiniaTrendu){
      wykres <- wykres +
        geom_smooth(aes(x = id, y = ocena), se = FALSE)
    }
    wykres
    ### Koniec - Zadanie 6 ###
    
  })
  ### Koniec - Zadanie 5 ###
  
  ### Zadanie 7 ###
   output$tabela <- renderTable({
      
      table <- sapply(unique(serialeIMDB$sezon[serialeIMDB$serial == input$serial]), function(x){
        summary(serialeIMDB$ocena[serialeIMDB$serial == input$serial & serialeIMDB$sezon == x])
      })
      
      rownames(table) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
      colnames(table) <- unique(serialeIMDB$sezon[serialeIMDB$serial == input$serial])
      
      table
      
    }, rownames = TRUE)
   ### Koniec - Zadanie 7 ###
}


shinyApp(ui = ui, server = server)
