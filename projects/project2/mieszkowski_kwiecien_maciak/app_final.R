#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

actors <- read.csv("https://raw.githubusercontent.com/FifiMelo/IMDBdashboard/main/datatables/actors.csv", sep = ";")
films <- read.csv("https://raw.githubusercontent.com/FifiMelo/IMDBdashboard/main/datatables/films.csv", sep =";")
nominations <- read.csv("https://raw.githubusercontent.com/FifiMelo/IMDBdashboard/main/datatables/nominations.csv", sep = ";")

library(stringr)
library(shiny) # basic shiny related functions and features
library(shinydashboard) # for dashboard related functions and features
library(shinyBS) # for modal 
library(shinyjs) # easy javascript functionalities with shiny
library(DT) # for interactive data tables
library(ggplot2) # for ggplot plot
library(shinycssloaders)
library(dplyr)
library(plotly)

# strsplit(actors$image.urls[1], "', '")[[1]][2]



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Hollywood Actors"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Actors", tabName = "Test"),
      menuItem("Plots", tabName = "Plots")
    )
  ),
  
  dashboardBody(
    includeCSS("./style.css"),
    tabItems(
      tabItem(tabName = "Test",
              fluidRow(
                tabPanel("Boxes",uiOutput("myboxes"))
              )),
      tabItem(tabName = "Plots",
              fluidRow(
                column(6, 
                       
                       checkboxGroupInput("plec",
                                          "Choose gender:",
                                          unique(nowiaktorzy$gender))
                       
                ),
                column(6,
                       
                       sliderInput("zakres",
                                   "Select a range of years:",
                                   value = c(min(filmyn$yearn), max(filmyn$yearn)),
                                   min = min(filmyn$yearn),
                                   max = max(filmyn$yearn),
                                   step = 2))
                
              ),
              
              fluidRow(
                column(6,
                       
                       plotlyOutput("pointPlot")
                ),
                column(6,
                       
                       plotlyOutput("histPlot")
                )
              ),
              fluidRow(
                column(6,
                       sliderInput("zakres2",
                                   "Select a range of years:",
                                   value = c(1954, 1979),
                                   min = min(new_aktorzy$date),
                                   max = max(new_aktorzy$date),
                                   step = 1),
                       
                       plotlyOutput("piechart")
                ),
                column(6, 
                       div(style="display: inline-block; width: 300px;",
                           sliderInput("bins",
                                       "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 30)),
                       div(style="display: inline-block; width: 300px;",
                           sliderInput("zakres1",
                                       "Select a range of years:",
                                       value = c(1954, 1979),
                                       min = min(new_aktorzy$date),
                                       max = max(new_aktorzy$date),
                                       step = 1)),
                       plotlyOutput("hist")
                ),
              ),
              
              
              # fluidRow(
              #   column(6,
              #          
              #          checkboxGroupInput("plec",
              #                             "Choose gender:",
              #                             unique(new_aktorzy$gender)),
              #   ),
              # ),
              
              fluidRow(
                column(6,
                       checkboxGroupInput("nom",
                                          "Choose won awards/nominations:",
                                          choices = c("won awards", "nominations")),
                       
                       plotlyOutput("barplot")
                ),
              ),
              
              
              
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pointPlot <- renderPlotly({
    
    plot_ly(nowiaktorzy %>% 
              filter(nowiaktorzy$gender %in% input$plec), 
            x = ~date,
            y = ~ilosc_filmow,
            color = ~gender,
            colors = c("#FF7F14","#1F77B4")) %>% 
      layout(title = "Dependence between year of birth and number of films played in", 
             xaxis = list(title = 'Year of birth', range=c(1940,2020)), 
             yaxis = list(title = 'Number of films', range=c(0,250)),
             legend = list(orientation = "v", x = 1, y = 0.7))
    
  })
  
  output$histPlot <- renderPlotly({
    
    plot_ly(filmyn %>% 
              filter(filmyn$yearn >= input$zakres[1],
                     filmyn$yearn <= input$zakres[2]) ,
            x = ~year,
            type = "histogram") %>% 
      layout(barmode="overlay",
             title = "Number of films delivered in a given year",
             xaxis = list(title = "Year", color ="black", tickangle = 45), 
             yaxis = list(title = 'Number of films', range=c(0,450)))
    
  })
  
  
  output$hist <- renderPlotly({
    # generate bins based on input$bins from ui.R
    df1 <- new_aktorzy %>% 
      filter(date >= input$zakres1[1],
             date <= input$zakres1[2])
    helper    <- df1$date
    #bins <- seq(min(helper), max(helper), length.out = input$bins + 1)
    
    #draw the histogram with the specified number of bins
    plot_ly(df1, x = ~date, type = "histogram", nbinsx = input$bins) %>%
      layout(barmode="overlay",
             title = "Birth of actors in years - Histogram",
             xaxis = list(title = 'Year of birth', range=c(min(helper), max(helper))),
             yaxis = list(title = 'Number of actors', range=c(0,12)))
    
    
    # hist(helper, breaks = bins, col = 'darkgray', border = 'white',
    #      xlab = 'Year of birth',
    #      ylab = 'Number of actors',
    #      xlim = c(min(helper), max(helper)),
    #      ylim = c(0,12),
    #      main = 'Birth of actors in years - Histogram')
  })
  
  output$piechart <- renderPlotly({
    
    df <- new_aktorzy %>% 
      filter(date >= input$zakres2[1],
             date <= input$zakres2[2])  %>% 
      count(gender)
    
    
    plot_ly(df, labels = ~gender, values = ~n, text = ~n, type = 'pie')%>% 
      layout(title = paste('Percentage of actors and actresses born in years', input$zakres2[1], '-',input$zakres2[2]),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  output$barplot <- renderPlotly({
    
    
    ifelse(input$nom == c("won awards", "nominations"),
           
           # barplot - Nominations/Won awards
           fig <- plot_ly(df_nom_both, x = ~name, y = ~n.x, type = "bar", name = "nominations", marker = list(color = "#7F8C8D")) %>% 
             add_trace(y = ~n.y, name = 'won awards', marker = list(color ="#F1C40F")) %>% 
             layout(title = 'Nominations/Won awards by actors',
                    xaxis = list(title = "Names of actors"),
                    yaxis = list(title = "Number of awards/nominations"), 
                    barmode = 'group'),
           
           ifelse(input$nom == c("nominations", "won awards"),
                  
                  # barplot - Nominations/Won awards
                  fig <- plot_ly(df_nom_both, x = ~name, y = ~n.x, type = "bar", name = "nominations", marker = list(color = "#7F8C8D")) %>% 
                    add_trace(y = ~n.y, name = 'won awards', marker = list(color ="#F1C40F")) %>% 
                    layout(title = 'Nominations/Won awards by actors',
                           xaxis = list(title = "Names of actors"),
                           yaxis = list(title = "Number of awards/nominations"), 
                           barmode = 'group'),
                  
                  ifelse(input$nom == c("won awards"),
                         
                         # barplot - awards
                         fig <- plot_ly(df_nom, x = ~name, y = ~n, type = "bar", name = 'won awards', marker = list(color ="#F1C40F")) %>% 
                           layout(title = 'Won awards by actors',
                                  xaxis = list(title = "Names of actors"),
                                  yaxis = list(title = "Number of awards")),
                         
                         ifelse(input$nom == c("nominations"), 
                                
                                # barplot - nominations
                                fig <- plot_ly(df_nom_all, x = ~name, y = ~n, type = "bar", name = 'won awards', marker = list(color = "#7F8C8D")) %>% 
                                  layout(title = 'Nominations by actors',
                                         xaxis = list(title = "Names of actors"),
                                         yaxis = list(title = "Number of nominations")), 
                                
                                # barplot - blank
                                # fig <- plot_ly(type = "bar") %>% 
                                #   layout(title = 'Nominations/Won awards by actors',
                                #          xaxis = list(title = "Names of actors", range = list(0,7)),
                                #          yaxis = list(title = "Number of awards/nominations", range = list(0, 80)))
                         )
                  )
           )
    )
    
    fig
    
  })
  
  
  boxes <- list()
  for (i in 1:nrow(actors)){

    # galeria
    image_urls <- strsplit(actors$image.urls[i], "', '")[[1]]
    image_urls[1] <- str_sub(image_urls[1], 3)
    image_urls[length(image_urls)] <- str_sub(image_urls[length(image_urls)], 0, -3)
    images <- list()
    for(j in 1:10) {
      images[[j]] <- img(src = image_urls[j])
    }

    #filmy
    film_ids <- strsplit(actors$film.ids[i], "', '")[[1]]
    film_ids[1] <- str_sub(film_ids[1], 3)
    film_ids[length(film_ids)] <- str_sub(film_ids[length(film_ids)], 0, -3)
    current_films <- list()
    for(j in 1:length(film_ids)) {
      films %>%
        filter(id == film_ids[j]) -> film
      current_films[[j]] <- div(
        class = "film",
        style="display:inline-block",
        h3(film$title),
        br()
      )
    }


    #nagrody
    act.id <- actors$id[i]
    nominations %>%
      filter(actor.id == actor.id) -> nominations_df
    current_nominations <- list()
    for(j in 1:nrow(nominations_df)) {
      current_nominations[[j]] <- div(
        class = "nomination",
        style="display:inline-block",
        h3(nominations_df$event.name[j]),
        h4(nominations_df$category[j]),
        p(paste0("Won: ", nominations$is.winner[j])),
        p(paste0("Year: ", nominations$year[j])),
        br()
      )
    }



    boxes[[i]] <- box(
      width = 3,
      class = "actor",
      h3(actors$name[i]),
      br(),
      p(paste("full name:",actors$full.name[i])),
      br(),
      p(paste0("born: ", actors$birth.date[i],", ", actors$birth.place[i])),
      br(),
      div(class = "gallery-button", style="display:inline-block",
          actionButton(paste0("gallery", actors$id[i]),  "gallery", class="button")
      ),
      bsModal(id = paste0("gallerymodal", actors$id[i]), title = paste0(actors$name[i], "'s gallery"), trigger = paste0("gallery", actors$id[i]), size="large",
              images

      ),

      div(class = "biography-button", style="display:inline-block",
          actionButton(paste0("bio", actors$id[i]),"biography", class = "button")
      ),
      bsModal(id = paste0("biomodal", actors$id[i]), title = paste0(actors$name[i], "'s biography"), trigger = paste0("bio", actors$id[i]), size="large",
              p(actors$bigraphy[i])
      ),


      div(class = "films-button", style="display:inline-block",
          actionButton(paste0("films", actors$id[i]),"films", class = "button")
      ),
      bsModal(id = paste0("filmsmodal", actors$id[i]), title = paste0(actors$name[i], "'s filmography"), trigger = paste0("films", actors$id[i]), size="large",
              current_films
      ),



      div(class = "awards-button", style="display:inline-block",
          actionButton(paste0("awards", actors$id[i]),"awards", class = "button")
      ),
      bsModal(id = paste0("awardsmodal", actors$id[i]), title = paste0(actors$name[i], "'s awards and nominations"), trigger = paste0("awards", actors$id[i]), size="large",
              current_nominations,
              br()
      ),



    )

  }
  output$myboxes <- renderUI(boxes)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
