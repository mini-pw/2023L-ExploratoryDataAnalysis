source('dataProcessingPage1.R')
source('dataProcessingPage2.R')

ui <- navbarPage(
  "Find a Film",
  id = "nav-page",
  tabPanel(
    "Find a Film by Tags",
    titlePanel("Select criteria to search for a film"),
    fluidRow(
      column(
        6,
        sidebarPanel(
          selectizeInput("Tags", "Select tags", choices = unique(full_df$tag), multiple = TRUE),
          checkboxInput("included", "All tags present?", FALSE),
          sliderInput(
            inputId = "year",
            label = "Year of Production",
            min = 1950,
            max = 2020,
            value = c(1950, 2020),
            width = "220px"
          ),
          numericInput("Count", "Number of films", 20, min = 1, max = 20),
          actionButton('goPlot', 'Show Films!')
        )
      ),
      column(
        6,
        mainPanel(
          tags$script(HTML(
            "$(document).on('click', '#canvas', function() {
                                  word = $('#wcLabel').text();
                                  Shiny.onInputChange('clicked_word', word);
                                });")
          ),
          wordcloud2Output("wordcloud2", height = "400px")
        )
      )
    )
  ),
  tabPanel(
    "More about Films",
    titlePanel("Learn more about the selected film"),
    fluidRow(
      column(
        6,
        selectizeInput("Film", "Select a film", choices = unique(full_df$title), multiple = FALSE),
        htmlOutput("poster"),
        h3("Reviews"),
        uiOutput("reviews"),
        fluidRow(
          column(
            6,
            actionButton("prevReview", label = "Previous Review")
          ),
          column(
            6,
            actionButton("nextReview", label = "Next Review")
          )
        )
      ),
      column(
        6,
        box(
          h3("IMDb Score"),
          gaugeOutput("gauge", height = "800px")
        )
      )
    )
  ),
  tabPanel(
    "About this app",
    mainPanel(
      h1('Description'),
      p('This application provides you with the ultimate film selection experience tailored to your preferences. Imagine having a personal movie connoisseur guiding you through the process.
', ),
      p('By simply choosing the desired tags in the first tab and specifying the production year, a curated list of films meeting your criteria will be displayed with just a click of the "Show Films!" button.'),
      p(' Once you find a film that catches your interest, click on it and a transition to the second tab awaits, where you can see its  poster, explore audience ratings, and delve into reviews not only for that particular film but also for numerous others.'),
      h1('Sources'),
      p('We used freely distributed data to create this application.'),
      p('Data with tags and reviews comes from'),
      a(href = 'https://grouplens.org/datasets/movielens/', 'Grouplens'),
      p('Data with posters comes from'),
      a(href = 'https://www.kaggle.com/datasets/neha1703/movie-genre-from-its-poster', 'Kaggle'),
      h1('Authors'),
      p(a(href = 'https://github.com/mklein02', 'Michał Klein '),' and ', a(href = 'https://github.com/shaukunoua', ' Aliaksei Shaukunou'))
      
    )
  )
)

server <- function(input, output, session) {
  output$text2 <- renderText({
    str_remove(input$clicked_word, ":[0-9]+$")
  })
  
  output$text1 <- renderText({
    paste("You have selected", str_remove(input$clicked_word, ":[0-9]+$"))
  })
  
  temp <- eventReactive(input$goPlot, {
    getFilmsByCriterions(
      full_df,
      tags = as.vector(input$Tags),
      AreAllTagsIncluded = input$included,
      yearMin = input$year[1],
      yearMax = input$year[2]
    ) %>%
      group_by(title) %>%
      summarise(Rating = round(10 * mean(avgRating))) %>%
      arrange(-Rating) %>%
      head(input$Count) %>% 
      mutate(Rating = Rating - min(Rating) + 1) 
  })
  
  output$wordcloud2 <- renderWordcloud2({
    wordcloud2(data = temp(), size = .2, shape = 'triangle-forward', maxRotation = .3)
  })
  
  observeEvent(input$clicked_word, {
    updateNavbarPage(session, "nav-page", selected = "More about Films")
    updateSelectizeInput(session, "Film", selected = str_remove(input$clicked_word, ":[0-9]+$"))
  })
  
  output$poster <- renderText({
    c('<img src="', images[images$Title == input$Film, 'Poster'], '">')
  })
  
  output$gauge <- renderGauge({
    wart3 <- images$IMDB.Score[images$Title == input$Film]
    
    gauge(wart3,
          min = 0,
          max = 10,
          gaugeSectors(
            success = c(7.5, 10),
            warning = c(4, 7.5),
            danger = c(0, 4)
          ))
  })
  
  reviewText <- reactive({
    itId <- full_df %>% filter(title == input$Film) %>% pull(item_id)
    itId <- itId[1]
    reviews$txt[reviews$item_id == itId]
  })
  
  currentReviewIndex <- reactiveVal(1)
  
  observeEvent(input$prevReview, {
    if (currentReviewIndex() > 1) {
      currentReviewIndex(currentReviewIndex() - 1)
    }
  })
  
  observeEvent(input$nextReview, {
    if (currentReviewIndex() < length(reviewText())) {
      currentReviewIndex(currentReviewIndex() + 1)
    }
  })
  
  output$reviews <- renderUI({
    reviewIndex <- currentReviewIndex()
    totalReviews <- length(reviewText())
    review <- if (reviewIndex >= 1 && reviewIndex <= totalReviews) {
      reviewText()[reviewIndex]
    } else {
      ""
    }
    
    tagList(
      h5(paste("Review", reviewIndex, "of", totalReviews)),
      HTML(review)
    )
  })

}
shinyApp(ui = ui, server = server)
