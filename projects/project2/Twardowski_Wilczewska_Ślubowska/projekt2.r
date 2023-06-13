library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggplot2)
require(gridExtra)
library(plotly)
library(wordcloud)
library(wordcloud2)



movies <- read.csv("movies.csv") # Replace with your own data
movies <- na.omit(movies)
names(movies)
head(movies)
movies$box_office <- as.numeric(gsub("[^0-9]", "", movies$box_office))
movies$budget <- as.numeric(gsub("[^0-9]", "", movies$budget))
movies <- movies[!is.na(as.numeric(as.character(movies$budget))),]
movies <- movies[!is.na(as.numeric(as.character(movies$box_office))),]
movies$genre <- gsub(",.*$", "", movies$genre)
movies[movies$name == "Princess Mononoke",]$budget = movies[movies$name == "Princess Mononoke",]$budget /142
movies[movies$name == "Princess Mononoke",]$box_office = movies[movies$name == "Princess Mononoke",]$box_office / 142

convert_to_minutes <- function(time_string) {
  time_components <- strsplit(time_string, "\\s+")[[1]]
  hours <- as.numeric(gsub("h", "", time_components[1]))
  minutes <- as.numeric(gsub("m", "", time_components[2]))
  total_minutes <- hours * 60 + minutes
  
  return(total_minutes)
}

df <- movies
df$run_time <- sapply(movies$run_time, convert_to_minutes)



grupowane <- aggregate(movies[,c("budget","box_office")], list(movies[,c("year")]), mean)
colnames(grupowane)[1] <- "year"

movies %>% group_by(directors) %>% 
  summarise(observed_sum=n()) %>% 
  arrange(desc(observed_sum)) -> data_wc_directors


as.data.frame(table(unlist(strsplit(movies$casts, ",")))) -> data_wc_casts


ui <- dashboardPage(
  
  dashboardHeader(title = span("Analiza filmów z listy IMDb Top 250",style = "font-size: 12px")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = span("Analiza ze względu na gatunek",style = "font-size: 13px"), tabName = "page1", icon = icon("dashboard")),
      menuItem(text = span("Analiza ze względu na rok produkcji",style = "font-size: 13px"), tabName = "page2", icon = icon("dashboard")),
      menuItem(text = span("Inne ciekawostki",style = "font-size: 13px"), tabName = "page3", icon = icon("dashboard")),
      collapsed = FALSE

    )
  ),
    
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "page1",
              fluidPage(
                  checkboxGroupInput("genre",
                                     "Gatunek filmu",
                                     unique(df$genre),
                                     select = c("Action", "Drama", "Adventure"),
                                     inline = TRUE),
                mainPanel(
                   fluidRow(
                     box(textOutput("output_text1"),width = 4,height = 390),
                      column(8, align="center", offset = 0,style = 'padding-right70px', align = "right",
                             plotOutput("boxPlot", width = 1025,height = 390))
                  ),
                  fluidRow(
                           column(width = 4,div(style = "height:53.5px"),offset = 0,style = 'padding-left:0px',
                                  plotlyOutput("pointPlot", width = 400)),
                           column(width = 4, div(style = "height:53.5px"),offset = 0,style = 'padding-left:150px',
                                  plotlyOutput("pointPlot2", width = 400)),
                           column(width = 4, div(style = "height:53.5px"),offset = 0,style = 'padding-left:300px',
                                  plotlyOutput("pointPlot3", width = 400))
                           )
                )
              )
      ),
      
      tabItem(tabName = "page2",
              fluidPage(
                box(textOutput("output_text2"),width = 10),
                fluidRow(
                  column(10, align="center", offset = 0,
                         plotOutput("yearPlot", width = 1000, height = 700))
                )
              )
      ),
    
      tabItem(tabName = "page3",
            fluidPage(
              box(textOutput("output_text3"),width = 10),
              
              fluidRow(
                column(4, align="center", offset = 0,
                       plotOutput("worldCloud", width = 800, height = 700)),
                column(4, align="center", offset = 0,
                       plotOutput("worldCloud2",width = 800, height = 700))
              )
            )
    )
    )
  ), skin ="purple"
    
)


server <- function(input, output) {

  output$pointPlot <- renderPlotly({
    plot_ly(df %>% 
              filter(df$genre %in% input$genre), 
            x = ~run_time,
            y = ~budget,
            text = ~name,
            hovertemplate = paste('<b>%{text}</b> <br>', 
                                  'Czas trwania: %{x} min <br>',
                                  'Budżet: $%{y}'),
            color = ~genre,
            colors = "Set1") %>% 
      layout(title = "Im dłuższy, tym droższy?", 
             xaxis = list(title = 'Czas trwania filmu [min]', autorange = TRUE), 
             yaxis = list(title = 'Budżet [USD]', autorange = TRUE))
    
  })
  output$pointPlot2 <- renderPlotly({
    plot_ly(df %>% 
              filter(df$genre %in% input$genre), 
            x = ~run_time,
            y = ~box_office,
            text = ~name,
            hovertemplate = paste('<b>%{text}</b> <br>', 
                                  'Czas trwania: %{x} min <br>',
                                  'Box office: $%{y}'),
            color = ~genre,
            colors = "Set1") %>% 
      layout(title = "Im dłuższy, tym więcej wart?", 
             xaxis = list(title = 'Czas trwania filmu [min]', autorange = TRUE), 
             yaxis = list(title = 'Box office [USD]', autorange = TRUE))
  })
  
  output$pointPlot3 <- renderPlotly({
    plot_ly(df %>% 
              filter(df$genre %in% input$genre), 
            x = ~budget,
            y = ~box_office,
            text = ~name,
            hovertemplate = paste('<b>%{text}</b> <br>', 
                                  'Budżet: $%{x} <br>',
                                  'Box office: $%{y}'),
            color = ~genre,
            colors = "Set1") %>% 
      layout(title = "Im więcej włożysz, tym więcej wyjmiesz?",
             xaxis = list(title = 'Budżet [USD]', autorange = TRUE, scaleanchor="x", scaleratio=1), 
             yaxis = list(title = 'Box office [USD]', autorange = TRUE, scaleanchor="x", scaleratio=1))
  })
  
  output$boxPlot <- renderPlot({
    df <- movies %>% 
      filter(#movies$year >= input$rok[1],
             #movies$year <= input$rok[2],
             movies$genre %in% input$genre)
    ggplot(df ,aes(x=genre, y=budget, fill=genre)) +
      geom_boxplot() +
      scale_fill_manual(values=c("#68228B","#CD00CD","#EE1289","#EE0000","#FF4500","#FFFF00",
                                          "#7CFC00","#32CD32","#00BFFF","#0000CD")) +
                                          ylim(c(0,2e+08)) +
      scale_x_discrete(guide = guide_axis(n.dodge = 1, title = "Genre", angle = 40)) +
      labs(title="Rozkład budżetu dla wybranych gatunków filmów",y=("Budget [MM]")) +
      theme_minimal()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            plot.title = element_text(size=14,face = 'plain', hjust = 0.5)) +
      theme(legend.position="bottom")
  })
  output$worldCloud <- renderPlot({
    
    wordcloud(words = data_wc_directors$directors, freq = data_wc_directors$observed_sum, 
              min.freq = 1, scale=c(1,0.125), max.words = 250, random.order = FALSE, rot.per = 0.35,
              colors = c("#ef4b7e", "#3cb9ac", "#ff7e23", "#a4711e", "#82bfec", "#a6207a"))
    
  })
  
  output$worldCloud2 <- renderPlot({

    wordcloud(words = data_wc_casts$Var1, freq = data_wc_casts$Freq,
              min.freq = 1, scale=c(1,0.125), max.words = 250, random.order = FALSE, rot.per = 0.35,
              colors = c("#ef4b7e", "#3cb9ac", "#ff7e23", "#a4711e", "#82bfec", "#a6207a")
    ) 
    
  })
  
  
  output$yearPlot <- renderPlot({

    p1<-ggplot(grupowane, aes(x=year, y=budget)) +
      geom_bar(stat="identity",fill = "red2")+
      ylim(0,2e+09)+
      labs(title ="Średni budżet filmów w poszczególnych latach", x = "lata") +
      theme(plot.title = element_text(size=14,face = 'plain', hjust = 0.5))+
      theme_minimal()
    
    
    p2 <- ggplot(grupowane, aes(x=year, y=box_office)) +
      geom_bar(stat="identity",fill = "red2")+
      ylim(0,2e+09)+
      labs(title ="Średni boxoffice  filmów w poszczególnych latach",x = "lata") +
      theme(plot.title = element_text(size=14,face = 'plain', hjust = 0))+
      theme_minimal()
    
    
    grid.arrange(p1, p2, ncol=2)
    
  })
  
  output$output_text1 <- renderText({
    "International Movie Database (IMDb) jest największą na świecie bazą danych dotyczących filmów. Zawiera informacje m.in. o twórcach i aktorach, kosztach i przychodach, a także zdobytych nagrodach. Dodatkowo, zarejestrowani użytkownicy serwisu mogą oceniać produkcje w skali od 1 do 10. Na podstawie ich głosów tworzona jest lista 250 najlepszych filmów – IMDb Top 250, którą zdecydowaliśmy się wykorzystać w naszym projekcie. Wyniki analizy dostępne są w 3 zakładkach.
    
    
    W tej części analizy skupiliśmy się na wpływie gatunku filmu na różne aspekty związane z jego kosztem i przychodami (ang. box office). 
    Wykresy pokazują dane wyłącznie dla zaznaczonych przez użytkownika gatunków filmów.
    W sytuacji, w której do filmu przypisany był w bazie więcej niż jeden gatunek, braliśmy pod uwagę tylko pierwszy z nich.
    "
    })
  
  output$output_text2 <- renderText({
      "Ta część analizy także dotyczy kosztów i przychodów (ang. box office) filmów, ale tym razem wykresy przedstawiają średni budżet oraz średni box office filmów wyprodukowanych w danym roku."
          })
  
  output$output_text3 <- renderText({
        "W ostatniej części, w ramach urozmaicenia, z tematów finansowych przerzuciliśmy się na osobowe. Postanowiliśmy sprawdzić, czy któreś z nazwisk aktorów i reżyserów przewijają w pierwszej 250 filmów częściej niż pozostałe. Odpowiedź na to pytanie zawierają poniższe wordcloudy."
          })
  
}

# Run the app
shinyApp(ui = ui, server = server)
