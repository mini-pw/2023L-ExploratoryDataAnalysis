library(grid)
library(shiny)
library(stringr)
library(dplyr)
library(ggplot2)
library(bslib)
library(tidyverse)
library(cli)
library(shinyWidgets)
library(plotly)



dane_1 <-read.csv("movies.csv")
#na.omit(dane_1)
gatunki<-str_split_fixed(dane_1$genres, '-', 16)
a<-na.omit(unique(as.vector( gatunki)))
a<-a[-20]
b<-dane_1 %>% 
  mutate(Rok = as.numeric(str_sub(release_date, 1, 4)))
MoviesOnStreamingPlatforms <-  read.csv("MoviesOnStreamingPlatforms.csv") %>% 
  select('Title', 'Netflix', 'Hulu', 'Prime.Video', 'Disney.')

Checkmarks <- MoviesOnStreamingPlatforms %>% 
  transmute(Title = Title,
            Netflix = ifelse(Netflix == 1,paste(print(symbol$tick)),paste(print(symbol$cross))),
            Hulu = ifelse(Hulu == 1,paste(print(symbol$tick)),paste(print(symbol$cross))),
            Prime.Video = ifelse(Prime.Video == 1,paste(print(symbol$tick)),paste(print(symbol$cross))),
            Disney. = ifelse(Disney. == 1,paste(print(symbol$tick)),paste(print(symbol$cross)))
            )

polaczone<-dane_1 %>% inner_join(MoviesOnStreamingPlatforms,by=c('title'='Title')) %>% mutate(Rok = as.numeric(str_sub(release_date, 1, 4)))


z_wektorem<- polaczone%>% mutate(Netflix=ifelse(Netflix==1,'Netflix',''))
z_wektorem<- z_wektorem%>% mutate(Hulu=ifelse(Hulu==1,'Hulu',''))
z_wektorem<- z_wektorem%>% mutate(Prime.Video=ifelse(Prime.Video==1,'Prime Video',''))
z_wektorem<- z_wektorem%>% mutate(Disney.=ifelse(Disney.==1,'Disney ',''))
z_wektorem<- z_wektorem %>% mutate(wszystkie=paste0(Netflix,Hulu,Prime.Video,Disney.))
firma<-c('Netflix','Hulu','Prime Video','Disney')

ui1 <- fluidPage(
  tags$style(HTML("
    .tabbable > .nav > li > a {  color:black; }
  ")),
  
  titlePanel("Movies analysis"),
  p(style="text-align: justify;","Have you ever wondered which films produced the most revenue across different genres?"),
  p(style="text-align: justify;","Or maybe you wanted to know how many good comedies premiered lately?"),
  p(style="text-align: justify;","Find your answers using graphs below!"),
  
  tags$hr(style="border-color: black;"),
  setSliderColor(c("#1DB954 ", "#1DB954", "#1DB954", "#1DB954", "#1DB954", "#1DB954", "#1DB954"), c(1, 2, 3,4,5,6,7)),
      fluidRow(
        column(5,
        selectInput("gatunek",
                    "Choose genre:",
                    na.omit(unique(as.vector(str_split_fixed(dane_1$genres, '-', 16))))),
        sliderInput("zakres_1",
                    "Time interval",
                    value = c(min(b$Rok, na.rm=TRUE), 2023),
                    min = min(b$Rok, na.rm=TRUE),
                    max = 2023,
                    step = 1,
                    sep="")
        )
   ,
    column(6,
           selectInput("Gatunek",
                       "Choose genre:",
                       a),
           sliderInput("zakres",
                       "Time interval",
                       value = c(min(b$Rok, na.rm=TRUE), 2023),
                       min = min(b$Rok, na.rm=TRUE),
                       max = 2023,
                       step = 1,
                       sep="")
    ))
    ,
  fluidRow(column(5,
                  plotOutput("gatunek")),
           column(6,
                  plotOutput("wykresPunktowy")
           )         
    
  ),
  )

ui2<-fluidPage(
  tags$style(HTML("
    .tabbable > .nav > li > a {  color:black; }
  ")),
  
  titlePanel("Discover what streaming platform suits you the most!"),
  p(style="text-align: justify;","Would you like to watch some movies but there are so many different streaming platforms to choose from?"),
  p(style="text-align: justify;","Don't worry, with our platform browser you can choose your settings and demands and we will tell you what you should buy!"),
  p(style="text-align: justify;","In addition you can also check which platforms, on average, have the best quality of content that you prefer."),
  p(style="text-align: justify;","Or maybe you are looking for a specific movie you really want to watch?"),
  p(style="text-align: justify;","Find where you can watch it by using our searcher at the bottom of the page."),
  tags$hr(style="border-color: black;"),
  fluidRow(
    column(3,
           plotOutput("wyszukiwarka",width='90%')),
   
      column(4,plotlyOutput("boxplotplatforms",width='90%'))
    
           ,
           
    column(5,
           selectizeInput("Gatunek_2",
                          "Choose genres:",
                          a,selected = a[1],multiple=TRUE),
           sliderInput("zakres_2",
                       "Choose time interval",
                       value = c(min(b$Rok, na.rm=TRUE), 2023),
                       min = min(b$Rok, na.rm=TRUE),
                       max = 2023,
                       step = 1,
                       sep=""),
           sliderInput("ocena",
                       "Minimal rating",
                       value = c(5,10),
                       min =0,
                       max = 10),
          
    ),
    
           tableOutput("ceny"),width = 6
    ),
  
  fluidRow(
    column(3,
           selectizeInput("Title",
                          "Choose movies/shows:",
                          unique(MoviesOnStreamingPlatforms$Title),multiple=TRUE)),
    fluidRow(
                  tableOutput("tabela"),width = 6
  
  
    
    )
  ))
ui3<-fluidPage(
  tags$style(HTML("
    .tabbable > .nav > li > a {color:black;}
  ")),
  
  titlePanel("Top 10 blockbusters!"),
  p(style="text-align: justify;","On this tab you can check what are the top 10 movies from given genre, based on their availability, rankings threshhold and time of premiere."),
  p(style="text-align: justify;","Additionally you can choose whether to sort by popularity or average rating."),
  tags$hr(style="border-color: black;"),
  fluidRow(
    column(5,selectInput("Gatunek_3",
                         "Choose genre:",
                         a),
           checkboxGroupInput("Platformy",
                              "Choose platforms",
                              firma ,selected = firma[1]),
           sliderInput("zakres_3",
                       "Choose time interval",
                       value = c(min(b$Rok, na.rm=TRUE), 2023),
                       min = min(b$Rok, na.rm=TRUE),
                       max = 2023,
                       step = 1,
                       sep=""),
           sliderInput("ocena_1",
                       "Minimal rating",
                       value = c(5,10),
                       min =0,
                       max = 10),
           selectizeInput("sortowanie",
                              "Sorting by",
                              c("Popularity", "Average_rating"))),
    column(6,
           tableOutput("topowe"))),
  )

server <- function(input, output,session) {
  output$tabela <- renderTable({
    
    table <- Checkmarks %>% 
      filter(Title %in% input$Title)
    colnames(table) <- c("Title", "Netflix", "Hulu", "Prime Video", "Disney+")
    
    table
    
  }, rownames = TRUE)
  output$gatunek <- renderPlot({
    
    wykres <- b %>% 
      filter(str_detect(genres, input$gatunek))   %>% 
      filter(Rok >= input$zakres_1[1] & Rok <= input$zakres_1[2]) %>% arrange(-revenue)%>% arrange(-revenue) %>% head(5)
    wykres<-ggplot(wykres, aes(x=title, y=revenue))+
      theme_bw()+geom_col(fill = '#1DB954') + 
      labs(title = paste0(input$gatunek, " movies with the most produced revenue"),
           x = 'Title', y = "Revenue") +
      theme(plot.title = element_text(hjust = 0.5,size=20,face="bold"),
            axis.title = element_text(hjust = 0.5,size=15,face="bold"),axis.text=element_text(size=15)) +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      scale_y_continuous(labels = scales::comma) 
    wykres
  })
  output$wykresPunktowy <- renderPlot({
    
    
    
    dane1 <- dane_1 %>% 
      mutate(bruh = str_detect(genres,input$Gatunek)) %>% 
      filter(bruh == 1) %>% 
      mutate(Rok = as.numeric(str_sub(release_date, 1, 4))) %>% 
      filter(Rok >= input$zakres[1] & Rok <= input$zakres[2]) %>% 
      mutate(Ocena = case_when(vote_average < 2 ~ "0-1.99",
                               vote_average >= 2 & vote_average < 4 ~ "2-3.99",
                               vote_average >= 4 & vote_average < 6 ~ "4-5.99",
                               vote_average >= 6 & vote_average < 8 ~ "6-7.99",
                               vote_average >= 8 ~ "8-10")) %>% 
      group_by(Ocena) %>% 
      summarise(n = n()) %>%
      mutate(Procent = (n/sum(n))*100) %>% 
      ggplot(aes(x = factor(Ocena, levels = c("0-1.99", "2-3.99", "4-5.99", "6-7.99", "8-10")), y = Procent)) + theme_bw()+
      geom_col(fill = "#1DB954") +
      geom_text(aes(label = round(Procent)), vjust = 1.5, colour = "black",size = 10)+
      labs(title = paste0("Distribution of ",input$Gatunek," movies based on their rating"), x = "Rating", y = "Percentage") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold",size=20)) +
      theme(legend.title = element_text(color = "black", size = 20),
            axis.text = element_text(color = "black",size=15),
            axis.title = element_text(face = "bold",size = 15))
    
    dane1
    
  })
  output$wyszukiwarka<-renderPlot({
    do_wykresu<-polaczone  %>%  filter(Rok >= input$zakres_2[1] & Rok <= input$zakres_2[2]) %>%
      filter(vote_average >= input$ocena[1] & vote_average <= input$ocena[2])
    indeks<-numeric(nrow(do_wykresu))
    do_wykresu<-cbind(do_wykresu,indeks)
    for(i in 1:length(input$Gatunek_2)){
      do_wykresu<-do_wykresu %>% mutate(indeks=indeks+str_detect(genres, input$Gatunek_2[i])) 
    }
    do_wykresu<-do_wykresu %>%  filter(indeks>0)
    
    netflix<-do_wykresu %>% filter(Netflix != 0)
    netflix<-nrow(netflix)
    
    hulu<-do_wykresu %>% filter(Hulu != 0)
    hulu<-nrow(hulu)
    
    prime_video<-do_wykresu %>% filter(Prime.Video != 0)
    prime_video<-nrow(prime_video)
    
    disney<-do_wykresu %>% filter(Disney. != 0)
    disney<-nrow(disney)
    ceny<-c(15.49,12.99,8.99,13.99)
    
    netflix_r<-netflix*(1-(ceny[1]/(6*max(ceny))))
    hulu_r<-hulu*(1-(ceny[2]/(6*max(ceny))))
    prime_video_r<-prime_video*(1-(ceny[3]/(6*max(ceny))))
    disney_r<-disney*(1-(ceny[4]/(6*max(ceny))))
    
    ocena<-c(netflix_r,hulu_r,prime_video_r,disney_r)
    firma<-c('Netflix','Hulu','Prime Video','Disney')
    df<-data.frame(Platform=firma, Evaluation=ocena) %>%
      mutate(Percentage=Evaluation/max(Evaluation)*100)
    
ggplot(df, aes(x=Platform,y=Percentage))+geom_col(fill='#1DB954')+ theme_bw()+
      labs(title = 'Recommendation level based on your choices')+
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      theme(plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.text = element_text(color = "black", size =15),
            axis.title = element_text(face = "bold",size = 15))

     })
output$ceny <- renderTable({
  Price<-c('15.49$','12.99$','8.99$','13.99$')
  Platform <- c('Netflix', 'Hulu', 'Prime.Video', 'Disney.')
  data.frame(Platform, Price)
})  
  output$topowe<-renderTable({
  indeks<-numeric(nrow(z_wektorem))
  do_tabeli<-cbind(z_wektorem,indeks)
  for(i in 1:length(input$Platformy)){
    do_tabeli<-do_tabeli %>% mutate(indeks=indeks+str_detect(wszystkie, input$Platformy[i])) 
  }
  do_tabeli<-do_tabeli %>%  filter(indeks>0) %>% filter(Rok >= input$zakres_3[1] & Rok <= input$zakres_3[2]) %>%
    filter(vote_average >= input$ocena_1[1] & vote_average <= input$ocena_1[2])%>% 
    mutate(bruh = str_detect(genres,input$Gatunek_3)) %>% 
    filter(bruh == 1) %>% select(title,vote_average,popularity)
    ifelse(input$sortowanie=='Popularity',do_tabeli<-do_tabeli %>%arrange(-popularity),do_tabeli<-do_tabeli %>%arrange(-vote_average))
  do_tabeli<-do_tabeli %>%  mutate(Average_rating = vote_average, Popularity = popularity, Title = title) %>% select('Title','Average_rating','Popularity') %>% 
    head(10)
  do_tabeli
})
  output$boxplotplatforms <- renderPlotly({
    polaczone_1<-polaczone  %>%  filter(Rok >= input$zakres_2[1] & Rok <= input$zakres_2[2])
    
    indeks_1<-numeric(nrow(polaczone_1))
    polaczone_1<-cbind(polaczone_1,indeks_1)
    for(i in 1:length(input$Gatunek_2)){
      polaczone_1<-polaczone_1 %>% mutate(indeks_1=indeks_1+str_detect(genres, input$Gatunek_2[i])) 
    }
    polaczone_1<-polaczone_1 %>%  filter(indeks_1>0)
    
    df.r <- do.call(rbind, lapply(names(polaczone_1)[21:24], function(x) 
      data.frame(x, y =polaczone_1$vote_average[as.logical(unlist(polaczone_1[,x], 
                                                                use.names = FALSE))])))
    
    plot_ly(df.r, x = ~x, y = ~y, type = "box", color=I("#1DB954")) %>% 
      layout(title = list(text = '<b>Ratings on different services</b>',font=20), xaxis = list(title = list(text='<b>Platforms</b>',font=15)), 
                                                                               yaxis = list(title = list(text='<b>Ratings</b>',font=15)))
  })
  }


app_ui <- navbarPage(
  title = "Platform web - all about movies",
  tabPanel("General", ui1),
  tabPanel("Streaming platforms", ui2),
  tabPanel("Movie searcher", ui3),
  tags$head(tags$style(HTML('.navbar-static-top {background-color: #1DB954;}',
                            '.navbar-default .navbar-nav>.active>a {background-color: #1DB954;}',
                            '.navbar-default .navbar-brand {color: #000000;}',
                            ".navbar-default .navbar-nav>li>a {color: black; font-weight: bold" )))
  
)
   
  
shinyApp(app_ui, server = server)




