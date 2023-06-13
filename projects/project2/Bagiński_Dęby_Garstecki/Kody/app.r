#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(syuzhet)
library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggtern)
library(shinythemes)
library(lubridate)
library(ggthemes)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(wordcloud2)
library(shinyWidgets)
library(shinyjs)

library(devtools)
library(shinycssloaders)
library(bslib)
library(stringr)
df1<-read.csv("haripotah.csv")


movies <- c("Harry Potter and the Philosopher's Stone",
            "Harry Potter and the Chamber of Secrets",
            "Harry Potter and the Prisoner of Azkaban",
            "Harry Potter and the Gobelt of Fire",
            "Harry Potter and the Order of the Phoenix",
            "Harry Potter and the Half-Blood Prince",
            "Harry Potter and the Deathly Hallows Part 1",
            "Harry Potter and the Deathly Hallows Part 2")
names(movies) <- c("Philosopher's Stone",
                   "Chamber of Secrets",
                   "Prisoner of Azkaban",
                   "Gobelt of Fire",
                   "Order of the Phoenix",
                   "Half-Blood Prince",
                   "Deathly Hallows Part 1",
                   "Deathly Hallows Part 2")

zaklecia<-c("Expecto Patronum", "Accio", "Expelliarmus", "Stupefy", "Avada Kedavra",
            "Lumos", "Riddikulus", "Crucio", "Impedimenta", "Reparo", "Alohomora", 
            "Petrificus Totalus", "Wingardium Leviosa", "Sectumsempra", "Obliviate", 
            "Protego", "Imperio", "Rictusempra", "Nox", "Aguamenti", "Bombarda", 
            "Confringo", "Diffindo", "Engorgio", "Episkey", "Expulso", 
            "Finite Incantatem", "Immobulus", "Incendio", "Levicorpus", "Locomotor",
            'Muffliato', "Oppugno", "Portus", "Reducio", "Relashio", "Repello Muggletum",
            "Salvio Hexia")



Caly <- df1

#### Sprawdzmy ile i jakich czarów było rzucone w całym filmie ####

ExpectoPatronum <- Caly %>% 
  filter(grepl('Expecto Patronum', dialog)) %>% 
  mutate(Spell = 'Expecto Patronum') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Accio <- Caly %>% 
  filter(grepl('Accio', dialog)) %>% 
  mutate(Spell = 'Accio') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Expelliarmus <- Caly %>% 
  filter(grepl('Expelliarmus', dialog)) %>% 
  mutate(Spell = 'Expelliarmus') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Stupefy <- Caly %>% 
  filter(grepl('Stupefy', dialog)) %>% 
  mutate(Spell = 'Stupefy') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

AvadaKedavra <- Caly %>% 
  filter(grepl('Avada Kedavra', dialog)) %>% 
  mutate(Spell = 'Avada Kedavra') %>% 
  mutate(Unforgivable = 'Yes') %>% 
  mutate(Battle = 'Yes')

Lumos <- Caly %>% 
  filter(grepl('Lumos', dialog)) %>% 
  mutate(Spell = 'Lumos') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Riddikulus <- Caly %>% 
  filter(grepl('Riddikulus', dialog)) %>% 
  mutate(Spell = 'Riddikulus') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Crucio <- Caly %>% 
  filter(grepl('Crucio', dialog)) %>% 
  mutate(Spell = 'Crucio') %>% 
  mutate(Unforgivable = 'Yes') %>% 
  mutate(Battle = 'Yes')

Impedimenta <- Caly %>% 
  filter(grepl('Impedimenta', dialog)) %>% 
  mutate(Spell = 'Impedimenta') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Reparo <- Caly %>% 
  filter(grepl('Reparo', dialog)) %>% 
  mutate(Spell = 'Reparo') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Alohomora <- Caly %>% 
  filter(grepl('Alohomora', dialog)) %>% 
  mutate(Spell = 'Alohomora') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

PetrificusTotalus <- Caly %>% 
  filter(grepl('Petrificus Totalus', dialog)) %>% 
  mutate(Spell = 'Petrificus Totalus') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

WingardiumLeviosa <-  Caly %>% 
  filter(grepl('Wingardium Leviosa', dialog)) %>% 
  mutate(Spell = 'Wingardium Leviosa') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Sectumsempra <- Caly %>% 
  filter(grepl('Sectumsempra', dialog)) %>% 
  mutate(Spell = 'Sectumsempra') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Obliviate <- Caly %>% 
  filter(grepl('Obliviate', dialog)) %>% 
  mutate(Spell = 'Obliviate') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Protego <- Caly %>% 
  filter(grepl('Protego', dialog)) %>% 
  mutate(Spell = 'Protego') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Imperio <- Caly %>% 
  filter(grepl('Imperio', dialog)) %>% 
  mutate(Spell = 'Imperio') %>% 
  mutate(Unforgivable = 'Yes') %>% 
  mutate(Battle = 'Yes')

Rictusempra <- Caly %>% 
  filter(grepl('Rictusempra', dialog)) %>% 
  mutate(Spell = 'Rictusempra') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Nox <- Caly %>% 
  filter(grepl('Nox', dialog)) %>% 
  mutate(Spell = 'Nox') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Aguamenti <- Caly %>% 
  filter(grepl('Aguamenti', dialog)) %>% 
  mutate(Spell = 'Aguamenti') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Bombarda <- Caly %>% 
  filter(grepl('Bombarda', dialog)) %>% 
  mutate(Spell = 'Bombarda') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Confringo <- Caly %>% 
  filter(grepl('Confringo', dialog)) %>% 
  mutate(Spell = 'Confringo') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Diffindo <- Caly %>% 
  filter(grepl('Diffindo', dialog)) %>% 
  mutate(Spell = 'Diffindo') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Engorgio <- Caly %>% 
  filter(grepl('Engorgio', dialog)) %>% 
  mutate(Spell = 'Engorgio') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No') 

Episkey <-  Caly %>% 
  filter(grepl('Episkey', dialog)) %>% 
  mutate(Spell = 'Episkey') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Expulso <-  Caly %>% 
  filter(grepl('Expulso', dialog)) %>% 
  mutate(Spell = 'Expulso') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

FiniteIncantatem <- Caly %>% 
  filter(grepl('Finite Incantatem', dialog)) %>% 
  mutate(Spell = 'Finite Incantatem') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Immobulus <- Caly %>% 
  filter(grepl('Immobulus', dialog)) %>% 
  mutate(Spell = 'Immobulus') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Incendio <- Caly %>% 
  filter(grepl('Incendio', dialog)) %>% 
  mutate(Spell = 'Incendio') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Levicorpus <- Caly %>% 
  filter(grepl('Levicorpus', dialog)) %>% 
  mutate(Spell = 'Levicorpus') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Locomotor <- Caly %>% 
  filter(grepl('Locomotor', dialog)) %>% 
  mutate(Spell = 'Locomotor') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Muffliato <- Caly %>% 
  filter(grepl('Muffliato', dialog)) %>% 
  mutate(Spell = 'Muffliato') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Oppugno <- Caly %>% 
  filter(grepl('Oppugno', dialog)) %>% 
  mutate(Spell = 'Oppugno') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Portus <- Caly %>% 
  filter(grepl('Portus', dialog)) %>% 
  mutate(Spell = 'Portus') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Reducio <- Caly %>% 
  filter(grepl('Reducio', dialog)) %>% 
  mutate(Spell = 'Reducio') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

Relashio <- Caly %>% 
  filter(grepl('Relashio', dialog)) %>% 
  mutate(Spell = 'Relashio') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

RepelloMuggletum <- Caly %>% 
  filter(grepl('Repello Muggletum', dialog)) %>% 
  mutate(Spell = 'Repello Muggletum') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'No')

SalvioHexia <- Caly %>% 
  filter(grepl('Salvio Hexia', dialog)) %>% 
  mutate(Spell = 'Salvio Hexia') %>% 
  mutate(Unforgivable = 'No') %>% 
  mutate(Battle = 'Yes')

Spells <- rbind(ExpectoPatronum, Accio, Expelliarmus, Stupefy, AvadaKedavra,
                Lumos, Riddikulus, Crucio, Impedimenta, Reparo, Alohomora, 
                PetrificusTotalus, WingardiumLeviosa, Sectumsempra, Obliviate, 
                Protego, Imperio, Rictusempra, Nox, Aguamenti, Bombarda, 
                Confringo, Diffindo, Engorgio, Episkey, Expulso, 
                FiniteIncantatem, Immobulus, Incendio, Levicorpus, Locomotor,
                Muffliato, Oppugno, Portus, Reducio, Relashio, RepelloMuggletum,
                SalvioHexia)

SpellsBetter <- Spells

SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Philosopher's Stone"] <- "Philosopher's Stone"
SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Chamber of Secrets"] <- "Chamber of Secrets"
SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Prisoner of Azkaban"] <- "Prisoner of Azkaban"
SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Gobelt of Fire"] <- "Gobelt of Fire"
SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Half-Blood Prince"] <- "Half-Blood Prince"
SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Order of the Phoenix"] <- "Order of the Phoenix"
SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Deathly Hallows Part 1"] <- "Deathly Hallows Part 1"
SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Deathly Hallows Part 2"] <- "Deathly Hallows Part 2"

Spell <- c("Expecto Patronum", "Accio", "Expelliarmus", "Stupefy", "Avada Kedavra",
           "Lumos", "Riddikulus", "Crucio", "Impedimenta", "Reparo", "Alohomora", 
           "Petrificus Totalus", "Wingardium Leviosa", "Sectumsempra", "Obliviate", 
           "Protego", "Imperio", "Rictusempra", "Nox", "Aguamenti", "Bombarda", 
           "Confringo", "Diffindo", "Engorgio", "Episkey", "Expulso", 
           "Finite Incantatem", "Immobulus", "Incendio", "Levicorpus", "Locomotor",
           "Muffliato", "Oppugno", "Portus", "Reducio", "Relashio", "Repello Muggletum",
           "Salvio Hexia")

Description <- c("Channel the caster's positive emotions into a powerful protection and evoke an energy force known as a Patronus",
                 "Summon an object toward the caster",
                 "Force whatever an opponent was holding to fly out of their hand",
                 "Stun the target, rendering them unconscious",
                 "Cause instantaneous and painless death",
                 "Illuminate the tip of the caster's wand",
                 "Cause the Boggart to assume a form that was humorous to the caster",
                 "Inflict intense, excruciating physical pain on the victim",
                 "Hinder the movement of the target, slowing it down or stopping it in its tracks",
                 "Seamlessly repair a broken object",
                 "Unlock objects such as doors or chests",
                 "Temporarily paralysed the opponent",
                 "Make objects fly, or levitate",
                 "Lacerate the target and causes severe haemorrhaging",
                 "Erase specific memories from an individual's mind",
                 "Protect the caster with an invisible shield that reflected spells and blocked physical entities",
                 "Place the victim completely under the caster's control, making the victim unquestionably obedient to the caster",
                 "Cause the target to buckle with laughter, weakening them",
                 "Cause the light at the end of the caster's wand to be extinguished",
                 "Conjure a jet of clean, drinkable water from the tip of the caster's wand",
                 "Detonate the target in a small explosion",
                 "Cause the target to explode",
                 "Precisely and accurately cut something",
                 "Cause the target to swell immensely",
                 "Heal relatively minor injuries such as broken noses, toes and split lips",
                 "Produce immense explosions, blasting the target apart with a burst of blue light",
                 "Counter or reverse minor damage",
                 "Immobilise and stopped the actions of the target",
                 "Conjure a jet of flames that could be used to set things alight",
                 "Cause the victim to be hoisted into the air by their ankle",
                 "Affect the movement of the target of the spell",
                 "Fill the ears of any person in the vicinity of the caster with an unidentifiable buzzing sound",
                 "Direct an object or individual to attack the victim",
                 "Turn an ordinary object into a Portkey",
                 "Cause an object to shrink",
                 "Force the target to release its grip on whatever it was holding",
                 "Prevent Muggles from seeing or entering an area",
                 "Deflecte hexes from the area")

OPISY <- as.data.frame(cbind(Spell, Description))



most_said <- c("Harry Potter", "Albus Dumbledore", "Hermione Granger", "Ron Weasley",
               "Rubeus Hagrid", "Severus Snape", "Minerva McGonagall",
               "Remus Lupin", "Voldemort")
# Define UI for application that draws a histogram
ui <- fluidPage(setBackgroundColor("#FAD7A0"),
                theme = shinythemes::shinytheme("sandstone"),
                    setBackgroundImage(src = "karta.webp", shinydashboard = FALSE),
                    useShinyjs(),
                 navlistPanel(fluid=T,widths = c(3,12),
                      
                     
                            tabPanel("Word analysis",
                                     selectInput(inputId = 'Chapter',label= 'Select Movie:', movies),
                                      selectInput(inputId = 'Character',label= 'Select Character:', most_said),
                                     fluidRow(
                                       column(width =2,uiOutput("dataImage")),
                                       column(width =2,uiOutput("dataImage2"))),
                            
                            fluidRow(column(width=10,wordcloud2Output("plot",width = "100%"))),
                            fluidRow(column(width=10,withSpinner(plotlyOutput("plot2")))),
                             ),
                    
                    
                            tabPanel("Dialog analysis",
                                     withSpinner(plotlyOutput('plot1')),
                                     selectInput(inputId = 'Charact', label = "Select the character you'd like to see the boxplot of:", most_said),
                                     fluidRow(column(width=10,withSpinner(plotlyOutput("plot10"))))                             ),
                            tabPanel("Spell analysis",
                                     selectInput(inputId = 'spell',label= 'Select Spell:', zaklecia),
                                     textOutput("text"),
                                     tags$head(tags$style("#text{color: #463100;
                                 font-size: 30px;
                                 font-style: italic;
                                 }"
                                     )
                                     ),
                                     withSpinner(plotlyOutput('plot3')),
                                     withSpinner(plotlyOutput('plot4')))))


# Define server logic required to draw a histogram
server <- function(input, output) {
  complete_harrys_len <- df1 %>% mutate(dialog = nchar(dialog)) 
  
  complete_harrys_len$movie <- gsub("Harry Potter and the ", '', complete_harrys_len$movie)
  movie_order <- c("Philosopher's Stone",
                   "Chamber of Secrets",
                   "Prisoner of Azkaban",
                   "Gobelt of Fire",
                   "Order of the Phoenix",
                   "Half-Blood Prince",
                   "Deathly Hallows Part 1",
                   "Deathly Hallows Part 2")
  SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Philosopher's Stone"] <- "Philosopher's Stone"
  SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Chamber of Secrets"] <- "Chamber of Secrets"
  SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Prisoner of Azkaban"] <- "Prisoner of Azkaban"
  SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Gobelt of Fire"] <- "Gobelt of Fire"
  SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Half-Blood Prince"] <- "Half-Blood Prince"
  SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Order of the Phoenix"] <- "Order of the Phoenix"
  SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Deathly Hallows Part 1"] <- "Deathly Hallows Part 1"
  SpellsBetter$movie[SpellsBetter$movie %in% "Harry Potter and the Deathly Hallows Part 2"] <- "Deathly Hallows Part 2"
  
  
  
  
  
  complete_harrys_len$movie <- factor(complete_harrys_len$movie, levels = movie_order)
  
  SpellsBetter$movie <- factor(SpellsBetter$movie, levels = movie_order)
  
  complete_harrys_len %>%
    group_by(character) %>% summarise(dialog = sum(dialog)) %>% arrange(desc(dialog)) %>% head(10) %>% 
    select(character) -> most_said
  
  most_said <- c("Harry Potter", "Albus Dumbledore", "Hermione Granger", "Ron Weasley",
                 "Rubeus Hagrid", "Severus Snape", "Minerva McGonagall",
                 "Remus Lupin", "Voldemort")
  
  
  avg_dialog <- complete_harrys_len %>% filter(character %in% most_said) %>%
    group_by(movie, character) %>% summarise(avarage = mean(dialog)) %>% 
    arrange(desc(avarage)) 
  
  ggplot_plot <- ggplot(avg_dialog, aes(x = character, y = avarage, color = movie)) +
    geom_jitter(width = 0.2, height = 0, size = 3) +
    labs(x = "Character", y = "Average Dialog Length", title = "Average Dialog Length by Character and Movie", 
         color = "Movie: ") +
    theme_minimal() + scale_color_manual(values = c(
      "Philosopher's Stone" = "#463100",
      "Chamber of Secrets" = "#8c98da",
      "Prisoner of Azkaban" = "#37592b",
      "Goblet of Fire" = "#291002",
      "Order of the Phoenix" = "#2a6641",
      "Half-Blood Prince" = "#ff9ec7",
      "Deathly Hallows Part 1" = "#cd9ad9",
      "Deathly Hallows Part 2" = "#008aa2"
    )) +
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent')
    )
  
  
  # Convert the ggplot2 plot to a plotly plot
  plotly_plot <- ggplotly(ggplot_plot) %>% layout(xaxis = list(tickangle = 20))
  output$plot1<-renderPlotly({plotly_plot})
  
 # "#2a6641"  
  
  
  observe({
    colors<-c("#463100", "#443f09", "#37592b", "#291002", "#2a6641", "#077462", "#008083", "#008aa2", "#4894c6", "#8c98da", "#cd9ad9", "#ff9ec7");
    
    
    zakl <- toString(input$spell)
    
    output$text<- renderText({with(OPISY%>%filter(Spell==zakl),paste(Description, sep="\n"))})
    
    plot3 <- Spells%>%filter(Spell==zakl)%>%
      group_by(character) %>%
      summarise(liczba = n()) %>%
      as.data.frame() %>%
      arrange(-liczba) %>%
      ggplot(aes(x = fct_inorder(character), y = liczba, fill = character))+
      geom_col() +
      
      labs(title = "Number of spell uses by individual characters",
           x = "", y = "") +
      
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),

        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
      ) +scale_fill_manual(values=colors)

    
    plot4 <- SpellsBetter%>%filter(Spell==zakl)%>%
      count(movie, .drop =F,name="liczba")%>%
      
      as.data.frame() %>%
      
      ggplot(aes(x = fct_inorder(movie), y = liczba, fill = movie))+
      geom_col() +
      
      labs(title = "The number of uses of the spell in each film",
           x = "", y = "") +
      
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
      ) + scale_fill_manual(values = c(
        "Philosopher's Stone" = "#463100",
        "Chamber of Secrets" = "#8c98da",
        "Prisoner of Azkaban" = "#37592b",
        "Goblet of Fire" = "#291002",
        "Order of the Phoenix" = "#2a6641",
        "Half-Blood Prince" = "#ff9ec7",
        "Deathly Hallows Part 1" = "#cd9ad9",
        "Deathly Hallows Part 2" = "#008aa2"
      ))
    
    
    plot3 <- ggplotly(plot3) %>% layout(xaxis = list(tickangle = 20))
    output$plot3 <- renderPlotly({plot3})
    plot4 <- ggplotly(plot4) %>% layout(xaxis = list(tickangle = 20))
    output$plot4 <- renderPlotly({plot4})
    
    
    
    charact1 <- toString(input$Charact)
    char_film <- complete_harrys_len %>% filter(character == charact1)
    
    plot100 <- ggplot(char_film, aes(x = movie, y = dialog,fill = movie)) + geom_boxplot( ) +
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
      ) + labs(title = 'The lengths of dialog in each movie', y = "Dialog", x = "Movie")+scale_fill_manual(values = c(
        "Philosopher's Stone" = "#463100",
        "Chamber of Secrets" = "#8c98da",
        "Prisoner of Azkaban" = "#37592b",
        "Goblet of Fire" = "#291002",
        "Order of the Phoenix" = "#2a6641",
        "Half-Blood Prince" = "#ff9ec7",
        "Deathly Hallows Part 1" = "#cd9ad9",
        "Deathly Hallows Part 2" = "#008aa2"
      ))
    plot10 <- ggplotly(plot100) %>% layout(xaxis = list(tickangle = 20))
    output$plot10 <- renderPlotly({plot10})
    
    
    
    output$plot1<-renderPlotly({plotly_plot})
    origin <- toString(input$Chapter)
    
    
    selected_book <- toString(input$Character)
    
    output$dataImage<-renderUI({
      img(src=paste(gsub(" ","",origin),".jpg",sep = ""), height = '150px')
    })
    output$dataImage2<-renderUI({
      img(src=paste(gsub(" ","",selected_book),".jpg",sep = ""), height = '150px')
    })
    
    baza <- df1 %>%filter(movie==origin,character==selected_book)
    
    tekst<- paste(baza%>%select(dialog))
    d<-get_nrc_sentiment(tekst)
    d<-d%>%pivot_longer(cols = everything(),names_to="sentiment",values_to="count")
    
    
    
    
    
    output$plot2 <- renderPlotly({ggplot(d,aes(x=sentiment,y=count, fill = sentiment))+geom_col()+
        theme_minimal() + labs(x = "Sentiment", y = "Number of appearance", title = "Presence of emotions in text for a given movie and character")+
        theme(
          legend.position="none",
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent')
        )+scale_fill_manual(values=colors)
    })
    docs <- Corpus(VectorSource(paste(baza%>%select(dialog))))
    docs <- docs %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace)
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df2 <- data.frame(word = names(words),freq=words)%>% mutate_if(is.character, ~gsub('[^ -~]', '', .))
    
    
    output$plot <- renderWordcloud2({
      
      wordcloud2(data = df2, color = rep_len(colors,100),fontFamily = 'Segoe UI',size = 0.5,
                 backgroundColor= "transparent")
                })
     # backgroundColor = "#FAD7A0")
  })
  
  
  # output$text <- renderText({
  #   paste("Aplikacja zawiera wstępną analizę zbioru danych o pingwinach.", 
  #         "W zbiorze danych jest", nrow(penguins), "pingwinów.", "Najmniejszy waży", min(na.omit(penguins['body_mass_g'])), "gramów!")
  # })
  
  # output$pointPlot <- renderPlotly({
  #   print(input$year)
  #   png <- penguins[penguins$species %in% input$specie,]
  #   png <- png [png$year == input$year, ]
  #   plot_ly(
  #     data = png, 
  #     x = ~flipper_length_mm, 
  #     y = ~body_mass_g,
  #     color = ~species,
  #     colors = "Set1"
  #   )%>%
  #     layout(
  #       xaxis = list(
  #         range=c(170,240)
  #       ),
  #       yaxis = list(
  #         range=c(2500,6500)
  #       )
  #     )
  # })
  
  # output$histPlot <- renderPlotly({
  #   peng <- as.data.frame(na.omit(penguins[penguins$species %in% input$specie,]))
  #   ggplot(peng, aes_string(x = input$colname, fill = "species"))+
  #     geom_histogram()
  # })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
