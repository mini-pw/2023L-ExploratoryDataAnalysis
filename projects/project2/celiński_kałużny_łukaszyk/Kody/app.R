library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(tidyr)
library(stringr)
library("reshape2")

# UWAGA!!!
# wszystkie ścieżki do plików muszą być właściwe z tym co jest folderze "eksploracja-projekt-2"

wykresowa <- read.csv("ramki_danych/paczka_ramek_do_obrobki/finalna_git.csv")
wykresowa <- wykresowa %>% 
  mutate(category = factor(category, levels = unique(category))) %>% 
  mutate(winner = as.logical(winner))

x1 <- read.csv("ramki_danych/paczka_ramek_do_obrobki/aktorzy_wygrani2.csv")
x1<- x1 %>% mutate(sex = case_when(str_detect(category, "ACTRESS") == 1 ~ "k",
                                   T ~ "m")) 
x1[160, "birth_year"] <- 1974
x1[160, "oscar_age"] <- 35

ui_dlugosci <- fluidPage(
  
    column(3,
           checkboxGroupInput("kategoria_dlugosci", "Kategoria", 
                              choiceNames = c("Najlepszy film", "Najlepszy film nieanglojęzyczny", "Najlepszy scenariusz", "Najlepszy film animowany"), 
                              choiceValues = unique(wykresowa$category),
                              selected = unique(wykresowa$category)),
           
           sliderInput("rok_dlugosci", 
                       "Lata ceremonii", 
                       min=1970, 
                       max=2023, 
                       value = c(min = 1970, max=2023), 
                       step = 1,
                       sep = '',
                       dragRange = T),
           
           radioButtons("nominacje_dlugosci",
                        "Pokaż", 
                        choiceNames = c("Tylko zwycięzców", "Zwycięzców i nominowanych"), 
                        choiceValues = c(TRUE, FALSE)),
    ),
    column(9,
           shinycssloaders::withSpinner(plotlyOutput("wykres_dlugosci"),
                                        type = getOption("spinner.type", default = 8),
                                        color = getOption("spinner.color", default = "#201c1c"),
                                        size = getOption("spinner.size", default = 1.5)
      )
    ),
  
  fluidRow()
)

ui_budzety <- fluidPage(
  fluidRow(
    column(3,
           checkboxGroupInput("kategoria_budzety", "Kategoria", 
                              choiceNames = c("Najlepszy film", "Najlepszy film nieanglojęzyczny", "Najlepszy scenariusz", "Najlepszy film animowany"), 
                              choiceValues = unique(wykresowa$category),
                              selected = unique(wykresowa$category)),
           
           radioButtons("nominacje_budzety",
                        "Pokaż", 
                        choiceNames = c("Tylko zwycięzców", "Zwycięzców i nominowanych"), 
                        choiceValues = c(TRUE, FALSE))
    ),
    column(9,
           shinycssloaders::withSpinner(plotlyOutput("wykres_budzety"),
                                        type = getOption("spinner.type", default = 8),
                                        color = getOption("spinner.color", default = "#201c1c"),
                                        size = getOption("spinner.size", default = 1.5)
           )
    )
  )
)

ui_gatunki <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "lux"),
  
  fluidRow(
    column(3,
           checkboxGroupInput("kategoria_gatunki", "Kategoria", 
                              choiceNames = c("Najlepszy film", "Najlepszy film nieanglojęzyczny", "Najlepszy scenariusz", "Najlepszy film animowany"), 
                              choiceValues = unique(wykresowa$category),
                              selected = unique(wykresowa$category)),
           
           sliderInput("rok_gatunki", 
                       "Lata ceremonii", 
                       min=1970, 
                       max=2023, 
                       value = c(min = 1970, max=2023), 
                       step = 1,
                       sep = '',
                       dragRange = T),
           
           radioButtons("nominacje_gatunki",
                        "Pokaż", 
                        choiceNames = c("Tylko zwycięzców", "Zwycięzców i nominowanych"), 
                        choiceValues = c(TRUE, FALSE))
    ),
    column(9,
           shinycssloaders::withSpinner(plotlyOutput("wykres_gatunki"),
                                        type = getOption("spinner.type", default = 8),
                                        color = getOption("spinner.color", default = "#201c1c"),
                                        size = getOption("spinner.size", default = 1.5)
           )
    )
  )
)

# ui aktorowe ----
ui_aktorzy <- fluidPage(
  fluidRow(
    column(3,
           sliderInput("rok_aktorzy", 
                       "Lata ceremonii", 
                       min=1970, 
                       max=2023, 
                       value = c(min = 1970, max=2023), 
                       step = 1,
                       sep = '',
                       dragRange = T)
    ),
    column(9,
           plotlyOutput("wykres_a1")
    )
  ), 
  fluidRow(
    column(6,
           plotlyOutput("wykres_a2")
    ),
    column(6,
           plotlyOutput("wykres_a3")
    )
  ),
  fluidRow(
    column(6,
           plotlyOutput("wykres_a4")
    ),
    column(6,
           plotlyOutput("wykres_a5")
    )
  )
)


server <- function(input, output) {
  
  #wykres dlugosci filmow ----
  output$wykres_dlugosci <- renderPlotly(
    {
      df_dlugosci <- wykresowa %>% 
        mutate(category = factor(category)) %>% 
        filter(year_ceremony >= input$rok_dlugosci[1], year_ceremony <= input$rok_dlugosci[2],
               category %in% input$kategoria_dlugosci)
      
      #tu chyba trzeba ifem zrobić, żeby przy zwyciezcach i nominowanych brało wszystkich
      #bo jak NIE DAMY ifa, to przy "tylko zwyciezcy" wezmie winner = true
      #a przy "zwyc. + nominow." wezmie winner = false
      #dobrze mysle?
      
      global_colors = setNames(c("dodgerblue", "#FDE100", "#DC052D", "limegreen"),unique(wykresowa$category)) # do stałych kolorów
      
      if (input$nominacje_dlugosci == TRUE) {
        df_dlugosci <- df_dlugosci %>% 
          filter(winner == input$nominacje_dlugosci)
      }
      
      p1 <- ggplot(data = df_dlugosci) +
        geom_density(aes(x = runtime, color = category, fill = category),
                     alpha = 0.3, bw = "nrd") +
        
        #nie udalo mi sie tutaj zmienic tych kolorow, ogarnij to Wojtek
        scale_fill_manual(values = c("dodgerblue", "#FDE100", "#DC052D", "limegreen")) +
        scale_color_manual(values = c("dodgerblue", "#FDE100", "#DC052D", "limegreen")) +
        theme_bw() +
        scale_y_continuous(expand = c(0,0), limits = c(0,0.09)) +
        scale_x_continuous(expand = c(0,0), limits = c(0, 300)) +
        labs(x = "czas trwania (min)", y = "gęstość", title = "Gęstości czasów trwania filmów w zależności od kategorii nagród")
      
      p1 <- plotly::ggplotly(p1) 
      
      p1$x$data = lapply(p1$x$data, function(x) {
        color = global_colors[which(names(global_colors) %in% x$name)]
        x$line$color = color
        x$fillcolor = c(color,0.7)
        return(x)
      })
      
      return(p1)
    }
  )
  
  
  
  
  #wykres budzetow ----
  
  output$wykres_budzety <- renderPlotly(
    {
      df_budzetowa <- wykresowa %>%
        filter(category %in% input$kategoria_budzety)
      

      if (input$nominacje_budzety == TRUE) {
        df_budzetowa <- df_budzetowa %>% 
          filter(winner == input$nominacje_budzety)
      }
      
      df_budzetowa <- df_budzetowa %>% 
        group_by(year_ceremony) %>% 
        mutate(gross = gross/1000000) %>% 
        mutate(budzet_mediana = median(gross, na.rm = TRUE)) %>% 
        mutate(budzet_min = min(gross, na.rm = TRUE)) %>% 
        mutate(budzet_max = max(gross, na.rm = TRUE)) %>% 
        select(year_ceremony, budzet_mediana, budzet_min, budzet_max) %>% 
        unique()
      
      p2 <- ggplot(data = df_budzetowa) +
        geom_line(aes(x = year_ceremony, y = budzet_mediana)) +
        geom_point(aes(x = year_ceremony, y = budzet_min)) +
        geom_point(aes(x = year_ceremony, y = budzet_max)) +
        theme_bw() +
        scale_y_continuous(expand = c(0,10), limits = c(0, 800)) +
        scale_x_continuous(expand = c(0,0)) +
        labs(x = "rok ceremonii", y = "budżet (w mln $)", title = "Budżety filmów na przestrzeni czasu")
        
      
      plotly::ggplotly(p2) 
    }
  )
  
  #wykres gatunkow ----
  output$wykres_gatunki <- renderPlotly(
    {
      df_gatunki <- wykresowa %>% 
        filter(year_ceremony >= input$rok_gatunki[1], year_ceremony <= input$rok_gatunki[2],
               category %in% input$kategoria_gatunki)
      
      
      if (input$nominacje_gatunki == TRUE) {
        df_gatunki <- df_gatunki %>% 
          filter(winner == input$nominacje_gatunki)
      }
      
      wykresowa_dluga <- df_gatunki %>% 
        pivot_longer(cols = c(genre1, genre2, genre3), names_to = "kolumna_gatunku", values_to = "gatunek") %>% 
        na.omit(gatunek)
      zliczone_gatunki <- wykresowa_dluga %>% 
        count(gatunek) %>%
        arrange(desc(n))
      top10 <- zliczone_gatunki %>% 
        slice(1:10)
      inne_gatunki <- zliczone_gatunki %>% 
        slice(-(1:10)) %>% 
        summarise(n = sum(n), gatunek = "Other")
      zliczone_gatunki_other <- rbind(top10, inne_gatunki)
      zliczone_gatunki_other$gatunek <- factor(zliczone_gatunki_other$gatunek, levels = rev(zliczone_gatunki_other$gatunek))
      
      p3 <- ggplot(data = zliczone_gatunki_other) +
        geom_col(aes(x = n, y = gatunek), width = 0.7, fill = "navyblue") +
        theme_bw() +
        scale_x_continuous(expand = c(0,0), limits = c(0, max(zliczone_gatunki_other$n) * 1.1), n.breaks = 10) +
        labs(x = "liczba wystąpień", y = "gatunek", 
             title = "Liczba filmów z podziałem na gatunki", 
             caption = "*niektórym filmom przypisane zostały 2 lub 3 gatunki") +
        scale_y_discrete(expand = c(0.06,0.06)) +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
        )
      plotly::ggplotly(p3) 
    }
  )
  
  #wykresy aktorskie ----
  output$wykres_a1 <- renderPlotly(
    {
      x1 <- x1 %>% 
        filter(year_ceremony >= input$rok_aktorzy[1], year_ceremony <= input$rok_aktorzy[2])
      a1 <- ggplot(x1,aes(oscar_age)) + 
        geom_histogram(data=filter(x1, sex == "k"),fill = "red", alpha = 0.2, bins = 25) +
        geom_histogram(data=filter(x1, sex == "m"),fill = "blue", alpha = 0.2, bins = 25) +
        theme_bw() +
        scale_y_continuous(expand = c(0,0)) +
        scale_x_continuous(expand = c(0,0)) +
        labs(x = "wiek", y = "liczba aktorów i aktorek", title = "Wiek aktorów i aktorek")
      
      plotly::ggplotly(a1) 
    }
  )
  output$wykres_a2 <- renderPlotly(
    {
      y1 <- x1 %>% mutate(role = case_when(str_detect(category, "LEADING") == 1 ~ "leading",
                                           T ~ "supporting")) %>% 
        filter(year_ceremony >= input$rok_aktorzy[1], year_ceremony <= input$rok_aktorzy[2])
      a2 <- ggplot(filter(y1, sex == "k"),aes(oscar_age)) + 
        geom_histogram(data=filter(filter(y1, sex == "k"), role == "leading"),fill = "yellow", alpha = 0.2, bins = 15) +
        geom_histogram(data=filter(filter(y1, sex == "k"), role == "supporting"),fill = "green", alpha = 0.2, bins = 15) +
        theme_bw() +
        scale_y_continuous(expand = c(0,0)) +
        scale_x_continuous(expand = c(0,0)) +
        labs(x = "wiek", y = "liczba aktorów", title = "Wiek aktorów")
      
      plotly::ggplotly(a2) 
    }
  )
  output$wykres_a3 <- renderPlotly(
    {
      y1 <- x1 %>% mutate(role = case_when(str_detect(category, "LEADING") == 1 ~ "leading",
                                           T ~ "supporting")) %>% 
        filter(year_ceremony >= input$rok_aktorzy[1], year_ceremony <= input$rok_aktorzy[2])
      a3 <- ggplot(filter(y1, sex == "m"),aes(oscar_age)) + 
        geom_histogram(data=filter(filter(y1, sex == "m"), role == "leading"),fill = "yellow", alpha = 0.2, bins = 15) +
        geom_histogram(data=filter(filter(y1, sex == "m"), role == "supporting"),fill = "green", alpha = 0.2, bins = 15) +
        theme_bw() +
        scale_y_continuous(expand = c(0,0)) +
        scale_x_continuous(expand = c(0,0)) +
        labs(x = "wiek", y = "liczba aktorek", title = "Wiek aktorek")
      
      plotly::ggplotly(a3) 
    }
  )
  output$wykres_a4 <- renderPlotly(
    {
      y1 <- x1 %>% mutate(role = case_when(str_detect(category, "LEADING") == 1 ~ "leading",
                                           T ~ "supporting")) %>% 
        filter(year_ceremony >= input$rok_aktorzy[1], year_ceremony <= input$rok_aktorzy[2])
      
      a4 <- ggplot(y1 %>% filter(sex == "m"), aes(x = role, y = heigth))+geom_boxplot(fill = c("lightblue", "lightblue"))+ 
        ggtitle("Wzrost aktorów")+
        xlab("kategoria") + ylab("wzrost") + theme_bw()
      plotly::ggplotly(a4)
      
    }
  )
  output$wykres_a5 <- renderPlotly(
    {
      y1 <- x1 %>% mutate(role = case_when(str_detect(category, "LEADING") == 1 ~ "leading",
                                           T ~ "supporting")) %>% 
        filter(year_ceremony >= input$rok_aktorzy[1], year_ceremony <= input$rok_aktorzy[2])
      
      a5 <- ggplot(y1 %>% filter(sex == "k"), aes(x = role, y = heigth))+geom_boxplot(fill = c("lightblue", "lightblue"))+ 
        ggtitle("Wzrost aktorek")+
        xlab("kategoria") + ylab("wzrost") + theme_bw()
      plotly::ggplotly(a5) 
    }
  )
}

app_ui <- navbarPage(title = "Analiza filmów oraz aktorów nominowanych do Oscarów w latach 1970-2023",
                     tabPanel("Czas trwania", ui_dlugosci, icon = icon("clock")),
                     tabPanel("Gatunki", ui_gatunki, icon = icon("bolt-lightning")),
                     tabPanel("Budżety", ui_budzety, icon = icon("money-bill-wave")),
                     tabPanel("Aktorzy", ui_aktorzy, icon = icon("person")),
                     inverse = TRUE)


# Run the application 
shinyApp(ui = app_ui, server = server)
