# Load openxlsx
library(openxlsx)
library(dplyr)
library(plotly)
library(htmlwidgets)

# Read excel file
df <- read.xlsx('countries_results.xlsx')
df$result_reading <- round(df$result_reading,0)
df$result_science <- round(df$result_science,0)

df <- df %>% filter(continent == "Europe")

df %>% 
  plot_ly(x = ~result_maths, y = ~country,
          name='matematyka', type='bar', orientation = "h", marker = list(color = "db6d57")) %>%
  add_trace(x = ~result_reading, name = 'czytanie', type='bar', orintation = "h", marker = list(color = "0ca1d5")) %>%
  add_trace(x = ~result_science, name = 'przyroda', type='bar',orientation = "h", marker = list(color = "51a112")) %>%
  hide_legend() %>% 
  layout(
    margin = list(
      t = 100,  
      r = 50,  
      b = 50,
      l = 50  ),
    title = list(text = "Średni wynik badania PISA w poszczególnych dziedzinach w krajach Europy", x = 0.5, y = 10),
    xaxis = list(title = "Średni wynik (liczba punktów"),
    yaxis = list(title = "Kraj"),
    updatemenus = list(
      list(
        x=1, y=1,
        type = "list",
        label = 'Kategoria',
        buttons = list(
          list(method = "restyle",
               args = list('visible', c(TRUE, FALSE, FALSE)),
               label = "Matematyka"),
          list(method = "restyle",
               args = list('visible', c(FALSE, TRUE, FALSE)),
               label = "Czytanie"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, TRUE)),
               label = "Nauki przyrodnicze")
        )
      )
    )
  )



