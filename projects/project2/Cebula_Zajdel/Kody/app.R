library(shiny)
library(shinycssloaders)
library(shinydashboard)

library(dplyr)
library(ggplot2)
library(plotly)
library(readr)

source("helpers.R")


# Data loading ----
posts_se <- read_rds(file.path("data", "posts_se.rds"))
tags_monthly <- read_rds(file.path("data", "tags_monthly.rds"))
users_daily <- read_rds(file.path("data", "users_daily.rds"))

tags_top5_2022 <- count_tags_year(posts_se, 2022, top = 5)
posts_daily <- posts_se |>
  select(post_type_id, creation_date) |>
  filter(
    post_type_id %in% c(1, 2),
    between(creation_date, ymd("2012-01-01"), ymd("2022-12-31"))
  ) |>
  transmute(
    creation_date = as_date(creation_date),
    post_type = case_match(post_type_id,
      1 ~ "question",
      2 ~ "answer"
    )
  ) |>
  count(post_type, creation_date, name = "count")


# UI ----
ui <- dashboardPage(
  # Interface ----
  dashboardHeader(title = "Movies & Community"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("The Community",
        tabName = "users",
        icon = icon("user", lib = "glyphicon")
      ),
      menuItem("Why Tags?",
        tabName = "tags",
        icon = icon("tags", lib = "glyphicon")
      ),
      hr(style = "margin: 10px; border: 1px solid grey;"),
      menuItem("About",
        tabName = "about",
        icon = icon("info-sign", lib = "glyphicon"),
        selected = TRUE
      ),
      menuItem("References",
        tabName = "references",
        icon = icon("link", lib = "glyphicon")
      )
    ),
    collapsed = TRUE
  ),
  dashboardBody(
    tabItems(
      # The Community ----
      tabItem("users",
        fluidRow(
          column(12,
            box_story(
              includeHTML("html/the_community.html"),
              title = "The Community",
            )
          )
        ),
        fluidRow(
          column(9,
            box_default(
              plotlyOutput_default("line_users"),
              title = "Size of the User Base over Time"
            )
          ),
          column(3,
            box_default(
              includeHTML("html/line_users_description.html"),
            ),
            box_story(
              includeHTML("html/line_users_insights.html")
            )
          )
        ),
        fluidRow(
          column(9,
            box_default(
              plotlyOutput_default("lines_posts"),
              title = "Created Posts over Time"
            )
          ),
          column(3,
            box_default(
              includeHTML("html/lines_posts_description.html")
            ),
            box_story(
              includeHTML("html/lines_posts_insights.html")
            )
          )
        )
      ),
      # Why Tags? ----
      tabItem("tags",
        fluidRow(
          column(12,
            box_story(
              includeHTML("html/why_tags.html"),
              title = "Why Tags?",
            )
          )
        ),
        fluidRow(
          column(9,
            box_default(
              plotlyOutput_default("bars_tags"),
              title = "Top 10 Tags in Selected Year"
            )
          ),
          column(3,
            box_default(
              selectInput("select_year_tags", "",
                choices = 2012:2022,
                selected = 2022
              ),
              title = "Select Year:"
            ),
            box_default(
              includeHTML("html/bars_tags_description.html"),
            ),
            box_story(
              includeHTML("html/bars_tags_insights.html")
            )
          )
        ),
        fluidRow(
          column(9,
            box_default(
              plotlyOutput_default("lines_tags"),
              title = "Trend Lines for Selected Tags"
            )
          ),
          column(3,
            box_default(
              selectizeInput("select_tags", "",
                choices = NULL,
                multiple = TRUE
              ),
              title = "Select Tags (Max 10):"
            ),
            box_default(
              includeHTML("html/lines_tags_description.html"),
            ),
            box_story(
              includeHTML("html/lines_tags_insights.html")
            )
          )
        )
      ),
      # References ----
      tabItem("references",
        box_story(
          includeHTML("html/references.html"),
          title = "References",
        ),
      ),
      # About ----
      tabItem("about",
        box_story(
          includeHTML("html/about.html"),
          title = "About"
        ),
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  # Size of the user base
  output$line_users <- renderPlotly({
    fig <- plot_ly(users_daily,
      type = 'scatter',
      mode = 'lines',
      x = ~date,
      y = ~count
    ) |> layout(
      xaxis = list(
        rangeslider = list(
          visible = TRUE,
          thickness = 0.075
        ),
        title = "<b>Date</b>"
      ),
      yaxis = list(
        title = "<b>Number of Active Users</b>"
      )
    )
  })

  output$lines_posts <- renderPlotly({
    plot_ly(posts_daily,
      type = 'scatter',
      mode = 'lines',
      x = ~creation_date,
      y = ~count,
      color = ~post_type,
      colors = "viridis"
    ) |> layout(
      xaxis = list(
        rangeslider = list(
          visible = TRUE,
          thickness = 0.075
        ),
        title = "<b>Date</b>"
      ),
      yaxis = list(
        title = "<b>Number of Posts</b>"
      )
    )
  })

  # Top 10 Tags ----
  output$bars_tags <- renderPlotly({
    df <- count_tags_year(posts_se, input$select_year_tags)

    plot_ly(df,
      y = ~tag_name,
      x = ~count,
      type = "bar",
      orientation = "h"
    ) |> layout(
      yaxis = list(
        title = list(text = "<b>Tag Name</b>", standoff = 10),
        categoryorder = "total ascending"
      ),
      xaxis = list(
        title = list(text = "<b>Number of Tagged Posts</b>", standoff = 10)
      )
    )
  })

  # Select Tags ----
  updateSelectizeInput(session, 'select_tags',
    choices = unique(tags_monthly$tag_name),
    selected = unique(tags_top5_2022$tag_name),
    options = list(maxItems = 10),
    server = TRUE
  )

  # Trend Lines for Tags ----
  output$lines_tags <- renderPlotly({
    df <- tags_monthly |>
      filter(tag_name %in% input$select_tags)

    fig <- plot_ly(df,
      type = 'scatter',
      mode = 'lines',
      x = ~creation_date,
      y = ~count,
      color = ~tag_name,
      colors = "viridis"
    ) |> layout(
      xaxis = list(
        rangeslider = list(
          visible = TRUE,
          thickness = 0.075
        ),
        title = "<b>Post Creation Date</b>"
      ),
      yaxis = list(
        title = "<b>Number of Tagged Posts</b>"
      )
    )
  })
}

# App ----
shinyApp(ui, server)
