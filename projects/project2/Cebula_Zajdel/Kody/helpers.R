library(dplyr)
library(lubridate)
library(stringr)
library(tibble)


# UI helpers ----
box_default <- function(..., title = "Description") {
  box(
    ...,
    title = title,
    footer = NULL,
    status = "primary",
    solidHeader = FALSE,
    background = NULL,
    width = NULL,
    height = NULL,
    collapsible = FALSE,
    collapsed = FALSE
  )
}

box_story <- function(..., title = "Insights") {
  box(
    ...,
    title = title,
    footer = NULL,
    status = "info",
    solidHeader = FALSE,
    background = NULL,
    width = NULL,
    height = NULL,
    collapsible = FALSE,
    collapsed = FALSE
  )
}

plotlyOutput_default <- function(outputID) {
  withSpinner(plotlyOutput(outputID, height = "800px"))
}

# Other helpers ----
count_tags_year <- function(posts, year, top = 10) {
  posts |>
    filter(
      !is.na(tags),
      year(creation_date) == year
    ) |>
    pull(tags) |>
    str_extract_all(pattern = "<.*?>|NA") |>
    unlist() |>
    as_tibble_col(column_name = "tag_name") |>
    count(tag_name, name = "count") |>
    slice_max(n = top, order_by = count)
}
