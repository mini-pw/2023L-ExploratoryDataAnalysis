library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(xml2)


# Prepare Movies & TV Stack Exchange data ----
paths_se <- list.files(
  file.path("data", "movies.stackexchange.com"),
  pattern = "\\.xml",
  full.names = TRUE
)
names(paths_se) <- paths_se |>
  basename() |>
  tools::file_path_sans_ext() |>
  janitor::make_clean_names()
iwalk(
  paths_se,
  \(path, name) path |>
    read_xml(options = c("HUGE", "NOBLANKS")) |>
    xml_children() |>
    xml_attrs() |>
    list_transpose(default = NA) |>
    as_tibble() |>
    janitor::clean_names() |>
    type_convert(guess_integer = TRUE) |>
    write_rds(file.path("data", paste0(name, "_se.rds")))
)

# Prepare monthly tags data ----
tags_monthly <- read_rds("data/posts_se.rds") |>
  select(creation_date, tags) |>
  filter(!is.na(tags)) |>
  mutate(creation_date = floor_date(
    as_date(creation_date),
    unit = "month"
  )) |>
  group_by(creation_date) |>
  group_modify(
    \(df, key) df |>
      pull(tags) |>
      str_extract_all(pattern = "<.*?>|NA") |>
      unlist() |>
      as_tibble_col(column_name = "tag_name") |>
      count(tag_name, name = "count")
  ) |>
  ungroup() |>
  write_rds("data/tags_monthly.rds")

# Prepare daily user data ----
users_filtered <- read_rds("data/users_se.rds") |>
  select(id, creation_date, last_access_date) |>
  mutate(
    creation_date = as_date(creation_date),
    last_access_date = as_date(last_access_date)
  ) |>
  filter(between(creation_date, ymd("2012-01-01"), ymd("2022-12-31")))

date <- seq(ymd("2012-01-01"), ymd("2022-12-31"), by = "day")
count = numeric(length(date))
for (i in 1:nrow(users_filtered)) {
  tmp <- date >= users_filtered$creation_date[i] &
    date <= users_filtered$last_access_date[i]
  count = count + tmp
}
write_rds(
  tibble(date = date, count = count),
  file.path("data", "users_daily.rds")
)
