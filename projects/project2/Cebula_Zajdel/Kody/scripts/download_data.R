url <- "https://archive.org/download/stackexchange/movies.stackexchange.com.7z"
data_se <- file.path("data", "movies.stackexchange.com")
data_se_archive <- file.path(data_se, basename(url))

dir.create(data_se, recursive = TRUE, showWarnings = FALSE)
curl::multi_download(url, data_se_archive, resume = TRUE)
archive::archive_extract(data_se_archive, data_se)
