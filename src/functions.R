url_to_data_table <- function(request_url) {
  res = GET(request_url)
  data <- res$content %>%
    rawToChar() %>%
    fread %>%
    as_tibble()
  return(data)
}