
# fill a series of weeks

series_week <- function(first, last) {

  int <- c(first, first + (1:(last - first)))

  series <- int %>%
    format_wk() %>%
    unique()

  return(series)
}
