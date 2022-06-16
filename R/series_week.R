
# fill a series of weeks

series_week <- function(first, last) {

  int <- first + 0:(last - first)

  series <- int %>%
    format_wk() %>%
    unique()

  return(series)
}
