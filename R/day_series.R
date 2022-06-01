
# fill a series of days

day_series <- function(first, last) {

  series <- c(first, first + (1:(last - first)))

  return(series)
}
