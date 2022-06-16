
# format instant as half year

format_hy <- function(xx) {
  xx <- paste0(lubridate::year(xx),
               "-H",
               ifelse((lubridate::month(xx) < 7), 1, 2))

  return(xx)
}
