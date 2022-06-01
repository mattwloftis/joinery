
# format instant as ISO week

format_wk <- function(xx) {
  xx <- paste0(lubridate::year(xx),
               "-W",
               str_pad(string = lubridate::isoweek(xx),
                       width = 2,
                       side = 'left',
                       pad = '0'))

  return(xx)
}
