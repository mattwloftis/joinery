
# format instant as ISO month

format_mo <- function(xx) {
  xx <- paste0(lubridate::year(xx),
               "-",
               str_pad(string = lubridate::month(xx),
                       width = 2,
                       side = 'left',
                       pad = '0'))

  return(xx)
}
