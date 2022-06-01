
# format instant as quarter

format_qr <- function(xx) {

  qrtrs <- ifelse(lubridate::month(xx) < 4, 1, NA)
  qrtrs <- ifelse(lubridate::month(xx) %in% 4:6, 2, qrtrs)
  qrtrs <- ifelse(lubridate::month(xx) %in% 7:9, 3, qrtrs)
  qrtrs <- ifelse(lubridate::month(xx) > 9, 4, qrtrs)

  xx <- paste0(lubridate::year(xx),
               "-Q",
               qrtrs)

  return(xx)
}
