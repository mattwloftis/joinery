
# fill a series of months

month_series <- function(first, last) {

  first.yr <- first %>%
    stringr::str_split("-") %>%
    unlist() %>%
    `[`(1) %>%
    as.numeric()

  last.yr <- last %>%
    stringr::str_split("-") %>%
    unlist() %>%
    `[`(1) %>%
    as.numeric()

  first.mo <- first %>%
    stringr::str_split("-") %>%
    unlist() %>%
    `[`(2) %>%
    as.numeric()

  last.mo <- last %>%
    stringr::str_split("-") %>%
    unlist() %>%
    `[`(2) %>%
    as.numeric()

  series <- character(0)

  inner.years <- (1 + first.yr):(last.yr - 1)

  for (mo in first.mo:12) {
    series <- c(series,
                paste0(first.yr, "-",
                       stringr::str_pad(string = mo,
                                        width = 2,
                                        side = 'left',
                                        pad = '0')))
  }

  for (yr in inner.years) {
    series <- c(series,
                paste0(yr, "-",
                       stringr::str_pad(string = 1:12,
                                        width = 2,
                                        side = 'left',
                                        pad = '0')))
  }

  for (mo in 1:last.mo) {
    series <- c(series,
                paste0(last.yr, "-",
                       stringr::str_pad(string = mo,
                                        width = 2,
                                        side = 'left',
                                        pad = '0')))
  }

  return(series)
}
