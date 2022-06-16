
# fill a series of months

series_month <- function(first, last) {

  first_yr <- first %>%
    stringr::str_split("-") %>%
    unlist() %>%
    `[`(1) %>%
    as.numeric()

  last_yr <- last %>%
    stringr::str_split("-") %>%
    unlist() %>%
    `[`(1) %>%
    as.numeric()

  first_mo <- first %>%
    stringr::str_split("-") %>%
    unlist() %>%
    `[`(2) %>%
    as.numeric()

  last_mo <- last %>%
    stringr::str_split("-") %>%
    unlist() %>%
    `[`(2) %>%
    as.numeric()

  series <- character(0)

  inner_years <- (1 + first_yr):(last_yr - 1)

  for (mo in first_mo:12) {
    series <- c(series,
                paste0(first_yr, "-",
                       stringr::str_pad(string = mo,
                                        width = 2,
                                        side = "left",
                                        pad = "0")))
  }

  for (yr in inner_years) {
    series <- c(series,
                paste0(yr, "-",
                       stringr::str_pad(string = 1:12,
                                        width = 2,
                                        side = "left",
                                        pad = "0")))
  }

  for (mo in 1:last_mo) {
    series <- c(series,
                paste0(last_yr, "-",
                       stringr::str_pad(string = mo,
                                        width = 2,
                                        side = "left",
                                        pad = "0")))
  }

  return(series)
}
