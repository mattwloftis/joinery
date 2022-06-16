
# format instant as half year
format_hy <- function(xx) {
  xx <- paste0(lubridate::year(xx),
               "-H",
               ifelse((lubridate::month(xx) < 7), 1, 2))

  return(xx)
}

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

# format instant as ISO month
format_mo <- function(xx) {
  xx <- paste0(lubridate::year(xx),
               "-",
               stringr::str_pad(string = lubridate::month(xx),
                                width = 2,
                                side = "left",
                                pad = "0"))

  return(xx)
}

# format instant as ISO week
format_wk <- function(xx) {
  xx <- paste0(lubridate::year(xx),
               "-W",
               stringr::str_pad(string = lubridate::isoweek(xx),
                                width = 2,
                                side = "left",
                                pad = "0"))

  return(xx)
}



# convert year incoming granularity time vars to lubridate::instant
wrangle_years <- function(xx) {
  xx <- xx %>% paste0("-01") %>% lubridate::ym()

  return(xx)
}

# convert HALF year incoming granularity time vars to lubridate::instant
wrangle_halfyr <- function(xx) {
  # pull half yrs
  mos <- xx %>% stringr::str_extract("-H\\d")

  # convert to correct months
  hy <- ifelse(stringr::str_detect(mos, "1"), 1, 7) %>%
    stringr::str_pad(width = 2,
                     side = "left",
                     pad = "0")

  # package into a date
  xx <- xx %>%
    stringr::str_extract("\\d{4}") %>%
    stringr::str_c("-", hy) %>%
    lubridate::ym()

  return(xx)
}

# convert ISO week format to a lubridate::instant
wrangle_weeks <- function(xx) {
  prep <- xx %>%
    stringr::str_extract("\\d{4}") %>%
    unique %>%
    purrr::map(
      function(k) {
        tibble::tibble(
          day = seq(
            from = lubridate::ymd(paste0(k, "-01-01")),
            to = lubridate::ymd(paste0(k, "-12-31")),
            by = "1 day"
          )
        ) %>%
          dplyr::mutate(week = lubridate::isoweek(day))
      }
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(week = paste0(lubridate::year(day), "-W",
                                stringr::str_pad(string = week,
                                                 width = 2,
                                                 side = "left",
                                                 pad = "0"))) %>%
    dplyr::group_by(week) %>%
    dplyr::summarize(day = dplyr::first(day))

  out <- tibble::tibble(week = xx) %>%
    dplyr::left_join(prep, by = "week")

  return(out$day)
}
