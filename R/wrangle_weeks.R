
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
                                                 side = 'left',
                                                 pad = "0"))) %>%
    dplyr::group_by(week) %>%
    dplyr::summarize(day = dplyr::first(day))

  out <- tibble::tibble(week = xx) %>%
    dplyr::left_join(prep, by = "week")

  return(out$day)
}
