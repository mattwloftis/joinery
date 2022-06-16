
# fill a series of quarters

series_halfyr <- function(first, last) {

  fyear <- as.numeric(stringr::str_extract(first, "\\d{4}"))
  fh <- as.numeric(stringr::str_extract(first, "\\d$"))
  lyear <- as.numeric(stringr::str_extract(last, "\\d{4}"))
  lh <- as.numeric(stringr::str_extract(last, "\\d$"))

  helper <- tibble::tibble(
    yrs = rep(fyear:lyear, each = 2)
  ) %>%
    dplyr::mutate(
      halves = rep(1:2, times = length(unique(yrs)))
    )

  too_early <- (helper$yrs == fyear) & (helper$halves < fh)
  too_late <- (helper$yrs == lyear) & (helper$halves > lh)

  helper <- helper %>% dplyr::filter(!too_early & !too_late)

  series <- paste0(helper$yrs, "-H", helper$halves)

  return(series)
}
