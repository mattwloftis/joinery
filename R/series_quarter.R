
# fill a series of quarters

series_quarter <- function(first, last) {

  fyear <- as.numeric(stringr::str_extract(first, "\\d{4}"))
  fq <- as.numeric(stringr::str_extract(first, "\\d$"))
  lyear <- as.numeric(stringr::str_extract(last, "\\d{4}"))
  lq <- as.numeric(stringr::str_extract(last, "\\d$"))

  helper <- tibble::tibble(
    yrs = rep(fyear:lyear, each = 4)
  ) %>%
    dplyr::mutate(
      qs = rep(1:4, times = length(unique(yrs)))
    )

  too_early <- (helper$yrs == fyear) & (helper$qs < fq)
  too_late <- (helper$yrs == lyear) & (helper$qs > lq)

  helper <- helper %>% dplyr::filter(!too_early & !too_late)

  series <- paste0(helper$yrs, "-Q", helper$qs)

  return(series)
}
