
# stretch Party Facts party obs over year_first:year_last
stretch_pf <- function(xx) {
  l_pf <- xx %>%
    dplyr::select(country,
                  partyfacts_id,
                  tidyselect::starts_with("name"),
                  tidyselect::starts_with("year"),
                  wikipedia) %>%
    dplyr::mutate(year_last = ifelse(is.na(year_last),
                              as.numeric(
                                stringr::str_extract(Sys.Date(), "^\\d{4}")
                                ),
                              year_last)) %>%
    purrr::array_branch(margin = 1) %>%
    purrr::map(
      function(k) {
        k %>%
          t %>%
          tibble::as_tibble()
      }
    )

  return(
    purrr::map(l_pf,
        function(k) {
          k %>%
            dplyr::mutate(jnry_year = list(.data$year_first:.data$year_last)) %>%
            tidyr::unnest(jnry_year)
        }
    ) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(partyfacts_id = as.numeric(partyfacts_id)) %>%
      dplyr::rename(jnry_country = country)
  )
}
