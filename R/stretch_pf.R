
# stretch Party Facts party obs over year_first:year_last

stretch_pf <- function(xx) {
  l_pf <- xx %>%
    select(country, partyfacts_id, starts_with("name"), starts_with("year"), wikipedia) %>%
    mutate(year_last = ifelse(is.na(year_last),
                              as.numeric(str_extract(Sys.Date(), "^\\d{4}")),
                              year_last)) %>%
    array_branch(margin = 1) %>%
    map(
      function(k) {
        k %>%
          t %>%
          as_tibble
      }
    )

  return(
    map(l_pf,
        function(k) {
          k %>%
            mutate(jnry_year = list(.data$year_first:.data$year_last)) %>%
            unnest(jnry_year)
        }
    ) %>%
      bind_rows %>%
      mutate(partyfacts_id = as.numeric(partyfacts_id)) %>%
      rename(jnry_country = country)
  )
}
