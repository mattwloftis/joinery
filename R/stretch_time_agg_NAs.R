
# Stretch time aggregated data sets to account for intermediate NAs

stretch_time_agg_NAs <- function(xx,
                                 grouping_vars,
                                 granularity = c("adhoc", "minute", "hour", "day", "week", "month", "year")) {

  # gen granularity
  granularity = match.arg(granularity)


  ##---------------------------------------------------------##
  ## YEAR
  ##---------------------------------------------------------##
  if (granularity == "year") {
    to.stretch <- xx %>%
      dplyr::summarise(
        jnry_time_first = min(jnry_year),
        jnry_time_last = max(jnry_year)
      ) %>%
      purrr::array_branch(margin = 1) %>%
      purrr::map(
        function(k) {
          k %>%
            t %>%
            tibble::as_tibble()
        }
      )

    return(
      to.stretch %>%
        purrr::map(function(k) {
          k %>%
            dplyr::mutate(jnry_year = list(.data$jnry_time_first:.data$jnry_time_last)) %>%
            tidyr::unnest(jnry_year)
        } ) %>%
        dplyr::bind_rows() %>%
        dplyr::select(-tidyselect::starts_with("jnry_time"))
    )
  }

  ##---------------------------------------------------------##
  ## MONTH
  ##---------------------------------------------------------##
  if (granularity == "month") {
    to.stretch <- xx %>%
      dplyr::summarise(
        jnry_time_first = min(jnry_month),
        jnry_time_last = max(jnry_month)
      ) %>%
      purrr::array_branch(margin = 1) %>%
      purrr::map(
        function(k) {
          k %>%
            t %>%
            tibble::as_tibble()
        }
      )

    # executing the stretch
    to.stretch <- to.stretch %>%
      purrr::map(function(k) {
        k %>%
          dplyr::mutate(jnry_month = list(month_series(.data$jnry_time_first,
                                                       .data$jnry_time_last))) %>%
          tidyr::unnest(jnry_month)
      } ) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to.stretch)) {
      class(to.stretch[[nume]]) <- vroom::guess_type(to.stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        str_replace("^collector_", "")
    }

    return(to.stretch)
  }

  ##---------------------------------------------------------##
  ## WEEK
  ##---------------------------------------------------------##
  if (granularity == "week") {

  }

  ##---------------------------------------------------------##
  ## DAY
  ##---------------------------------------------------------##
  if (granularity == "day") {

  }

  ##---------------------------------------------------------##
  ## HOUR
  ##---------------------------------------------------------##
  if (granularity == "hour") {

  }

  ##---------------------------------------------------------##
  ## MINUTE
  ##---------------------------------------------------------##
  if (granularity == "minute") {

  }

  ##---------------------------------------------------------##
  ## AD HOC ... different rules, here
  ##---------------------------------------------------------##
  if (granularity == "adhoc") {

  }

}
