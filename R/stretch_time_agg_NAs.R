
# Stretch time aggregated data sets to account for intermediate NAs

stretch_time_agg_NAs <- function(clpsd,
                                 x,
                                 # grouping_vars = NULL,
                                 granularity = c("adhoc", "quarter", "day", "week", "month", "year")) {

  # gen granularity
  granularity = match.arg(granularity)


  ##---------------------------------------------------------##
  ## YEAR
  ##---------------------------------------------------------##
  if (granularity == "year") {
    to.stretch <- clpsd %>%
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
  ## QUARTER
  ##---------------------------------------------------------##
  if (granularity == "quarter") {
    to.stretch <- clpsd %>%
      dplyr::summarise(
        jnry_time_first = min(jnry_quarter),
        jnry_time_last = max(jnry_quarter)
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
          dplyr::mutate(jnry_quarter = list(series_quarter(.data$jnry_time_first,
                                                         .data$jnry_time_last))) %>%
          tidyr::unnest(jnry_quarter)
      } ) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-tidyselect::starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to.stretch)) {
      class(to.stretch[[nume]]) <- vroom::guess_type(to.stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        stringr::str_replace("^collector_", "")
    }

    return(to.stretch)
  }

  ##---------------------------------------------------------##
  ## MONTH
  ##---------------------------------------------------------##
  if (granularity == "month") {
    to.stretch <- clpsd %>%
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
          dplyr::mutate(jnry_month = list(series_month(.data$jnry_time_first,
                                                       .data$jnry_time_last))) %>%
          tidyr::unnest(jnry_month)
      } ) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-tidyselect::starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to.stretch)) {
      class(to.stretch[[nume]]) <- vroom::guess_type(to.stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        stringr::str_replace("^collector_", "")
    }

    return(to.stretch)
  }

  ##---------------------------------------------------------##
  ## WEEK
  ##---------------------------------------------------------##
  if (granularity == "week") {
    to.stretch <- x %>%
      dplyr::ungroup(jnry_week) %>%
      dplyr::summarise(
        jnry_time_last = max(.data[[time]]),
        jnry_time_first = min(.data[[time]])
      ) %>%
      purrr::array_branch(margin = 1) %>%
      purrr::map(
        function(k) {
          k %>%
            t %>%
            tibble::as_tibble() %>%
            dplyr::mutate(
              jnry_time_last = lubridate::ymd(jnry_time_last),
              jnry_time_first = lubridate::ymd(jnry_time_first)
            )
        }
      )

    # executing the stretch
    to.stretch <- to.stretch %>%
      purrr::map(function(k) {
        k %>%
          dplyr::mutate(jnry_week = list(series_week(.data$jnry_time_first,
                                                     .data$jnry_time_last))) %>%
          tidyr::unnest(jnry_week)
      } ) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-tidyselect::starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to.stretch)) {
      class(to.stretch[[nume]]) <- vroom::guess_type(to.stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        stringr::str_replace("^collector_", "")
    }

    return(to.stretch)
  }

  ##---------------------------------------------------------##
  ## DAY
  ##---------------------------------------------------------##
  if (granularity == "day") {
    to.stretch <- clpsd %>%
      dplyr::summarise(
        jnry_time_first = min(jnry_day),
        jnry_time_last = max(jnry_day)
      ) %>%
      purrr::array_branch(margin = 1) %>%
      purrr::map(
        function(k) {
          k %>%
            t %>%
            tibble::as_tibble() %>%
            dplyr::mutate(
              jnry_time_first = lubridate::ymd(jnry_time_first),
              jnry_time_last = lubridate::ymd(jnry_time_last)
            )
        }
      )

    # executing the stretch
    to.stretch <- to.stretch %>%
      purrr::map(function(k) {
        k %>%
          dplyr::mutate(jnry_day = list(day_series(.data$jnry_time_first,
                                                   .data$jnry_time_last))) %>%
          tidyr::unnest(jnry_day)
      } ) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-tidyselect::starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to.stretch)) {
      class(to.stretch[[nume]]) <- vroom::guess_type(to.stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        stringr::str_replace("^collector_", "")
    }

    return(to.stretch)
  }

  ##---------------------------------------------------------##
  ## AD HOC ... different rules, here
  ##---------------------------------------------------------##
  if (granularity == "adhoc") {

  }

}
