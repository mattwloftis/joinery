
# Stretch time aggregated data sets to account for intermediate NAs

stretch_time_agg_nas <- function(clpsd,
                                 x,
                                 # grouping_vars = NULL,
                                 granularity = c("adhoc", "day", "week",
                                                 "month", "quarter",
                                                 "half year", "year")) {

  # gen granularity
  granularity <- match.arg(granularity)


  ##---------------------------------------------------------##
  ## YEAR
  ##---------------------------------------------------------##
  if (granularity == "year") {
    to_stretch <- clpsd %>%
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
      to_stretch %>%
        purrr::map(function(k) {
          k %>%
            dplyr::mutate(jnry_year = list(
              .data$jnry_time_first:.data$jnry_time_last)
            ) %>%
            tidyr::unnest(jnry_year)
        }) %>%
        dplyr::bind_rows() %>%
        dplyr::select(-tidyselect::starts_with("jnry_time"))
    )
  }


  ##---------------------------------------------------------##
  ## HALF YEAR
  ##---------------------------------------------------------##
  if (granularity == "half year") {
    to_stretch <- clpsd %>%
      dplyr::summarise(
        jnry_time_first = min(jnry_halfyr),
        jnry_time_last = max(jnry_halfyr)
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
    to_stretch <- to_stretch %>%
      purrr::map(function(k) {
        k %>%
          dplyr::mutate(jnry_halfyr = list(
            series_halfyr(.data$jnry_time_first,
                          .data$jnry_time_last))
          ) %>%
          tidyr::unnest(jnry_halfyr)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-tidyselect::starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to_stretch)) {
      class(to_stretch[[nume]]) <- vroom::guess_type(to_stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        stringr::str_replace("^collector_", "")
    }

    return(to_stretch)
  }


  ##---------------------------------------------------------##
  ## QUARTER
  ##---------------------------------------------------------##
  if (granularity == "quarter") {
    to_stretch <- clpsd %>%
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
    to_stretch <- to_stretch %>%
      purrr::map(function(k) {
        k %>%
          dplyr::mutate(jnry_quarter = list(
            series_quarter(.data$jnry_time_first,
                           .data$jnry_time_last))
          ) %>%
          tidyr::unnest(jnry_quarter)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-tidyselect::starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to_stretch)) {
      class(to_stretch[[nume]]) <- vroom::guess_type(to_stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        stringr::str_replace("^collector_", "")
    }

    return(to_stretch)
  }

  ##---------------------------------------------------------##
  ## MONTH
  ##---------------------------------------------------------##
  if (granularity == "month") {
    to_stretch <- clpsd %>%
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
    to_stretch <- to_stretch %>%
      purrr::map(function(k) {
        k %>%
          dplyr::mutate(jnry_month = list(
            series_month(.data$jnry_time_first,
                         .data$jnry_time_last))
          ) %>%
          tidyr::unnest(jnry_month)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-tidyselect::starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to_stretch)) {
      class(to_stretch[[nume]]) <- vroom::guess_type(to_stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        stringr::str_replace("^collector_", "")
    }

    return(to_stretch)
  }

  ##---------------------------------------------------------##
  ## WEEK
  ##---------------------------------------------------------##
  if (granularity == "week") {
    to_stretch <- x %>%
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
    to_stretch <- to_stretch %>%
      purrr::map(function(k) {
        k %>%
          dplyr::mutate(jnry_week = list(
            series_week(.data$jnry_time_first,
                        .data$jnry_time_last))
          ) %>%
          tidyr::unnest(jnry_week)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-tidyselect::starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to_stretch)) {
      class(to_stretch[[nume]]) <- vroom::guess_type(to_stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        stringr::str_replace("^collector_", "")
    }

    return(to_stretch)
  }

  ##---------------------------------------------------------##
  ## DAY
  ##---------------------------------------------------------##
  if (granularity == "day") {
    to_stretch <- clpsd %>%
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
    to_stretch <- to_stretch %>%
      purrr::map(function(k) {
        k %>%
          dplyr::mutate(jnry_day = list(
            day_series(.data$jnry_time_first,
                       .data$jnry_time_last))
          ) %>%
          tidyr::unnest(jnry_day)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::select(-tidyselect::starts_with("jnry_time")) %>%
      dplyr::mutate_all(stringr::str_trim)

    # fix data types (thanks 'vroom')
    for (nume in names(to_stretch)) {
      class(to_stretch[[nume]]) <- vroom::guess_type(to_stretch[[nume]]) %>%
        class %>%
        `[`(1) %>%
        stringr::str_replace("^collector_", "")
    }

    return(to_stretch)
  }

  ##---------------------------------------------------------##
  ## AD HOC ... different rules, here
  ##---------------------------------------------------------##
  if (granularity == "adhoc") {

  }

}
