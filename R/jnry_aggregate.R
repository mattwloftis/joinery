
# fx to perform aggregate_time collapse step

jnry_aggregate <- function(xx, aggregation = aggregation) {
  if (!(aggregation %in% "count")) {
    clpsd <- xx %>%
      dplyr::summarise_if(
        .predicate = is.numeric,
        .funs = {{ aggregation }}
      )
  } else {
    clpsd <- xx %>%
      dplyr::summarise(jnry_count = dplyr::n())
  }

  return(clpsd)
}
