
#' Aggregate time granularity of a data set
#'
#' Aggregate time granularity of a data set, i.e. take a data set from more to
#' less granular time scales. \code{aggregate_time} can aggregate data to a set
#' of standard time granularities (e.g. week, month, year), or it can accept
#' user-supplied ad hoc intervals (e.g. election periods).
#'
#' @param x Data frame in which observations are labeled with a time or date.
#' @param time A string identifying the variable in the data, \code{x}, containing the date/time of observations.
#' @param grouping_vars A string or vector of strings identifying the variables in the data, \code{x}, containing the identity of groups by which time should be aggregated. If \code{NULL}, data are aggregated without regard to any subgroups in the data.
#' @param granularity Target time granularity for aggregated data set.
#' @param adhoc_granularity A vector of class \code{Date} (or \code{numeric}, four-digit years) identifying the ad hoc time granularity desired for the aggregation. Provide only the unique time points for aggregation.
#'
#' @return A tibble, \code{x}, transformed to aggregate it to the level of ...
#'
#' @export
#'
#' @importFrom magrittr `%>%`
#'
#' @examples

aggregate_time <- function(x,
                           time = NULL,
                           grouping_vars = NULL,
                           granularity = c("adhoc", "minute", "hour", "day", "week", "month", "year"),
                           adhoc_granularity = NULL,
                           aggregation = c("mean", "sum", "max", "min"),
                           fill_time_series = FALSE) {

  # break if 'x' not a data frame / tibble
  stopifnot(is.data.frame(x))

  # get target granularity & aggregation
  granularity <- match.arg(granularity)
  aggregation <- match.arg(aggregation)

  # sanity checks for NAs
  if (any(is.na(x[[time]]))) {
    stop("Time variable contains missing values.")
  }

  if (
    x %>%
    dplyr::select(all_of(grouping_vars)) %>%
    purrr::map(is.na) %>%
    purrr::map(any) %>%
    unlist %>%
    any
  ) stop ("Grouping variables contain missing values.")


  #############################################################
  ## 1) identify and sanity check time field & grouping variables

  # sanity check time field
  stopifnot(is.character(time),
            length(time) == 1,
            time %in% names(x))

  # sanity check grouping_vars
  stopifnot(is.character(grouping_vars),
            all(grouping_vars %in% names(x)))


  #############################################################
  ## 2) check granularity of incoming data (is this necessary??)
  ## let's assume not necessary for now...


  #############################################################
  ## 3) handle ad hoc granularities

  if (granularity == "adhoc") {

    #############################################################
    ## 4) execute aggregation

  } else {
    #############################################################
    ## 3) regular granularities

    ##---------------------------------------------------------##
    ## YEAR
    ##---------------------------------------------------------##
    if (granularity == "year") {
      # set joinery year
      x[['jnry_year']] <- x[[time]] %>%
        lubridate::year()

      # group by each grouping variable
      for (i in 1:length(grouping_vars)) {
        x <- x %>%
          group_by(.data[[grouping_vars[i]]], .add = TRUE)
      }

      # group by year
      x <- x %>% group_by(jnry_year, .add = TRUE)

      # collapse data to year level, applying respective transformation
      clpsd <- x %>%
        summarise_if(
          .predicate = is.numeric,
          .funs = {{ aggregation }}
        )

      # if requested, auto-fill time series with NAs
      if (fill_time_series) {
        clpsd <- clpsd %>%
          right_join(
            clpsd %>%
              stretch_time_agg_NAs(grouping_vars = grouping_vars,
                                   granularity = granularity)
          )
      }

      # return annualized data
      return(clpsd)

    }

    ##---------------------------------------------------------##
    ## MONTH
    ##---------------------------------------------------------##
    if (granularity == "month") {
      # set joinery month
      x[['jnry_month']] <- paste0(x[[time]] %>% lubridate::year(), "-",
                                  stringr::str_pad(
                                    string = x[[time]] %>% lubridate::month(),
                                    width = 2,
                                    side = "left",
                                    pad = "0"
                                    ))

      # group by each grouping variable
      for (i in 1:length(grouping_vars)) {
        x <- x %>%
          group_by(.data[[grouping_vars[i]]], .add = TRUE)
      }

      # group by month
      x <- x %>% group_by(jnry_month, .add = TRUE)

      # collapse data to month level, applying respective transformation
      clpsd <- x %>%
        summarise_if(
          .predicate = is.numeric,
          .funs = {{ aggregation }}
        )

      # if requested, auto-fill time series with NAs
      if (fill_time_series) {
        clpsd <- clpsd %>%
          right_join(
            clpsd %>%
              stretch_time_agg_NAs(grouping_vars = grouping_vars,
                                   granularity = granularity),
            by = c(grouping_vars, "jnry_month")
          )
      }

      # return annualized data
      return(clpsd)
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
      # grains <- min(x[[time]]):max(x[[time]])
    }


    #############################################################
    ## 4) execute aggregation


  }




  #############################################################
  ## 5) return aggregated data set

}
