
#' Aggregate time granularity of a data set
#'
#' Aggregate time granularity of a data set, i.e. take a data set from more to
#' less granular time scales. \code{aggregate_time} can aggregate data to a set
#' of standard time granularities (e.g. week, month, year), or it can accept
#' user-supplied ad hoc intervals (e.g. election periods).
#'
#' @param x Data frame in which observations are labeled with a time or date.
#'
#' @param time A string identifying the variable in the data, \code{x},
#' containing the date/time of observations. Time variable must be one of:
#' \itemize{
#'   \item \code{Date} object (date or datetime)
#'   \item \code{character} object in ISO calendar month basic format
#'   (e.g. 2022-05)
#'   \item \code{character} object in ISO calendar week extended format
#'   (e.g. 2022-W25)
#'   \item \code{character} object with years and quarters (i.e. 2022-Q2)
#' }
#'
#' @param grouping_vars A string or vector of strings identifying the variables
#' in the data, \code{x}, containing groups by which time should be aggregated.
#'
#' If \code{NULL}, data are aggregated without regard to any subgroups in the
#' data.
#'
#' @param granularity Target time granularity for aggregated data set. One of:
#' \itemize{
#'   \item \code{year}
#'   \item \code{half year}
#'   \item \code{quarter}
#'   \item \code{month}
#'   \item \code{week}
#'   \item \code{day}
#' }
#' Ignored if \code{adhoc_granularity} is provided. Note that the requested
#' granularity must be coarser than the time granularity of the data (\code{x}).
#' For example, if observations in the data measured at the level of months,
#' \code{granularity} must be \code{quarter} or \code{year}.
#'
#' @param adhoc_granularity A vector of time points defining the ad-hoc time
#' granularity desired for the aggregation. Provide only the unique time points
#' for aggregation. Accepted formats are:
#' \itemize{
#'   \item \code{Date} object (date or datetime)
#'   \item \code{character} object in ISO calendar month basic format
#'   (e.g. 2022-05)
#'   \item \code{character} object in ISO calendar week extended format
#'   (e.g. 2022-W25)
#'   \item \code{numeric} four-digit year
#' }
#' Observations are aggregated to the nearest past time point. For example, if
#' the adhoc granularity includes \code{c(1999, 2004, 2012)}, the years 1999,
#' 2000, 2001, 2002, and 2003 are aggregated to 1999, the years 2004-2011
#' are aggregated to 2004, and observations at or after 2012 are aggregated to
#' 2012.
#'
#' @param aggregation Aggregation function applied when collapsing observations
#' to the time granularity specified in \code{granularity}. One of:
#' \itemize{
#'   \item \code{mean} - take mean of all numeric variables by group and time
#'   \item \code{sum} - sum all numeric variables by group and time
#'   \item \code{max} - find maximum of each numeric variable by group and time
#'   \item \code{min} - find minimum of each numeric variable by group and time
#'   \item \code{median} - find median of each numeric variable by group and
#'   time
#'   \item \code{count} - count the number of observations by group and time
#' }
#'
#' @param fill_time_series Logical indicating whether to fill NAs in the output
#' data for any time points missing from the input data. Ignored if
#' \code{adhoc_granularity} is provided.
#'
#' @return A tibble, \code{x}, transformed to aggregate it to the desired time
#' granularity by applying the function specified in \code{aggregation}.
#'
#'
#' ## ISO 8601 formats
#'
#' Depending on the time granularity chosen, \code{aggregate_time} returns the
#' appropriately formatted time in a variable titled "jnry_(time)", i.e.:
#'
#' \itemize{
#'   \item \strong{year} - \code{jnry_year} - ISO calendar year basic format
#'   (e.g. 2022)
#'   \item \strong{half year} - \code{jnry_halfyr} - sortable half year format
#'   (e.g. 2022-H1)
#'   \item \strong{quarter} - \code{jnry_quarter} - sortable quarter format
#'   (e.g. 2022-Q2)
#'   \item \strong{month} - \code{jnry_month} - ISO calendar month basic format
#'   (e.g. 2022-05)
#'   \item \strong{week} - \code{jnry_week} - ISO calendar week extended format
#'   (e.g. 2022-W25)
#'   \item \strong{day} - \code{jnry_day} - ISO calendar date extended format
#'   (e.g. 2022-05-03)
#'   \item \strong{\emph{adhoc}} - \code{jnry_adhoctimetime} - format taken
#'   from user input
#' }
#'
#'
#' ## Filling time series
#'
#' If \code{fill_time_series = FALSE}, any year, month, etc. not represented in
#' the input data will not appear in the return data.
#'
#' If \code{fill_time_series = TRUE}, \code{aggregate_time} will fill in
#' missing time points with NAs for all variables not measuring time or
#' included in \code{grouping_vars}. Time series are filled with respect to
#' grouping variables, spanning the first and last observations within each
#' group and not globally over the input data.
#'
#' \code{fill_time_series} is ignored if \code{adhoc_granularity} is provided.
#'
#' @export
#'
#' @importFrom magrittr `%>%`
#' @importFrom rlang `.data`
#'
#' @examples
#'

aggregate_time <- function(x,
                           time = NULL,
                           grouping_vars = NULL,
                           granularity = c("year", "half year", "quarter",
                                           "month", "week", "day"),
                           adhoc_granularity = NULL,
                           aggregation = c("mean", "sum", "max", "min",
                                           "median", "count"),
                           fill_time_series = FALSE) {

  # sanity check: 'x' is a data frame / tibble
  stopifnot(is.data.frame(x))

  # match target granularity & aggregation
  aggregation <- match.arg(aggregation)
  if (!is.null(adhoc_granularity)) {
    granularity <- "adhoc"
  } else {
    granularity <- match.arg(granularity)
  }

  # sanity check: NAs & grouping_vars
  if (any(is.na(x[[time]]))) {
    stop("Time variable contains missing values.")
  }

  if (!is.null(grouping_vars)) {
    # NAs in grouping_vars
    if (
      x %>%
      dplyr::select(tidyselect::all_of(grouping_vars)) %>%
      purrr::map(is.na) %>%
      purrr::map(any) %>%
      unlist %>%
      any
    ) stop("Grouping variables contain missing values.")

    # type of grouping_vars
    stopifnot(is.character(grouping_vars),
              all(grouping_vars %in% names(x)))
  }



  #############################################################
  ## 1) measure granularity of incoming data (convert all to instants)

  # sanity check: time field reference
  stopifnot(is.character(time),
            length(time) == 1,
            time %in% names(x))

  # get incoming granularity
  incoming_gran <- x[[time]] %>% determine_granularity()

  # sanity check that time field can be parsed
  if (is.null(incoming_gran)) stop("Time variable incorrectly defined.
                                   See help(aggregate_time) for accepted
                                   formats.")

  # fudge an instant from coarser incoming time granularity
  if (incoming_gran %in% "week") {
    x[[time]] <- x[[time]] %>% wrangle_weeks()
  }
  if (incoming_gran %in% "month") {
    x[[time]] <- x[[time]] %>% lubridate::ym()
  }
  if (incoming_gran %in% "quarter") {
    x[[time]] <- x[[time]] %>% lubridate::yq()
  }
  if (incoming_gran %in% "half year") {
    x[[time]] <- x[[time]] %>% wrangle_halfyr()
  }
  if (incoming_gran %in% "year") {
    x[[time]] <- x[[time]] %>% wrangle_years()
  }



  #############################################################
  ## 2) handle ad hoc granularities

  if (granularity == "adhoc") {

    ## measure adhoc granularity
    in_adhoc_gran <- adhoc_granularity %>% determine_granularity()

    ## sanity check adhoc granularity
    if (is.null(in_adhoc_gran)) stop("Adhoc granularity incorrectly defined.
                                     See help(aggregate_time) for accepted
                                     formats.")

    # fudge an instant from different incoming adhoc granularities (??)
    if (in_adhoc_gran %in% "week") {
      adhoc_granularity <- adhoc_granularity %>% wrangle_weeks()
    }
    if (in_adhoc_gran %in% "month") {
      adhoc_granularity <- adhoc_granularity %>% lubridate::ym()
    }
    if (in_adhoc_gran %in% "quarter") {
      adhoc_granularity <- adhoc_granularity %>% lubridate::yq()
    }
    if (in_adhoc_gran %in% "half year") {
      adhoc_granularity <- adhoc_granularity %>% wrangle_halfyr()
    }
    if (in_adhoc_gran %in% "year") {
      adhoc_granularity <- adhoc_granularity %>% wrangle_years()
    }


    ## YEAR `time` format
    if (in_adhoc_gran %in% "year") {

      ## if adhoc granularity is year --> convert `time` to year
      x[[time]] <- x[[time]] %>%
        lubridate::year()

      # order adhoc granularities chronologically
      adhoc_granularity <- adhoc_granularity[order(adhoc_granularity,
                                                   decreasing = TRUE)]

      # get the adhoc time mapped into the main data
      x[["jnry_adhoctime"]] <- NA
      for (i in seq_along(adhoc_granularity)) {
        x <- x %>%
          dplyr::mutate(
            jnry_adhoctime = ifelse((is.na(jnry_adhoctime) &
                                       (.data[[time]] >= adhoc_granularity[i])),
                                    adhoc_granularity[i],
                                    jnry_adhoctime)
          )
      }

      # remove eventual NAs & report any dropped obs
      if (any(is.na(x$jnry_adhoctime))) {
        cat("\n--------------------------------------------------------\n")
        cat(
          paste0("Dropping ", sum(is.na(x$jnry_adhoctime)),
                 " observations that precede defined adhoc time granularity.")
        )
        cat("\n--------------------------------------------------------\n")

        x <- x %>% dplyr::filter(!is.na(jnry_adhoctime))
      }

      # group by each grouping variable
      if (!is.null(grouping_vars)) {
        for (i in seq_along(grouping_vars)) {
          x <- x %>%
            dplyr::group_by(.data[[grouping_vars[i]]], .add = TRUE)
        }
      }

      # group by year
      x <- x %>% dplyr::group_by(jnry_adhoctime, .add = TRUE)

      # collapse data to adhoc level, apply respective transformation, ungroup
      clpsd <- x %>%
        jnry_aggregate(aggregation = aggregation) %>%
        dplyr::ungroup()

      # adjust names to reflect aggregation
      if (!(aggregation %in% "count")) {
        names(clpsd)[!(names(clpsd) %in%
                         c("jnry_adhoctime",
                           grouping_vars))] <- paste0(
                             names(clpsd)[!(names(clpsd) %in%
                                              c("jnry_adhoctime",
                                                grouping_vars))],
                             "_",
                             aggregation
                           )
      }

      # return adhoc-time-collapsed data
      return(clpsd)


      ## DATE / DATETIME adhoc_granularity format
    } else {

      ## sanity check that the requested aggregation is sensical
      if (in_adhoc_gran %in% "quarter") {
        if (incoming_gran %in% "year") {
          stop("Data time granularity must be
               finer than adhoc time granularity.")
        }
      }
      if (in_adhoc_gran %in% "month") {
        if (incoming_gran %in% c("year", "quarter")) {
          stop("Data time granularity must be finer than adhoc time
             granularity.")
        }
      }
      if (in_adhoc_gran %in% "week") {
        if (incoming_gran %in% c("year", "quarter", "month")) {
          stop("Data time granularity must be finer than adhoc
               time granularity.")
        }
      }

      # order adhoc granularities chronologically
      adhoc_granularity <- adhoc_granularity[order(adhoc_granularity,
                                                   decreasing = TRUE)]

      # get the adhoc time mapped into the main data
      x[["jnry_adhoctime"]] <- NA
      for (i in seq_along(adhoc_granularity)) {
        x <- x %>%
          dplyr::mutate(jnry_adhoctime = ifelse(
            (is.na(jnry_adhoctime) & (.data[[time]] >= adhoc_granularity[i])),
            as.character(adhoc_granularity[i]),
            jnry_adhoctime)
          )
      }
      x <- x %>%
        dplyr::mutate(jnry_adhoctime = lubridate::ymd(jnry_adhoctime))

      # remove eventual NAs & report any dropped obs
      if (any(is.na(x$jnry_adhoctime))) {
        cat("\n--------------------------------------------------------\n")
        cat(
          paste0("Dropping ",
                 sum(is.na(x$jnry_adhoctime)),
                 " observations that precede defined adhoc time granularity.")
        )
        cat("\n--------------------------------------------------------\n")

        x <- x %>% dplyr::filter(!is.na(jnry_adhoctime))
      }

      # group by each grouping variable
      if (!is.null(grouping_vars)) {
        for (i in seq_along(grouping_vars)) {
          x <- x %>%
            dplyr::group_by(.data[[grouping_vars[i]]], .add = TRUE)
        }
      }

      # group by adhoc timing
      x <- x %>% dplyr::group_by(jnry_adhoctime, .add = TRUE)

      # collapse data to adhoc level, apply respective transformation, ungroup
      clpsd <- x %>%
        jnry_aggregate(aggregation = aggregation) %>%
        dplyr::ungroup()

      # adjust names to reflect aggregation
      if (!(aggregation %in% "count")) {
        names(clpsd)[!(names(clpsd) %in%
                         c("jnry_adhoctime",
                           grouping_vars))] <- paste0(
                             names(clpsd)[!(names(clpsd) %in%
                                              c("jnry_adhoctime",
                                                grouping_vars))],
                             "_",
                             aggregation)
      }

      # format jnry_adhoctime to match adhoc_granularity
      if (in_adhoc_gran %in% "week") {
        clpsd <- clpsd %>%
          dplyr::mutate(jnry_adhoctime = format_wk(jnry_adhoctime))
      }
      if (in_adhoc_gran %in% "month") {
        clpsd <- clpsd %>%
          dplyr::mutate(jnry_adhoctime = format_mo(jnry_adhoctime))
      }
      if (in_adhoc_gran %in% "quarter") {
        clpsd <- clpsd %>%
          dplyr::mutate(jnry_adhoctime = format_qr(jnry_adhoctime))
      }

      # return adhoc-time-collapsed data
      return(clpsd)
    }



  } else {
    #############################################################
    ## 3) regular granularities

    ##---------------------------------------------------------##
    ## YEAR
    ##---------------------------------------------------------##
    if (granularity == "year") {
      ## sanity check requested aggregation
      if (incoming_gran %in% "year") {
        stop("Data time granularity is already annual.")
      }

      # set joinery year
      x[["jnry_year"]] <- x[[time]] %>% lubridate::year()

      # group by each grouping variable
      if (!is.null(grouping_vars)) {
        for (i in seq_along(grouping_vars)) {
          x <- x %>%
            dplyr::group_by(.data[[grouping_vars[i]]], .add = TRUE)
        }
      }

      # group by year
      x <- x %>% dplyr::group_by(jnry_year, .add = TRUE)

      # collapse data to year level, apply respective transformation
      clpsd <- x %>% jnry_aggregate(aggregation = aggregation)

      # if requested, auto-fill time series with NAs
      if (fill_time_series) {
        clpsd <- clpsd %>%
          dplyr::right_join(
            clpsd %>%
              stretch_time_agg_nas(x = x,
                                   granularity = granularity),
            by = c(grouping_vars, "jnry_year")
          )
      }

      # ungroup return data
      clpsd <- clpsd %>% dplyr::ungroup()

      # adjust names to reflect aggregation
      if (!(aggregation %in% "count")) {
        names(clpsd)[!(names(clpsd) %in%
                         c("jnry_year",
                           grouping_vars))] <- paste0(
                             names(clpsd)[!(names(clpsd) %in%
                                              c("jnry_year",
                                                grouping_vars))],
                             "_",
                             aggregation
                           )
      }

      # return annualized data
      return(clpsd)
    }



    ##---------------------------------------------------------##
    ## HALF YEAR
    ##---------------------------------------------------------##
    if (granularity == "half year") {
      ## sanity check requested aggregation
      if (incoming_gran %in% "half year") {
        stop("Data time granularity is already semiannual.")
      }

      # set joinery year
      x[["jnry_halfyr"]] <- x[[time]] %>% format_hy()

      # group by each grouping variable
      if (!is.null(grouping_vars)) {
        for (i in seq_along(grouping_vars)) {
          x <- x %>%
            dplyr::group_by(.data[[grouping_vars[i]]], .add = TRUE)
        }
      }

      # group by year
      x <- x %>% dplyr::group_by(jnry_halfyr, .add = TRUE)

      # collapse data to year level, apply respective transformation
      clpsd <- x %>% jnry_aggregate(aggregation = aggregation)

      # if requested, auto-fill time series with NAs
      if (fill_time_series) {
        clpsd <- clpsd %>%
          dplyr::right_join(
            clpsd %>%
              stretch_time_agg_nas(x = x,
                                   granularity = granularity),
            by = c(grouping_vars, "jnry_halfyr")
          )
      }

      # ungroup return data
      clpsd <- clpsd %>% dplyr::ungroup()

      # adjust names to reflect aggregation
      if (!(aggregation %in% "count")) {
        names(clpsd)[!(names(clpsd) %in%
                         c("jnry_halfyr",
                           grouping_vars))] <- paste0(
                             names(clpsd)[!(names(clpsd) %in%
                                              c("jnry_halfyr",
                                                grouping_vars))],
                             "_",
                             aggregation
                           )
      }

      # return annualized data
      return(clpsd)
    }



    ##---------------------------------------------------------##
    ## QUARTER
    ##---------------------------------------------------------##
    if (granularity == "quarter") {
      ## sanity check requested aggregation
      if (incoming_gran %in% c("year")) {
        stop("Data time granularity must be finer than requested
             time granularity.")
      }
      if (incoming_gran %in% "quarter") {
        stop("Data time granularity is already quarter")
      }

      # set joinery quarter
      x[["jnry_quarter"]] <- x[[time]] %>% format_qr()

      if (!is.null(grouping_vars)) {
        # group by each grouping variable
        for (i in seq_along(grouping_vars)) {
          x <- x %>%
            dplyr::group_by(.data[[grouping_vars[i]]], .add = TRUE)
        }
      }

      # group by quarter
      x <- x %>% dplyr::group_by(jnry_quarter, .add = TRUE)

      # collapse data to quarter level, apply respective transformation
      clpsd <- x %>% jnry_aggregate(aggregation = aggregation)

      # if requested, auto-fill time series with NAs
      if (fill_time_series) {
        clpsd <- clpsd %>%
          dplyr::right_join(
            clpsd %>%
              stretch_time_agg_nas(x = x,
                                   granularity = granularity),
            by = c(grouping_vars, "jnry_quarter")
          )
      }

      # ungroup return data
      clpsd <- clpsd %>% dplyr::ungroup()

      # adjust names to reflect aggregation
      if (!(aggregation %in% "count")) {
        names(clpsd)[!(names(clpsd) %in%
                         c("jnry_quarter",
                           grouping_vars))] <- paste0(
                             names(clpsd)[!(names(clpsd) %in%
                                              c("jnry_quarter",
                                                grouping_vars))],
                             "_",
                             aggregation
                           )
      }

      # return quarterly data
      return(clpsd)
    }



    ##---------------------------------------------------------##
    ## MONTH
    ##---------------------------------------------------------##
    if (granularity == "month") {
      ## sanity check requested aggregation
      if (incoming_gran %in% c("year", "quarter")) {
        stop("Data time granularity must be finer than requested
             time granularity.")
      }
      if (incoming_gran %in% "month") {
        stop("Data time granularity is already month.")
      }

      # set joinery month
      x[["jnry_month"]] <- x[[time]] %>% format_mo()

      if (!is.null(grouping_vars)) {
        # group by each grouping variable
        for (i in seq_along(grouping_vars)) {
          x <- x %>%
            dplyr::group_by(.data[[grouping_vars[i]]], .add = TRUE)
        }
      }

      # group by month
      x <- x %>% dplyr::group_by(jnry_month, .add = TRUE)

      # collapse data to month level, apply respective transformation
      clpsd <- x %>% jnry_aggregate(aggregation = aggregation)

      # if requested, auto-fill time series with NAs
      if (fill_time_series) {
        clpsd <- clpsd %>%
          dplyr::right_join(
            clpsd %>%
              stretch_time_agg_nas(x = x,
                                   granularity = granularity),
            by = c(grouping_vars, "jnry_month")
          )
      }

      # ungroup return data
      clpsd <- clpsd %>% dplyr::ungroup()

      # adjust names to reflect aggregation
      if (!(aggregation %in% "count")) {
        names(clpsd)[!(names(clpsd) %in%
                         c("jnry_month",
                           grouping_vars))] <- paste0(
                             names(clpsd)[!(names(clpsd) %in%
                                              c("jnry_month",
                                                grouping_vars))],
                             "_",
                             aggregation
                           )
      }

      # return monthly data
      return(clpsd)
    }



    ##---------------------------------------------------------##
    ## WEEK
    ##---------------------------------------------------------##
    if (granularity == "week") {
      ## sanity check requested aggregation
      if (incoming_gran %in% c("year", "quarter", "month")) {
        stop("Data time granularity must be finer than requested
             time granularity.")
      }
      if (incoming_gran %in% "week") {
        stop("Data time granularity is already week.")
      }

      # set joinery week
      x[["jnry_week"]] <- x[[time]] %>% format_wk()

      if (!is.null(grouping_vars)) {
        # group by each grouping variable
        for (i in seq_along(grouping_vars)) {
          x <- x %>%
            dplyr::group_by(.data[[grouping_vars[i]]], .add = TRUE)
        }
      }

      # group by week
      x <- x %>% dplyr::group_by(jnry_week, .add = TRUE)

      # collapse data to week level, apply respective transformation
      clpsd <- x %>% jnry_aggregate(aggregation = aggregation)

      # if requested, auto-fill time series with NAs
      if (fill_time_series) {
        clpsd <- clpsd %>%
          dplyr::right_join(
            clpsd %>%
              stretch_time_agg_nas(x = x,
                                   granularity = granularity),
            by = c(grouping_vars, "jnry_week")
          )
      }

      # ungroup return data
      clpsd <- clpsd %>% dplyr::ungroup()

      # adjust names to reflect aggregation
      if (!(aggregation %in% "count")) {
        names(clpsd)[!(names(clpsd) %in%
                         c("jnry_week",
                           grouping_vars))] <- paste0(
                             names(clpsd)[!(names(clpsd) %in%
                                              c("jnry_week",
                                                grouping_vars))],
                             "_",
                             aggregation
                           )
      }

      # return weekly data
      return(clpsd)
    }



    ##---------------------------------------------------------##
    ## DAY
    ##---------------------------------------------------------##
    if (granularity == "day") {
      ## sanity check requested aggregation
      if (incoming_gran %in% c("year", "quarter", "month", "week")) {
        stop("Data time granularity must be finer than requested
             time granularity.")
      }

      # set joinery day
      x[["jnry_day"]] <- lubridate::as_date(x[[time]])

      if (!is.null(grouping_vars)) {
        # group by each grouping variable
        for (i in seq_along(grouping_vars)) {
          x <- x %>%
            dplyr::group_by(.data[[grouping_vars[i]]], .add = TRUE)
        }
      }

      # group by day
      x <- x %>% dplyr::group_by(jnry_day, .add = TRUE)

      # collapse data to day level, apply respective transformation
      clpsd <- x %>% jnry_aggregate(aggregation = aggregation)

      # if requested, auto-fill time series with NAs
      if (fill_time_series) {
        clpsd <- clpsd %>%
          dplyr::right_join(
            clpsd %>%
              stretch_time_agg_nas(x = x,
                                   granularity = granularity) %>%
              dplyr::mutate(jnry_day = lubridate::ymd(jnry_day)),
            by = c(grouping_vars, "jnry_day")
          )
      }

      # ungroup return data
      clpsd <- clpsd %>% dplyr::ungroup()

      # adjust names to reflect aggregation
      if (!(aggregation %in% "count")) {
        names(clpsd)[!(names(clpsd) %in%
                         c("jnry_day",
                           grouping_vars))] <- paste0(
                             names(clpsd)[!(names(clpsd) %in%
                                              c("jnry_day",
                                                grouping_vars))],
                             "_",
                             aggregation
                           )
      }

      # return daily data
      return(clpsd)
    }
  }
}
## to be added to documentation later if group-wise adhoc granularities
## are added to the functionality
# If \code{grouping_vars} are provided, then this must be a
# data frame or tibble
# including \code{grouping_vars} and their respective ad-hoc time
# aggregation points.
# The variable containing the ad-hoc time granularity should have the
# same name as \code{time}.
