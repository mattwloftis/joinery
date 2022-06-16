
# determine granularity of incoming time variables

determine_granularity <- function(xx) {

  # instant
  if (lubridate::is.instant(xx)) return("instant")

  # week
  if (all(stringr::str_detect(xx, "\\d{4}-W\\d{2}"))) return("week")

  # month
  if (all(stringr::str_detect(xx, "\\d{4}-\\d{2}"))) return("month")

  # quarter
  if (all(stringr::str_detect(xx, "\\d{4}-Q\\d"))) return("quarter")

  # half year
  if (all(stringr::str_detect(xx, "\\d{4}-H\\d"))) return("half year")

  # year
  if (all(stringr::str_length(xx) == 4) && all(is.numeric(xx))) return("year")
}
