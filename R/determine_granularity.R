
# determine granularity of incoming time variables

determine_granularity <- function(xx) {

  # instant
  if (lubridate::is.instant(xx)) return("instant")

  # week
  if (all(str_detect(xx, "\\d{4}-W\\d{2}"))) return("week")

  # month
  if (all(str_detect(xx, "\\d{4}-\\d{2}"))) return("month")

  # quarter
  if (all(str_detect(xx, "\\d{4}-Q\\d"))) return("quarter")

  # year
  if (all(str_length(xx) == 4) & all(is.numeric(xx))) return("year")
}
