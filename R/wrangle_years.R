
# convert year incoming granularity time vars to lubridate::instant

wrangle_years <- function(xx) {

  xx <- xx %>% paste0("-01") %>% lubridate::ym()

  return(xx)
}
