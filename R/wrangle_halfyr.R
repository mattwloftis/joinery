
# convert HALF year incoming granularity time vars to lubridate::instant

wrangle_halfyr <- function(xx) {

  # pull half yrs
  mos <- xx %>% str_extract("-H\\d")

  # convert to correct months
  hy <- ifelse(str_detect(mos, "1"), 1, 7) %>%
    str_pad(width = 2,
            side = "left",
            pad = "0")

  # package into a date
  xx <- xx %>%
    str_extract("\\d{4}") %>%
    str_c("-", hy) %>%
    lubridate::ym()

  return(xx)
}
