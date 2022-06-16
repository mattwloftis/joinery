## code to prepare `daily_fake` dataset goes here

set.seed(1234)

daily_fake <- tibble::tibble(
  date = rep(lubridate::dmy("01-01-2000") + 0:9999, times = 3),
  a = rep(rnorm(10000), times = 3),
  b = rep(letters[1:3], each = 10000),
  c = rep(letters[7:16], each = 3000)
)

usethis::use_data(daily_fake, overwrite = TRUE)
