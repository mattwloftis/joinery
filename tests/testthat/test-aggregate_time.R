
data("daily_fake")

test_that("aggregations from daily work", {
  # year
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "year",
                                aggregation = "mean") %>%
                 dim(),
               c(28, 2))

  # half year
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "half year",
                                aggregation = "mean") %>%
                 dim(),
               c(55, 2))

  # quarter
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "quarter",
                                aggregation = "mean") %>%
                 dim(),
               c(110, 2))

  # month
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "month",
                                aggregation = "mean") %>%
                 dim(),
               c(329, 2))

  # week
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "week",
                                aggregation = "mean") %>%
                 dim(),
               c(1434, 2))

})

dfwk <- daily_fake %>%
  aggregate_time(time = "date",
                 granularity = "week",
                 aggregation = "mean")

test_that("aggregations from week work", {
  # year
  expect_equal(dfwk %>%
                 aggregate_time(time = "jnry_week",
                                granularity = "year",
                                aggregation = "mean") %>%
                 dim(),
               c(28, 2))

  # half year
  expect_equal(dfwk %>%
                 aggregate_time(time = "jnry_week",
                                granularity = "half year",
                                aggregation = "mean") %>%
                 dim(),
               c(55, 2))

  # quarter
  expect_equal(dfwk %>%
                 aggregate_time(time = "jnry_week",
                                granularity = "quarter",
                                aggregation = "mean") %>%
                 dim(),
               c(110, 2))

  # month
  expect_equal(dfwk %>%
                 aggregate_time(time = "jnry_week",
                                granularity = "month",
                                aggregation = "mean") %>%
                 dim(),
               c(329, 2))
})

dfmo <- daily_fake %>%
  aggregate_time(time = "date",
                 granularity = "month",
                 aggregation = "mean")

test_that("aggregations from month work", {
  # year
  expect_equal(dfmo %>%
                 aggregate_time(time = "jnry_month",
                                granularity = "year",
                                aggregation = "mean") %>%
                 dim(),
               c(28, 2))

  # half year
  expect_equal(dfmo %>%
                 aggregate_time(time = "jnry_month",
                                granularity = "half year",
                                aggregation = "mean") %>%
                 dim(),
               c(55, 2))

  # quarter
  expect_equal(dfmo %>%
                 aggregate_time(time = "jnry_month",
                                granularity = "quarter",
                                aggregation = "mean") %>%
                 dim(),
               c(110, 2))
})

dfqr <- daily_fake %>%
  aggregate_time(time = "date",
                 granularity = "quarter",
                 aggregation = "mean")

test_that("aggregations from quarter work", {
  # year
  expect_equal(dfqr %>%
                 aggregate_time(time = "jnry_quarter",
                                granularity = "year",
                                aggregation = "mean") %>%
                 dim(),
               c(28, 2))

  # half year
  expect_equal(dfqr %>%
                 aggregate_time(time = "jnry_quarter",
                                granularity = "half year",
                                aggregation = "mean") %>%
                 dim(),
               c(55, 2))
})

dfhy <- daily_fake %>%
  aggregate_time(time = "date",
                 granularity = "half year",
                 aggregation = "mean")

test_that("aggregations from half year work", {
  # year
  expect_equal(dfhy %>%
                 aggregate_time(time = "jnry_halfyr",
                                granularity = "year",
                                aggregation = "mean") %>%
                 dim(),
               c(28, 2))
})

test_that("aggregations with grouping work", {
  # single grouping
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "year",
                                grouping_vars = "b",
                                aggregation = "mean") %>%
                 dim(),
               c(84, 3))

  # two groupings
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "year",
                                grouping_vars = c("b", "c"),
                                aggregation = "mean") %>%
                 dim(),
               c(93, 4))
})

test_that("aggregation methods work", {
  # mean
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "year",
                                aggregation = "mean") %>%
                 dplyr::slice(1:5) %>%
                 dplyr::pull(a_mean) %>%
                 round(digits = 4),
               c(0.0189, -0.0850, -0.0325, 0.0605, -0.0444))

  # median
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "year",
                                aggregation = "median") %>%
                 dplyr::slice(1:5) %>%
                 dplyr::pull(a_median) %>%
                 round(digits = 4),
               c(-0.0298, -0.0327, -0.0833, 0.1133, -0.0706))

  # sum
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "year",
                                aggregation = "sum") %>%
                 dplyr::slice(1:5) %>%
                 dplyr::pull(a_sum) %>%
                 round(digits = 4),
               c(20.7478, -93.1012, -35.5685, 66.2499, -48.7902))

  # max
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "year",
                                aggregation = "max") %>%
                 dplyr::slice(1:5) %>%
                 dplyr::pull(a_max) %>%
                 round(digits = 4),
               c(3.0438, 3.1959, 2.7058, 2.6909, 3.1679))

  # min
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "year",
                                aggregation = "min") %>%
                 dplyr::slice(1:5) %>%
                 dplyr::pull(a_min) %>%
                 round(digits = 4),
               c(-3.2332, -3.3961, -2.9067, -2.9305, -3.1216))

  # count
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "year",
                                aggregation = "count") %>%
                 dplyr::slice(1:5) %>%
                 dplyr::pull(jnry_count),
               c(1098, 1095, 1095, 1095, 1098))

  # mean X group aggregate
  expect_equal(daily_fake %>%
                 aggregate_time(time = "date",
                                granularity = "week",
                                grouping_vars = "b",
                                aggregation = "mean") %>%
                 dplyr::slice(3000:3005) %>%
                 dplyr::pull(a_mean) %>%
                 round(digits = 4),
               c(-0.0491, -0.1417, 0.1621, -0.1454, -0.6528, 0.1907))
})
