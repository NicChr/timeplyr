
testthat::test_that("Expect error", {
  testthat::expect_error(age_years(Inf))
  testthat::expect_error(age_years(-Inf))
  testthat::expect_error(age_years(NaN))
  testthat::expect_error(age_years(NULL))
})

testthat::test_that("Expect zero length vector", {
  empty_values <- list(as.POSIXlt(numeric(0)),
                       lubridate::Date(0),
                       lubridate::POSIXct(0))
  testthat::expect_identical(lapply(empty_values, function(x) age_years(x)),
                             list(integer(0), integer(0), integer(0)))
})

testthat::test_that("Expect NA", {
  # testthat::expect_identical(suppressWarnings(age_years(-Inf)),
  #                            NA_real_)
  # testthat::expect_identical(suppressWarnings(age_years(Inf)),
  #                            NA_real_)
  # testthat::expect_identical(age_years(NA),
  #                            NA_real_)
  testthat::expect_identical(age_years(lubridate::NA_Date_),
                             NA_integer_)
  testthat::expect_identical(age_years(lubridate::NA_POSIXct_),
                             NA_integer_)
  testthat::expect_identical(age_years(as.POSIXlt(NA)),
                             NA_integer_)
  # x <- c(NA, Inf, -Inf, NaN)
  # testthat::expect_identical(suppressWarnings(age_years(x)), rep(NA_real_, 4))
})

testthat::test_that("Leap year", {
  leap1 <- lubridate::dmy("29-02-2020")
  leap2 <- lubridate::dmy("28-02-2021")
  leap3 <- lubridate::dmy("01-03-2021")
  testthat::expect_identical(age_years(leap1, leap2), 0L)
  testthat::expect_identical(age_years(leap1, leap3), 1L)
})

# testthat::test_that("DST", {
#   dst_start1 <- lubridate::dmy_hms("27-03-2022 00:30:00", tz = "Europe/London")
#   dst_start2 <- lubridate::dmy_hms("27-03-2022 02:30:00", tz = "Europe/London")
#   dst_start3 <- lubridate::dmy_hms("27-04-2022 00:30:00", tz = "Europe/London")
#   testthat::expect_identical(age_years(dst_start1, dst_start3, units = "months", round_down = FALSE), 1)
#   testthat::expect_identical(age_years(dst_start1, dst_start3, units = "days", round_down = FALSE), 31)
#   testthat::expect_identical(age_years(dst_start1, dst_start2, units = "hours", date_class = "duration", round_down = FALSE), 1)
# })
