

testthat::test_that("Compare to lubridate", {
  x <- time_seq(lubridate::today(),
                lubridate::today() + lubridate::years(20),
                time_by = "day")
  x2 <- time_seq(lubridate::now(),
                lubridate::now() + lubridate::years(20),
                time_by = "12 hours")
  x <- sample(x)
  x2 <- sample(x2)
  iso_week2 <- function(x){
    paste0(lubridate::isoyear(x), "-W",
           stringr::str_pad(lubridate::isoweek(x),
                            width = 2, pad = "0",
                            side = "left"))
  }
  iso_week3 <- function(x){
    paste0(lubridate::isoyear(x), "-W",
           stringr::str_pad(lubridate::isoweek(x),
                            width = 2, pad = "0",
                            side = "left"), "-",
           lubridate::wday(x, week_start = 1))
  }
  iso_week4 <- function(x){
    paste0("W", stringr::str_pad(lubridate::isoweek(x),
                            width = 2, pad = "0",
                            side = "left"), "-",
           lubridate::wday(x, week_start = 1))
  }
  iso_week5 <- function(x){
    paste0("W", stringr::str_pad(lubridate::isoweek(x),
                            width = 2, pad = "0",
                            side = "left"))
  }
  testthat::expect_identical(isoday(x),
                             as.integer(lubridate::wday(x,
                                                        week_start = 1)))
  testthat::expect_identical(isoday(x2),
                             as.integer(lubridate::wday(x2,
                                                        week_start = 1)))
  testthat::expect_identical(iso_week(x, day = FALSE),
                             iso_week2(x))
  testthat::expect_identical(iso_week(x, day = TRUE),
                             iso_week3(x))
  testthat::expect_identical(iso_week(x, year = FALSE, day = TRUE),
                             iso_week4(x))
  testthat::expect_identical(iso_week(x, year = FALSE, day = FALSE),
                             iso_week5(x))
})
