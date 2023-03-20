testthat::test_that("time diff", {
  start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
                               tz = "GB")
  end1 <- start1 + lubridate::ddays(37)
  end2 <- start1 + lubridate::days(37)
  testthat::expect_identical(time_diff(3, 27, by = 1),
                             27 - 3)
  testthat::expect_identical(time_diff(3, 27, by = 3.5),
                             (27 - 3)/3.5)
  testthat::expect_identical(time_diff(3, 27, by = 0), Inf)
  testthat::expect_identical(time_diff(-3, -27, by = 0), -Inf)
  testthat::expect_identical(time_diff(start1, end2, by = "2 days",
                                       type = "period"),
                             18.5)
  testthat::expect_identical(time_diff(start1, end2, by = "2 days"),
                             18.5)
  testthat::expect_identical(time_diff(start1, end1, by = "48 hours"),
                             18.5)
  testthat::expect_identical(time_diff(start1, end1, by = "2 days",
                                       type = "duration"),
                             18.5)
  testthat::expect_equal(time_diff(start1, end1, by = "-2 days",
                                       type = "duration"),
                             -18.5)
  testthat::expect_equal(time_diff(end1, start1, by = list("days" = 2),
                                   type = "duration"),
                         -18.5)
  testthat::expect_equal(time_diff(end1, start1, by = list("days" = -2),
                                   type = "duration"),
                         18.5)
  testthat::expect_identical(time_diff(lubridate::Date(0), lubridate::Date(0), by = "2 days",
                                       type = "duration"),
                             numeric(0))
  testthat::expect_identical(time_diff(lubridate::Date(0), lubridate::POSIXct(0), by = "2 days",
                                       type = "duration"),
                             numeric(0))
  testthat::expect_identical(time_diff(lubridate::today(), lubridate::POSIXct(0), by = "2 days",
                                       type = "duration"),
                             numeric(0))
  for (i in seq_along(.duration_units)){
    unit <- .duration_units[i]
    by <- setnames(list(1), unit)
    testthat::expect_identical(time_diff(start1, end1, by = by,
                                         type = "duration"),
                               lubridate::interval(start1, end1) /
                                 duration_unit(unit)(1))

  }
  for (i in seq_along(.period_units)){
    unit <- .period_units[i]
    by <- setnames(list(1), unit)
    testthat::expect_identical(time_diff(start1, end1, by = by,
                                         type = "period"),
                               lubridate::interval(start1, end1) /
                                 period_unit(unit)(1))

  }
  leap1 <- lubridate::dmy("29-02-2020")
  leap2 <- lubridate::dmy("28-02-2021")
  leap3 <- lubridate::dmy("01-03-2021")
  testthat::expect_true(floor(
    time_diff(leap1, leap2, by = "year", as_period = FALSE,
              type = "period")
  ) == 1)
  testthat::expect_true(floor(
    time_diff(leap1, leap2, by = "year", as_period = TRUE,
              type = "period")
  ) == 0)
  testthat::expect_true(floor(
    time_diff(leap1, leap3, by = "year", as_period = FALSE,
              type = "period")
  ) == 1)
  testthat::expect_true(
    floor(
      time_diff(leap1, leap3, by = "year", as_period = TRUE,
                type = "period")
    ) == 1)

  # Test vectorization
  seq1 <- seq(-10, 10, 2)
  res1 <- time_diff(start1, end1,
                   by = list("days" = seq1),
                   type = "duration")
  res2 <- numeric(length(res1))
  for (i in seq_along(seq1)){
    res2[i] <- time_diff(start1, end1,
                         by = list("days" = seq1[i]),
                         type = "duration")
  }
  testthat::expect_equal(res1, res2)

  res3 <- time_diff(start1, end2,
                    by = list("days" = seq1),
                    type = "period")
  res4 <- numeric(length(res3))
  for (i in seq_along(seq1)){
    res4[i] <- time_diff(start1, end2,
                         by = list("days" = seq1[i]),
                         type = "period")
  }
  testthat::expect_equal(res3, res4)

  res5 <- time_diff(start1, end1 + lubridate::ddays(seq1),
                    by = "days",
                    type = "duration")
  res6 <- numeric(length(res5))
  for (i in seq_along(seq1)){
    res6[i] <- time_diff(start1, end1 + lubridate::ddays(seq1[i]),
                         by = list("days" = 1),
                         type = "duration")
  }
  testthat::expect_equal(res5, res6)
})
