# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

testthat::test_that("time diff", {
  start1 <- lubridate::ymd_hms("2023-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(37)
  end2 <- start1 + lubridate::days(37)
  testthat::expect_identical(as.double(difftime(end1, start1, units = "secs")),
                             time_diff(start1, end1, time_by = 1))
  testthat::expect_identical(as.double(difftime(
    lubridate::with_tz(lubridate::as_date(end1), tzone = "Europe/London"),
    start1, units = "secs")),
    time_diff(start1, lubridate::as_date(end1), time_by = 1))
  testthat::expect_identical(as.double(difftime(lubridate::as_date(end1),
                                                lubridate::as_date(start1),
                                                units = "days")),
                             time_diff(lubridate::as_date(start1),
                                       lubridate::as_date(end1),
                                       time_by = 1))
  testthat::expect_identical(time_diff(3, 27, time_by = 1),
                             27 - 3)
  testthat::expect_identical(time_diff(3, 27, time_by = 3.5),
                             (27 - 3)/3.5)
  testthat::expect_identical(time_diff(3, 27, time_by = 0), Inf)
  testthat::expect_identical(time_diff(-3, -27, time_by = 0), -Inf)
  testthat::expect_identical(time_diff(start1, end2, time_by = "2 days",
                                       time_type = "period"),
                             18.5)
  testthat::expect_identical(time_diff(start1, end2, time_by = "2 days"),
                             18.5)
  testthat::expect_identical(time_diff(start1, end1, time_by = "48 hours"),
                             18.5)
  testthat::expect_identical(time_diff(start1, end1, time_by = "2 days",
                                       time_type = "duration"),
                             18.5)
  testthat::expect_equal(time_diff(start1, end1, time_by = "-2 days",
                                       time_type = "duration"),
                             -18.5)
  testthat::expect_equal(time_diff(end1, start1, time_by = list("days" = 2),
                                   time_type = "duration"),
                         -18.5)
  testthat::expect_equal(time_diff(end1, start1, time_by = list("days" = -2),
                                   time_type = "duration"),
                         18.5)
  testthat::expect_identical(time_diff(lubridate::Date(0), lubridate::Date(0), time_by = "2 days",
                                       time_type = "duration"),
                             numeric(0))
  testthat::expect_identical(time_diff(lubridate::Date(0), lubridate::POSIXct(0), time_by = "2 days",
                                       time_type = "duration"),
                             numeric(0))
  testthat::expect_identical(time_diff(lubridate::today(), lubridate::POSIXct(0), time_by = "2 days",
                                       time_type = "duration"),
                             numeric(0))
  for (i in seq_along(.duration_units)){
    unit <- .duration_units[i]
    by <- add_names(list(1), unit)
    testthat::expect_identical(time_diff(start1, end1, time_by = by,
                                         time_type = "duration"),
                               lubridate::interval(start1, end1) /
                                 duration_unit(unit)(1))

  }
  for (i in seq_along(.period_units)){
    unit <- .period_units[i]
    by <- add_names(list(1), unit)
    testthat::expect_identical(time_diff(start1, end1, time_by = by,
                                         time_type = "period"),
                               lubridate::interval(start1, end1) /
                                 period_unit(unit)(1))

  }
  leap1 <- lubridate::dmy("29-02-2020")
  leap2 <- lubridate::dmy("28-02-2021")
  leap3 <- lubridate::dmy("01-03-2021")
  testthat::expect_true(floor(
    time_diff(leap1, leap2, time_by = "year", as_period = FALSE,
              time_type = "period")
  ) == 1)
  testthat::expect_true(floor(
    time_diff(leap1, leap2, time_by = "year", as_period = TRUE,
              time_type = "period")
  ) == 0)
  testthat::expect_true(floor(
    time_diff(leap1, leap3, time_by = "year", as_period = FALSE,
              time_type = "period")
  ) == 1)
  testthat::expect_true(
    floor(
      time_diff(leap1, leap3, time_by = "year", as_period = TRUE,
                time_type = "period")
    ) == 1)

  # Test vectorization
  seq1 <- seq(-10, 10, 2)
  res1 <- time_diff(start1, end1,
                   time_by = list("days" = seq1),
                   time_type = "duration")
  res2 <- numeric(length(res1))
  for (i in seq_along(seq1)){
    res2[i] <- time_diff(start1, end1,
                         time_by = list("days" = seq1[i]),
                         time_type = "duration")
  }
  testthat::expect_equal(res1, res2)

  res3 <- time_diff(start1, end2,
                    time_by = list("days" = seq1),
                    time_type = "period")
  res4 <- numeric(length(res3))
  for (i in seq_along(seq1)){
    res4[i] <- time_diff(start1, end2,
                         time_by = list("days" = seq1[i]),
                         time_type = "period")
  }
  testthat::expect_equal(res3, res4)

  res5 <- time_diff(start1, end1 + lubridate::ddays(seq1),
                    time_by = "days",
                    time_type = "duration")
  res6 <- numeric(length(res5))
  for (i in seq_along(seq1)){
    res6[i] <- time_diff(start1, end1 + lubridate::ddays(seq1[i]),
                         time_by = list("days" = 1),
                         time_type = "duration")
  }
  testthat::expect_equal(res5, res6)
})
