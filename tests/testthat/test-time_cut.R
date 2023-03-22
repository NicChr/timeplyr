testthat::test_that("time breaks", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "GB")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)
  x_max <- max(x)
  tseq <- time_span(x, by = "hour")
  x_missed <- time_cast(setdiff(tseq, x), tseq)

  res1 <- time_breaks(x, n = 5)
  res2 <- time_breaks(x, n = 5, by = "week")
  res3 <- time_breaks(x, n = Inf, by = "month")
  res4 <- time_breaks(x, n = Inf, by = "month", seq_type = "duration")
  res5 <- time_breaks(x, n = 5, by = "week", n_at_most = FALSE)
  testthat::expect_identical(res3, time_span(x, by = "month"))
  testthat::expect_identical(res4, time_span(x, by = "month",
                                             seq_type = "duration"))
  testthat::expect_equal(time_diff(res1,
                                   dplyr::lag(res1),
                                   by = "months", type = "period"),
                         c(NA, rep(-3, 3)))
  testthat::expect_equal(time_diff(res2,
                                   dplyr::lag(res2),
                                   by = "weeks", type = "period"),
                         c(NA, rep(-11, 4)))
  testthat::expect_equal(time_diff(res5,
                                   dplyr::lag(res5),
                                   by = "weeks", type = "period"),
                         c(NA, rep(-10, 5)))

  testthat::expect_identical(time_breaks(x, n = Inf, by = "month",
                                         from = start1,
                                         to = end2 + period_unit("months")(4)),
                             time_span(x, by = "month",
                                       from = start1,
                                       to = end2 + period_unit("months")(4)))
  testthat::expect_identical(time_breaks(x, n = Inf, by = "month",
                                         from = start1),
                             time_span(x, by = "month",
                                       from = start1))
  testthat::expect_identical(time_breaks(x, n = Inf, by = "month",
                                         to = end2),
                             time_span(x, by = "month",
                                       to = end2))
  testthat::expect_identical(time_breaks(x, n = Inf,
                                      n_at_most = FALSE),
                             time_span(x, by = "hour"))
})

testthat::test_that("time cut", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "GB")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)
  x_max <- max(x)
  tseq <- time_span(x, by = "hour")
  x_missed <- time_cast(setdiff(tseq, x), tseq)

  res1 <- time_cut(x, n = 5, as_factor = FALSE)
  testthat::expect_identical(res1,
                             time_summarisev(x, by = list("months" = 3),
                                             sort = FALSE, unique = FALSE))
  res2 <- time_cut(x, n = 5, by = "week",
                   from = start2, to = end1)
  testthat::expect_equal(sum(is.na(res2)),
                             length(x[x < time_cast(start2, x) |
                                        x > time_cast(end1, x)]))
  testthat::expect_equal(levels(res2),
                         c("[2013-03-15 20:00:00, 2013-03-22 20:00:00)",
                           "[2013-03-22 20:00:00, 2013-03-26 07:43:48]"))
  testthat::expect_identical(time_cut(x, n = Inf, by = "30 minutes",
                                      n_at_most = FALSE),
                             time_cut(x, n = Inf, by = "hour",
                                      n_at_most = FALSE))
  testthat::expect_identical(time_cut(x, n = Inf, by = "30 minutes",
                                      n_at_most = TRUE),
                             time_cut(x, n = Inf, by = "hour"))
})
