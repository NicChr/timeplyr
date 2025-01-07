# Set number of data.table threads to 2
data.table::setDTthreads(threads = 2L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

options(timeplyr.interval_sub_formatter = identity)
options(timeplyr.use_intervals = FALSE)

test_that("time breaks", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)
  x_max <- max(x)
  tseq <- time_grid(x, "hour")
  x_missed <- time_cast(setdiff(tseq, x), tseq)

  res1 <- time_breaks(x, n = 5)
  res2 <- time_breaks(x, n = 5, "week")
  res3 <- time_breaks(x, n = 100, "month")
  res4 <- time_breaks(x, n = 100, dmonths(1))

  # res5 <- time_breaks(x, n = 5, "week", n_at_most = FALSE)
  expect_equal(res3, time_grid(x, "month"))
  expect_equal(res4, time_grid(x, dmonths(1)))
  expect_equal(time_diff(res1, dplyr::lag(res1), "months"),
                         c(NA, rep(-3, 3)))
  expect_equal(time_diff(res2,
                                   dplyr::lag(res2),
                                   "weeks", time_type = "period"),
                         c(NA, rep(-11, 4)))
  # expect_equal(time_diff(res5,
  #                                  dplyr::lag(res5),
  #                                  "weeks", time_type = "period"),
  #                        c(NA, rep(-10, 5)))
  # expect_error(supressWarnings(time_breaks(x, n = Inf)))
  expect_equal(time_breaks(x, n = 100, "month",
                                         from = start1,
                                         to = end2 + period_unit("months")(4)),
                             time_span(x, "month",
                                       from = start1,
                                       to = end2 + period_unit("months")(4)))
  expect_equal(time_breaks(x, n = 100, "month",
                                         from = start1),
                             time_span(x, "month",
                                       from = start1))
  expect_equal(time_breaks(x, n = 100, "month",
                                         to = end2),
                             time_span(x, "month",
                                       to = end2))
  expect_equal(time_breaks(x, n = Inf, "hour"),
                             time_span(x, "hour"))
})

test_that("time cut", {
  start1 <- lubridate::ymd_hms("2013-03-16 11:43:48",
                               tz = "Europe/London")
  end1 <- start1 + lubridate::ddays(10)
  start2 <- lubridate::as_date(start1)
  end2 <- lubridate::as_date(end1)
  x <- nycflights13::flights$time_hour
  y <- lubridate::as_date(x)
  x_max <- max(x)
  tseq <- time_span(x, "hour")
  x_missed <- time_cast(setdiff(tseq, x), tseq)

  res1 <- time_cut(x, n = 5)
  expect_equal(res1, time_summarisev(x, list("months" = 3),
                                             sort = FALSE, unique = FALSE))
  res2 <- time_cut(x, n = 5, "week",
                   from = start2, to = end1)
  expect_equal(sum(is.na(res2)),
                             length(x[x < time_cast(start2, x) |
                                        x > time_cast(end1, x)]))

  expect_equal(
    time_cut(c(1, 5, 10), n = 100, as_interval = TRUE, 3),
    structure(list(start = c(1, 4, 10), end = c(4, 7, 13)), class = c("time_interval",
                                                                      "vctrs_rcrd", "vctrs_vctr"))
  )
  expect_equal(
    time_cut(c(1, 5, 10), n = Inf, as_interval = TRUE, 3),
    structure(list(start = c(1, 4, 10), end = c(4, 7, 13)), class = c("time_interval",
                                                                      "vctrs_rcrd", "vctrs_vctr"))
  )

  expect_equal(time_cut(x, n = Inf), x)
  expect_equal(time_cut(y, n = Inf), y)
  expect_equal(time_cut(x, n = 10^5), x)
  expect_equal(time_cut(y, n = 10^5), y)
  expect_equal(time_cut(x, n = Inf, "weeks"),
               time_aggregate(x, "weeks"))
  expect_equal(time_cut(y, n = Inf, "weeks"),
               time_aggregate(y, "weeks"))
  expect_equal(
    time_cut(y, n = Inf, "weeks"),
    time_aggregate(y, "weeks")
  )
})

reset_timeplyr_options()
